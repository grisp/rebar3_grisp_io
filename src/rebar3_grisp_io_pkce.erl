-module(rebar3_grisp_io_pkce).

% API
-export([auth/1]).

%--- Macros --------------------------------------------------------------------

-define(LOGIN_TIMEOUT, 300000).
-define(REQUEST_TIMEOUT, 10000).
-define(MAX_BODY, 4096).

%--- API -----------------------------------------------------------------------

%% @doc Authenticate through the browser and redeem the loopback callback code.
-spec auth(rebar_state:t()) -> binary().
auth(RState) ->
    Verifier = base64url(crypto:strong_rand_bytes(32)),
    Challenge = base64url(crypto:hash(sha256, Verifier)),
    Nonce = base64url(crypto:strong_rand_bytes(24)),
    Deadline = erlang:monotonic_time(millisecond) + ?LOGIN_TIMEOUT,
    Listen = open_listener(),
    try
        {ok, Port} = inet:port(Listen),
        log("Opening a CLI login session"),
        Session = rebar3_grisp_io_api:cli_session(RState, Challenge, Nonce, Port),
        #{<<"id">> := SessionId, <<"auth_url">> := AuthUrl} = Session,
        Code = receive_code(Listen, Nonce, AuthUrl, Deadline),
        %% Close the listener before the code is redeemed.
        gen_tcp:close(Listen),
        log("Redeeming the authentication code"),
        Token = rebar3_grisp_io_api:cli_redeem(RState, SessionId, Code, Verifier),
        log("Access token received"),
        Token
    after
        close_listener(Listen)
    end.

%--- Internals -----------------------------------------------------------------

base64url(Data) ->
    Encoded = base64:encode(Data),
    NoPlus = binary:replace(Encoded, <<"+">>, <<"-">>, [global]),
    NoSlash = binary:replace(NoPlus, <<"/">>, <<"_">>, [global]),
    binary:replace(NoSlash, <<"=">>, <<>>, [global]).

open_listener() ->
    case gen_tcp:listen(0, [binary, {active, false}, {packet, http_bin},
                              {packet_size, 8192}, {reuseaddr, true},
                              {ip, {127, 0, 0, 1}}]) of
        {ok, Listen} ->
            Listen;
        {error, Reason} ->
            throw({cli_listener_failed, Reason})
    end.

receive_code(Listen, Nonce, AuthUrl, Deadline) ->
    rebar3_grisp_io_io:console("Opening browser: ~s", [AuthUrl]),
    open_browser(AuthUrl),
    log("Waiting for approval"),
    wait_for_code(Listen, Nonce, Deadline, start_code_prompt()).

start_code_prompt() ->
    Parent = self(),
    Prompt = "Paste the authentication code if prompted: ",
    spawn_monitor(fun() -> Parent ! {cli_code_input, self(), io:get_line(Prompt)} end).

wait_for_code(Listen, Nonce, Deadline, Prompt) ->
    Timeout = min(1000, remaining(Deadline)),
    case gen_tcp:accept(Listen, Timeout) of
        {ok, Socket} ->
            Result = try
                callback(Socket, Nonce, Deadline)
            catch
                error:_ ->
                    reply(Socket, 400, <<"Invalid login callback.">>),
                    retry
            after
                gen_tcp:close(Socket)
            end,
            case Result of
                {ok, Code} ->
                    stop_code_prompt(Prompt),
                    Code;
                retry -> wait_for_code(Listen, Nonce, Deadline, Prompt)
            end;
        {error, timeout} ->
            case read_code_prompt(Prompt) of
                {ok, Code} ->
                    stop_code_prompt(Prompt),
                    Code;
                {empty, NewPrompt} ->
                    wait_for_code(Listen, Nonce, Deadline, NewPrompt);
                {pending, NewPrompt} ->
                    wait_for_code(Listen, Nonce, Deadline, NewPrompt);
                unavailable ->
                    wait_for_code(Listen, Nonce, Deadline, unavailable)
            end;
        {error, Reason} -> throw({cli_listener_failed, Reason})
    end.

read_code_prompt({Pid, Ref}) ->
    receive
        {cli_code_input, Pid, Line} when is_list(Line) ->
            Code = string:trim(Line),
            case Code of
                [] -> {empty, start_code_prompt()};
                _ -> {ok, unicode:characters_to_binary(Code)}
            end;
        {cli_code_input, Pid, _EofOrError} -> unavailable;
        {'DOWN', Ref, process, Pid, _Reason} -> unavailable
    after 0 -> {pending, {Pid, Ref}}
    end;
read_code_prompt(unavailable) -> unavailable.

stop_code_prompt({Pid, Ref}) ->
    exit(Pid, kill),
    receive {'DOWN', Ref, process, Pid, _} -> ok end,
    receive {cli_code_input, Pid, _} -> ok after 0 -> ok end;
stop_code_prompt(unavailable) -> ok.

close_listener(Listen) ->
    try
        gen_tcp:close(Listen)
    catch
        error:badarg -> ok
    end.

open_browser(Url) ->
    {Command, Args} = case os:type() of
        {unix, darwin} -> {"open", [binary_to_list(Url)]};
        {unix, _} -> {"xdg-open", [binary_to_list(Url)]};
        {win32, _} -> {"rundll32.exe", ["url.dll,FileProtocolHandler",
                                      binary_to_list(Url)]}
    end,
    case os:find_executable(Command) of
        false -> throw({cli_browser_failed, browser_not_found});
        Executable ->
            Browser = open_port({spawn_executable, Executable},
                                [{args, Args}, binary, exit_status, use_stdio,
                                 stderr_to_stdout, hide]),
            try
                browser_result(Browser,
                               erlang:monotonic_time(millisecond) + ?REQUEST_TIMEOUT)
            after
                close_browser_port(Browser)
            end
    end.

close_browser_port(Browser) ->
    try
        port_close(Browser)
    catch
        error:badarg -> ok
    end.

browser_result(Browser, Deadline) ->
    receive
        {Browser, {exit_status, 0}} -> ok;
        {Browser, {exit_status, Status}} ->
            throw({cli_browser_failed, {exit_status, Status}});
        {Browser, {data, _}} -> browser_result(Browser, Deadline)
    after remaining(Deadline) ->
        throw({cli_browser_failed, timeout})
    end.

callback(Socket, Nonce, Deadline) ->
    {ok, {http_request, Method, Path, _}} = recv(Socket, 0, Deadline),
    ok = inet:setopts(Socket, [{packet, httph_bin}]),
    Length = read_headers(Socket, Deadline, undefined, 0),
    case {Method, Path} of
        {'OPTIONS', {abs_path, <<"/callback">>}} ->
            reply(Socket, 204, <<>>),
            retry;
        {'POST', {abs_path, <<"/callback">>}} when is_integer(Length),
                                                Length > 0, Length =< ?MAX_BODY ->
            ok = inet:setopts(Socket, [{packet, raw}]),
            {ok, Body} = recv(Socket, Length, Deadline),
            case jsx:decode(Body, [return_maps]) of
                #{<<"nonce">> := Nonce, <<"code">> := Code}
                  when is_binary(Code), byte_size(Code) > 0 ->
                    ok = reply(Socket, 200,
                               <<"Login approved. You can return to the terminal.">>),
                    {ok, Code};
                _ ->
                    reply(Socket, 400, <<"Invalid login callback.">>),
                    retry
            end;
        _ ->
            reply(Socket, 404, <<"Not found.">>),
            retry
    end.

read_headers(Socket, Deadline, Length, Count) when Count < 64 ->
    case recv(Socket, 0, Deadline) of
        {ok, http_eoh} -> Length;
        {ok, {http_header, _, Name, _, Value}} ->
            NewLength = case string:lowercase(rebar_utils:to_list(Name)) of
                "content-length" when Length =:= undefined -> binary_to_integer(Value);
                "content-length" -> error(duplicate_content_length);
                _ -> Length
            end,
            read_headers(Socket, Deadline, NewLength, Count + 1);
        _ -> error(invalid_headers)
    end;
read_headers(_, _, _, _) ->
    error(too_many_headers).

recv(Socket, Length, Deadline) ->
    gen_tcp:recv(Socket, Length, min(?REQUEST_TIMEOUT, remaining(Deadline))).

remaining(Deadline) ->
    case Deadline - erlang:monotonic_time(millisecond) of
        Timeout when Timeout > 0 -> Timeout;
        _ -> throw(cli_login_timeout)
    end.

reply(Socket, Status, Body) ->
    Reason = case Status of
        200 -> "OK";
        204 -> "No Content";
        400 -> "Bad Request";
        404 -> "Not Found"
    end,
    gen_tcp:send(Socket,
                 ["HTTP/1.1 ", integer_to_list(Status), " ", Reason, "\r\n",
                  "Content-Type: text/plain; charset=utf-8\r\n",
                  "Connection: close\r\n",
                  "Access-Control-Allow-Origin: *\r\n",
                  "Access-Control-Allow-Methods: POST, OPTIONS\r\n",
                  "Access-Control-Allow-Headers: Content-Type\r\n",
                  "Access-Control-Allow-Private-Network: true\r\n",
                  "Content-Length: ", integer_to_list(byte_size(Body)),
                  "\r\n\r\n", Body]).

log(Message) ->
    rebar3_grisp_io_io:console("~s", [Message]).
