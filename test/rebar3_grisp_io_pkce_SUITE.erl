-module(rebar3_grisp_io_pkce_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([browser_login/1]).

-include_lib("stdlib/include/assert.hrl").

all() -> [browser_login].

init_per_testcase(_, Config) ->
    {ok, _} = application:ensure_all_started(rebar3_grisp_io),
    Directory = proplists:get_value(priv_dir, Config),
    Script = <<"#!/bin/sh\nprintf '%s' \"$1\" > \"$GRISP_IO_PKCE_TEST_URL\"\n">>,
    lists:foreach(fun(Name) ->
        File = filename:join(Directory, Name),
        ok = file:write_file(File, Script),
        ok = file:change_mode(File, 8#700)
    end, ["open", "xdg-open"]),
    OldPath = os:getenv("PATH"),
    OldUrl = os:getenv("GRISP_IO_PKCE_TEST_URL"),
    true = os:putenv("PATH", Directory ++ ":" ++ OldPath),
    true = os:putenv("GRISP_IO_PKCE_TEST_URL", filename:join(Directory, "browser-url")),
    ok = meck:new(hackney, [no_link, passthrough]),
    ok = meck:new(rebar3_grisp_io_io, [no_link]),
    ok = meck:expect(rebar3_grisp_io_io, console, 1, fun(_) -> ok end),
    ok = meck:expect(rebar3_grisp_io_io, console, 2, fun(_, _) -> ok end),
    %% Browser callbacks must not read from Common Test's terminal.
    ok = meck:expect(rebar3_grisp_io_io, read_line, 1, eof),
    [{old_path, OldPath}, {old_url, OldUrl} | Config].

end_per_testcase(_, Config) ->
    meck:unload(),
    restore_env("PATH", proplists:get_value(old_path, Config)),
    restore_env("GRISP_IO_PKCE_TEST_URL", proplists:get_value(old_url, Config)).

browser_login(Config) ->
    AuthUrl = <<"https://example.test/login?return_to=%2Flogin%2Fcli%2Fsession">>,
    State = rebar_state:set(rebar_state:new(), rebar3_grisp_io,
                            [{base_url, <<"https://example.test">>}]),
    ok = meck:expect(hackney, request, fun(post, Url, Headers, Body, _) ->
        ?assertEqual(<<"application/json">>,
                     proplists:get_value(<<"content-type">>, Headers)),
        Payload = jsx:decode(Body, [return_maps]),
        case Url of
            <<"https://example.test/eresu/api/cli_session">> ->
                #{<<"code_challenge">> := Challenge, <<"nonce">> := Nonce,
                  <<"redirect_port">> := Port} = Payload,
                ?assert(is_integer(Port) andalso Port > 0),
                ?assertEqual(43, byte_size(Challenge)),
                put(pkce_challenge, Challenge),
                put(pkce_port, Port),
                spawn_link(fun() ->
                    %% A mismatched nonce must not consume the login attempt.
                    request_callback(Port, 'POST',
                                     #{code => <<"code">>, nonce => <<"wrong">>}, 400),
                    request_callback(Port, 'OPTIONS', #{}, 204),
                    request_callback(Port, 'POST',
                                     #{code => <<"code">>, nonce => Nonce}, 200)
                end),
                {ok, 201, [], jsx:encode(#{id => <<"session">>, auth_url => AuthUrl})};
            <<"https://example.test/eresu/api/cli_redeem">> ->
                #{<<"session_id">> := <<"session">>, <<"code">> := <<"code">>,
                  <<"code_verifier">> := Verifier} = Payload,
                Port = get(pkce_port),
                ?assertEqual(get(pkce_challenge), base64url(crypto:hash(sha256, Verifier))),
                %% The listener must be closed before redemption.
                ?assertMatch({error, econnrefused},
                             gen_tcp:connect({127, 0, 0, 1}, Port,
                                             [{active, false}], 1000)),
                {ok, 200, [], jsx:encode(#{access_token => <<"access-token">>})}
        end
    end),
    ?assertEqual(<<"access-token">>, rebar3_grisp_io_pkce:auth(State)),
    ?assertEqual({ok, AuthUrl}, file:read_file(
                                 filename:join(proplists:get_value(priv_dir, Config),
                                               "browser-url"))),
    ?assert(meck:called(rebar3_grisp_io_io, read_line,
                        ["Paste the authentication code if prompted: "])),
    ?assert(meck:validate(rebar3_grisp_io_io)),
    ?assert(meck:validate(hackney)).

request_callback(Port, Method, Payload, Status) ->
    {ok, Socket} = connect(Port, 100),
    try
        Body = jsx:encode(Payload),
        ok = gen_tcp:send(Socket,
                         [atom_to_list(Method), " /callback HTTP/1.1\r\n",
                          "Host: 127.0.0.1\r\nContent-Type: text/plain\r\n",
                          "Content-Length: ", integer_to_list(byte_size(Body)),
                          "\r\n\r\n", Body]),
        {ok, {http_response, _, Status, _}} = gen_tcp:recv(Socket, 0, 5000)
    after
        gen_tcp:close(Socket)
    end.

connect(Port, Retries) ->
    case gen_tcp:connect({127, 0, 0, 1}, Port,
                         [binary, {active, false}, {packet, http_bin}], 1000) of
        {error, econnrefused} when Retries > 0 ->
            timer:sleep(20),
            connect(Port, Retries - 1);
        Result -> Result
    end.

base64url(Data) ->
    NoPlus = binary:replace(base64:encode(Data), <<"+">>, <<"-">>, [global]),
    NoSlash = binary:replace(NoPlus, <<"/">>, <<"_">>, [global]),
    binary:replace(NoSlash, <<"=">>, <<>>, [global]).

restore_env(Name, false) -> os:unsetenv(Name);
restore_env(Name, Value) -> os:putenv(Name, Value).
