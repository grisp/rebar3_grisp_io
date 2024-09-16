-module(rebar3_grisp_io_io).

% API
-export([abort/1, abort/2]).
-export([ask/2]).
-export([console/1, console/2]).
-export([error_message/1, error_message/2]).
-export([success/1, success/2]).

%--- MACROS --------------------------------------------------------------------

-define(WHITESPACE, unicode_util:whitespace()).

%--- Types ---------------------------------------------------------------------

-type input_type() :: string | password.

%--- API -----------------------------------------------------------------------

abort(Msg) ->
    abort(Msg, []).
abort(Msg, Args) ->
    rebar_api:abort(color(196, Msg), Args).

ask(Prompt, Type) ->
    UserInput = do_ask(Prompt, Type),
    case get(Type, UserInput) of
        {error, Reason} ->
            error_message("Error wrong input. Reason: ~s", [Reason]);
        {ok, Value} ->
            Value
    end.

console(Msg) ->
    console(Msg, []).
console(Msg, Args) ->
    rebar_api:console(Msg, Args).

error_message(Msg) ->
    error_message(Msg, []).
error_message(Msg, Args) ->
    rebar_api:error(Msg, Args).

success(Msg) ->
    success(Msg, []).
success(Msg, Args) ->
    Text = io_lib:format(Msg, Args),
    rebar_api:console(color(40, Text), Args).

%--- Internals -----------------------------------------------------------------

-spec do_ask(Prompt, Type) -> UserInput | no_return() when
      Type      :: input_type(),
      Prompt    :: binary(),
      UserInput :: unicode:chardata().
do_ask(Prompt, password) ->
    NewPrompt = erlang:iolist_to_binary([Prompt, " > "]),
    UserInput = do_get_password(NewPrompt),
    trim(trim(UserInput), both, [$\n]);
do_ask(Prompt, _) ->
    NewPrompt = erlang:iolist_to_binary(["\n", Prompt, " > "]),
    RawUserInput = io:get_line(erlang:binary_to_list(NewPrompt)),
    trim(trim(RawUserInput), both, [$\n]).

do_get_password(Prompt) ->
    ok = io:setopts([binary]),
    Overwriter = fun() ->
        prompt_password(Prompt),
        receive
            {done, _Pid, _Ref} ->
                ok
        end
    end,
    Pid = spawn_link(Overwriter),
    PwLine = try
        io:get_line(binary_to_list(Prompt))
    after
        Ref = make_ref(),
        Pid ! {done, self(), Ref},
        receive
            {done, Pid, Ref} ->
                ok
        after
            timer:seconds(5) ->
                error(win32_prompt_timeout)
        end
    end,
    PwLine.

prompt_password(Prompt) ->
    % This is spawned to continually overwrite the prompt the user is
    % entering data on, in order to hide characters typed.
    ClearLine = "\e[2K",
    receive
        {done, Parent, Ref} ->
            Parent ! {done, self(), Ref},
            Spaces = lists:duplicate(byte_size(Prompt) + 24, $ ),
            io:fwrite(standard_error, "~ts\r~ts\r", [ClearLine, Spaces])
    after
        1 ->
            Spaces = lists:duplicate(24, $ ),
            io:fwrite(standard_error, "~ts\r~ts~ts\r~ts",
                      [ClearLine, Prompt, Spaces, Prompt]),
            prompt_password(Prompt)
    end.

get(Type, String) when Type =:= string orelse Type =:= password ->
    case String of
        [] ->
            {error, no_data};
        _ when is_list(String) ->
            Trimmed = string:trim(String, both, ?WHITESPACE),
            {ok, unicode:characters_to_binary(Trimmed)};
        _ ->
            {error, wrong_data_type}
    end;
get(Type, UserInput) ->
    error({unknown_type, Type, UserInput}).

-ifdef(unicode_str).
trim(Str, right, Chars) -> string:trim(Str, trailing, Chars);
trim(Str, left, Chars) -> string:trim(Str, leading, Chars);
trim(Str, both, Chars) -> string:trim(Str, both, Chars).
-else.
trim(Str) -> string:strip(rebar_utils:to_list(Str)).
trim(Str, Dir, [Chars|_]) -> string:strip(rebar_utils:to_list(Str), Dir, Chars).
-endif.

% @doc Add foreground coloring to the given text using ANSI ecape code
% See: https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit
% @end
-spec color(Code, Text) -> string() when
      Code :: 0..255,
      Text :: string().
color(Code, Text) ->
    io_lib:format("\033[38;5;~pm~s\033[m", [Code, Text]).
