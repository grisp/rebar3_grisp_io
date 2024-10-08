-module(rebar3_grisp_io_auth).

% Callbacks
-export([init/1]).
-export([do/1]).
-export([format_error/1]).

%--- Includes ------------------------------------------------------------------

-include("rebar3_grisp_io.hrl").
-import(rebar3_grisp_io_io, [
    abort/1,
    abort/2,
    ask/2,
    console/1,
    console/2,
    success/1,
    success/2]).

%--- API -----------------------------------------------------------------------

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, auth},
        {module, ?MODULE},
        {bare, true},
        {example, "rebar3 grisp_io auth"},
        {opts, []},
        {profile, [default]},
        {short_desc, "Authenticate yourself to grisp.io"},
        {desc, "Authenticate yourself to your grisp.io account"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(RState) ->
    {ok, _} = application:ensure_all_started(rebar3_grisp_io),
    try
        Username = ask("Username", string),
        Password = ask("Password", password),

        Token = rebar3_grisp_io_api:auth(RState, Username, Password),
        success("Authentication successful - " ++
                "Please provide new local password"),
        LocalPassword = ask_local_password(),
        EncToken = rebar3_grisp_io_config:encrypt_token(LocalPassword, Token),
        Config = #{encrypted_token => EncToken,
                   username => Username},

        rebar3_grisp_io_config:write_config(RState, Config),

        success("Token successfully requested"),

        {ok, RState}
    catch
        throw:wrong_credentials ->
            abort("Error: Wrong credentials");
        throw:token_limit_reached ->
            abort("Error: Maximum number of tokens per user reached" ++
                  " Revoke unused tokens and try again");
        throw:forbidden ->
            abort("Error: No permission to perform this operation");
        throw:not_matching ->
            abort("Error: The 2 local password entries don't match")
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%--- Internals -----------------------------------------------------------------
ask_local_password() ->
    LocalPassword = ask("Local password", password),
    RepeatedLocalPswd = ask("Confirm your local password", password),

    case LocalPassword =:= RepeatedLocalPswd of
        true -> LocalPassword;
        _ -> throw(not_matching)
    end.
