-module(rebar3_grisp_io_deauth).

% Callbacks
-export([init/1]).
-export([do/1]).
-export([format_error/1]).

%--- Includes ------------------------------------------------------------------

-include("rebar3_grisp_io.hrl").
-import(rebar3_grisp_io_io, [abort/1, ask/2, success/1]).

%--- API -----------------------------------------------------------------------

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, deauth},
        {module, ?MODULE},
        {bare, true},
        {example, "rebar3 grisp-io deauth"},
        {opts, []},
        {profile, [default]},
        {short_desc, "Revoke the current authentication token"},
        {desc, "Revoke the current GRiSP.io authentication token and remove "
               "the locally stored credentials"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(RState) ->
    {ok, _} = application:ensure_all_started(rebar3_grisp_io),
    try
        Config = rebar3_grisp_io_config:read_config(RState),
        EncryptedToken = maps:get(encrypted_token, Config),
        Password = ask("Local password", password),
        Token = rebar3_grisp_io_config:try_decrypt_token(Password,
                                                        EncryptedToken),
        ok = rebar3_grisp_io_api:deauth(RState, Token),
        ok = rebar3_grisp_io_config:delete_config(RState),
        success("Authentication token successfully revoked"),
        {ok, RState}
    catch
        throw:enoent ->
            abort("No configuration available. " ++
                  "First run 'rebar3 grisp-io auth' to authenticate");
        throw:wrong_local_password ->
            abort("Wrong local password. Try again");
        throw:wrong_credentials ->
            ok = rebar3_grisp_io_config:delete_config(RState),
            success("Authentication token is no longer valid; " ++
                    "local credentials removed"),
            {ok, RState}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
