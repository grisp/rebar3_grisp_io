-module(rebar3_grisp_io_upload).

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
        {name, upload},
        {module, ?MODULE},
        {bare, true},
        {example, "rebar3 grisp_io upload"},
        {opts, []},
        {profile, [default]},
        {short_desc, "Upload an update to GRiSP.io"},
        {desc, "Upload an update for the current package on GRiSP.io"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(RState) ->
    {ok, _} = application:ensure_all_started(rebar3_grisp_io),
    try
        Config = rebar3_grisp_io_config:read_config(RState),
        EncryptedToken = maps:get(encrypted_token, Config),
        Password = ask("Local password", password),
        Token = rebar3_grisp_io_config:decrypt_token(Password, EncryptedToken),

        % TODO rebar3 grisp pack

        % TODO upload

        {ok, RState}
    catch
        throw:enoent ->
            abort("No configuration available." ++
                  "First run 'rebar3 grisp-io auth' to authenticate");
        error:E ->
            abort("Unexpected error: ~p~n", [E])
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%--- Internals -----------------------------------------------------------------
