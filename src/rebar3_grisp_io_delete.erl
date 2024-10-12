-module(rebar3_grisp_io_delete).

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
        {name, delete},
        {module, ?MODULE},
        {bare, true},
        {example, "rebar3 grisp-io delete"},
        {opts, []},
        {profile, [default]},
        {short_desc, "Delete an update package"},
        {desc, "Delete an update package~n~n" ++
         "Example: rebar3 grisp-io delete~n"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

% Questions:
% - How should we target the update group ?

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(RState) ->
    {ok, _} = application:ensure_all_started(rebar3_grisp_io),
    try
        {Args, _} = rebar_state:command_parsed_args(RState),
        RelNameArg = proplists:get_value(relname, Args, undefined),
        RelVsnArg = proplists:get_value(relvsn, Args, undefined),
        {RelName, RelVsn}
            = rebar3_grisp_util:select_release(RState, RelNameArg, RelVsnArg),

        Config = rebar3_grisp_io_config:read_config(RState),
        EncryptedToken = maps:get(encrypted_token, Config),
        Password = ask("Local password", password),
        Token = rebar3_grisp_io_config:try_decrypt_token(Password,
                                                        EncryptedToken),

        PackageName = rebar3_grisp_util:update_file_name(RState, RelName,
                                                                 RelVsn),
        rebar3_grisp_io_api:delete_package(RState, Token, PackageName),

        success(iolist_to_binary(["Package ", PackageName,
                                             " successfully deleted!"])),

        {ok, RState}
    catch
        throw:{error, <<"no_process">>} ->
            abort("Error: no deployment process is running.");
        throw:{error, <<"disconnected">>} ->
            abort("Error: the device is not connected to GRiSP.io");
        throw:{error, <<"validate_from_unbooted">>} ->
            abort("Error: device needs to be rebooted.");
        throw:{error, <<"wait_device">>} ->
            abort("Error: deployment waiting for device");
        throw:{error, <<"download">>} ->
            abort("Error: the device is still downloading the updated");
        throw:package_not_found ->
            abort("Error: package not found");
        throw:wrong_local_password ->
            abort("Wrong local password. Try again");
        throw:wrong_credentials ->
            abort("Error: Wrong credentials");
        throw:forbidden ->
            abort("Error: No permission to perform this operation");
        throw:device_does_not_exist->
            abort("Error: The given board doesn't exists or isn't linked")
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%--- Internals -----------------------------------------------------------------
