-module(rebar3_grisp_io_validate).

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
        {name, validate},
        {module, ?MODULE},
        {bare, true},
        {example, "rebar3 grisp-io validate -d 1337"},
        {opts, options()},
        {profile, [default]},
        {short_desc, "Validate an update"},
        {desc, "Validate an update deployed on a specific device~n~n" ++
         "Example: rebar3 grisp-io validate -d 1337~n"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

% Questions:
% - How should we target the update group ?

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(RState) ->
    {ok, _} = application:ensure_all_started(rebar3_grisp_io),
    try
        {Args, _} = rebar_state:command_parsed_args(RState),
        Device = integer_to_list(try_get_device_serial(Args)),

        Config = rebar3_grisp_io_config:read_config(RState),
        EncryptedToken = maps:get(encrypted_token, Config),
        Password = ask("Local password", password),
        Token = rebar3_grisp_io_config:try_decrypt_token(Password,
                                                        EncryptedToken),

        rebar3_grisp_io_api:validate_update(RState, Token, Device),

        success("Update validated for device #" ++ Device),

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
        throw:no_device_serial_number ->
            abort("Error: The serial number of the target device is missing." ++
                  " Specify it with -d or --device");
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

options() -> [
    {device, $d, "device", integer, "Specify the serial number of the device"}
].

try_get_device_serial(Args) ->
    case proplists:is_defined(device, Args) of
        true ->
            proplists:get_value(device, Args);
        false ->
            throw(no_device_serial_number)
    end.
