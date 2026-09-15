-module(rebar3_grisp_io_reboot).

% Callbacks
-export([init/1]).
-export([do/1]).
-export([format_error/1]).

%--- Includes ------------------------------------------------------------------

-include("rebar3_grisp_io.hrl").
-import(rebar3_grisp_io_io, [abort/1, abort/2, ask/2, success/2]).

%--- API -----------------------------------------------------------------------

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, reboot},
        {module, ?MODULE},
        {bare, true},
        {example, "rebar3 grisp-io reboot -d DEVICE"},
        {opts, options()},
        {profile, [default]},
        {short_desc, "Reboot a device"},
        {desc, "Request a reboot of a device linked to grisp.io"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(RState) ->
    {ok, _} = application:ensure_all_started(rebar3_grisp_io),
    try
        {Args, _} = rebar_state:command_parsed_args(RState),
        Device = unicode:characters_to_binary(try_get_device(Args)),
        Config = rebar3_grisp_io_config:read_config(RState),
        EncryptedToken = maps:get(encrypted_token, Config),
        Password = ask("Local password", password),
        Token = rebar3_grisp_io_config:try_decrypt_token(Password,
                                                        EncryptedToken),
        ok = rebar3_grisp_io_api:reboot_device(RState, Token, Device),
        success("Reboot requested for device #~s", [Device]),
        {ok, RState}
    catch
        throw:no_device ->
            abort("Error: The device identifier is missing. " ++
                  "Specify it with -d or --device");
        throw:enoent ->
            abort("No configuration available. " ++
                  "First run 'rebar3 grisp-io auth' to authenticate");
        throw:wrong_local_password ->
            abort("Wrong local password. Try again");
        throw:wrong_credentials ->
            abort("Error: Wrong credentials");
        throw:forbidden ->
            abort("Error: No permission to reboot this device");
        throw:device_does_not_exist ->
            abort("Error: The given device does not exist or is not linked");
        throw:{error, Reason} ->
            abort("Error: reboot request rejected: ~s", [Reason])
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%--- Internal ------------------------------------------------------------------

options() -> [
    {device, $d, "device", string, "Identifier of the device to reboot"}
].

try_get_device(Args) ->
    case proplists:get_value(device, Args) of
        undefined -> throw(no_device);
        Device -> Device
    end.
