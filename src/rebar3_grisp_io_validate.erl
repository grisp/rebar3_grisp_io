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
        {example, "rebar3 grisp-io validate"},
        {opts, options()},
        {profile, [default]},
        {short_desc, "Validate an update"},
        {desc, "Validate an update deployed on a specific device"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

% Questions:
% - How should we target the update group ?

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(RState) ->
    {ok, _} = application:ensure_all_started(rebar3_grisp_io),
    try
        Device = try_get_device_serial(RState),

        Config = rebar3_grisp_io_config:read_config(RState),
        EncryptedToken = maps:get(encrypted_token, Config),
        Password = ask("Local password", password),
        Token = rebar3_grisp_io_config:try_decrypt_token(Password,
                                                        EncryptedToken),

        rebar3_grisp_io_api:validate_update(RState, Token, Device),

        success("Update validated for device #" ++ Device),

        {ok, RState}
    catch
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
            abort("Error: The given board doesn't exists or isn't linked");
        error:E:S ->
            case lists:member(test, rebar_state:current_profiles(RState)) of
                true ->
                    abort("Unexpected error: ~p -> ~p ~n", [E, S]);
                false ->
                    abort("Unexpected error: ~p ~n", [E])
            end
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%--- Internals -----------------------------------------------------------------

options() -> [].

try_get_device_serial(RState) ->
    case rebar_state:command_args(RState) of
        [] ->
            throw(no_device_serial_number);
        [Serial | _] ->
            Serial
    end.
