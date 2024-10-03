-module(rebar3_grisp_io_deploy).

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
        {name, deploy},
        {module, ?MODULE},
        {bare, true},
        {example, "rebar3 grisp_io deploy"},
        {opts, options()},
        {profile, [default]},
        {short_desc, "Deploy the release"},
        {desc, "Deploy the release"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

% Questions:
% - How should we target the update group ?

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(RState) ->
    {ok, _} = application:ensure_all_started(rebar3_grisp_io),
    try
        {Args, _} = rebar_state:command_parsed_args(RState),
        Device = try_get_device_serial(Args),
        CurrentRelease = rebar3_grisp_io_utils:expected_package_name(RState),
        PackageName = proplists:get_value(package, Args, CurrentRelease),

        Config = rebar3_grisp_io_config:read_config(RState),
        EncryptedToken = maps:get(encrypted_token, Config),
        Password = ask("Local password", password),
        Token = rebar3_grisp_io_config:try_decrypt_token(Password,
                                                        EncryptedToken),

        rebar3_grisp_io_api:deploy_update(RState, Token, PackageName, Device),

        success("Deployement request for package " ++ PackageName
                ++ " on device #" ++ integer_to_list(Device)),

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
        throw:package_does_not_exist ->
            abort("Error: The package doesn't exists. Use the upload command" ++
                  " first to upload an update package to grisp.io")
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%--- Internals -----------------------------------------------------------------

options() -> [
    {device, $d, "device", integer, "The serial number of the GRiSP board"},
    {package, $p, "package", string,
     "The name of the package that will be deployed"}
].

try_get_device_serial(Args) ->
    case proplists:is_defined(device, Args) of
        true ->
            proplists:get_value(device, Args);
        false ->
            throw(no_device_serial_number)
    end.
