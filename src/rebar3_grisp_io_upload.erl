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


%--- MACROS --------------------------------------------------------------------

-define(MAX_DDOT, 2).


%--- API -----------------------------------------------------------------------

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, upload},
        {module, ?MODULE},
        {bare, true},
        {example, "rebar3 grisp_io upload"},
        {opts, options()},
        {profile, [default]},
        {short_desc, "Upload an update to GRiSP.io"},
        {desc, "Upload an update for the current package on GRiSP.io"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(RState) ->
    {ok, _} = application:ensure_all_started(rebar3_grisp_io),
    try
        {Args, ExtraArgs} = rebar_state:command_parsed_args(RState),
        RelNameArg = proplists:get_value(relname, Args, undefined),
        RelVsnArg = proplists:get_value(relvsn, Args, undefined),
        {RelName, RelVsn}
            = rebar3_grisp_util:select_release(RState, RelNameArg, RelVsnArg),

        Force = proplists:get_value(force, Args),
        Refresh = proplists:get_value(refresh, Args),

        Config = rebar3_grisp_io_config:read_config(RState),
        EncryptedToken = maps:get(encrypted_token, Config),
        Password = ask("Local password", password),
        Token = rebar3_grisp_io_config:try_decrypt_token(Password,
                                                        EncryptedToken),

        case get_package(RState, Refresh, RelName, RelVsn, ExtraArgs) of
            {error, _Reason} = Error -> Error;
            {ok, PackageName, PackagePath, RState2} ->
                rebar3_grisp_io_api:update_package(RState2, Token, PackageName,
                                                   PackagePath, Force),
                success("Package ~s succesfully uploaded to grisp.io",
                        [PackageName]),
                {ok, RState2}
        end

    catch
        throw:enoent ->
            abort("No configuration available." ++
                  "First run 'rebar3 grisp-io auth' to authenticate");
        throw:wrong_local_password ->
            abort("Wrong local password. Try again");
        throw:wrong_credentials ->
            abort("Error: Wrong credentials");
        throw:forbidden ->
            abort("Error: No permission to perform this operation");
        throw:package_limit_reached ->
            abort("Error: The limit number of uploaded package " ++
                  "has been reached for this account");
        throw:package_already_exists ->
            abort("Error: A package has already been uploaded for " ++
                  "the same release. Use -f or --force to force the upload");
        throw:{package_not_found, Name} ->
            abort("Error: Package file ~s not found",
                  [grisp_tools_util:maybe_relative(Name, ?MAX_DDOT)]);
        throw:package_too_big ->
            abort("Package size is too big")
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%--- Internals -----------------------------------------------------------------

options() -> [
    {relname, $n, "relname", string,
     "Specify the name for the release that will be uploaded"},
    {relvsn, $v, "relvsn", string,
     "Specify the version of the release"},
    {force, $f, "force", {boolean, false},
     "Force overwriting of the package remotely"},
    {refresh, $r, "refresh", {boolean, false},
     "Force software package building even if it already exists"}
].

-spec get_package(rebar_state:t(), boolean(), binary(), binary(), [binary()])
    -> {ok, rebar_state:t()} | {error, term()}.
get_package(RState, Refresh, RelName, RelVsn, ExtraRelArgs) ->
    PackageName = rebar3_grisp_util:update_file_name(RState, RelName, RelVsn),
    PackagePath = rebar3_grisp_util:update_file_path(RState, RelName, RelVsn),
    case filelib:is_file(PackagePath) of
        true when Refresh =:= false ->
            RelPath = grisp_tools_util:maybe_relative(PackagePath, ?MAX_DDOT),
            console("* Using existing package: ~s", [RelPath]),
            {ok, PackageName, PackagePath, RState};
        _ ->
            console("* Building software package...", []),
            case build_package(RState, Refresh, RelName,
                               RelVsn, ExtraRelArgs) of
                {ok, RState2} -> {ok, PackageName, PackagePath, RState2};
                {error, _Reason} = Error -> Error
            end
    end.

-spec build_package(rebar_state:t(), boolean(), binary(), binary(), [binary()])
    -> {ok, rebar_state:t()} | {error, term()}.
build_package(RState, Refresh, RelName, RelVsn, ExtraRelArgs) ->
    Args = [
        "--force",
        "--quiet",
        "--relname", atom_to_list(RelName),
        "--relvsn", RelVsn
    ] ++ case Refresh of
        true -> ["--refresh"];
        false -> []
    end,
    rebar3_grisp_io_utils:grisp_pack(RState, Args, ExtraRelArgs).
