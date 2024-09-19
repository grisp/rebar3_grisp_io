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
        {Args, _} = rebar_state:command_parsed_args(RState),
        Force = proplists:get_value(force, Args),
        NoPack = proplists:get_value(no_pack, Args),

        Config = rebar3_grisp_io_config:read_config(RState),
        EncryptedToken = maps:get(encrypted_token, Config),
        Password = ask("Local password", password),
        Token = try_decrypt_token(Password, EncryptedToken),

        RState1 = case NoPack of
                      false ->
                          try_pack_command(RState, Force);
                      true ->
                          RState
                  end,

        ProjectDir = rebar_state:dir(RState),
        PackageName = rebar3_grisp_io_utils:expected_package_name(RState),
        {ok, PackageBin} = try_get_package_bin(ProjectDir, PackageName),

        rebar3_grisp_io_api:update_package(RState1,
                                           Token,
                                           PackageName,
                                           PackageBin,
                                           Force),

        success("Package " ++ PackageName ++
                " succesfully uploaded to grisp.io"),

        {ok, RState1}
    catch
        throw:enoent ->
            abort("No configuration available." ++
                  "First run 'rebar3 grisp-io auth' to authenticate");
        throw:wrong_local_password ->
            abort("Wrong local password. Try again");
        throw:wrong_credentials ->
            abort("Error: Wrong credentials");
        throw:package_limit_reached ->
            abort("Error: The limit number of uploaded package " ++
                  "has been reached for this account");
        throw:package_already_exists ->
            abort("Error: A package has already been uploaded for " ++
                  "the same release. Use -f or --force to force the upload");
        throw:package_too_big ->
            abort("Package size is too big");
        error:E ->
            abort("Unexpected error: ~p ~n", [E])
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%--- Internals -----------------------------------------------------------------
options() -> [
    {force, $f, "force", {boolean, false},
     "Force overwriting of the files both locally and remotely"},
    {no_pack, $n, "no-pack", {boolean, false},
     "Do not run the pack command on " ++
     "the current project before uploading"}
].

-spec try_decrypt_token(Password, EncryptedToken) -> Result | no_return() when
    Password :: binary(),
    EncryptedToken :: rebar3_grisp_io_config:encrypted_token(),
    Result         :: rebar3_grisp_io_config:clear_token().
try_decrypt_token(Password, EncryptedToken) ->
    case rebar3_grisp_io_config:decrypt_token(Password, EncryptedToken) of
        error ->
            throw(wrong_local_password);
        T ->
            T
    end.

-spec try_pack_command(rebar_state:t(), boolean()) -> rebar_state:t().
try_pack_command(RState, Force) ->
    Args = case Force of
               true ->
                   ["--force"];
               false ->
                   []
           end,
    case rebar3_grisp_io_utils:grisp_pack(RState, Args) of
        {error, Reason} ->
            error(Reason);
        {ok, NewRState} ->
            NewRState
    end.

-spec try_get_package_bin(ProjectDir, PackageName) -> Result when
      ProjectDir  :: string(),
      PackageName :: string(),
      Result      :: {ok, binary()} | {error, term()}.
try_get_package_bin(ProjectDir, PackageName) ->
    Path = filename:join([ProjectDir, "_grisp/update/", PackageName]),
    case filelib:is_file(Path) of
        false ->
            error(package_not_found);
        true ->
            file:read_file(Path)
    end.
