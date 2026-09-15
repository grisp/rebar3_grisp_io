-module(rebar3_grisp_io_test_utils).

% api
-export([run_grisp_io_command/2]).
-export([run_grisp_io_command/3]).
-export([auth_user/1]).
-export([auth_user/4]).
-export([remember_token/2]).
-export([token/1]).
-export([upload_test_package/1]).
-export([delete_test_package/1]).
-export([test_package_name/0]).

%--- Includes ------------------------------------------------------------------

-include("../src/rebar3_grisp_io.hrl").

-include_lib("common_test/include/ct.hrl").

%--- API -----------------------------------------------------------------------
run_grisp_io_command(Provider, Args) ->
    InitRState = rebar_state:new(),
    run_grisp_io_command(InitRState, Provider, Args).

run_grisp_io_command(RState, Provider, Args) ->
    {ok, RState1} = Provider:init(RState),
    [ProviderT] = rebar_state:providers(RState1),
    Command = element(2, ProviderT),
    RState2 = rebar_state:command_args(RState1, Args),
    RState3 = rebar_state:namespace(RState2, ?NAMESPACE),
    rebar_core:process_command(RState3, Command).

auth_user(Config) ->
    auth_user(Config,
              ?config(ci_username, Config),
              ?config(ci_password, Config),
              ?config(local_password, Config)).

auth_user(Config, Username, Password, LocalPassword) ->
    RState = ?config(rebar_state, Config),
    Token = case persistent_term:get(token_key(Username), undefined) of
        undefined ->
            NewToken = rebar3_grisp_io_api:auth(RState, Username, Password),
            remember_token(Username, NewToken),
            NewToken;
        CachedToken ->
            CachedToken
    end,
    EncryptedToken = rebar3_grisp_io_config:encrypt_token(LocalPassword, Token),
    GIOConfig = #{username => Username,
                  encrypted_token => EncryptedToken},
    rebar3_grisp_io_config:write_config(RState, GIOConfig).

remember_token(Username, Token) ->
    persistent_term:put(token_key(Username), Token).

token(Config) ->
    RState = ?config(rebar_state, Config),
    #{encrypted_token := EncryptedToken} =
        rebar3_grisp_io_config:read_config(RState),
    rebar3_grisp_io_config:try_decrypt_token(
        ?config(local_password, Config), EncryptedToken).

upload_test_package(Config) ->
    RState = ?config(rebar_state, Config),
    Token = token(Config),
    PackageName = test_package_name(),
    ok = rebar3_grisp_io_api:update_package(RState, Token, PackageName,
                                             test_package_path(), true),
    Packages = rebar3_grisp_io_api:list_packages(RState, Token),
    true = lists:any(
             fun(#{<<"name">> := Name}) -> Name =:= PackageName;
                (_) -> false
             end,
             Packages),
    ok.

delete_test_package(Config) ->
    RState = ?config(rebar_state, Config),
    try rebar3_grisp_io_api:delete_package(RState, token(Config),
                                           test_package_name()) of
        ok -> ok
    catch
        throw:package_not_found -> ok
    end.

%--- Internal ------------------------------------------------------------------

test_package_name() ->
    <<"grisp2.grisp_io_robot.0.1.0.tar">>.

test_package_path() ->
    TestDir = filename:join(code:lib_dir(rebar3_grisp_io), "test"),
    unicode:characters_to_binary(filename:join([
        TestDir,
        "rebar3_grisp_io_upload_SUITE_data",
        "_grisp",
        "update",
        binary_to_list(test_package_name())
    ])).

token_key(Username) ->
    {?MODULE, ci_token, Username}.
