-module(rebar3_grisp_io_auth_SUITE).


% callbacks
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

% testcases
-export([run_auth/1]).

%--- Include -------------------------------------------------------------------

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%--- Macros --------------------------------------------------------------------

-define(AUTH_PROV, rebar3_grisp_io_auth).
-define(DEAUTH_PROV, rebar3_grisp_io_deauth).

%--- Callbacks -----------------------------------------------------------------

all() -> [
    run_auth
].

init_per_suite(Config) ->
    rebar3_grisp_io_common_test:init_per_suite(Config).

end_per_suite(Config) ->
    rebar3_grisp_io_common_test:end_per_suite(Config).

init_per_testcase(_, Config) ->
    Username = ?config(ci_username, Config),
    Password = ?config(ci_password, Config),
    LocalPassword = ?config(local_password, Config),
    ok = meck:new(rebar3_grisp_io_io, [no_link]),
    ok = meck:expect(rebar3_grisp_io_io, ask,
                     fun(Prompt, Type) ->
                         fake_ask(Prompt, Type, Username, Password,
                                  LocalPassword)
                     end),
    ok = meck:expect(rebar3_grisp_io_io, abort, 2, fun (Msg, Args) -> ct:fail(Msg, Args) end),
    ok = meck:expect(rebar3_grisp_io_io, abort, 1, fun (Msg) -> ct:fail(Msg) end),
    ok = meck:expect(rebar3_grisp_io_io, success, 1, fun (_) -> ok end),
    Config.

end_per_testcase(_, _Config) ->
    meck:unload().

%--- Testcases -----------------------------------------------------------------

run_auth(Config) ->
    RState = ?config(rebar_state, Config),
    ProviderOutput = rebar3_grisp_io_test_utils:run_grisp_io_command(RState,
                                                                     ?AUTH_PROV,
                                                                     []),
    ?assertMatch({ok, _}, ProviderOutput),
    {ok, RState2} = ProviderOutput,
    GIOConfig = rebar3_grisp_io_config:read_config(RState2),
    ?assertMatch(#{encrypted_token := _}, GIOConfig),
    ?assertEqual(?config(ci_username, Config), maps:get(username, GIOConfig)),
    #{encrypted_token := EncryptedToken} = GIOConfig,
    ?assertThrow(wrong_local_password,
        rebar3_grisp_io_config:try_decrypt_token(<<"incorrect">>,
                                                 EncryptedToken)),
    Token = rebar3_grisp_io_config:try_decrypt_token(
              ?config(local_password, Config), EncryptedToken),
    ?assertMatch(<<_/binary>>, Token),
    %% Cache the token before testing deauth so suite teardown can revoke it
    %% if any subsequent assertion fails.
    rebar3_grisp_io_test_utils:remember_token(
      ?config(ci_username, Config), Token),
    ?assertMatch(
       {ok, _},
       rebar3_grisp_io_test_utils:run_grisp_io_command(RState2,
                                                        ?DEAUTH_PROV,
                                                        [])),
    ?assertThrow(enoent, rebar3_grisp_io_config:read_config(RState2)),
    ?assertThrow(wrong_credentials,
                 rebar3_grisp_io_api:list_packages(RState2, Token)).

%--- Internal ------------------------------------------------------------------
fake_ask("Username", _, Username, _, _) ->
    Username;
fake_ask("Password", _, _, Password, _) ->
    Password;
fake_ask(Prompt, _, _, _, LocalPassword) when
      Prompt =:= "Local password" orelse
      Prompt =:= "Confirm your local password" ->
    LocalPassword.
