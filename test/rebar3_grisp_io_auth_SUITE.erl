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

-define(PROV, rebar3_grisp_io_auth).

%--- Callbacks -----------------------------------------------------------------

all() -> [
    run_auth
].

init_per_suite(Config) ->
    Config1 = rebar3_grisp_io_common_test:init_per_suite(Config),
    rebar3_grisp_io_common_test:init_backend(Config1).

end_per_suite(Config) ->
    rebar3_grisp_io_common_test:end_per_suite(Config).

init_per_testcase(_, Config) ->
    ok = meck:new(rebar3_grisp_io_io, [no_link]),
    ok = meck:expect(rebar3_grisp_io_io, ask, fun fake_ask/2),
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
                                                                     ?PROV,
                                                                     []),
    ?assertMatch({ok, _}, ProviderOutput),
    {ok, RState2} = ProviderOutput,
    GIOConfig = rebar3_grisp_io_config:read_config(RState2),
    ?assertMatch(#{username := <<"Testuser">>,
                   encrypted_token := _}, GIOConfig),
    #{encrypted_token := EncryptedToken} = GIOConfig,
    ?assertThrow(wrong_local_password,
        rebar3_grisp_io_config:try_decrypt_token(<<"1234">>, EncryptedToken)),
    ?assertMatch(<<_/binary>>,
        rebar3_grisp_io_config:try_decrypt_token(<<"azerty">>, EncryptedToken)).

%--- Internal ------------------------------------------------------------------
fake_ask("Username", _) ->
    <<"Testuser">>;
fake_ask("Password", _) ->
    <<"1234">>;
fake_ask(Prompt, _) when
      Prompt =:= "Local password" orelse
      Prompt =:= "Confirm your local password" ->
    <<"azerty">>.
