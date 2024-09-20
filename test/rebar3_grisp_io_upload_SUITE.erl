-module(rebar3_grisp_io_upload_SUITE).


% callbacks
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

% testcases
-export([run_upload/1]).

%--- Include -------------------------------------------------------------------

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%--- Macros --------------------------------------------------------------------

-define(PROV, rebar3_grisp_io_upload).

%--- Callbacks -----------------------------------------------------------------

all() -> [
    run_upload
].

init_per_suite(Config) ->
    Config1 = rebar3_grisp_io_common_test:init_per_suite(Config),
    Config2 = rebar3_grisp_io_common_test:init_backend(Config1),
    rebar3_grisp_io_test_utils:auth_user(Config2,
                                         <<"Testuser">>,
                                         <<"1234">>,
                                         <<"azerty">>),
    Config2.

end_per_suite(Config) ->
    rebar3_grisp_io_common_test:end_per_suite(Config).

init_per_testcase(_, Config) ->
    ok = meck:new(rebar3_grisp_io_io, [no_link]),
    ok = meck:expect(rebar3_grisp_io_io, ask, fun fake_ask/2),
    ok = meck:expect(rebar3_grisp_io_io, abort, 2, fun (_, [E, S]) -> ct:pal(error, "Error Stack: ~p", [S]), ct:fail("Fail: ~p", [E]) end),
    ok = meck:expect(rebar3_grisp_io_io, abort, 1, fun (Msg) -> ct:fail(Msg) end),
    ok = meck:expect(rebar3_grisp_io_io, success, 1, fun (_) -> ok end),
    Config.

end_per_testcase(_, _Config) ->
    meck:unload().

%--- Testcases -----------------------------------------------------------------

run_upload(Config) ->
    RState = ?config(rebar_state, Config),
    ProviderOutput = rebar3_grisp_io_test_utils:run_grisp_io_command(RState,
                                                                     ?PROV,
                                                                     ["-f"]),
    ?assertMatch({ok, _}, ProviderOutput),
    {ok, RState2} = ProviderOutput.

%--- Internal ------------------------------------------------------------------
fake_ask("Local password", _) ->
    <<"azerty">>.
