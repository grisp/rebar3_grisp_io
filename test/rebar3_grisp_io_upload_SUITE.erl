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
    setup_meck_io(),
    setup_meck_gio_utils(),
    Config.

end_per_testcase(_, _Config) ->
    meck:unload().

%--- Testcases -----------------------------------------------------------------

run_upload(Config) ->
    RState = ?config(rebar_state, Config),
    RState1 = rebar_state:set(RState, root_dir, ?config(data_dir, Config)),

    ProviderOutput = rebar3_grisp_io_test_utils:run_grisp_io_command(RState1,
                                                                     ?PROV,
                                                                     []),

    ?assertMatch({ok, _}, ProviderOutput),
    StoredPackage = grisp_manager_data:get_packages(#{user_id => <<"Uuid">>}),
    ?assertMatch([#{name := <<"grisp2.grisp_io_robot.0.1.0.tar">>}], StoredPackage).

%--- Internal ------------------------------------------------------------------
setup_meck_io() ->
    ok = meck:new(rebar3_grisp_io_io, [no_link]),
    ok = meck:expect(rebar3_grisp_io_io, ask, fun fake_ask/2),
    ok = meck:expect(rebar3_grisp_io_io, console, fun (_, _) -> ok end),
    ok = meck:expect(rebar3_grisp_io_io, abort, 2, fun (_, [E, S]) -> ct:pal(error, "Error Stack: ~p", [S]), ct:fail("Fail: ~p", [E]) end),
    ok = meck:expect(rebar3_grisp_io_io, abort, 1, fun (Msg) -> 
                                                           ct:fail(Msg)
                                                   end),
    ok = meck:expect(rebar3_grisp_io_io, success, 2, fun (_, _) -> ok end).

setup_meck_gio_utils() ->
    ok = meck:new(rebar3_grisp_io_utils, [no_link, passthrough]),
    ok = meck:expect(rebar3_grisp_io_utils, grisp_pack, fun(RState, _, _) -> {ok, RState} end).

fake_ask("Local password", _) ->
    <<"azerty">>.
