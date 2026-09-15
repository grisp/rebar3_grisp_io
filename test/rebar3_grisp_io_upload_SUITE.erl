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
    rebar3_grisp_io_test_utils:auth_user(Config1),
    Config1.

end_per_suite(Config) ->
    try rebar3_grisp_io_test_utils:delete_test_package(Config)
    after
        rebar3_grisp_io_common_test:end_per_suite(Config)
    end.

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
                                                                     ["--force"]),

    ?assertMatch({ok, _}, ProviderOutput),
    Packages = rebar3_grisp_io_api:list_packages(
                 RState, rebar3_grisp_io_test_utils:token(Config)),
    PackageName = <<"grisp2.grisp_io_robot.0.1.0.tar">>,
    ?assert(lists:any(fun(#{<<"name">> := Name}) -> Name =:= PackageName end,
                      Packages)).

%--- Internal ------------------------------------------------------------------
setup_meck_io() ->
    ok = meck:new(rebar3_grisp_io_io, [no_link]),
    ok = meck:expect(rebar3_grisp_io_io, ask, fun fake_ask/2),
    ok = meck:expect(rebar3_grisp_io_io, console, fun (_, _) -> ok end),
    ok = meck:expect(rebar3_grisp_io_io, abort, 2,
                     fun(Msg, Args) -> ct:fail(Msg, Args) end),
    ok = meck:expect(rebar3_grisp_io_io, abort, 1, fun (Msg) -> 
                                                           ct:fail(Msg)
                                                   end),
    ok = meck:expect(rebar3_grisp_io_io, success, 2, fun (_, _) -> ok end),
    ok = meck:expect(rebar3_grisp_io_io, spinner_start,
                     fun() -> spinner end),
    ok = meck:expect(rebar3_grisp_io_io, spinner_stop,
                     fun(spinner, _) -> ok end).

setup_meck_gio_utils() ->
    ok = meck:new(rebar3_grisp_io_utils, [no_link, passthrough]),
    ok = meck:expect(rebar3_grisp_io_utils, grisp_pack, fun(RState, _, _) -> {ok, RState} end).

fake_ask("Local password", _) ->
    <<"grisp-ci-local-password">>.
