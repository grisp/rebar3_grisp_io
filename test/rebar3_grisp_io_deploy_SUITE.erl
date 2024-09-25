-module(rebar3_grisp_io_deploy_SUITE).


% callbacks
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

% testcases
-export([run_deploy_no_args/1]).
-export([run_deploy/1]).

%--- Include -------------------------------------------------------------------

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%--- Macros --------------------------------------------------------------------

-define(PROV, rebar3_grisp_io_deploy).

%--- Callbacks -----------------------------------------------------------------

all() -> [
    run_deploy_no_args,
    run_deploy
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

run_deploy_no_args(Config) ->
    RState = ?config(rebar_state, Config),
    RState1 = rebar_state:dir(RState, ?config(data_dir, Config)),

    ?assertError(no_serial_nb,
                 rebar3_grisp_io_test_utils:run_grisp_io_command(RState1,
                                                                 ?PROV,
                                                                 [])).

run_deploy(Config) ->
    RState = ?config(rebar_state, Config),
    RState1 = rebar_state:dir(RState, ?config(data_dir, Config)),

    ProviderOutput = rebar3_grisp_io_test_utils:run_grisp_io_command(RState1,
                                                                     ?PROV,
                                                                     ["-d", "1337"]),

    ?assertMatch({ok, _}, ProviderOutput).

%--- Internal ------------------------------------------------------------------
setup_meck_io() ->
    ok = meck:new(rebar3_grisp_io_io, [no_link]),
    ok = meck:expect(rebar3_grisp_io_io, ask, fun fake_ask/2),
    ok = meck:expect(rebar3_grisp_io_io, abort, 2, fun (_, [E, S]) -> ct:pal(error, "Error Stack: ~p", [S]), ct:fail("Fail: ~p", [E]) end),
    ok = meck:expect(rebar3_grisp_io_io, abort, 1,
                     fun(Msg) ->
                             case Msg of
                                 "Error: The serial number of the target device is missing. Specify it with -d or --device" ->
                                     error(no_serial_nb);
                                 _ ->
                                     ct:fail(Msg)
                             end
                     end),
    ok = meck:expect(rebar3_grisp_io_io, success, 1, fun (_) -> ok end).

setup_meck_gio_utils() ->
    ok = meck:new(rebar3_grisp_io_utils, [no_link, passthrough]),
    ok = meck:expect(rebar3_grisp_io_utils, grisp_pack, fun(RState, _) -> {ok, RState} end).

fake_ask("Local password", _) ->
    <<"azerty">>.
