-module(rebar3_grisp_io_version_SUITE).


% callbacks
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

% testcases
-export([run_version_command/1]).

%--- Include -------------------------------------------------------------------

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%--- Macros --------------------------------------------------------------------

-define(PROV, rebar3_grisp_io_version).

%--- Callbacks -----------------------------------------------------------------

all() -> [
    run_version_command
].

init_per_suite(Config) ->
    Config1 = rebar3_grisp_io_common_test:init_per_suite(Config),
    {ok, _} = application:ensure_all_started(rebar3_grisp_io),
    {ok, Release} = application:get_key(rebar3_grisp_io, vsn),
    [{release, Release} | Config1].

end_per_suite(Config) ->
    rebar3_grisp_io_common_test:end_per_suite(Config).

init_per_testcase(_, Config) ->
    ok = meck:new(rebar3_grisp_io_io, [no_link, passthrough]),
    ok = meck:expect(rebar3_grisp_io_io, console, 2, ok),
    Config.

end_per_testcase(_, _Config) ->
    meck:unload(rebar3_grisp_io_io).

%--- Testcases -----------------------------------------------------------------

run_version_command(Config) ->
    ProvOutput = rebar3_grisp_io_test_utils:run_grisp_io_command(?PROV, []),
    ?assertMatch({ok, _}, ProvOutput),
    ?assert(meck:called(rebar3_grisp_io_io, console,
                        ["rebar3_grisp_io: ~s", [?config(release, Config)]])),
    ?assertEqual(1, meck:num_calls(rebar3_grisp_io_io, console, 2)),
    ?assert(meck:validate(rebar3_grisp_io_io)).

%--- Internal ------------------------------------------------------------------
