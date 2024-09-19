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
    {ok, _} = application:ensure_all_started(rebar3_grisp_io),
    {ok, Release} = application:get_key(rebar3_grisp_io, vsn),
    [{release, Release} | Config].

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    rebar3_grisp_io_test_utils:setup_capture_output(),
    Config.

end_per_testcase(_, _Config) ->
    ok.

%--- Testcases -----------------------------------------------------------------

run_version_command(Config) ->
    ProvOutput = rebar3_grisp_io_test_utils:run_grisp_io_command(?PROV, []),
    ?assertMatch({ok, _}, ProvOutput),
    ExpectedOutput = "rebar3_grisp_io: " ++ ?config(release, Config) ++"\n",
    Output = rebar3_grisp_io_test_utils:fetch_all_io_outputs(),
    ?assertEqual([ExpectedOutput], Output).

%--- Internal ------------------------------------------------------------------
