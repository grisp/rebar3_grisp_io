-module(rebar3_grisp_io_list_SUITE).

% Callbacks
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

% Test cases
-export([list_packages/1]).
-export([list_no_packages/1]).

%--- Includes ------------------------------------------------------------------

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%--- Macros --------------------------------------------------------------------

-define(PROV, rebar3_grisp_io_list).

%--- Callbacks -----------------------------------------------------------------

all() -> [list_packages, list_no_packages].

init_per_suite(Config) ->
    Config1 = rebar3_grisp_io_common_test:init_per_suite(Config),
    rebar3_grisp_io_test_utils:auth_user(Config1),
    Config1.

end_per_suite(Config) ->
    try rebar3_grisp_io_test_utils:delete_test_package(Config)
    after
        rebar3_grisp_io_common_test:end_per_suite(Config)
    end.

init_per_testcase(list_packages, Config) ->
    ok = rebar3_grisp_io_test_utils:upload_test_package(Config),
    setup_io_mock(Config),
    Config;
init_per_testcase(list_no_packages, Config) ->
    ok = rebar3_grisp_io_test_utils:delete_test_package(Config),
    setup_io_mock(Config),
    Config.

setup_io_mock(Config) ->
    Parent = self(),
    ok = meck:new(rebar3_grisp_io_io, [no_link]),
    ok = meck:expect(rebar3_grisp_io_io, ask,
                     fun("Local password", password) ->
                         ?config(local_password, Config)
                     end),
    ok = meck:expect(rebar3_grisp_io_io, console,
                     fun(Format, Args) ->
                         Parent ! {console, Format, Args},
                         ok
                     end),
    ok = meck:expect(rebar3_grisp_io_io, abort,
                     fun(Message) -> error({unexpected_abort, Message}) end).

end_per_testcase(_, _Config) ->
    meck:unload().

%--- Test cases ----------------------------------------------------------------

list_packages(Config) ->
    RState = ?config(rebar_state, Config),
    Packages = rebar3_grisp_io_api:list_packages(
                 RState, rebar3_grisp_io_test_utils:token(Config)),
    ?assertMatch({ok, _},
                 rebar3_grisp_io_test_utils:run_grisp_io_command(
                   RState, ?PROV, [])),
    Sorted = lists:sort(
               fun(A, B) -> maps:get(<<"name">>, A) =<
                            maps:get(<<"name">>, B)
               end, Packages),
    ExpectedRows = [[<<"NAME">>, <<"APPLICATION">>, <<"VERSION">>,
                     <<"PLATFORM">>, <<"LAST MODIFIED">>]
                    | lists:map(fun package_values/1, Sorted)],
    ?assertEqual(ExpectedRows, console_rows()).

list_no_packages(Config) ->
    RState = ?config(rebar_state, Config),
    ?assertMatch({ok, _},
                 rebar3_grisp_io_test_utils:run_grisp_io_command(
                   RState, ?PROV, [])),
    ?assertEqual([[]], console_rows()).

%--- Internals -----------------------------------------------------------------

package_values(Package) ->
    [maps:get(<<"name">>, Package),
     maps:get(<<"app_name">>, Package),
     maps:get(<<"version">>, Package),
     maps:get(<<"platform">>, Package),
     maps:get(<<"last_modified">>, Package)].

console_rows() ->
    console_rows([]).

console_rows(Rows) ->
    receive
        {console, "No update packages found.", []} ->
            console_rows([[] | Rows]);
        {console, "~ts  ~ts  ~ts  ~ts  ~ts", Args} ->
            console_rows([Args | Rows])
    after
        0 -> lists:reverse(Rows)
    end.
