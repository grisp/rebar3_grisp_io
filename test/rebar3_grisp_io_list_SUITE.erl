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

-include_lib("stdlib/include/assert.hrl").

%--- Macros --------------------------------------------------------------------

-define(PROV, rebar3_grisp_io_list).

%--- Callbacks -----------------------------------------------------------------

all() -> [list_packages, list_no_packages].

init_per_suite(Config) ->
    rebar3_grisp_io_common_test:init_per_suite(Config).

end_per_suite(Config) ->
    rebar3_grisp_io_common_test:end_per_suite(Config).

init_per_testcase(_, Config) ->
    Parent = self(),
    ok = meck:new(rebar3_grisp_io_io, [no_link]),
    ok = meck:expect(rebar3_grisp_io_io, ask,
                     fun("Local password", password) -> <<"password">> end),
    ok = meck:expect(rebar3_grisp_io_io, console,
                     fun(Format, Args) ->
                         Parent ! {console, Format, Args},
                         ok
                     end),
    ok = meck:expect(rebar3_grisp_io_io, abort,
                     fun(Message) -> error({unexpected_abort, Message}) end),
    ok = meck:new(rebar3_grisp_io_config, [no_link, passthrough]),
    ok = meck:expect(rebar3_grisp_io_config, read_config,
                     fun(_) -> #{encrypted_token => <<"encrypted">>} end),
    ok = meck:expect(rebar3_grisp_io_config, try_decrypt_token,
                     fun(_, _) -> <<"token">> end),
    ok = meck:new(rebar3_grisp_io_api, [no_link, passthrough]),
    Config.

end_per_testcase(_, _Config) ->
    meck:unload().

%--- Test cases ----------------------------------------------------------------

list_packages(_Config) ->
    Packages = [
        package(<<"grisp2.zeta.2.0.0.tar">>, <<"zeta">>, <<"2.0.0">>),
        package(<<"grisp2.alpha.1.0.0.tar">>, <<"alpha">>, <<"1.0.0">>)
    ],
    ok = meck:expect(rebar3_grisp_io_api, list_packages,
                     fun(_, <<"token">>) -> Packages end),
    ?assertMatch({ok, _},
                 rebar3_grisp_io_test_utils:run_grisp_io_command(?PROV, [])),
    ?assertEqual([
        [<<"NAME">>, <<"APPLICATION">>, <<"VERSION">>, <<"PLATFORM">>,
         <<"LAST MODIFIED">>],
        package_values(lists:nth(2, Packages)),
        package_values(lists:nth(1, Packages))
    ], console_rows()).

list_no_packages(_Config) ->
    ok = meck:expect(rebar3_grisp_io_api, list_packages,
                     fun(_, <<"token">>) -> [] end),
    ?assertMatch({ok, _},
                 rebar3_grisp_io_test_utils:run_grisp_io_command(?PROV, [])),
    ?assertEqual([[]], console_rows()).

%--- Internals -----------------------------------------------------------------

package(Name, App, Version) ->
    #{<<"name">> => Name,
      <<"app_name">> => App,
      <<"version">> => Version,
      <<"platform">> => <<"grisp2">>,
      <<"last_modified">> => <<"2026-09-14T10:00:00Z">>}.

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
