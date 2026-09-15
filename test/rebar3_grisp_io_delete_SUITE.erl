-module(rebar3_grisp_io_delete_SUITE).

% Callbacks
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

% Test cases
-export([delete_named_package/1]).
-export([delete_current_package/1]).

%--- Includes ------------------------------------------------------------------

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%--- Macros --------------------------------------------------------------------

-define(PROV, rebar3_grisp_io_delete).

%--- Callbacks -----------------------------------------------------------------

all() -> [delete_named_package, delete_current_package].

init_per_suite(Config) ->
    Config1 = rebar3_grisp_io_common_test:init_per_suite(Config),
    rebar3_grisp_io_test_utils:auth_user(Config1),
    Config1.

end_per_suite(Config) ->
    rebar3_grisp_io_common_test:end_per_suite(Config).

init_per_testcase(_, Config) ->
    ok = rebar3_grisp_io_test_utils:upload_test_package(Config),
    Parent = self(),
    ok = meck:new(rebar3_grisp_io_io, [no_link]),
    ok = meck:expect(rebar3_grisp_io_io, ask,
                     fun("Local password", password) ->
                         ?config(local_password, Config)
                     end),
    ok = meck:expect(rebar3_grisp_io_io, success,
                     fun(Message) ->
                         Parent ! {success, Message},
                         ok
                     end),
    ok = meck:expect(rebar3_grisp_io_io, abort,
                     fun(Message) -> error({unexpected_abort, Message}) end),
    Config.

end_per_testcase(_, Config) ->
    meck:unload(),
    rebar3_grisp_io_test_utils:delete_test_package(Config).

%--- Test cases ----------------------------------------------------------------

delete_named_package(Config) ->
    PackageName = rebar3_grisp_io_test_utils:test_package_name(),
    RState = ?config(rebar_state, Config),
    ?assertMatch({ok, _},
                 rebar3_grisp_io_test_utils:run_grisp_io_command(
                   RState, ?PROV, [binary_to_list(PackageName)])),
    ?assertNot(package_exists(Config, PackageName)).

delete_current_package(Config) ->
    RState = ?config(rebar_state, Config),
    PackageName = rebar3_grisp_io_test_utils:test_package_name(),
    ?assertMatch({ok, _},
                 rebar3_grisp_io_test_utils:run_grisp_io_command(
                   RState, ?PROV, [])),
    ?assertNot(package_exists(Config, PackageName)).

%--- Internals -----------------------------------------------------------------

package_exists(Config, PackageName) ->
    Packages = rebar3_grisp_io_api:list_packages(
                 ?config(rebar_state, Config),
                 rebar3_grisp_io_test_utils:token(Config)),
    lists:any(fun(#{<<"name">> := Name}) -> Name =:= PackageName;
                 (_) -> false
              end, Packages).
