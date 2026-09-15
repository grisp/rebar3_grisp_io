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

-include_lib("stdlib/include/assert.hrl").

%--- Macros --------------------------------------------------------------------

-define(PROV, rebar3_grisp_io_delete).

%--- Callbacks -----------------------------------------------------------------

all() -> [delete_named_package, delete_current_package].

init_per_suite(Config) ->
    rebar3_grisp_io_common_test:init_per_suite(Config).

end_per_suite(Config) ->
    rebar3_grisp_io_common_test:end_per_suite(Config).

init_per_testcase(_, Config) ->
    Parent = self(),
    ok = meck:new(rebar3_grisp_io_io, [no_link]),
    ok = meck:expect(rebar3_grisp_io_io, ask,
                     fun("Local password", password) -> <<"password">> end),
    ok = meck:expect(rebar3_grisp_io_io, success,
                     fun(Message) ->
                         Parent ! {success, Message},
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
    ok = meck:expect(rebar3_grisp_io_api, delete_package,
                     fun(_, <<"token">>, PackageName) ->
                         Parent ! {deleted, PackageName},
                         ok
                     end),
    Config.

end_per_testcase(_, _Config) ->
    meck:unload().

%--- Test cases ----------------------------------------------------------------

delete_named_package(_Config) ->
    PackageName = "kontron-albl-imx8mm.grisp_runtime.0.2.0.tar",
    ?assertMatch({ok, _},
                 rebar3_grisp_io_test_utils:run_grisp_io_command(
                     ?PROV, [PackageName])),
    ?assertEqual({deleted, list_to_binary(PackageName)}, receive_message()).

delete_current_package(_Config) ->
    Parent = self(),
    ok = meck:new(rebar3_grisp_util, [no_link, passthrough]),
    ok = meck:expect(rebar3_grisp_util, select_release,
                     fun(_, undefined, undefined) -> {myapp, <<"0.1.0">>} end),
    ok = meck:expect(rebar3_grisp_util, update_file_name,
                     fun(_, myapp, <<"0.1.0">>) ->
                         PackageName = <<"grisp2.myapp.0.1.0.tar">>,
                         Parent ! {selected, PackageName},
                         PackageName
                     end),
    ?assertMatch({ok, _},
                 rebar3_grisp_io_test_utils:run_grisp_io_command(?PROV, [])),
    ?assertEqual({selected, <<"grisp2.myapp.0.1.0.tar">>}, receive_message()),
    ?assertEqual({deleted, <<"grisp2.myapp.0.1.0.tar">>}, receive_message()).

%--- Internals -----------------------------------------------------------------

receive_message() ->
    receive
        Message -> Message
    after
        1000 -> error(timeout)
    end.
