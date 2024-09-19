-module(rebar3_grisp_io_common_test).

% API
-export([init_per_suite/1]).
-export([end_per_suite/1]).

%--- Includes ------------------------------------------------------------------

-include_lib("common_test/include/ct.hrl").

%--- API -----------------------------------------------------------------------

init_per_suite(Config) ->
    DataDir = ?config(data_dir, Config),
    os:putenv("REBAR_GLOBAL_CONFIG_DIR", DataDir),
    RState = rebar_state:new(),

    ok = meck:new(rebar3_grisp_io_api, [no_link]),
    ok = meck:expect(rebar3_grisp_io_api, auth, fun rebar3_grisp_io_api_mockup:auth/3),

    [{rebar_state, RState} | Config].

end_per_suite(_Config) ->
    meck:unload().

%--- Internal ------------------------------------------------------------------

