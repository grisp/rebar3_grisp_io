-module(rebar3_grisp_io_common_test).

% API
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_backend/1]).

%--- Includes ------------------------------------------------------------------

-include_lib("common_test/include/ct.hrl").

%--- API -----------------------------------------------------------------------

init_per_suite(Config) ->
    DataDir = ?config(data_dir, Config),
    os:putenv("REBAR_GLOBAL_CONFIG_DIR", DataDir),
    RState = rebar_state:current_profiles(rebar_state:new(), [default, test]),
    RState1 = rebar_state:set(RState, relx, [{release, 
                                              {grisp_io_robot, "0.1.0"}, 
                                              [grisp_io_robot]}]),
    [{rebar_state, RState1} | Config].

init_backend(Config) ->
    CertDir = filename:join(code:lib_dir(rebar3_grisp_io, test), "certs"),


    RState = ?config(rebar_state, Config),
    RState2 = rebar_state:set(RState, rebar3_grisp_io,
                              [{base_url, <<"https://localhost:8443">>}]),

    Config2 = rebar3_grisp_io_manager:start(Config),
    rebar3_grisp_io_manager:kraft_start(CertDir),

    Config3 = proplists:delete(rebar_state, Config2),
    [{cert_dir, CertDir}, {rebar_state, RState2} | Config3].


end_per_suite(Config) ->
    Apps = ?config(apps, Config),
    rebar3_grisp_io_manager:cleanup_apps(Apps).

%--- Internal ------------------------------------------------------------------
