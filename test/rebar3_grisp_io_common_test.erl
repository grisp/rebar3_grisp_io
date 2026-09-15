-module(rebar3_grisp_io_common_test).

% API
-export([init_per_suite/1]).
-export([end_per_suite/1]).

%--- Includes ------------------------------------------------------------------

-include_lib("common_test/include/ct.hrl").

%--- API -----------------------------------------------------------------------

init_per_suite(Config) ->
    {Username, Password, Device} = credentials(),
    DataDir = ?config(data_dir, Config),
    os:putenv("REBAR_GLOBAL_CONFIG_DIR", DataDir),
    RState = rebar_state:current_profiles(rebar_state:new(), [default, test]),
    RState1 = rebar_state:set(RState, relx, [{release,
                                              {grisp_io_robot, "0.1.0"},
                                              [grisp_io_robot]}]),
    [{rebar_state, RState1},
     {ci_username, Username},
     {ci_password, Password},
     {ci_device, Device},
     {local_password, <<"grisp-ci-local-password">>} | Config].

end_per_suite(_Config) ->
    ok.

%--- Internals -----------------------------------------------------------------

credentials() ->
    Names = ["GRISP_CI_USERNAME", "GRISP_CI_PASSWORD", "GRISP_CI_DEVICE"],
    Values = [{Name, os:getenv(Name)} || Name <- Names],
    Missing = [Name || {Name, Value} <- Values,
                       Value =:= false orelse Value =:= ""],
    case Missing of
        [] ->
            [{_, Username}, {_, Password}, {_, Device}] = Values,
            {rebar_utils:to_binary(Username), rebar_utils:to_binary(Password),
             rebar_utils:to_binary(Device)};
        _ ->
            ct:fail({missing_environment_variables, Missing})
    end.
