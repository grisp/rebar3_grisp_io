-module(rebar3_grisp_io_test_utils).

% api
-export([run_grisp_io_command/2]).
-export([run_grisp_io_command/3]).
-export([setup_capture_output/0]).
-export([fetch_all_io_outputs/0]).

%--- API -----------------------------------------------------------------------
run_grisp_io_command(Provider, _Args) ->
    InitRState = rebar_state:new(),
    {ok, RState2} = Provider:init(InitRState),
    Provider:do(RState2).

run_grisp_io_command(RState, Provider, _Args) ->
    {ok, RState1} = Provider:init(RState),
    Provider:do(RState1).

% Source: https://stackoverflow.com/questions/4334420/eunit-and-ioformat
setup_capture_output() ->
    ok = meck:new(io_lib, [unstick, passthrough]),
    meck:expect(io_lib, format, 2, meck:passthrough()).

fetch_all_io_outputs() ->
    lists:map(fun({_, _, Result}) ->
                      lists:flatten(Result)
              end, meck:history(io_lib)).

%--- Internal ------------------------------------------------------------------