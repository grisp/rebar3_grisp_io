-module(rebar3_grisp_io_test_utils).

% api
-export([run_grisp_io_command/2]).
-export([run_grisp_io_command/3]).
-export([setup_capture_output/0]).
-export([fetch_all_io_outputs/0]).
-export([auth_user/4]).

%--- Includes ------------------------------------------------------------------

-include("../src/rebar3_grisp_io.hrl").

-include_lib("common_test/include/ct.hrl").

%--- API -----------------------------------------------------------------------
run_grisp_io_command(Provider, Args) ->
    InitRState = rebar_state:new(),
    run_grisp_io_command(InitRState, Provider, Args).

run_grisp_io_command(RState, Provider, Args) ->
    {ok, RState1} = Provider:init(RState),
    [ProviderT] = rebar_state:providers(RState1),
    Command = element(2, ProviderT),
    RState2 = rebar_state:command_args(RState1, Args),
    RState3 = rebar_state:namespace(RState2, ?NAMESPACE),
    rebar_core:process_command(RState3, Command).

% Source: https://stackoverflow.com/questions/4334420/eunit-and-ioformat
setup_capture_output() ->
    ok = meck:new(io_lib, [unstick, passthrough]),
    meck:expect(io_lib, format, 2, meck:passthrough()).

fetch_all_io_outputs() ->
    lists:map(fun({_, _, Result}) ->
                      lists:flatten(Result)
              end, meck:history(io_lib)).

auth_user(Config, Username, Password, LocalPassword) ->
    RState = ?config(rebar_state, Config),
    Token = rebar3_grisp_io_api:auth(RState, Username, Password),
    EncryptedToken = rebar3_grisp_io_config:encrypt_token(LocalPassword, Token),
    GIOConfig = #{username => Username,
                  encrypted_token => EncryptedToken},
    rebar3_grisp_io_config:write_config(RState, GIOConfig).

%--- Internal ------------------------------------------------------------------
