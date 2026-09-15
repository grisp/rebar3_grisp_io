-module(rebar3_grisp_io_validate_SUITE).

% callbacks
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

% testcases
-export([run_validate_no_args/1]).
-export([run_validate/1]).

%--- Include -------------------------------------------------------------------

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%--- Macros --------------------------------------------------------------------

-define(PROV, rebar3_grisp_io_validate).

%--- Callbacks -----------------------------------------------------------------

all() -> [
    run_validate_no_args,
    run_validate
].

init_per_suite(Config) ->
    RState = rebar_state:current_profiles(rebar_state:new(), [default, test]),
    [{rebar_state, RState}, {ci_device, <<"ci-dummy">>} | Config].

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    setup_mocks(),
    Config.

end_per_testcase(_, _Config) ->
    meck:unload().

%--- Testcases -----------------------------------------------------------------

run_validate_no_args(Config) ->
    RState = ?config(rebar_state, Config),
    RState1 = rebar_state:dir(RState, ?config(data_dir, Config)),

    ?assertError(no_serial_nb,
                 rebar3_grisp_io_test_utils:run_grisp_io_command(RState1,
                                                                 ?PROV,
                                                                 [])).

run_validate(Config) ->
    RState = ?config(rebar_state, Config),
    RState1 = rebar_state:dir(RState, ?config(data_dir, Config)),
    Device = binary_to_list(?config(ci_device, Config)),

    ProviderOutput = rebar3_grisp_io_test_utils:run_grisp_io_command(RState1,
                                                                     ?PROV,
                                                                     ["-d", Device]),

    ?assertMatch({ok, _}, ProviderOutput),
    ?assertEqual({validated, Device}, receive_message()),
    ?assertEqual({success, "Update validated for device #" ++ Device},
                 receive_message()).

%--- Internal ------------------------------------------------------------------
setup_mocks() ->
    Parent = self(),
    ok = meck:new(rebar3_grisp_io_io, [no_link]),
    ok = meck:expect(rebar3_grisp_io_io, ask, fun fake_ask/2),
    ok = meck:expect(rebar3_grisp_io_io, console, fun (_, _) -> ok end),
    ok = meck:expect(rebar3_grisp_io_io, abort, 2,
                     fun(Msg, Args) -> ct:fail(Msg, Args) end),
    ok = meck:expect(rebar3_grisp_io_io, abort, 1,
                     fun(Msg) ->
                             case Msg of
                                 "Error: The serial number of the target device is missing. Specify it with -d or --device" ->
                                     error(no_serial_nb);
                                 _ ->
                                 ct:fail(Msg)
                         end
                     end),
    ok = meck:expect(rebar3_grisp_io_io, success,
                     fun(Message) ->
                         Parent ! {success, Message},
                         ok
                     end),
    ok = meck:new(rebar3_grisp_io_config, [no_link, passthrough]),
    ok = meck:expect(rebar3_grisp_io_config, read_config,
                     fun(_) -> #{encrypted_token => <<"encrypted">>} end),
    ok = meck:expect(rebar3_grisp_io_config, try_decrypt_token,
                     fun(_, _) -> <<"token">> end),
    ok = meck:new(rebar3_grisp_io_api, [no_link, passthrough]),
    ok = meck:expect(rebar3_grisp_io_api, validate_update,
                     fun(_, <<"token">>, Device) ->
                         Parent ! {validated, Device},
                         ok
                     end).

fake_ask("Local password", _) ->
    <<"password">>.

receive_message() ->
    receive
        Message -> Message
    after
        1000 -> error(timeout)
    end.
