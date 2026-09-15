-module(rebar3_grisp_io_cancel_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).
-export([run_cancel_no_args/1, run_cancel/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(PROV, rebar3_grisp_io_cancel).

all() -> [run_cancel_no_args, run_cancel].

init_per_suite(Config) ->
    Config1 = rebar3_grisp_io_common_test:init_per_suite(Config),
    rebar3_grisp_io_test_utils:auth_user(Config1),
    ok = rebar3_grisp_io_test_utils:upload_test_package(Config1),
    Config1.

end_per_suite(Config) ->
    try
        rebar3_grisp_io_test_utils:delete_test_package(Config)
    after
        rebar3_grisp_io_common_test:end_per_suite(Config)
    end.

init_per_testcase(TestCase, Config) ->
    Parent = self(),
    ok = meck:new(rebar3_grisp_io_io, [no_link]),
    ok = meck:expect(rebar3_grisp_io_io, ask,
                     fun("Local password", password) ->
                         ?config(local_password, Config)
                     end),
    ok = meck:expect(rebar3_grisp_io_io, success,
                     fun(Format, Args) ->
                         Parent ! {success, Format, Args},
                         ok
                     end),
    ok = meck:expect(rebar3_grisp_io_io, abort,
                     fun(Message) ->
                         case Message of
                             "Error: The device identifier is missing. " ++
                             "Specify it with -d or --device" ->
                                 error(no_device);
                             _ ->
                                 error({unexpected_abort, Message})
                         end
                     end),
    ok = meck:expect(rebar3_grisp_io_io, abort,
                     fun(Format, Args) ->
                         error({unexpected_abort, Format, Args})
                     end),
    maybe_start_deployment(TestCase, Config),
    Config.

end_per_testcase(TestCase, Config) ->
    meck:unload(),
    maybe_cancel_deployment(TestCase, Config).

run_cancel_no_args(Config) ->
    ?assertError(no_device,
                 rebar3_grisp_io_test_utils:run_grisp_io_command(
                   ?config(rebar_state, Config), ?PROV, [])).

run_cancel(Config) ->
    Device = binary_to_list(?config(ci_device, Config)),
    ?assertMatch({ok, _},
                 rebar3_grisp_io_test_utils:run_grisp_io_command(
                   ?config(rebar_state, Config), ?PROV, ["-d", Device])),
    ?assertEqual({success, "Update cancellation requested for device #~s",
                  [list_to_binary(Device)]}, receive_message()).

receive_message() ->
    receive
        Message -> Message
    after
        1000 -> error(timeout)
    end.

maybe_start_deployment(run_cancel, Config) ->
    RState = ?config(rebar_state, Config),
    Token = rebar3_grisp_io_test_utils:token(Config),
    Package = rebar3_grisp_io_test_utils:test_package_name(),
    Device = ?config(ci_device, Config),
    ok = rebar3_grisp_io_api:deploy_update(RState, Token, Package, Device);
maybe_start_deployment(_, _Config) ->
    ok.

maybe_cancel_deployment(run_cancel, Config) ->
    RState = ?config(rebar_state, Config),
    Token = rebar3_grisp_io_test_utils:token(Config),
    Device = ?config(ci_device, Config),
    try rebar3_grisp_io_api:cancel_update(RState, Token, Device) of
        ok -> ok
    catch
        %% The command normally removed the update process. If the testcase
        %% failed before doing so, this is a best-effort cleanup.
        throw:{error, _} -> ok
    end;
maybe_cancel_deployment(_, _Config) ->
    ok.
