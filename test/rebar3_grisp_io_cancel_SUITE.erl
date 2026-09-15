-module(rebar3_grisp_io_cancel_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([run_cancel_no_args/1, run_cancel/1]).

-include_lib("stdlib/include/assert.hrl").

-define(PROV, rebar3_grisp_io_cancel).

all() -> [run_cancel_no_args, run_cancel].

init_per_testcase(_, Config) ->
    Parent = self(),
    ok = meck:new(rebar3_grisp_io_io, [no_link]),
    ok = meck:expect(rebar3_grisp_io_io, ask,
                     fun("Local password", password) -> <<"password">> end),
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
    ok = meck:new(rebar3_grisp_io_config, [no_link, passthrough]),
    ok = meck:expect(rebar3_grisp_io_config, read_config,
                     fun(_) -> #{encrypted_token => <<"encrypted">>} end),
    ok = meck:expect(rebar3_grisp_io_config, try_decrypt_token,
                     fun(_, _) -> <<"token">> end),
    ok = meck:new(rebar3_grisp_io_api, [no_link, passthrough]),
    ok = meck:expect(rebar3_grisp_io_api, cancel_update,
                     fun(_, <<"token">>, Device) ->
                         Parent ! {cancelled, Device},
                         ok
                     end),
    Config.

end_per_testcase(_, _Config) ->
    meck:unload().

run_cancel_no_args(_Config) ->
    ?assertError(no_device,
                 rebar3_grisp_io_test_utils:run_grisp_io_command(
                   ?PROV, [])).

run_cancel(_Config) ->
    ?assertMatch({ok, _},
                 rebar3_grisp_io_test_utils:run_grisp_io_command(
                   ?PROV, ["-d", "ci-dummy"])),
    ?assertEqual({cancelled, <<"ci-dummy">>}, receive_message()),
    ?assertEqual({success, "Update cancellation requested for device #~s",
                  [<<"ci-dummy">>]}, receive_message()).

receive_message() ->
    receive
        Message -> Message
    after
        1000 -> error(timeout)
    end.
