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

    Config2 = start(Config),
    kraft_start(CertDir),

    Config3 = proplists:delete(rebar_state, Config2),
    [{cert_dir, CertDir}, {rebar_state, RState2} | Config3].


end_per_suite(Config) ->
    Apps = ?config(apps, Config),
    cleanup_apps(Apps).

%--- Internal ------------------------------------------------------------------
start(Config) ->
    PrivDir = ?config(priv_dir, Config),
    application:set_env(mnesia, dir, PrivDir),
    setup_policies(PrivDir),

    eresu:install([node()]),
    {ok, Started1} = application:ensure_all_started(eresu),
    register_user(),

    grisp_manager:install([node()]),

    application:start(mnesia),

    {ok, Started2} = application:ensure_all_started(kraft),

    {ok, Started3} = application:ensure_all_started(grisp_manager),
    ok = mnesia:wait_for_tables([grisp_device], 500),
    link_board(),
    Apps = Started1 ++ Started2 ++ Started3,
    [{apps, Apps} | Config].

kraft_start(CertDir) ->
    kraft_start(CertDir, #{}).

kraft_start(CertDir, OverrideOpts) ->
    SslOpts = [
        {verify, verify_none},
        {keyfile, filename:join(CertDir, "server.key")},
        {certfile, filename:join(CertDir, "server.crt")},
        {cacertfile, filename:join(CertDir, "CA.crt")}
    ],
    Opts = #{
        port => 8443,
        ssl_opts => SslOpts,
        app => grisp_manager
    },
    KraftOpts = mapz:deep_merge(Opts, OverrideOpts),
    KraftRoutes = [
       {"/eresu/api/[...]", {cowboy, eresu_rest_api}, #{}},
       {"/grisp-manager/api/:object/[:id]", {cowboy, grisp_manager_rest_api}, #{}}
    ],
    kraft:start(KraftOpts, KraftRoutes).

cleanup_apps(Apps) ->
    mnesia:delete_table(eresu_user),
    mnesia:delete_table(eresu_token),
    mnesia:delete_table(update_package),
    mnesia:delete_table(grisp_device),
    [application:stop(App) || App <- Apps],
    application:stop(mnesia).

register_user() ->
    Hash = erlpass:hash(<<"1234">>),
    WriteUser = fun() ->
                        mnesia:write({eresu_user,
                                      <<"Uuid">>,
                                      <<"Testuser">>,
                                      <<"foo">>,
                                      <<"a@a.a">>,
                                      erlang:system_time(),
                                      Hash,
                                      <<"Max Mustermann">>,
                                      undefined,
                                      undefined,
                                      <<"customer_id">>,
                                      []})
                end,
    mnesia:activity(transaction, WriteUser).

link_board() ->
    AddBoard = fun() ->
                       mnesia:write({grisp_device,
                                     <<"1337">>,
                                     <<"Uuid">>,
                                     null,
                                     null,
                                     null,
                                     manual,
                                     null})
               end,
    mnesia:activity(transaction, AddBoard).

setup_policies(PrivDir) ->
    Policies = [#{subject => #{uuid => <<"Uuid">>},
                  object => #{application => grisp_manager},
                  operations => '_'},
                #{subject => #{uuid => '$1'},
                  object => #{type => grisp_device, user_id => '$1'},
                  operations => '_'}],
    PoliciesString = list_to_binary(io_lib:format("~p.", [Policies])),
    PolicyFile = filename:join(PrivDir, "policies.term"),
    ok = file:write_file(PolicyFile, PoliciesString),
    application:set_env(seabac, policy_file, PolicyFile).
