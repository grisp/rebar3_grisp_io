-module(rebar3_grisp_io_manager).

% api
-export([start/1]).
-export([kraft_start/1]).
-export([cleanup_apps/1]).


%--- Includes ------------------------------------------------------------------

-include_lib("common_test/include/ct.hrl").

%--- API -----------------------------------------------------------------------
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
    Apps = Started1 ++ Started2 ++ Started3,
    [{apps, Apps} | Config].

kraft_start(CertDir) ->
    kraft_start(CertDir, #{}).


cleanup_apps(Apps) ->
    mnesia:delete_table(eresu_user),
    mnesia:delete_table(eresu_token),
    mnesia:delete_table(update_package),
    [application:stop(App) || App <- Apps],
    application:stop(mnesia).

%--- Internal ------------------------------------------------------------------

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
