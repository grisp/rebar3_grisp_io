-module(rebar3_grisp_io_list).

% Callbacks
-export([init/1]).
-export([do/1]).
-export([format_error/1]).

%--- Includes ------------------------------------------------------------------

-include("rebar3_grisp_io.hrl").
-import(rebar3_grisp_io_io, [abort/1, ask/2, console/2]).

%--- API -----------------------------------------------------------------------

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, list},
        {module, ?MODULE},
        {bare, true},
        {example, "rebar3 grisp-io list"},
        {opts, []},
        {profile, [default]},
        {short_desc, "List update packages"},
        {desc, "List update packages stored on grisp.io"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(RState) ->
    {ok, _} = application:ensure_all_started(rebar3_grisp_io),
    try
        Config = rebar3_grisp_io_config:read_config(RState),
        EncryptedToken = maps:get(encrypted_token, Config),
        Password = ask("Local password", password),
        Token = rebar3_grisp_io_config:try_decrypt_token(Password,
                                                        EncryptedToken),
        Packages = rebar3_grisp_io_api:list_packages(RState, Token),
        print_packages(Packages),
        {ok, RState}
    catch
        throw:enoent ->
            abort("No configuration available. "
                  "First run 'rebar3 grisp-io auth' to authenticate");
        throw:wrong_local_password ->
            abort("Wrong local password. Try again");
        throw:wrong_credentials ->
            abort("Error: Wrong credentials");
        throw:forbidden ->
            abort("Error: No permission to list update packages")
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%--- Internals -----------------------------------------------------------------

print_packages([]) ->
    console("No update packages found.", []);
print_packages(Packages) ->
    console("~ts  ~ts  ~ts  ~ts  ~ts",
            [<<"NAME">>, <<"APPLICATION">>, <<"VERSION">>, <<"PLATFORM">>,
             <<"LAST MODIFIED">>]),
    lists:foreach(fun print_package/1, lists:sort(fun package_order/2, Packages)).

print_package(Package) ->
    console("~ts  ~ts  ~ts  ~ts  ~ts", [
        maps:get(<<"name">>, Package),
        maps:get(<<"app_name">>, Package),
        maps:get(<<"version">>, Package),
        maps:get(<<"platform">>, Package),
        maps:get(<<"last_modified">>, Package)
    ]).

package_order(A, B) ->
    maps:get(<<"name">>, A) =< maps:get(<<"name">>, B).
