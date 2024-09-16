-module(rebar3_grisp_io_version).

% Callbacks
-export([init/1]).
-export([do/1]).
-export([format_error/1]).

%--- Includes ------------------------------------------------------------------

-include("rebar3_grisp_io.hrl").
-import(rebar3_grisp_io_io, [
    console/1,
    console/2]).

%--- API -----------------------------------------------------------------------

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, version},
        {module, ?MODULE},
        {bare, true},
        {example, "rebar3 grisp_io version"},
        {opts, []},
        {profile, [default]},
        {short_desc, "Print the version of the plugin"},
        {desc, "Print the version of the plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {ok, _} = application:ensure_all_started(rebar3_grisp_io),
    case application:get_key(rebar3_grisp_io, vsn) of
        {ok, Version} ->
            console("rebar3_grisp_io: ~s", [Version]);
        undefined ->
            error({version_missing, rebar3_grisp_io})
    end,
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
