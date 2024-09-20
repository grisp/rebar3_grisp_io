-module(rebar3_grisp_io_deploy).

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
        {name, deploy},
        {module, ?MODULE},
        {bare, true},
        {example, "rebar3 grisp_io deploy"},
        {opts, []},
        {profile, [default]},
        {short_desc, "Deploy the release"},
        {desc, "Deploy the release"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

% Questions:
% - How should we target the package ?
% - How should we target the grisp board ?
% - How should we target the update group ?

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(RState) ->
    {ok, RState}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
