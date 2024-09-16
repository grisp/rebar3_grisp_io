-module(rebar3_grisp_io).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    % Start applications, because Rebar 3 doesn't do it for us
    {ok, AllDeps} = application:get_key(?MODULE, applications),
    Deps = AllDeps -- [kernel, stdlib],
    [{ok, _} = application:ensure_all_started(A) || A <- Deps],
    % Register tasks
    lists:foldl(fun(Mod, {ok, S}) -> Mod:init(S) end, {ok, State}, [
        rebar3_grisp_io_version
    ]).
