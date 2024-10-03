-module(rebar3_grisp_io_utils).

% api
-export([grisp_pack/2]).
-export([expected_package_name/1]).

%--- API -----------------------------------------------------------------------

grisp_pack(RState, Args) ->
    case rebar3:run(["grisp", "pack" | Args]) of
        {error, _Reason} = Error -> Error;
        {ok, _} -> {ok, RState}
    end.

-spec expected_package_name(rebar_state:t()) -> list().
expected_package_name(RState) ->
    {AppName, Version} = release(RState),
    Platform = platform(RState),
    Elems = [atom_to_list(Platform), atom_to_list(AppName), Version, "tar"],
    lists:flatten(lists:join(".", Elems)).

%--- Internals -----------------------------------------------------------------

release(RState) ->
    Relx = rebar_state:get(RState, relx, []),
    case lists:keyfind(release, 1, Relx) of
        false ->
            error(release_not_found);
        {release, Rel, _} ->
            Rel
    end.

platform(RState) ->
    GRiSP = rebar_state:get(RState, grisp, []),
    case lists:keyfind(platform, 1, GRiSP) of
        false ->
            grisp2;
        {platform, Platform} ->
            Platform
    end.
