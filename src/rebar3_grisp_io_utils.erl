-module(rebar3_grisp_io_utils).

% api
-export([grisp_pack/2]).
-export([expected_package_name/1]).

%--- API -----------------------------------------------------------------------

grisp_pack(RState, Args) ->
    % TODO remove workaround for profiles
    OriginalProfile = rebar_state:current_profiles(RState),

    RState1 = rebar_state:current_profiles(RState, [default]),

    case rebar_command(RState1, grisp, pack, Args) of
        {error, _} = Error ->
            Error;
        {ok, RS} ->
            RS1 = rebar_state:current_profiles(RS, OriginalProfile),
            {ok, RS1}
    end.

expected_package_name(RState) ->
    {AppName, Version} = release(RState),
    Platform = platform(RState),
    Elems = [atom_to_list(Platform), atom_to_list(AppName), Version, "tar"],
    lists:flatten(lists:join(".", Elems)).

%--- Internals -----------------------------------------------------------------

rebar_command(RState, Namespace, Command, Args) ->
    % Backup current command state
    OriginalNamespace = rebar_state:namespace(RState),
    OriginalArgs = rebar_state:command_args(RState),
    OriginalParsedArgs = rebar_state:command_parsed_args(RState),

    % Args are parsed by rebar_core
    RebarState2 = rebar_state:namespace(RState, Namespace),
    RebarState3 = rebar_state:command_args(RebarState2, Args),

    case rebar_core:process_command(RebarState3, Command) of
        {error, _Reason} = Error ->
            Error;
        {ok, RS} ->
            % Restore current command state
            RS2 = rebar_state:namespace(RS, OriginalNamespace),
            RS3 = rebar_state:command_args(RS2, OriginalArgs),
            RS4 = rebar_state:command_parsed_args(RS3, OriginalParsedArgs),
            {ok, RS4}
    end.

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
        {plaform, Platform} ->
            Platform
    end.
