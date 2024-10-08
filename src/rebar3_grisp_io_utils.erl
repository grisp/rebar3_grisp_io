-module(rebar3_grisp_io_utils).

% api
-export([grisp_pack/3]).

%--- API -----------------------------------------------------------------------

grisp_pack(RState, Args, ExtraRelArgs) ->
    RebarParams = [
        "as"
    ] ++ [
        lists:join(",", [atom_to_list(P)
                         || P <- rebar_state:current_profiles(RState),
                            P =/= test])
    ] ++ [
        "grisp",
        "pack"
    ] ++ Args ++ case ExtraRelArgs of
        [_ | _] -> ["--" | ExtraRelArgs];
        _ -> []
    end,
    case rebar3:run(RebarParams) of
        {error, _Reason} = Error -> Error;
        {ok, _} -> {ok, RState}
    end.
