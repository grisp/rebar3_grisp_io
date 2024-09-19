-module(rebar3_grisp_io_api_mockup).

% api
-export([auth/3]).

%--- API -----------------------------------------------------------------------

-spec auth(RState, Username, Password) -> Result when
      RState   :: rebar_state:t(),
      Username :: binary(),
      Password :: binary(),
      Result   :: Token :: binary() | no_return().
auth(_, Username, Password) ->
    <<Username/binary, ":", Password/binary>>.
