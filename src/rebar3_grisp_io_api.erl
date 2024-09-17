-module(rebar3_grisp_io_api).

% API
-export([auth/3]).

%--- Macros --------------------------------------------------------------------


%--- API -----------------------------------------------------------------------
%% @doc Performs a POST request to /eresu/api/auth using the credentials given
-spec auth(RState, Username, Password) -> Result when
      RState   :: rebar_state:t(),
      Username :: binary(),
      Password :: binary(),
      Result   :: Token :: binary() | no_return().
auth(RState, Username, Password) ->
    {ok, Hostname} = inet:gethostname(),
    BaseUrl = base_url(RState),
    Url = <<BaseUrl/binary, "/eresu/api/auth">>,
    Body = jsx:encode(#{name => rebar_utils:to_binary(Hostname)}),
    Headers = [{<<"authorization">>, basic_auth(Username, Password)},
               {<<"content-type">>, <<"application/json">>},
               {<<"content-length">>, integer_to_binary(byte_size(Body))}],
    Options = insecure_option(RState),

    case hackney:request(post, Url, Headers, Body, Options) of
        {ok, 200, _, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            #{<<"token">> := Token} = jsx:decode(RespBody),
            Token;
        {ok, 401, _, _} ->
            throw(wrong_credentials);
        {ok, 403, _, _} ->
            throw(token_limit_reached);
        Other ->
            error({error, Other})
    end.

%--- Internal ------------------------------------------------------------------
%% @private
%% @doc Create the Authorisation header <<"Basic Username:Password">>
basic_auth(Username, Password) ->
    AuthContent = base64:encode(<<Username/binary, ":", Password/binary>>),
    <<"Basic ", AuthContent/binary>>.

%% @doc fetch the base_url from the options (default points to prod)
base_url(RState) ->
    Options = rebar_state:get(RState, rebar3_grisp_io, []),
    proplists:get_value(base_url, Options, <<"https://app.grisp.io">>).

%% @doc adds the insecure options in the current profile is test (only for dev)
insecure_option(RState) ->
    case rebar_state:current_profiles(RState) of
        [default, test] ->
            [insecure];
        _ ->
            []
    end.
