-module(rebar3_grisp_io_api).

% API
-export([auth/3]).
-export([update_package/5]).

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

%% @todo specs
update_package(RState, Token, PackageName, PackageBin, Force) ->
    BaseUrl = base_url(RState),
    URI = list_to_binary("/grisp-manager/api/update-package/" ++ PackageName),
    Url = <<BaseUrl/binary, URI/binary>>,
    BinSize = byte_size(PackageBin),
    Headers = [{<<"authorization">>, bearer_token(Token)},
               {<<"content-type">>, <<"application/octet-stream">>},
               {<<"content-length">>, integer_to_binary(BinSize)}]
               ++ if_none_match(Force, "\"" ++ PackageName ++ "\""),
    Options = insecure_option(RState),

    case hackney:request(put, Url, Headers, PackageBin, Options) of
        {ok, 204, _, _} ->
            ok;
        {ok, 400, _, ClientRef} ->
            {ok, _RespBody} = hackney:body(ClientRef),
            error(unknown_request);
        {ok, 401, _, _} ->
            throw(wrong_credentials);
        {ok, 403, _, _} ->
            throw(package_limit_reached);
        {ok, 412, _, _} ->
            throw(package_already_exists);
        {ok, 413, _, _} ->
            throw(package_too_big);
        Other ->
            error({error, Other})
    end.

%--- Internal ------------------------------------------------------------------
%% @private
%% @doc Create the Authorisation header <<"Basic Username:Password">>
basic_auth(Username, Password) ->
    AuthContent = base64:encode(<<Username/binary, ":", Password/binary>>),
    <<"Basic ", AuthContent/binary>>.

bearer_token(Token) ->
    <<"Bearer ", Token/binary>>.

%% @doc fetch the base_url from the options (default points to prod)
base_url(RState) ->
    Options = rebar_state:get(RState, rebar3_grisp_io, []),
    proplists:get_value(base_url, Options, <<"https://app.grisp.io">>).

%% @doc adds the insecure options in the current profile is test (only for dev)
insecure_option(RState) ->
    case rebar_state:current_profiles(RState) of
        [default, test | _] ->
            [insecure];
        _ ->
            []
    end.

if_none_match(true, _) ->
    [];
if_none_match(false, Etag) ->
    [{<<"if-none-match">>, list_to_binary(Etag)}].

