-module(rebar3_grisp_io_api).

% API
-export([auth/3]).
-export([update_package/5]).
-export([deploy_update/4]).
-export([validate_update/3]).

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

%% @doc Performs a PUT request tp /grisp-manager/api/update-package/PackageName
%% @param Token is the clear token of the user
%% @param PackageName must have the following format
%%        platform.appname.x.y.z.[profilename[+profilename]].tar
%% @param PackagePath is the path to the package file to upload
%% @param Force indicates if the PUT request should overwrite
%%        the remote files if they exist
-spec update_package(RState, Token, PackageName, PackagePath, Force) -> Res
    when
      RState      :: rebar_state:t(),
      Token       :: rebar3_grisp_io_config:clear_token(),
      PackageName :: binary(),
      PackagePath :: binary(),
      Force       :: boolean(),
      Res         :: ok | no_return().
update_package(RState, Token, PackageName, PackagePath, Force) ->
    BaseUrl = base_url(RState),
    URI = <<"/grisp-manager/api/update-package/", PackageName/binary>>,
    Url = <<BaseUrl/binary, URI/binary>>,
    BinSize = filelib:file_size(PackagePath),
    Etag = <<"\"", PackageName/binary, "\"">>,
    Headers = [{<<"authorization">>, bearer_token(Token)},
               {<<"content-type">>, <<"application/octet-stream">>},
               {<<"content-length">>, integer_to_binary(BinSize)}]
               ++ if_none_match(Force, Etag),
    Options = [{recv_timeout, infinity}| insecure_option(RState)],
    case hackney:request(put, Url, Headers, {file, PackagePath}, Options) of
        {ok, 201, _, _} ->
            ok;
        {ok, 204, _, _} ->
            ok;
        {ok, 400, _, ClientRef} ->
            {ok, _RespBody} = hackney:body(ClientRef),
            error(unknown_request);
        {ok, 401, _, _} ->
            throw(wrong_credentials);
        {ok, 403, _, _} ->
            throw(forbidden);
        {ok, 409, _, _} ->
            throw(package_limit_reached);
        {ok, 412, _, _} ->
            throw(package_already_exists);
        {ok, 413, _, _} ->
            throw(package_too_big);
        Other ->
            error({error, Other})
    end.

%% @doc Performs a POST request to /grisp-manager/api/deploy-update
%% @param Token is the clear token of the user
%% @param PackageName must have the following format "platform.appname.x.y.z.tar
%% @param Device is the serial number of the targeted device
-spec deploy_update(RState, Token, PackageName, Device) -> Res when
      RState      :: rebar_state:t(),
      Token       :: rebar3_grisp_io_config:clear_token(),
      PackageName :: binary(),
      Device      :: binary(),
      Res         :: ok | no_return().
deploy_update(RState, Token, PackageName, Device) ->
    BaseUrl = base_url(RState),
    URI = <<"/grisp-manager/api/deploy-update/", PackageName/binary>>,
    QS = <<"device=", Device/binary>>,
    Url = hackney_url:make_url(BaseUrl, URI, QS),
    Headers = [{<<"authorization">>, bearer_token(Token)},
               {<<"content-type">>, <<"application/json">>},
               {<<"content-length">>, integer_to_binary(0)}],
    Options = insecure_option(RState),

    case hackney:request(post, Url, Headers, <<>>, Options) of
        {ok, 204, _, _} ->
            ok;
        {ok, 400, _, ClientRef} ->
            {ok, _RespBody} = hackney:body(ClientRef),
            error(unknown_request);
        {ok, 401, _, _} ->
            throw(wrong_credentials);
        {ok, 404, _, _} ->
            throw({package_does_not_exist, PackageName});
        Other ->
            error({error, Other})
    end.

%% @doc Performs a POST  request to /grisp-manager/api/validate-update/Device
%% @param Token is the clear token of the user
%% @param Device is the serial number of the targeted device
-spec validate_update(RState, Token, Device) -> Res when
      RState      :: rebar_state:t(),
      Token       :: rebar3_grisp_io_config:clear_token(),
      Device      :: string(),
      Res         :: ok | no_return().
validate_update(RState, Token, Device) ->
    BaseUrl = base_url(RState),
    URI = list_to_binary("/grisp-manager/api/validate-update/" ++ Device),
    Url = <<BaseUrl/binary, URI/binary>>,
    Headers = [{<<"authorization">>, bearer_token(Token)},
               {<<"content-type">>, <<"application/json">>},
               {<<"content-length">>, integer_to_binary(0)}],
    Options = insecure_option(RState),

    case hackney:request(post, Url, Headers, <<>>, Options) of
        {ok, 204, _, _} ->
            ok;
        {ok, 400, _, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            #{<<"error">> := Error} = jsx:decode(RespBody),
            throw({error, Error});
        {ok, 401, _, _} ->
            throw(wrong_credentials);
        {ok, 403, _, _} ->
            throw(forbidden);
        {ok, 404, _, _} ->
            throw(device_does_not_exist);
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

%% @doc Build the header "if-none-match" if force is false
%% The Etag format must be: <<"\"...\"">>
-spec if_none_match(Force, Etag) -> Result when
      Force  :: boolean(),
      Etag   :: binary(),
      Result :: [{binary(), binary()}].
if_none_match(true, _) ->
    [];
if_none_match(false, Etag) ->
    [{<<"if-none-match">>, Etag}].
