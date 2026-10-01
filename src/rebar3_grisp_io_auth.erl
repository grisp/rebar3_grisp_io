-module(rebar3_grisp_io_auth).

% Callbacks
-export([init/1]).
-export([do/1]).
-export([format_error/1]).

%--- Includes ------------------------------------------------------------------

-include("rebar3_grisp_io.hrl").
-import(rebar3_grisp_io_io, [
    abort/1,
    abort/2,
    ask/2,
    ask/3,
    console/1,
    console/2,
    error_message/1,
    success/1,
    success/2]).

%--- API -----------------------------------------------------------------------

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, auth},
        {module, ?MODULE},
        {bare, true},
        {example, "rebar3 grisp_io auth"},
        {opts, options()},
        {profile, [default]},
        {short_desc, "Authenticate yourself to grisp.io"},
        {desc, "Authenticate yourself to your grisp.io account"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(RState) ->
    {ok, _} = application:ensure_all_started(rebar3_grisp_io),
    try
        {Args, _} = rebar_state:command_parsed_args(RState),
        case proplists:get_value(credentials, Args, false) of
            true -> auth_credentials(RState, Args);
            false -> auth_pkce()
        end,
        {ok, RState}
    catch
        throw:wrong_credentials ->
            abort("Error: Wrong credentials");
        throw:token_limit_reached ->
            abort("Error: Maximum number of tokens per user reached" ++
                  " Revoke unused tokens and try again");
        throw:forbidden ->
            abort("Error: No permission to perform this operation");
        throw:not_matching ->
            abort("Error: The 2 local password entries don't match");
        throw:invalid_encrypt_token_choice ->
            abort("Error: --encrypt-token must be true or false")
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%--- Internals -----------------------------------------------------------------
options() -> [
    {credentials, undefined, "credentials", {boolean, false},
     "Authenticate with username and password instead of PKCE"},
    {encrypt_token, undefined, "encrypt-token", string,
     "Encrypt the saved token (true or false); omit to choose interactively"}
].

auth_credentials(RState, Args) ->
    Username = ask("Username", string),
    Password = ask("Password", password),
    Token = rebar3_grisp_io_api:auth(RState, Username, Password),
    EncryptToken = encryption_choice(proplists:get_value(encrypt_token, Args)),
    Config = save_config(EncryptToken, Username, Token),
    rebar3_grisp_io_config:write_config(RState, Config),
    success("Token successfully requested").

auth_pkce() ->
    console("PKCE login flow is not implemented yet").

encryption_choice(undefined) ->
    ask_encryption_choice();
encryption_choice("true") -> true;
encryption_choice("false") -> false;
encryption_choice(true) -> true;
encryption_choice(false) -> false;
encryption_choice(_) ->
    throw(invalid_encrypt_token_choice).

ask_encryption_choice() ->
    Response = ask("Encrypt token locally? (y/N)", string, <<"n">>),
    case string:lowercase(unicode:characters_to_list(Response)) of
        "y" -> true;
        "yes" -> true;
        "n" -> false;
        "no" -> false;
        _ ->
            error_message("Please answer yes or no"),
            ask_encryption_choice()
    end.

save_config(true, Username, Token) ->
    success("Please provide a local password to encrypt the token"),
    LocalPassword = ask_local_password(),
    EncToken = rebar3_grisp_io_config:encrypt_token(LocalPassword, Token),
    #{encrypted_token => EncToken, username => Username};
save_config(false, Username, Token) ->
    #{token => Token, username => Username}.

ask_local_password() ->
    LocalPassword = ask("Local password", password),
    RepeatedLocalPswd = ask("Confirm your local password", password),

    case LocalPassword =:= RepeatedLocalPswd of
        true -> LocalPassword;
        _ -> throw(not_matching)
    end.
