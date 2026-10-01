-module(rebar3_grisp_io_config).

% API
-export([write_config/2]).
-export([read_config/1]).
-export([delete_config/1]).
-export([encrypt_token/2]).
-export([try_decrypt_token/2]).
-export([get_token/1]).

%--- Includes ------------------------------------------------------------------

-import(rebar3_grisp_io_io, [
    abort/1,
    abort/2]).

%--- Macros --------------------------------------------------------------------

-define(CONFIG_FILE, "grisp-io.config").
-define(AES, aes_256_gcm).
-define(AES_KEY_SIZE, 256).
-define(AAD, <<"LetItCrash">>).

%--- Types ---------------------------------------------------------------------

-type encrypted_token() :: #{iv => binary(),
                             tag => binary(),
                             encrypted_token => binary()}.

-type clear_token() :: binary().
-type config() :: #{username => binary(),
                    encrypted_token := encrypted_token()} |
                  #{username => binary(), token := clear_token()}.

-export_type([encrypted_token/0, clear_token/0]).
%--- API -----------------------------------------------------------------------

%% @doc Write the new configuration stored
%% The Config map may contain either an encrypted_token or a clear token.
-spec write_config(rebar_state:t(), config()) -> ok.
write_config(State, Config) ->
    GIoConfigFile = auth_config_file(State),
    ok = filelib:ensure_dir(GIoConfigFile),
    NewConfig = iolist_to_binary(["%% coding: utf-8", io_lib:nl(),
                                  io_lib:print(Config), ".", io_lib:nl()]),
    ok = file:write_file(GIoConfigFile, NewConfig, [{encoding, utf8}]).


%% @doc Read the stored configuration file
%% Reads either the encrypted or unencrypted token representation.
-spec read_config(rebar_state:t()) -> config() | no_return().
read_config(State) ->
    GIoConfigFile = auth_config_file(State),
    case file:consult(GIoConfigFile) of
        {ok, [Config]} ->
            Config;
        {error, enoent} ->
            throw(enoent);
        {error, Reason} ->
            error(Reason)
    end.

%% @doc Delete the locally stored authentication configuration.
-spec delete_config(rebar_state:t()) -> ok | no_return().
delete_config(State) ->
    case file:delete(auth_config_file(State)) of
        ok -> ok;
        {error, enoent} -> ok;
        {error, Reason} -> error(Reason)
    end.

%% @doc encrypt the token provided in the args
-spec encrypt_token(binary(), clear_token()) -> encrypted_token().
encrypt_token(LocalPassword, Token) ->
    PaddedPswd = password_padding(LocalPassword),
    IV = crypto:strong_rand_bytes(16),
    {EncrToken, Tag} = crypto:crypto_one_time_aead(?AES, PaddedPswd, IV,
                                                   Token, ?AAD, true),
    #{iv => IV, tag => Tag, encrypted_token => EncrToken}.


-spec try_decrypt_token(Password, EncryptedToken) -> Result | no_return() when
    Password :: binary(),
    EncryptedToken :: rebar3_grisp_io_config:encrypted_token(),
    Result         :: rebar3_grisp_io_config:clear_token().
try_decrypt_token(Password, EncryptedToken) ->
    case decrypt_token(Password, EncryptedToken) of
        error ->
            throw(wrong_local_password);
        T ->
            T
    end.


%% Return a stored token, prompting for a password only for encrypted config.
-spec get_token(config()) -> clear_token().
get_token(#{token := Token}) ->
    Token;
get_token(#{encrypted_token := EncryptedToken}) ->
    Password = rebar3_grisp_io_io:ask("Local password", password),
    rebar3_grisp_io_config:try_decrypt_token(Password, EncryptedToken).
%--- Internals -----------------------------------------------------------------
%% @doc Decrypt the token present in Encrypted token
-spec decrypt_token(binary(), encrypted_token()) -> clear_token() | error.
decrypt_token(LocalPassword, TokenMap) ->
    PaddedPswd = password_padding(LocalPassword),
    #{iv := IV, tag := Tag, encrypted_token := EncrToken} = TokenMap,
    crypto:crypto_one_time_aead(?AES, PaddedPswd, IV,
                                EncrToken, ?AAD, Tag, false).

auth_config_file(State) ->
    filename:join(rebar_dir:global_config_dir(State), ?CONFIG_FILE).

password_padding(LocalPassword) when bit_size(LocalPassword) < ?AES_KEY_SIZE ->
    NbPaddingBits = ?AES_KEY_SIZE - bit_size(LocalPassword),
    <<LocalPassword/binary, 0:NbPaddingBits>>;
password_padding(_) ->
    error(local_password_too_big).
