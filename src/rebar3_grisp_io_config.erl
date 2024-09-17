-module(rebar3_grisp_io_config).

% API
-export([write_config/2]).
-export([read_config/1]).
-export([encrypt_token/2]).
-export([decrypt_token/2]).

%--- Includes ------------------------------------------------------------------

-import(rebar3_grisp_io_io, [
    abort/1,
    abort/2]).

%--- Macros --------------------------------------------------------------------

-define(CONFIG_FILE, "grisp-io.config").
-define(AES, aes_256_ecb).
-define(AES_KEY_SIZE, 256).

%--- Types ---------------------------------------------------------------------


%--- API -----------------------------------------------------------------------

%% @doc Write the new configuration stored
%% Note: The token must be already encrypted in the Config map
write_config(State, Config) ->
    GIoConfigFile = auth_config_file(State),
    ok = filelib:ensure_dir(GIoConfigFile),
    NewConfig = iolist_to_binary(["%% coding: utf-8", io_lib:nl(),
                                  io_lib:print(Config), ".", io_lib:nl()]),
    ok = file:write_file(GIoConfigFile, NewConfig, [{encoding, utf8}]).


%% @doc Read the stored configuration file
%% Note: The stored token stays encrypted in the Config map
-spec read_config(rebar_state:t()) -> map() | no_return().
read_config(State) ->
    GIoConfigFile = auth_config_file(State),
    case file:consult(GIoConfigFile) of
        {ok, [Config]} ->
            Config;
        {error, Reason} ->
            error(Reason)
    end.

encrypt_token(LocalPassword, Token) ->
    PaddedPassword = password_padding(LocalPassword),
    crypto:crypto_one_time(?AES, PaddedPassword, Token, true).

%% @todo
decrypt_token(_LocalPassword, _Config) ->
    ok.
%--- Internals -----------------------------------------------------------------
auth_config_file(State) ->
    filename:join(rebar_dir:global_config_dir(State), ?CONFIG_FILE).

password_padding(LocalPassword) when bit_size(LocalPassword) < ?AES_KEY_SIZE ->
    NbPaddingBits = ?AES_KEY_SIZE - bit_size(LocalPassword),
    <<LocalPassword/binary, 0:NbPaddingBits>>;
password_padding(_) ->
    error(local_password_too_big).

