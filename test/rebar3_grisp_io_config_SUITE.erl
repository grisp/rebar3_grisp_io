-module(rebar3_grisp_io_config_SUITE).

% callbacks
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

% testcases
-export([read_write_config_test/1]).
-export([encrypt_decrypt_token/1]).

%--- Include -------------------------------------------------------------------

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%--- Callbacks -----------------------------------------------------------------

all() -> [
    read_write_config_test,
    encrypt_decrypt_token
].

init_per_suite(Config) ->
    rebar3_grisp_io_common_test:init_per_suite(Config).

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    Token = <<"abcdefghijklmnop">>,
    Password = <<"s3cur3pa55w0rd">>,
    [{token, Token}, {local_password, Password} | Config].

end_per_testcase(_, _Config) ->
    ok.

%--- Testcases -----------------------------------------------------------------

read_write_config_test(Config) ->
    RState = ?config(rebar_state, Config),
    Token = ?config(token, Config),
    Password = ?config(local_password, Config),
    EncryptedToken = rebar3_grisp_io_config:encrypt_token(Password, Token),
    PluginConfig = #{username => <<"Test">>, encrypted_token => EncryptedToken},
    rebar3_grisp_io_config:write_config(RState, PluginConfig),
    ReadConfig = rebar3_grisp_io_config:read_config(RState),
    ?assertEqual(PluginConfig, ReadConfig).

encrypt_decrypt_token(Config) ->
    Token = ?config(token, Config),
    Password = ?config(local_password, Config),
    BadPassword = <<"aaaaaaaaaaaaaaaa">>,
    EncryptedToken = rebar3_grisp_io_config:encrypt_token(Password, Token),
    ?assertNotEqual(<<>>, EncryptedToken),
    ?assertNotEqual(Token, EncryptedToken),
    ?assertEqual(Token,
                 rebar3_grisp_io_config:try_decrypt_token(Password, EncryptedToken)),
    ?assertThrow(wrong_local_password,
                 rebar3_grisp_io_config:try_decrypt_token(BadPassword, EncryptedToken)).
