%--- Macros --------------------------------------------------------------------

-define(NAMESPACE, 'grisp-io').
-define(AES, aes_256_ecb).

%--- Types ---------------------------------------------------------------------

-type config() :: #{username => binary(),
                    encrypted_token => binary()}.
