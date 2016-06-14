-module(erlcloud_util).
-export([sha_mac/2,
         sha256_mac/2,
         md5/1,
         sha256/1,
         rand_uniform/1]).

sha_mac(K, S) ->
    try
        crypto:hmac(sha, K, S)
    catch
        error:undef ->
            R0 = crypto:hmac_init(sha, K),
            R1 = crypto:hmac_update(R0, S),
            crypto:hmac_final(R1)
    end.
        
sha256_mac(K, S) ->
    try
        crypto:hmac(sha256, K, S)
    catch
        error:undef ->
            R0 = crypto:hmac_init(sha256, K),
            R1 = crypto:hmac_update(R0, S),
            crypto:hmac_final(R1)
    end.

-ifdef(ERLANG_OTP_VERSION_14).
-else.
-ifdef(ERLANG_OTP_VERSION_15).
-else.
-define(ERLANG_OTP_VERSION_16_FEATURES, true).
-ifdef(ERLANG_OTP_VERSION_16).
-else.
-ifdef(ERLANG_OTP_VERSION_17).
-else.
-ifdef(ERLANG_OTP_VERSION_18).
-else.
-define(ERLANG_OTP_VERSION_19_FEATURES, true).
-endif.
-endif.
-endif.
-endif.
-endif.

-ifndef(ERLANG_OTP_VERSION_16_FEATURES).
sha256(V) ->
    crypto:sha256(V).
-else.
sha256(V) ->
    crypto:hash(sha256, V).
-endif.

-ifndef(ERLANG_OTP_VERSION_16_FEATURES).
md5(V) ->
    crypto:md5(V).
-else.
md5(V) ->
    crypto:hash(md5, V).
-endif.

-ifndef(ERLANG_OTP_VERSION_19_FEATURES).
rand_uniform(N) ->
    random:uniform(N).
-else.
rand_uniform(N) ->
    rand:uniform(N).
-endif.

