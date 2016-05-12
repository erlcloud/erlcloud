-module(erlcloud_util).
-export([sha_mac/2, sha256_mac/2,
         md5/1, sha256/1]).

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

-ifdef(old_hash).
sha256(V) ->
    crypto:sha256(V).
-else.
sha256(V) ->
    crypto:hash(sha256, V).
-endif.

-ifdef(old_hash).
md5(V) ->
    crypto:md5(V).
-else.
md5(V) ->
    crypto:hash(md5, V).
-endif.
