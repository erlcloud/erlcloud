-module(erlcloud_util).
-export([sha_mac/2, sha256_mac/2, md5/1, sha256/1]).

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

sha256(V) ->
    try
        crypto:hash(sha256, V)
    catch
        _:_ ->
            crypto:sha256(V)
    end.

md5(V) ->
    try
        crypto:hash(md5, V)
    catch
        _:_ ->
            crypto:md5(V)
    end.
