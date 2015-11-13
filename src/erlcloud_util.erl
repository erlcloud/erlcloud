-module(erlcloud_util).
-export([sha_mac/2, sha256_mac/2, md5/1, sha256/1,
         is_dns_compliant_name/1]).

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

-spec is_dns_compliant_name(string()) -> boolean().
is_dns_compliant_name(Name) ->
    RegExp = "^(([a-z0-9]|[a-z0-9][a-z0-9\\-]*[a-z0-9])\\.)*([a-z0-9]|[a-z0-9][a-z0-9\\-]*[a-z0-9])$",
    case re:run(Name, RegExp) of
        nomatch ->
            false;
        _ ->
            true
    end.
