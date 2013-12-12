-module(erlcloud_util).
-export([sha_mac/2, sha256_mac/2,
         md5/1, sha256/1]).

-ifndef(old_hash).
sha_mac(K, S) ->
    crypto:hmac(sha, K, S).
sha256_mac(K, S) ->
    crypto:hmac(sha256, K, S).

sha256(V) ->
    crypto:hash(sha256, V).
md5(V) ->
    crypto:hash(md5, V).
-else.
sha_mac(K, S) ->
    crypto:sha_mac(K, S).
sha256_mac(K, S) ->
    crypto:sha256(K, S).

sha256(V) ->
    crypto:sha256(V).
md5(V) ->
    crypto:md5(V).
-endif.

