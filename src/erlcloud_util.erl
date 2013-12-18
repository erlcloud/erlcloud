-module(erlcloud_util).
-export([sha_mac/2, sha256_mac/2,
         md5/1, sha256/1]).

sha_mac(K, S) ->
	case erlang:function_exported(crypto, hmac, 3) of
		true  -> crypto:hmac(sha, K, S);
		false -> crypto:sha_mac(K, S)
	end.

sha256_mac(K, S) ->
	case erlang:function_exported(crypto, hmac, 3) of
		true  -> crypto:hmac(sha256, K, S);
		false -> crypto:sha256(K, S)
	end.

sha256(V) ->
	case erlang:function_exported(crypto, hash, 2) of
		true  -> crypto:hash(sha256, V);
		false -> crypto:sha256(V)
	end.

md5(V) ->
	case erlang:function_exported(crypto, hash, 2) of
		true  -> crypto:hash(md5, V);
		false -> crypto:md5(V)
	end.
