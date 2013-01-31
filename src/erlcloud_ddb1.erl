%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%%% Inspired by, and some code taken from https://github.com/wagerlabs/ddb, which is:
%%%
%%% Copyright (C) 2012 Issuu ApS. All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%% 1. Redistributions of source code must retain the above copyright
%%%    notice, this list of conditions and the following disclaimer.
%%% 2. Redistributions in binary form must reproduce the above copyright
%%%    notice, this list of conditions and the following disclaimer in the
%%%    documentation and/or other materials provided with the distribution.
%%%
%%% THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS ``AS IS'' AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
%%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
%%% OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
%%% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
%%% SUCH DAMAGE.

%%% erlcloud_ddb1 is a low level wrapper around the DyanmoDB API.
%%% It is similar to the layer1 API in boto.
-module(erlcloud_ddb1).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

%% Library initialization.
-export([configure/2, configure/3, new/2, new/3]).

%% DDB API Functions
-export([get_item/2, get_item/3, get_item/4
        ]).

-export_type([key/0]).

-spec(new/2 :: (string(), string()) -> aws_config()).
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey}.

-spec(new/3 :: (string(), string(), string()) -> aws_config()).
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                ddb_host=Host}.

-spec(configure/2 :: (string(), string()) -> ok).
configure(AccessKeyID, SecretAccessKey) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey)),
    ok.

-spec(configure/3 :: (string(), string(), string()) -> ok).
configure(AccessKeyID, SecretAccessKey, Host) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host)),
    ok.

-type table_name() :: binary().
-type attr_type() :: binary().
-type attr_value() :: binary().
-type attr() :: {attr_type(), attr_value()}.
-type hash_key() :: attr().
-type range_key() :: attr().
-type hash_range_key() :: {hash_key(), range_key()}.
-type key() :: hash_key() | hash_range_key().
-type json_reply() :: {ok, jsx:json_term()} | {error, term()}.

-spec key_value(key()) -> jsx:json_term().
key_value({{HK, HV} = HashKey, {RK, RV} = RangeKey}) when
      is_binary(HK), is_binary(HV), is_binary(RK), is_binary(RV) ->
    [{<<"HashKeyElement">>, [HashKey]}, {<<"RangeKeyElement">>, [RangeKey]}];
key_value({HK, HV} = HashKey) when
      is_binary(HK), is_binary(HV) ->      
    [{<<"HashKeyElement">>, [HashKey]}].
    
-spec key_json(key()) -> {binary(), jsx:json_term()}.
key_json(Key) ->
    {<<"Key">>, key_value(Key)}.
    
-spec get_item(table_name(), key()) -> json_reply().
get_item(Table, Key) ->
    get_item(Table, Key, [], default_config()).

-spec get_item(table_name(), key(), jsx:json_term()) -> json_reply().
get_item(Table, Key, Optional) ->
    get_item(Table, Key, Optional, default_config()).

-spec get_item(table_name(), key(), jsx:json_term(), aws_config()) -> json_reply().
get_item(Table, Key, Optional, Config) ->
    JSON = [{<<"TableName">>, Table},
            key_json(Key)] 
        ++ Optional,
    request(Config, "GetItem", JSON).

-type operation() :: string().
-spec request(aws_config(), operation(), jsx:json_term()) -> json_reply().
request(Config0, Operation, JSON) ->
    Body = jsx:term_to_json(JSON),
    case erlcloud_aws:update_config(Config0) of
        {ok, Config} ->
            Headers = headers(Config, Operation, Body),
            request_and_retry(Config, Headers, Body);
        {error, Reason} ->
            {error, Reason}
    end.

-spec request_and_retry(aws_config(), headers(), jsx:json_text()) -> term().
request_and_retry(Config, Headers, Body) ->
    io:format("HEADERS ~p~n", [Headers]),
    case httpc:request(post, {url(Config), Headers, "application/x-amz-json-1.0", Body}, [], 
                       [{body_format, binary}]) of
        {ok, {{_HTTPVer, 200, _StatusLine}, _RespHeaders, RespBody}} ->
            %% TODO check crc
            {ok, jsx:decode(RespBody)}
            %% TODO retry logic
    end.

-spec headers(aws_config(), string(), binary()) -> headers().
headers(Config, Operation, Body) ->
    Date = httpd_util:rfc1123_date(erlang:localtime()),
    Headers = [{"x-amz-date", Date},
               {"x-amz-target", "DynamoDB_20111205." ++ Operation}]
        %% TODO removed content type header here - don't think I need it
        ++ case Config#aws_config.security_token of
               undefined -> [];
               SecurityToken -> [{"x-amz-security-token", SecurityToken}]
           end,
    Authorization = authorization(Config, Headers, Body),
    [{"x-amzn-authorization", Authorization} | Headers].

%% TODO switch to AWS4 authorization
authorization(Config, Headers, Body) ->
    Signature = signature(Config, Headers, Body),
    lists:flatten(io_lib:format("AWS3 AWSAccessKeyId=~s,Algorithm=HmacSHA1,Signature=~s", 
                                [Config#aws_config.access_key_id, Signature])).

signature(Config, Headers, Body) ->
    StringToSign = lists:flatten(["POST", $\n, "/", $\n, $\n, canonical(Config, Headers), $\n, Body]),
    BytesToSign = crypto:sha(StringToSign),
    base64:encode_to_string(binary_to_list(crypto:sha_mac(Config#aws_config.secret_access_key, BytesToSign))).

canonical(Config, Headers) ->
    Headers1 = lists:map(fun({K, V}) -> {string:to_lower(K), V} end, Headers),
    Amz = lists:filter(fun({K, _V}) -> lists:prefix("x-amz-", K) end, Headers1),
    Headers2 = [{"host", Config#aws_config.ddb_host} | lists:sort(Amz)],
    [[K, $:, V, $\n] || {K, V} <- Headers2].

url(#aws_config{ddb_scheme = Scheme, ddb_host = Host} = Config) ->
    lists:flatten([Scheme, Host, port_spec(Config)]).

port_spec(#aws_config{ddb_port=80}) ->
    "";
port_spec(#aws_config{ddb_port=Port}) ->
    [":", erlang:integer_to_list(Port)].

default_config() -> erlcloud_aws:default_config().
