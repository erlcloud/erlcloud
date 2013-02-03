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

%% DDB API Functions
-export([batch_get_item/1, batch_get_item/2,
         batch_write_item/1, batch_write_item/2,
         delete_item/2, delete_item/3, delete_item/4,
         get_item/2, get_item/3, get_item/4,
         put_item/2, put_item/3, put_item/4,
         %% Note that query is a Erlang reserved word, so we use q instead
         q/2, q/3, q/4,
         update_item/3, update_item/4, update_item/5
        ]).

%% Internal use only
-export([key_value/1]).

-export_type([key/0, batch_write_item_request/0]).

-type table_name() :: binary().
-type attr_type() :: binary().
-type attr_value() :: binary().
-type attr() :: {attr_type(), attr_value()}.
-type hash_key() :: attr().
-type range_key() :: attr().
-type hash_range_key() :: {hash_key(), range_key()}.
-type key() :: hash_key() | hash_range_key().
-type item() :: jsx:json_term().
-type opts() :: jsx:json_term().
-type updates() :: jsx:json_term().
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

-spec hash_key_json(hash_key()) -> {binary(), jsx:json_term()}.
hash_key_json(HashKey) ->
    {<<"HashKeyValue">>, [HashKey]}.

-spec item_json(item()) -> {binary(), item()}.
item_json(Item) ->
    {<<"Item">>, Item}.

-spec updates_json(updates()) -> {binary(), updates()}.
updates_json(Updates) ->
    {<<"AttributeUpdates">>, Updates}.


-type batch_get_item_request_item() :: {table_name(), [key(),...], opts()} | {table_name(), [key(),...]}.

-spec batch_get_item_request_item_json(batch_get_item_request_item()) -> {binary(), jsx:json_term()}.
batch_get_item_request_item_json({Table, Keys}) ->
    batch_get_item_request_item_json({Table, Keys, []});
batch_get_item_request_item_json({Table, Keys, Optional}) ->
    {Table, [{<<"Keys">>, [key_value(K) || K <- Keys]}] ++ Optional}.

-spec batch_get_item([batch_get_item_request_item()]) -> json_reply().
batch_get_item(RequestItems) ->
    batch_get_item(RequestItems, default_config()).

-spec batch_get_item([batch_get_item_request_item()], aws_config()) -> json_reply().
batch_get_item(RequestItems, Config) ->
    JSON = [{<<"RequestItems">>, [batch_get_item_request_item_json(R) || R <- RequestItems]}],
    request(Config, "BatchGetItem", JSON).

-type batch_write_item_put() :: {put, item()}.
-type batch_write_item_delete() :: {delete, key()}.
-type batch_write_item_request() :: batch_write_item_put() | batch_write_item_delete().
-type batch_write_item_request_item() :: {table_name(), [batch_write_item_request()]}.

-spec batch_write_item_request_json(batch_write_item_request()) -> {binary(), jsx:json_term()}.
batch_write_item_request_json({put, Item}) ->
    {<<"PutRequest">>, [item_json(Item)]};
batch_write_item_request_json({delete, Key}) ->
    {<<"DeleteRequest">>, [key_json(Key)]}.

-spec batch_write_item_request_item_json(batch_write_item_request_item()) -> {binary(), jsx:json_term()}.
batch_write_item_request_item_json({Table, Requests}) ->
    {Table, [[batch_write_item_request_json(R)] || R <- Requests]}.

-spec batch_write_item([batch_write_item_request_item()]) -> json_reply().
batch_write_item(RequestItems) ->
    batch_get_item(RequestItems, default_config()).

-spec batch_write_item([batch_write_item_request_item()], aws_config()) -> json_reply().
batch_write_item(RequestItems, Config) ->
    JSON = [{<<"RequestItems">>, [batch_write_item_request_item_json(R) || R <- RequestItems]}],
    request(Config, "BatchWriteItem", JSON).


-spec delete_item(table_name(), key()) -> json_reply().
delete_item(Table, Key) ->
    delete_item(Table, Key, [], default_config()).

-spec delete_item(table_name(), key(), opts()) -> json_reply().
delete_item(Table, Key, Optional) ->
    delete_item(Table, Key, Optional, default_config()).

-spec delete_item(table_name(), key(), opts(), aws_config()) -> json_reply().
delete_item(Table, Key, Optional, Config) ->
    JSON = [{<<"TableName">>, Table},
            key_json(Key)] 
        ++ Optional,
    request(Config, "DeleteItem", JSON).

    
-spec get_item(table_name(), key()) -> json_reply().
get_item(Table, Key) ->
    get_item(Table, Key, [], default_config()).

-spec get_item(table_name(), key(), opts()) -> json_reply().
get_item(Table, Key, Optional) ->
    get_item(Table, Key, Optional, default_config()).

-spec get_item(table_name(), key(), opts(), aws_config()) -> json_reply().
get_item(Table, Key, Optional, Config) ->
    JSON = [{<<"TableName">>, Table},
            key_json(Key)] 
        ++ Optional,
    request(Config, "GetItem", JSON).


-spec put_item(table_name(), item()) -> json_reply().
put_item(Table, Item) ->
    put_item(Table, Item, [], default_config()).

-spec put_item(table_name(), item(), opts()) -> json_reply().
put_item(Table, Item, Optional) ->
    put_item(Table, Item, Optional, default_config()).

-spec put_item(table_name(), item(), opts(), aws_config()) -> json_reply().
put_item(Table, Item, Optional, Config) ->
    JSON = [{<<"TableName">>, Table},
            item_json(Item)] 
        ++ Optional,
    request(Config, "PutItem", JSON).


-spec q(table_name(), hash_key()) -> json_reply().
q(Table, HashKey) ->
    q(Table, HashKey, [], default_config()).

-spec q(table_name(), hash_key(), opts()) -> json_reply().
q(Table, HashKey, Optional) ->
    q(Table, HashKey, Optional, default_config()).

-spec q(table_name(), hash_key(), opts(), aws_config()) -> json_reply().
q(Table, HashKey, Optional, Config) ->
    JSON = [{<<"TableName">>, Table},
            hash_key_json(HashKey)] 
        ++ Optional,
    request(Config, "Query", JSON).


-spec update_item(table_name(), key(), updates()) -> json_reply().
update_item(Table, Key, Updates) ->
    update_item(Table, Key, Updates, [], default_config()).

-spec update_item(table_name(), key(), updates(), opts()) -> json_reply().
update_item(Table, Key, Updates, Optional) ->
    update_item(Table, Key, Updates, Optional, default_config()).

-spec update_item(table_name(), key(), updates(), opts(), aws_config()) -> json_reply().
update_item(Table, Key, Updates, Optional, Config) ->
    JSON = [{<<"TableName">>, Table},
            key_json(Key),
            updates_json(Updates)] 
        ++ Optional,
    request(Config, "UpdateItem", JSON).

    
-type operation() :: string().
-spec request(aws_config(), operation(), jsx:json_term()) -> json_reply().
request(Config0, Operation, JSON) ->
    Body = jsx:term_to_json(JSON),
    case erlcloud_aws:update_config(Config0) of
        {ok, Config} ->
            Headers = headers(Config, Operation, Body),
            request_and_retry(Config, Headers, Body, 1, undefined);
        {error, Reason} ->
            {error, Reason}
    end.

%% Error handling
%% see http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ErrorHandling.html

%% Although it is documented that you should use exponential backoff, exact delays or number of retries
%% are not specified.
%% boto (if I read the code correctly) waits 2^(Attempt - 2)*50ms before an attempt and will make 10 attempts,
%% which means it will wait up to 12.8 seconds before the last attempt.
%% This algorithm is similar, except that it waits a random interval up to 2^(Attempt-2)*100ms. The average
%% wait time should be the same as boto.

%% TODO make delay configurable
%% TODO refactor retry logic so that it can be used by all requests and move to erlcloud_aws

-define(NUM_ATTEMPTS, 10).

-spec backoff(pos_integer()) -> ok.
backoff(1) -> ok;
backoff(2) -> ok;
backoff(Attempt) -> 
    timer:sleep(random:uniform((1 bsl (Attempt - 2)) * 100)).
    
-type headers() :: [{string(), string()}].
-spec request_and_retry(aws_config(), headers(), jsx:json_text(), pos_integer(), term()) -> 
                               {ok, jsx:json_term()} | {error, term()}.
request_and_retry(_, _, _, Attempt, LastReason) when Attempt > ?NUM_ATTEMPTS ->
    {error, LastReason};
request_and_retry(Config, Headers, Body, Attempt, _) ->
    backoff(Attempt),
    case httpc:request(post, {url(Config), Headers, "application/x-amz-json-1.0", Body}, [], 
                       [{body_format, binary}]) of

        {ok, {{_, 200, _}, _, RespBody}} ->
            %% TODO check crc
            {ok, jsx:decode(RespBody)};

        {ok, {{_, Status, StatusLine}, _, RespBody}} when Status >= 400 andalso Status < 500 ->
            case client_error(Status, StatusLine, RespBody) of
                {retry, Reason} ->
                    request_and_retry(Config, Headers, Body, Attempt + 1, Reason);
                {error, Reason} ->
                    {error, Reason}
            end;
                            
        {ok, {{_, Status, StatusLine}, _, RespBody}} when Status >= 500 ->
            request_and_retry(Config, Headers, Body, Attempt + 1, {http_error, Status, StatusLine, RespBody});
                            
        {ok, {{_, Status, StatusLine}, _, RespBody}} ->
            {error, {http_error, Status, StatusLine, RespBody}};
                            
        {error, Reason} ->
            %% TODO there may be some http errors, such as certificate error, that we don't want to retry
            request_and_retry(Config, Headers, Body, Attempt + 1, Reason)
    end.

-spec client_error(pos_integer(), string(), binary()) -> {retry, term()} | {error, term()}.
client_error(Status, StatusLine, Body) ->
    case jsx:is_json(Body) of
        false ->
            {error, {http_error, Status, StatusLine, Body}};
        true ->
            Json = jsx:decode(Body),
            case proplists:get_value(<<"__type">>, Json) of
                undefined ->
                    {error, {http_error, Status, StatusLine, Body}};
                FullType ->
                    case binary:split(FullType, <<"#">>) of
                        [_, <<"ProvisionedThroughputExceededException">> = Type] ->
                            {retry, Type};
                        [_, <<"ThrottlingException">> = Type] ->
                            {retry, Type};
                        [_, Type] ->
                            {error, Type};
                        _ ->
                            {error, {http_error, Status, StatusLine, Body}}
                    end
            end
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
