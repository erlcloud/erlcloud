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

%% @author Ransom Richardson <ransom@ransomr.net>
%% @doc
%%
%% Implementation of requests to DynamoDB. This code is shared accross
%% all API versions.
%%
%% @end

-module(erlcloud_kinesis_impl).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

%% Helpers
-export([backoff/1, retry/2]).

%% Internal impl api
-export([request/3]).

-export_type([json_return/0, attempt/0, retry_fun/0]).

-type json_return() :: {ok, jsx:json_term()} | {error, term()}.

-type operation() :: string().
-spec request(aws_config(), operation(), jsx:json_term()) -> json_return().
request(Config0, Operation, Json) ->
    Body = case Json of
               [] -> <<"{}">>;
               _ -> jsx:encode(Json)
           end,
    case erlcloud_aws:update_config(Config0) of
        {ok, Config} ->
            Headers = headers(Config, Operation, Body),
            request_and_retry(Config, Headers, Body, {attempt, 1});
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

%% Sleep after an attempt
-spec backoff(pos_integer()) -> ok.
backoff(1) -> ok;
backoff(Attempt) ->
    timer:sleep(random:uniform((1 bsl (Attempt - 1)) * 100)).

-type attempt() :: {attempt, pos_integer()} | {error, term()}.
-type retry_fun() :: fun((pos_integer(), term()) -> attempt()).
-spec retry(pos_integer(), term()) -> attempt().
retry(Attempt, Reason) when Attempt >= ?NUM_ATTEMPTS ->
    {error, Reason};
retry(Attempt, _) ->
    backoff(Attempt),
    {attempt, Attempt + 1}.

-type headers() :: [{string(), string()}].
-spec request_and_retry(aws_config(), headers(), jsx:json_text(), attempt()) ->
                               {ok, jsx:json_term()} | {error, term()}.
request_and_retry(_, _, _, {error, Reason}) ->
    {error, Reason};
request_and_retry(Config, Headers, Body, {attempt, Attempt}) ->
    RetryFun = Config#aws_config.kinesis_retry,
    case httpc:request(post, {url(Config), Headers, "application/x-amz-json-1.1", Body},
                       [{timeout, 1000}],
                       [{body_format, binary}]) of

        {ok, {{_, 200, _}, _, RespBody}} ->
            %% TODO check crc
            {ok, jsx:decode(RespBody)};

        {ok, {{_, Status, StatusLine}, _, RespBody}} when Status >= 400 andalso Status < 500 ->
            case client_error(Status, StatusLine, RespBody) of
                {retry, Reason} ->
                    request_and_retry(Config, Headers, Body, RetryFun(Attempt, Reason));
                {error, Reason} ->
                    {error, Reason}
            end;

        {ok, {{_, Status, StatusLine}, _, RespBody}} when Status >= 500 ->
            request_and_retry(Config, Headers, Body, RetryFun(Attempt, {http_error, Status, StatusLine, RespBody}));

        {ok, {{_, Status, StatusLine}, _, RespBody}} ->
            {error, {http_error, Status, StatusLine, RespBody}};

        {error, Reason} ->
            %% TODO there may be some http errors, such as certificate error, that we don't want to retry
            request_and_retry(Config, Headers, Body, RetryFun(Attempt, Reason))
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
                    Message = proplists:get_value(<<"message">>, Json, <<>>),
                    case binary:split(FullType, <<"#">>) of
                        [_, <<"ProvisionedThroughputExceededException">> = Type] ->
                            {retry, {Type, Message}};
                        [_, <<"ThrottlingException">> = Type] ->
                            {retry, {Type, Message}};
                        [_, Type] ->
                            {error, {Type, Message}};
                        _ ->
                            {error, {http_error, Status, StatusLine, Body}}
                    end
            end
    end.

-spec headers(aws_config(), string(), binary()) -> headers().
headers(Config, Operation, Body) ->
    Headers = [{"host", Config#aws_config.kinesis_host},
               {"x-amz-target", Operation}],
    Region =
        case string:tokens(Config#aws_config.kinesis_host, ".") of
            [_, Value, _, _] ->
                Value;
            _ ->
                "us-east-1"
        end,
    erlcloud_aws:sign_v4(Config, Headers, Body, Region, "kinesis").

url(#aws_config{kinesis_scheme = Scheme, kinesis_host = Host} = Config) ->
    lists:flatten([Scheme, Host, port_spec(Config)]).

port_spec(#aws_config{kinesis_port=80}) ->
    "";
port_spec(#aws_config{kinesis_port=Port}) ->
    [":", erlang:integer_to_list(Port)].

