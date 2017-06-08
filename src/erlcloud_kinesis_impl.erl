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
-export([request/4, request/5]).

-export_type([json_return/0, attempt/0, retry_fun/0]).

-type json_return() :: {ok, jsx:json_term() | binary()} | {error, term()}.

-type operation() :: string().
-spec request(kinesis | firehose, aws_config(), operation(), jsx:json_term()) -> json_return().
request(RequestType, Config, Operation, Json) ->
    request(RequestType, Config, Operation, Json, true).

-spec request(kinesis | firehose, aws_config(), operation(), jsx:json_term(), boolean()) ->
    json_return().
request(RequestType, Config0, Operation, Json, ShouldDecode) ->
    Body = case Json of
               [] -> <<"{}">>;
               _ -> jsx:encode(Json)
           end,
    case erlcloud_aws:update_config(Config0) of
        {ok, Config} ->
            Headers = headers(RequestType, Config, Operation, Body),
            request_and_retry(RequestType, Config, Headers, Body, ShouldDecode, {attempt, 1});
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
    timer:sleep(erlcloud_util:rand_uniform((1 bsl (Attempt - 1)) * 100)).

-type attempt() :: {attempt, pos_integer()} | {error, term()}.
-type retry_fun() :: fun((pos_integer(), term()) -> attempt()).
-spec retry(pos_integer(), term()) -> attempt().
retry(Attempt, Reason) when Attempt >= ?NUM_ATTEMPTS ->
    {error, Reason};
retry(Attempt, _) ->
    backoff(Attempt),
    {attempt, Attempt + 1}.

-type headers() :: [{string(), string()}].
-spec request_and_retry(kinesis | firehose,
                        aws_config(),
                        headers(),
                        jsx:json_text(),
                        boolean(),
                        attempt()) ->
    {ok, jsx:json_term() | binary()} | {error, term()}.
request_and_retry(_, _, _, _, _, {error, Reason}) ->
    {error, Reason};
request_and_retry(RequestType, Config, Headers, Body, ShouldDecode, {attempt, Attempt})->
    RetryFun = case RequestType of
                   kinesis -> Config#aws_config.kinesis_retry;
                   firehose -> Config#aws_config.firehose_retry
               end,
    case erlcloud_httpc:request(
           url(RequestType, Config), post,
           [{<<"content-type">>, <<"application/x-amz-json-1.1">>} | Headers],
           Body, erlcloud_aws:get_timeout(Config), Config) of

        {ok, {{200, _}, _, RespBody}} ->
            Result = case ShouldDecode of
                         true  -> decode(RespBody);
                         false -> RespBody
                     end,
            {ok, Result};

        {ok, {{Status, StatusLine}, _, RespBody}} when Status >= 400 andalso Status < 500 ->
            case client_error(Status, StatusLine, RespBody) of
                {retry, Reason} ->
                    request_and_retry(RequestType, Config, Headers, Body, ShouldDecode, RetryFun(Attempt, Reason));
                {error, Reason} ->
                    {error, Reason}
            end;

        {ok, {{Status, StatusLine}, _, RespBody}} when Status >= 500 ->
            request_and_retry(RequestType, Config, Headers, Body, ShouldDecode, RetryFun(Attempt, {http_error, Status, StatusLine, RespBody}));

        {ok, {{Status, StatusLine}, _, RespBody}} ->
            {error, {http_error, Status, StatusLine, RespBody}};

        {error, Reason} ->
            %% TODO there may be some http errors, such as certificate error, that we don't want to retry
            request_and_retry(RequestType, Config, Headers, Body, ShouldDecode, RetryFun(Attempt, Reason))
    end.

-spec client_error(pos_integer(), string(), binary()) -> {retry, term()} | {error, term()}.
client_error(Status, StatusLine, Body) ->
    try jsx:decode(Body) of
        Json ->
            Message = proplists:get_value(<<"message">>, Json, <<>>),
            case proplists:get_value(<<"__type">>, Json) of
                undefined ->
                    {error, {http_error, Status, StatusLine, Body}};
                <<"ProvisionedThroughputExceededException">> = Type ->
                    {retry, {Type, Message}};
                <<"ThrottlingException">> = Type ->
                    {retry, {Type, Message}};
                Other ->
                    {error, {Other, Message}}
            end
    catch
        error:badarg ->
            {error, {http_error, Status, StatusLine, Body}}
    end.

-spec headers(kinesis | firehose, aws_config(), string(), binary()) -> headers().
headers(RequestType, Config, Operation, Body) ->
    Host = case RequestType of
              kinesis -> Config#aws_config.kinesis_host;
              firehose -> Config#aws_config.firehose_host
           end,
    Headers = [{"host", Host},
               {"x-amz-target", Operation}],
    erlcloud_aws:sign_v4_headers(Config, Headers, Body, erlcloud_aws:aws_region_from_host(Host), atom_to_list(RequestType)).

url(kinesis, #aws_config{kinesis_scheme = Scheme, kinesis_host = Host} = Config) ->
    lists:flatten([Scheme, Host, port_spec(kinesis, Config)]);
url(firehose, #aws_config{firehose_scheme = Scheme, firehose_host = Host} = Config) ->
    lists:flatten([Scheme, Host, port_spec(firehose, Config)]).

port_spec(kinesis, #aws_config{kinesis_port=80}) ->
    "";
port_spec(kinesis, #aws_config{kinesis_port=Port}) ->
    [":", erlang:integer_to_list(Port)];
port_spec(firehose, #aws_config{firehose_port=80}) ->
    "";
port_spec(firehose, #aws_config{firehose_port=Port}) ->
    [":", erlang:integer_to_list(Port)].

decode(<<>>) -> [];
decode(JSON) -> jsx:decode(JSON).
