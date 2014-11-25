%% @author Ransom Richardson <ransom@ransomr.net>
%% @doc
%%
%% HTTP client abstraction for erlcloud. Simplifies changing http clients.
%% API matches lhttpc, except Config is passed instead of options for
%% future cusomizability.
%%
%% @end
-module(erlcloud_httpc).
-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").
-export([request/6]).

request(URL, Method, Hdrs, Body, Timeout, Config) ->
    Options = [{recv_timeout, Timeout},
               {connect_timeout, Timeout},
               {pool, Config#aws_config.hackney_pool}],

    case hackney:request(Method, URL, Hdrs, Body, Options) of
        {ok, Status, RespHeaders, Ref} ->
            {ok, RespBody} = hackney:body(Ref),
            {ok, {{Status, <<>>}, RespHeaders, RespBody}};
        {ok, Status, RespHeaders} ->
            {ok, {{Status, <<>>}, RespHeaders, <<>>}};
        Error ->
            Error
    end.
