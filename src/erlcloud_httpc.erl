%% @author Ransom Richardson <ransom@ransomr.net>
%% @doc
%%
%% HTTP client abstraction for erlcloud. Simplifies changing http clients.
%% API matches lhttpc, except Config is passed instead of options for
%% future cusomizability.
%%
%% @end

-module(erlcloud_httpc).

-export([request/6]).

request(URL, Method, Hdrs, Body, Timeout, Config) ->
    Pool =  proplists:get_value(hackney_pool, Config, default),

    Options = [{recv_timeout, Timeout},
               {connect_timeout, Timeout},
               {pool, Pool}],

    case hackney:request(Method, URL, Hdrs, Body, Options) of
        {ok, Status, RespHeaders, Ref} ->
            {ok, RespBody} = hackney:body(Ref),
            {ok, {{Status, <<>>}, RespHeaders, RespBody}};
        {ok, Status, RespHeaders} ->
            {ok, {{Status, <<>>}, RespHeaders, <<>>}};
        Error ->
            Error
    end.
