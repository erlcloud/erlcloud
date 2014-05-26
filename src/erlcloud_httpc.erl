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

request(URL, Method, Hdrs, Body, Timeout, _Config) ->
    lhttpc:request(URL, Method, Hdrs, Body, Timeout, []).
