%% @author Ransom Richardson <ransom@ransomr.net>
%% @doc
%%
%% HTTP client abstraction for erlcloud. Simplifies changing http clients.
%% API matches lhttpc, except Config is passed instead of options for
%% future cusomizability.
%%
%% @end

-module(erlcloud_httpc).

-include("include/erlcloud_aws.hrl").

-export([request/6]).


-define(LHTTPC_POOLID_PADDING, 2).

request(URL, Method, Hdrs, Body, Timeout, #aws_config{ httpc_config=(#lhttpc_config{}=LHttpcConfig) }) ->
    #lhttpc_config{
            pools=Pools, pool_prefix=PPrefix, pool_connection_timeout=PConnTimeout, pool_max_size=PMaxSize
        } = LHttpcConfig,

    PIndex = erlang:phash2({URL, Method, Hdrs, Body, Timeout}, Pools),
    PSuffix = case PIndex >= (?LHTTPC_POOLID_PADDING * 10) of
        true  -> integer_to_list(PIndex);
        false ->
            FormatStr = lists:flatten(["~", integer_to_list(?LHTTPC_POOLID_PADDING), "..0B"]),
            lists:flatten( io_lib:format(FormatStr, [PIndex]) )
    end,
    PoolId = list_to_atom(PPrefix ++ PSuffix),

    LHttpcConfigParams = [{pool, PoolId},
                          {pool_ensure, true},
                          {pool_connection_timeout, PConnTimeout},
                          {pool_max_size, PMaxSize}],
    lhttpc:request(URL, Method, Hdrs, Body, Timeout, LHttpcConfigParams).
