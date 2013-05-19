%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-

%% @author Ransom Richardson <ransom@ransomr.net>
%% @doc
%% Helpers for using DynamoDB from Erlang.
%%
%% This is a higher layer API that augments the operations supported
%% by erlcloud_ddb2. The functions in this file do not map directly to
%% DynamoDB operations. Instead they will perform multiple operations
%% in order to implement functionality that isn't available directly
%% using the DynamoDB API.
%%
%% @end

-module(erlcloud_ddb_util).

-include("erlcloud.hrl").
-include("erlcloud_ddb2.hrl").
-include("erlcloud_aws.hrl").

%%% DynamoDB Higher Layer API
-export([delete_hash_key/3, delete_hash_key/4, delete_hash_key/5,
         get_all/2, get_all/3, get_all/4,
         q_all/2, q_all/3, q_all/4
        ]).

-define(BATCH_WRITE_LIMIT, 25).
-define(BATCH_GET_LIMIT, 100).

-type attr_name() :: erlcloud_ddb2:attr_name().
-type batch_get_item_request_item() :: erlcloud_ddb2:batch_get_item_request_item().
-type conditions() :: erlcloud_ddb2:conditions().
-type ddb_opts() :: erlcloud_ddb2:ddb_opts().
-type get_item_opts() :: erlcloud_ddb2:get_item_opts().
-type key() :: erlcloud_ddb2:key().
-type hash_key() :: erlcloud_ddb2:in_attr().
-type out_item() :: erlcloud_ddb2:out_item().
-type q_opts() :: erlcloud_ddb2:q_opts().
-type table_name() :: erlcloud_ddb2:table_name().

-type items_return() :: {ok, [out_item()]} | {error, term()}.

default_config() -> erlcloud_aws:default_config().

%%%------------------------------------------------------------------------------
%%% delete_hash_key
%%%------------------------------------------------------------------------------

-spec delete_hash_key(table_name(), hash_key(), attr_name()) -> ok | {error, term()}.
delete_hash_key(Table, HashKey, RangeKeyName) ->
    delete_hash_key(Table, HashKey, RangeKeyName, [], default_config()).

-spec delete_hash_key(table_name(), hash_key(), attr_name(), ddb_opts()) -> ok | {error, term()}.
delete_hash_key(Table, HashKey, RangeKeyName, Opts) ->
    delete_hash_key(Table, HashKey, RangeKeyName, Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc 
%%
%% Delete all items with the specified table. Table must be a
%% hash-and-range primary key table. Opts is currently ignored and is
%% provided for future enhancements. This method is not transacted.
%%
%% ===Example===
%%
%% `
%% ok = erlcloud_ddb_util:delete_hash_key(<<"tn">>, {<<"hash-key-name">>, <<"hash-key-value">>}, <<"range-key-name">>, [])),
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec delete_hash_key(table_name(), hash_key(), attr_name(), ddb_opts(), aws_config()) -> ok | {error, term()}.
delete_hash_key(Table, HashKey, RangeKeyName, Opts, Config) ->
    case erlcloud_ddb2:q(Table, HashKey,
                         [{consistent_read, true},
                          {limit, ?BATCH_WRITE_LIMIT},
                          {attributes_to_get, [RangeKeyName]},
                          {out, record}], 
                         Config) of
        {error, Reason} ->
            {error, Reason};
        {ok, #ddb2_q{count = 0}} ->
            ok;
        {ok, QResult} ->
            case erlcloud_ddb2:batch_write_item(
                   [{Table, [{delete, [HashKey, RangeKey]} || [RangeKey] <- QResult#ddb2_q.items]}],
                   [{out, record}], Config) of
                {error, Reason} ->
                    {error, Reason};
                {ok, BatchResult} ->
                    if QResult#ddb2_q.last_evaluated_key == undefined andalso
                       BatchResult#ddb2_batch_write_item.unprocessed_items == [] ->
                            %% No more work to do
                            ok;
                       true ->
                            %% Some stuff was unprocessed - keep going
                            delete_hash_key(Table, HashKey, RangeKeyName, Opts, Config)
                    end
            end
    end.

%%%------------------------------------------------------------------------------
%%% get_all
%%%------------------------------------------------------------------------------

-spec get_all(table_name(), [key()]) -> items_return().
get_all(Table, Keys) ->
    get_all(Table, Keys, [], default_config()).

-spec get_all(table_name(), [key()], get_item_opts()) -> items_return().
get_all(Table, Keys, Opts) ->
    get_all(Table, Keys, Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc 
%%
%% Perform one or more BatchGetItem operations to get all matching
%% items. Operations are performed in parallel. Order may not be preserved.
%% Getting from only one table is supported.
%%
%% ===Example===
%%
%% `
%% {ok, Items} =
%%     erlcloud_ddb_util:get_all(
%%       <<"Forum">>, 
%%       [{<<"Name">>, {s, <<"Amazon DynamoDB">>}},
%%        {<<"Name">>, {s, <<"Amazon RDS">>}}, 
%%        {<<"Name">>, {s, <<"Amazon Redshift">>}}],
%%       [{attributes_to_get, [<<"Name">>, <<"Threads">>, <<"Messages">>, <<"Views">>]}]),
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec get_all(table_name(), [key()], get_item_opts(), aws_config()) -> items_return().
get_all(Table, Keys, Opts, Config) when length(Keys) =< ?BATCH_GET_LIMIT ->
    batch_get_retry([{Table, Keys, Opts}], Config, []);
get_all(Table, Keys, Opts, Config) ->
    BatchList = chop(?BATCH_GET_LIMIT, Keys),
    Results = pmap_unordered(
                fun(Batch) ->
                        %% try/catch to prevent hang forever if there is an exception
                        try
                            batch_get_retry([{Table, Batch, Opts}], Config, [])
                        catch
                            Type:Ex ->
                                {error, {Type, Ex}}
                        end
                end,
                BatchList),
    lists:foldl(fun parfold/2, {ok, []}, Results).

-spec batch_get_retry([batch_get_item_request_item()], aws_config(), [out_item()]) -> items_return().
batch_get_retry(RequestItems, Config, Acc) ->
    case erlcloud_ddb2:batch_get_item(RequestItems, [{out, record}], Config) of
        {error, Reason} ->
            {error, Reason};
        {ok, #ddb2_batch_get_item{unprocessed_keys = [], 
                                  responses = [#ddb2_batch_get_item_response{items = Items}]}} ->
            {ok, Items ++ Acc};
        {ok, #ddb2_batch_get_item{unprocessed_keys = Unprocessed, 
                                  responses = [#ddb2_batch_get_item_response{items = Items}]}} ->
            batch_get_retry(Unprocessed, Config, Items ++ Acc)
    end.

%%%------------------------------------------------------------------------------
%%% q_all
%%%------------------------------------------------------------------------------

-spec q_all(table_name(), conditions()) -> items_return().
q_all(Table, Conditions) ->
    q_all(Table, Conditions, [], default_config()).

-spec q_all(table_name(), conditions(), q_opts()) -> items_return().
q_all(Table, Conditions, Opts) ->
    q_all(Table, Conditions, Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc 
%%
%% Perform one or more Query operations to get all matching items.
%%
%% ===Example===
%%
%% `
%% {ok, Items} =
%%     erlcloud_ddb_util:q_all(
%%       <<"Thread">>,
%%       [{<<"LastPostDateTime">>, {{s, <<"20130101">>}, {s, <<"20130115">>}}, between},
%%        {<<"ForumName">>, {s, <<"Amazon DynamoDB">>}}],
%%       [{index_name, <<"LastPostIndex">>},
%%        {select, all_attributes},
%%        {consistent_read, true}]),
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec q_all(table_name(), conditions(), q_opts(), aws_config()) -> items_return().
q_all(Table, Conditions, Opts, Config) ->
    q_all(Table, Conditions, Opts, Config, [], undefined).

-spec q_all(table_name(), conditions(), q_opts(), aws_config(), [out_item()], key() | undefined) 
           -> items_return().
q_all(Table, Conditions, Opts, Config, Acc, StartKey) ->
    case erlcloud_ddb2:q(Table, Conditions, 
                         [{exclusive_start_key, StartKey}, {out, record} | Opts], 
                         Config) of
        {error, Reason} ->
            {error, Reason};
        {ok, #ddb2_q{last_evaluated_key = undefined, items = Items}} ->
            {ok, flatreverse([Items | Acc])};
        {ok, #ddb2_q{last_evaluated_key = LastKey, items = Items}} ->
            q_all(Table, Conditions, Opts, Config, [Items | Acc], LastKey)
    end.

%%%------------------------------------------------------------------------------
%%% Internal Functions
%%%------------------------------------------------------------------------------

%% Reverses a list of lists and flattens one level
flatreverse(List) ->
    lists:foldl(fun(I, A) -> I ++ A end, [], List).

%% fold a set of results from parallel operations producing lists
parfold(_, {error, Reason}) ->
    {error, Reason};
parfold({error, Reason}, _) ->
    {error, Reason};
parfold({ok, I}, {ok, A}) ->
    {ok, I ++ A}.

%% parallel map implementation. See Armstrong's Programming Erlang and
%% http://bc.tech.coop/blog/070601.html
pmap_unordered(F, L) ->
    Parent = self(),
    Ref = make_ref(),
    Pids = [spawn(fun() -> Parent ! {Ref, F(X)} end) || X <- L],
    [receive {Ref, Result} -> Result end || _ <- Pids].

%% creates a list of list each of which has N or fewer elements
chop(N, List) ->
    chop(N, List, []).

chop(_, [], Acc) ->
    lists:reverse(Acc);
chop(N, List, Acc) ->
    {H, T} = safe_split(N, List),
    chop(N, T, [H | Acc]).

%% lists:split throws if N is larger than the list, safe_split doesn't
safe_split(N, List) ->
    safe_split(N, List, []).

safe_split(0, List, Acc) ->
    {lists:reverse(Acc), List};
safe_split(_, [], Acc) ->
    {lists:reverse(Acc), []};
safe_split(N, [H|T], Acc) ->
    safe_split(N - 1, T, [H | Acc]).

