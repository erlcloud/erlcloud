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
         q_all/2, q_all/3, q_all/4
        ]).

-define(BATCH_WRITE_LIMIT, 25).

-type attr_name() :: erlcloud_ddb2:attr_name().
-type conditions() :: erlcloud_ddb2:conditions().
-type ddb_opts() :: erlcloud_ddb2:ddb_opts().
-type key() :: erlcloud_ddb2:key().
-type hash_key() :: erlcloud_ddb2:in_attr().
-type out_item() :: erlcloud_ddb2:out_item().
-type q_opts() :: erlcloud_ddb2:q_opts().
-type table_name() :: erlcloud_ddb2:table_name().

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
                       BatchResult#ddb2_batch_write_item.unprocessed_items == undefined ->
                            %% No more work to do
                            ok;
                       true ->
                            %% Some stuff was unprocessed - keep going
                            delete_hash_key(Table, HashKey, RangeKeyName, Opts, Config)
                    end
            end
    end.

%%%------------------------------------------------------------------------------
%%% q_all
%%%------------------------------------------------------------------------------

-type q_all_return() :: {ok, [out_item()]} | {error, term()}.

-spec q_all(table_name(), conditions()) -> q_all_return().
q_all(Table, Conditions) ->
    q_all(Table, Conditions, [], default_config()).

-spec q_all(table_name(), conditions(), q_opts()) -> q_all_return().
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

-spec q_all(table_name(), conditions(), q_opts(), aws_config()) -> q_all_return().
q_all(Table, Conditions, Opts, Config) ->
    q_all(Table, Conditions, Opts, Config, [], undefined).

-spec q_all(table_name(), conditions(), q_opts(), aws_config(), [out_item()], key() | undefined) 
           -> q_all_return().
q_all(Table, Conditions, Opts, Config, Acc, StartKey) ->
    case erlcloud_ddb2:q(Table, Conditions, 
                         [{exclusive_start_key, StartKey}, {out, record} | Opts], 
                         Config) of
        {error, Reason} ->
            {error, Reason};
        {ok, #ddb2_q{last_evaluated_key = undefined, items = Items}} ->
            %% Reverse and flatten the result. Can't use lists:flatten
            %% because we only want to flatten one level.
            Result = lists:foldl(
                       fun(Batch, A) -> Batch ++ A end,
                       [],
                       [Items | Acc]),
            {ok, Result};
        {ok, #ddb2_q{last_evaluated_key = LastKey, items = Items}} ->
            q_all(Table, Conditions, Opts, Config, [Items | Acc], LastKey)
    end.
