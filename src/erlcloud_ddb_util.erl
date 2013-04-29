%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-

%% @author Ransom Richardson <ransom@ransomr.net>
%% @doc
%% Helpers for using DynamoDB from Erlang.
%%
%% This is a layer2 API (in the style of boto) that augments the
%% operations in erlcloud_ddb. The functions in this file do not map
%% directly to DynamoDB operations. Instead they will perform multiple
%% operations in order to implement functionality that isn't available
%% directly using the DynamoDB API.
%%
%% @end

-module(erlcloud_ddb_util).

-include("erlcloud.hrl").
-include("erlcloud_ddb.hrl").
-include("erlcloud_aws.hrl").

%%% DynamoDB Layer2 API
-export([delete_hash_key/3, delete_hash_key/4, delete_hash_key/5
        ]).

-define(BATCH_WRITE_LIMIT, 25).

-type table_name() :: erlcloud_ddb:table_name().
-type attr_name() :: erlcloud_ddb:attr_name().
-type ddb_opts() :: erlcloud_ddb:ddb_opts().
-type hash_key() :: erlcloud_ddb:hash_key().

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
%% provided for future enhancements. This method is untransacted.
%%
%% ===Example===
%%
%% '
%% ok = erlcloud_ddb2:delete_hash_key(<<"table_name">>, <<"hash_key_value">>, <<"range_key_name">>, [])
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec delete_hash_key(table_name(), hash_key(), attr_name(), ddb_opts(), aws_config()) -> ok | {error, term()}.
delete_hash_key(Table, HashKey, RangeKeyName, Opts, Config) ->
    case erlcloud_ddb:q(Table, HashKey,
                        [{consistent_read, true},
                         {limit, ?BATCH_WRITE_LIMIT},
                         {attributes_to_get, [RangeKeyName]},
                         {out, record}], 
                        Config) of
        {error, Reason} ->
            {error, Reason};
        {ok, #ddb_q{count = 0}} ->
            ok;
        {ok, QResult} ->
            case erlcloud_ddb:batch_write_item(
                   [{Table, [{delete, {HashKey, RangeKey}} || [{_, RangeKey}] <- QResult#ddb_q.items]}],
                   [{out, record}], Config) of
                {error, Reason} ->
                    {error, Reason};
                {ok, BatchResult} ->
                    if QResult#ddb_q.last_evaluated_key == undefined andalso
                       BatchResult#ddb_batch_write_item.unprocessed_items == undefined ->
                            %% No more work to do
                            ok;
                       true ->
                            %% Some stuff was unprocessed - keep going
                            delete_hash_key(Table, HashKey, RangeKeyName, Opts, Config)
                    end
            end
    end.
