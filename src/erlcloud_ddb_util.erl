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

%%% DynamoDB Layer2 API
-export([delete_hash_key/3, delete_hash_key/4, delete_hash_key/5
        ]).

-define(BATCH_WRITE_LIMIT, 25).

-type table_name() :: erlcloud_ddb2:table_name().
-type attr_name() :: erlcloud_ddb2:attr_name().
-type ddb_opts() :: erlcloud_ddb2:ddb_opts().
-type hash_key() :: erlcloud_ddb2:in_attr().

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
