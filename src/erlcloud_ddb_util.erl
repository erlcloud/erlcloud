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
-export([delete_all/2, delete_all/3, delete_all/4,
         delete_hash_key/3, delete_hash_key/4, delete_hash_key/5,
         get_all/2, get_all/3, get_all/4,
         put_all/2, put_all/3, put_all/4,
         list_tables_all/0, list_tables_all/1,
         q_all/2, q_all/3, q_all/4,
         scan_all/1, scan_all/2, scan_all/3,
         wait_for_table_active/1, wait_for_table_active/2, wait_for_table_active/3, wait_for_table_active/4,
         write_all/2, write_all/3, write_all/4
        ]).

-ifdef(TEST).
-export([set_out_opt/1]).
-endif.

-define(BATCH_WRITE_LIMIT, 25).
-define(BATCH_GET_LIMIT, 100).

-type typed_out() :: {typed_out, boolean()}.
-type batch_read_ddb_opt() :: typed_out() | erlcloud_ddb2:out_opt().
-type batch_read_ddb_opts() :: [batch_read_ddb_opt()].

-type conditions() :: erlcloud_ddb2:conditions().
-type ddb_opts() :: erlcloud_ddb2:ddb_opts().
-type expression() :: erlcloud_ddb2:expression().
-type hash_key() :: erlcloud_ddb2:in_attr().
-type in_item() :: erlcloud_ddb2:in_item().
-type key() :: erlcloud_ddb2:key().
-type out_item() :: erlcloud_ddb2:out_item().
-type range_key_name() :: erlcloud_ddb2:range_key_name().
-type table_name() :: erlcloud_ddb2:table_name().

-type items_return() :: {ok, [out_item()]}
                      | {ok, non_neg_integer()}
                      | {error, term()}.

-export_type(
   [batch_read_ddb_opt/0,
    batch_read_ddb_opts/0,
    typed_out/0]).

default_config() -> erlcloud_aws:default_config().

%%%------------------------------------------------------------------------------
%%% delete_all
%%%------------------------------------------------------------------------------

-spec delete_all(table_name(), [key()]) -> ok | {error, term()}.
delete_all(Table, Keys) ->
    delete_all(Table, Keys, [], default_config()).

-spec delete_all(table_name(), [key()], ddb_opts()) -> ok | {error, term()}.
delete_all(Table, Keys, Opts) ->
    delete_all(Table, Keys, Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc
%%
%% Perform one or more BatchWriteItem operations to delete all items.
%% Operations are performed in parallel. Writing to only one table is supported.
%%
%% ===Example===
%%
%% `
%% ok =
%%     erlcloud_ddb_util:delete_all(
%%       [{<<"Forum">>,
%%         [{<<"Name">>, {s, <<"Amazon DynamoDB">>}},
%%          {<<"Name">>, {s, <<"Amazon RDS">>}},
%%          {<<"Name">>, {s, <<"Amazon Redshift">>}},
%%          {<<"Name">>, {s, <<"Amazon ElastiCache">>}}
%%         ]}]),
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec delete_all(table_name(), [key()], ddb_opts(), aws_config()) -> ok | {error, term()}.
delete_all(Table, Keys, Opts, Config) ->
    write_all(Table, [{delete, Key} || Key <- Keys], Opts, Config).

%%%------------------------------------------------------------------------------
%%% delete_hash_key
%%%------------------------------------------------------------------------------

-spec delete_hash_key(table_name(), hash_key(), range_key_name()) -> ok | {error, term()}.
delete_hash_key(Table, HashKey, RangeKeyName) ->
    delete_hash_key(Table, HashKey, RangeKeyName, [], default_config()).

-spec delete_hash_key(table_name(), hash_key(), range_key_name(), ddb_opts()) -> ok | {error, term()}.
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

-spec delete_hash_key(table_name(), hash_key(), range_key_name(), ddb_opts(), aws_config()) -> ok | {error, term()}.
delete_hash_key(Table, HashKey, RangeKeyName, Opts, Config) ->
    case erlcloud_ddb2:q(Table, HashKey,
                         [{consistent_read, true},
                          {limit, ?BATCH_WRITE_LIMIT},
                          {attributes_to_get, [RangeKeyName]},
                          {out, typed_record}],
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

-type get_all_opts() :: erlcloud_ddb2:batch_get_item_request_item_opts().

-spec get_all(table_name(), [key()]) -> items_return().
get_all(Table, Keys) ->
    get_all(Table, Keys, [], default_config()).

-spec get_all(table_name(), [key()], get_all_opts()) -> items_return().
get_all(Table, Keys, Opts) ->
    get_all(Table, Keys, Opts, default_config()).

-spec get_all(table_name(), [key()], get_all_opts(), aws_config() | batch_read_ddb_opts()) -> items_return().
get_all(Table, Keys, Opts, Config) when is_record(Config, aws_config) ->
    get_all(Table, Keys, Opts, [], Config);
get_all(Table, Keys, Opts, DdbOpts) ->
    get_all(Table, Keys, Opts, DdbOpts, default_config()).

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
%%       [{projection_expression, <<"Name, Threads, Messages, Views">>}],
%%       [{typed_out, false}]),
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec get_all(table_name(), [key()], get_all_opts(), batch_read_ddb_opts(), aws_config()) -> items_return().
get_all(Table, Keys, Opts, DdbOpts, Config) when length(Keys) =< ?BATCH_GET_LIMIT ->
    batch_get_retry([{Table, Keys, Opts}], DdbOpts, Config, []);
get_all(Table, Keys, Opts, DdbOpts, Config) ->
    BatchList = chop(?BATCH_GET_LIMIT, Keys),
    Results = pmap_unordered(
                fun(Batch) ->
                        %% try/catch to prevent hang forever if there is an exception
                        try
                            batch_get_retry([{Table, Batch, Opts}], DdbOpts, Config, [])
                        catch
                            Type:Ex ->
                                {error, {Type, Ex}}
                        end
                end,
                BatchList),
    lists:foldl(fun parfold/2, {ok, []}, Results).

-spec batch_get_retry([erlcloud_ddb2:batch_get_item_request_item()], ddb_opts(), aws_config(), [out_item()]) -> items_return().
batch_get_retry(RequestItems, DdbOpts, Config, Acc) ->
    case erlcloud_ddb2:batch_get_item(RequestItems, set_out_opt(DdbOpts), Config) of
        {error, Reason} ->
            {error, Reason};
        {ok, #ddb2_batch_get_item{unprocessed_keys = [],
                                  responses = [#ddb2_batch_get_item_response{items = Items}]}} ->
            {ok, Items ++ Acc};
        {ok, #ddb2_batch_get_item{unprocessed_keys = Unprocessed,
                                  responses = [#ddb2_batch_get_item_response{items = Items}]}} ->
            batch_get_retry(Unprocessed, DdbOpts, Config, Items ++ Acc)
    end.

%%%------------------------------------------------------------------------------
%%% list_tables_all
%%%------------------------------------------------------------------------------

list_tables_all() ->
    list_tables_all(default_config()).

-spec list_tables_all(aws_config()) -> {ok, [table_name()]} | {error, any()}.
list_tables_all(Config) ->
    do_list_tables_all(undefined, Config, []).

do_list_tables_all(LastTable, Config, Result) ->
    Options = [{exclusive_start_table_name, LastTable}, {out, record}],
    case erlcloud_ddb2:list_tables(Options, Config) of
        {ok, #ddb2_list_tables{table_names = TableNames, last_evaluated_table_name = undefined}} ->
            {ok, flatreverse([TableNames, Result])};
        {ok, #ddb2_list_tables{table_names = TableNames, last_evaluated_table_name = LastTableName}} ->
            do_list_tables_all(LastTableName, Config, flatreverse([TableNames, Result]));
        {error, _} = Error ->
            Error
    end.

%%%------------------------------------------------------------------------------
%%% put_all
%%%------------------------------------------------------------------------------

-spec put_all(table_name(), [in_item()]) -> ok | {error, term()}.
put_all(Table, Items) ->
    put_all(Table, Items, [], default_config()).

-spec put_all(table_name(), [in_item()], ddb_opts()) -> ok | {error, term()}.
put_all(Table, Items, Opts) ->
    put_all(Table, Items, Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc
%%
%% Perform one or more BatchWriteItem operations to put all items.
%% Operations are performed in parallel. Writing to only one table is supported.
%%
%% ===Example===
%%
%% `
%% ok =
%%     erlcloud_ddb_util:put_all(
%%       [{<<"Forum">>,
%%         [[{<<"Name">>, {s, <<"Amazon DynamoDB">>}},
%%           {<<"Category">>, {s, <<"Amazon Web Services">>}}],
%%          [{<<"Name">>, {s, <<"Amazon RDS">>}},
%%           {<<"Category">>, {s, <<"Amazon Web Services">>}}],
%%          [{<<"Name">>, {s, <<"Amazon Redshift">>}},
%%           {<<"Category">>, {s, <<"Amazon Web Services">>}}],
%%          [{<<"Name">>, {s, <<"Amazon ElastiCache">>}},
%%           {<<"Category">>, {s, <<"Amazon Web Services">>}}]
%%         ]}]),
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec put_all(table_name(), [in_item()], ddb_opts(), aws_config()) -> ok | {error, term()}.
put_all(Table, Items, Opts, Config) ->
    write_all(Table, [{put, Item} || Item <- Items], Opts, Config).

%%%------------------------------------------------------------------------------
%%% q_all
%%%------------------------------------------------------------------------------

-type q_all_opts() :: [erlcloud_ddb2:q_opt() | batch_read_ddb_opt()].

-spec q_all(table_name(), conditions() | expression()) -> items_return().
q_all(Table, KeyConditionsOrExpression) ->
    q_all(Table, KeyConditionsOrExpression, [], default_config()).

-spec q_all(table_name(), conditions() | expression(), q_all_opts()) -> items_return().
q_all(Table, KeyConditionsOrExpression, Opts) ->
    q_all(Table, KeyConditionsOrExpression, Opts, default_config()).

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
%%       <<"ForumName = :n AND LastPostDateTime BETWEEN :t1 AND :t2">>,
%%       [{expression_attribute_values,
%%         [{<<":n">>, <<"Amazon DynamoDB">>},
%%          {<<":t1">>, <<"20130101">>},
%%          {<<":t2">>, <<"20130115">>}]},
%%        {index_name, <<"LastPostIndex">>},
%%        {select, all_attributes},
%%        {consistent_read, true},
%%        {typed_out, true}]),
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec q_all(table_name(),
            conditions() | expression(),
            q_all_opts(),
            aws_config()) -> items_return().
q_all(Table, KeyConditionsOrExpression, Opts, Config) ->
    q_all(Table, KeyConditionsOrExpression, Opts, Config, [], undefined).

-spec q_all(table_name(),
            conditions() | expression(),
            q_all_opts(),
            aws_config(),
            [[out_item()]],
            key() | undefined) -> items_return().
q_all(Table, KeyCondOrExpr, Opts0, Config, Acc, StartKey) ->
    Opts = [{exclusive_start_key, StartKey}|set_out_opt(Opts0)],
    case erlcloud_ddb2:q(Table, KeyCondOrExpr, Opts, Config) of
        {error, Reason} ->
            {error, Reason};
        {ok, #ddb2_q{last_evaluated_key = undefined,
                     items              = undefined,
                     count              = Count}} ->
            {ok, lists:sum([Count|Acc])};
        {ok, #ddb2_q{last_evaluated_key = undefined,
                     items              = Items}} ->
            {ok, flatreverse([Items|Acc])};
        {ok, #ddb2_q{last_evaluated_key = LastKey,
                     items              = undefined,
                     count              = Count}} ->
            q_all(Table, KeyCondOrExpr, Opts0, Config, [Count|Acc], LastKey);
        {ok, #ddb2_q{last_evaluated_key = LastKey,
                     items              = Items}} ->
            q_all(Table, KeyCondOrExpr, Opts0, Config, [Items|Acc], LastKey)
    end.

%%%------------------------------------------------------------------------------
%%% scan_all
%%%------------------------------------------------------------------------------

-type scan_all_opts() :: [erlcloud_ddb2:scan_opt() | batch_read_ddb_opt()].

-spec scan_all(table_name()) -> items_return().
scan_all(Table) ->
    scan_all(Table, [], default_config()).

-spec scan_all(table_name(), scan_all_opts()) -> items_return().
scan_all(Table, Opts) ->
    scan_all(Table, Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc
%%
%% Perform one or more Scan operations to get all matching items.
%%
%% ===Example===
%%
%% `
%% {ok, Items} =
%%     erlcloud_ddb_util:scan_all(
%%       <<"Thread">>,
%%       [{segment, 0},
%%        {total_segments, 4},
%%        {typed_out, true}]),
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec scan_all(table_name(), scan_all_opts(), aws_config()) -> items_return().
scan_all(Table, Opts, Config) ->
    scan_all(Table, Opts, Config, [], undefined).

-spec scan_all(table_name(),
               scan_all_opts(),
               aws_config(),
               [[out_item()]],
               key() | undefined) -> items_return().
scan_all(Table, Opts0, Config, Acc, StartKey) ->
    Opts = [{exclusive_start_key, StartKey}|set_out_opt(Opts0)],
    case erlcloud_ddb2:scan(Table, Opts, Config) of
        {error, Reason} ->
            {error, Reason};
        {ok, #ddb2_scan{last_evaluated_key = undefined,
                        items              = undefined,
                        count              = Count}} ->
            {ok, lists:sum([Count|Acc])};
        {ok, #ddb2_scan{last_evaluated_key = undefined,
                        items              = Items}} ->
            {ok, flatreverse([Items|Acc])};
        {ok, #ddb2_scan{last_evaluated_key = LastKey,
                        items              = undefined,
                        count              = Count}} ->
            scan_all(Table, Opts0, Config, [Count|Acc], LastKey);
        {ok, #ddb2_scan{last_evaluated_key = LastKey,
                        items              = Items}} ->
            scan_all(Table, Opts0, Config, [Items|Acc], LastKey)
    end.

%%%------------------------------------------------------------------------------
%%% write_all
%%%------------------------------------------------------------------------------

-type write_all_item() :: erlcloud_ddb2:batch_write_item_request().

-spec write_all(table_name(), [write_all_item()]) -> ok | {error, term()}.
write_all(Table, Items) ->
    write_all(Table, Items, [], default_config()).

-spec write_all(table_name(), [write_all_item()], ddb_opts()) -> ok | {error, term()}.
write_all(Table, Items, Opts) ->
    write_all(Table, Items, Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc
%%
%% Perform one or more BatchWriteItem operations to put or delete all items.
%% Operations are performed in parallel. Writing to only one table is supported.
%%
%% ===Example===
%%
%% `
%% ok =
%%     erlcloud_ddb_util:write_all(
%%       [{<<"Forum">>,
%%         [{put, [{<<"Name">>, {s, <<"Amazon DynamoDB">>}},
%%                 {<<"Category">>, {s, <<"Amazon Web Services">>}}]},
%%          {put, [{<<"Name">>, {s, <<"Amazon RDS">>}},
%%                 {<<"Category">>, {s, <<"Amazon Web Services">>}}]},
%%          {put, [{<<"Name">>, {s, <<"Amazon Redshift">>}},
%%                 {<<"Category">>, {s, <<"Amazon Web Services">>}}]},
%%          {put, [{<<"Name">>, {s, <<"Amazon ElastiCache">>}},
%%                 {<<"Category">>, {s, <<"Amazon Web Services">>}}]}
%%         ]}]),
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec write_all(table_name(), [write_all_item()], ddb_opts(), aws_config()) -> ok | {error, term()}.
write_all(Table, Items, _Opts, Config) when length(Items) =< ?BATCH_WRITE_LIMIT ->
    batch_write_retry([{Table, Items}], Config);
write_all(Table, Items, _Opts, Config) ->
    BatchList = chop(?BATCH_WRITE_LIMIT, Items),
    Results = pmap_unordered(
                fun(Batch) ->
                        %% try/catch to prevent hang forever if there is an exception
                        try
                            batch_write_retry([{Table, Batch}], Config)
                        catch
                            Type:Ex ->
                                {error, {Type, Ex}}
                        end
                end,
                BatchList),
    write_all_result(Results).

-spec batch_write_retry([erlcloud_ddb2:batch_write_item_request_item()], aws_config()) -> ok | {error, term()}.
batch_write_retry(RequestItems, Config) ->
    case erlcloud_ddb2:batch_write_item(RequestItems, [{out, record}], Config) of
        {error, Reason} ->
            {error, Reason};
        {ok, #ddb2_batch_write_item{unprocessed_items = []}} ->
            ok;
        {ok, #ddb2_batch_write_item{unprocessed_items = Unprocessed}} ->
            batch_write_retry(Unprocessed, Config)
    end.

%%------------------------------------------------------------------------------
%% @doc
%%  wait until table_status==active.
%%
%% ===Example===
%%
%% `
%% erlcloud_ddb2:wait_for_table_active(<<"TableName">>, 3000, 40, Config)
%% '
%% @end
%%------------------------------------------------------------------------------

-spec wait_for_table_active(table_name(), pos_integer() | infinity, non_neg_integer(), aws_config()) ->
    ok | {error, deleting | retry_threshold_exceeded | any()}.
wait_for_table_active(Table, Interval, RetryTimes, Config) when is_binary(Table), Interval > 0, RetryTimes >= 0 ->
    case erlcloud_ddb2:describe_table(Table, [{out, record}], Config) of
        {ok, #ddb2_describe_table{table = #ddb2_table_description{table_status = active}}} ->
            ok;
        {ok, #ddb2_describe_table{table = #ddb2_table_description{table_status = deleting}}} ->
            {error, deleting};
        {ok, _} ->
            case RetryTimes of
                infinity ->
                    timer:sleep(Interval),
                    wait_for_table_active(Table, infinity, RetryTimes, Config);
                1 ->
                    {error, retry_threshold_exceeded};
                _ ->
                    timer:sleep(Interval),
                    wait_for_table_active(Table, Interval, RetryTimes - 1, Config)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

wait_for_table_active(Table, Interval, RetryTimes) ->
    wait_for_table_active(Table, Interval, RetryTimes, default_config()).

wait_for_table_active(Table, AWSCfg) ->
    wait_for_table_active(Table, 3000, 100, AWSCfg).

wait_for_table_active(Table) ->
    wait_for_table_active(Table, default_config()).


write_all_result([ok | T]) ->
    write_all_result(T);
write_all_result([{error, Reason} | _]) ->
    {error, Reason};
write_all_result([]) ->
    ok.

%%%------------------------------------------------------------------------------
%%% Internal Functions
%%%------------------------------------------------------------------------------

%% Set `out' option to record/typed_record output formats based on `typed_out'
%%  boolean setting for get_all, scan_all, q_all. Other output formats are not
%%  supported for multi_call reads. Validation is bypassed for backwards
%%  compatibility.
-spec set_out_opt(batch_read_ddb_opts()) -> ddb_opts().
set_out_opt(Opts) ->
    {OutOpt, NewOpts} = case lists:keytake(typed_out, 1, Opts) of
        {value, {typed_out, true}, Opts1} -> {{out, typed_record}, Opts1};
        {value, {typed_out, _}, Opts2} -> {{out, record}, Opts2};
        false -> {{out, record}, Opts}
    end,
    lists:keystore(out, 1, NewOpts, OutOpt).


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
