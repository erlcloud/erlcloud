%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-

%% @author Ransom Richardson <ransom@ransomr.net>
%% @doc
%% An Erlang interface to Amazon's DynamoDB.
%%
%% [http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/operationlist.html]
%%
%% Currently the entire 20111205 API is implemented.
%%
%% Method names match DynamoDB operations converted to
%% lower_case_with_underscores. The one exception is query, which is
%% an Erlang reserved word. The `q' method implements Query.
%%
%% Required parameters are passed as function arguments. In addition
%% all methods take an options proplist argument which can be used to
%% pass optional parameters. See function documentation for examples.
%%
%% Table names, key names, attribute names and any other input strings
%% except attribute values must be binary strings.
%%
%% Attribute values may be either `{Type, Value}' or `Value'. If only
%% `Value' is provided then the type is inferred. Lists (iolists are
%% handled) and binaries are assumed to be strings. The following are
%% equivalent: `{s, <<"value">>}', `<<"value">>', `"value"'. Numbers
%% are assumed to be numbers. The following are equivalent: `{n, 42}',
%% `42'. To specify the AWS binary or set types an explicit `Type'
%% must be provided. For example: `{b, <<1,2,3>>}' or `{ns,
%% [4,5,6]}'. Note that binary values will be base64 encoded and
%% decoded automatically.
%%
%% Output is in the form of `{ok, Value}' or `{error, Reason}'. The
%% format of `Value' is controlled by the `out' option, which defaults
%% to `simple'. The possible values are: 
%%
%% * `simple' - The most interesting part of the output. For example
%% `get_item' will return the item.
%%
%% * `record' - A record containing all the information from the
%% DynamoDB response. This is useful if you need more detailed
%% information than what is returned with `simple'. For example, with
%% `scan' and `query' the record will contain the last evaluated key
%% which can be used to continue the operation.
%%
%% * `json' - The output from DynamoDB as processed by `jsx:decode'
%% but with no further manipulation. This would rarely be useful,
%% unless the DynamoDB API is updated to include data that is not yet
%% parsed correctly.
%%
%% Items will be returned as a list of `{Name, Value}'. In most cases
%% the output will have type information removed. For example:
%% `[{<<"String Attribute">>, <<"value">>}, {<<"Number Attribute">>,
%% 42}, {<<"BinaryAttribute">>, <<1,2,3>>}]'. The exception is for
%% output fields that are intended to be passed to a subsequent call,
%% such as `unprocessed_keys' and `last_evaluated_key'. Those will
%% contain typed attribute values so that they may be correctly passed
%% to subsequent calls.
%%
%% DynamoDB errors are return in the form `{error, {ErrorCode,
%% Message}}' where `ErrorCode' and 'Message' are both binary
%% strings. List of error codes:
%% [http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ErrorHandling.html]. So
%% to handle conditional check failures, match `{error,
%% {<<"ConditionalCheckFailedException">>, _}}'.
%%
%% `erlcloud_ddb2' provides a higher level API that implements common
%% operations that may require multiple DynamoDB API calls.
%%
%% `erlcloud_ddb1' provides a lower level API that takes JSON terms as
%% defined by `jsx'. It may be useful to pass options that are not yet
%% supported by this module.
%%
%% See the unit tests for additional usage examples beyond what are
%% provided for each function.
%%
%% @end

-module(erlcloud_ddb).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").
-include("erlcloud_ddb.hrl").

%%% Library initialization.
-export([configure/2, configure/3, new/2, new/3]).

%%% DynamoDB API
-export([batch_get_item/1, batch_get_item/2, batch_get_item/3,
         batch_write_item/1, batch_write_item/2, batch_write_item/3,
         create_table/5, create_table/6, create_table/7,
         delete_item/2, delete_item/3, delete_item/4,
         delete_table/1, delete_table/2, delete_table/3,
         describe_table/1, describe_table/2, describe_table/3,
         get_item/2, get_item/3, get_item/4,
         list_tables/0, list_tables/1, list_tables/2,
         put_item/2, put_item/3, put_item/4,
         %% Note that query is a Erlang reserved word, so we use q instead
         q/2, q/3, q/4,
         scan/1, scan/2, scan/3,
         update_item/3, update_item/4, update_item/5,
         update_table/3, update_table/4, update_table/5
        ]).

-export_type([table_name/0, hash_key/0, hash_range_key/0, attr_name/0, in_attr/0, out_item/0, key_schema/0,
              ddb_opts/0,
              batch_get_item_request_item/0,
              batch_write_item_request_item/0
             ]).

%%%------------------------------------------------------------------------------
%%% Library initialization.
%%%------------------------------------------------------------------------------

-spec(new/2 :: (string(), string()) -> aws_config()).
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey}.

-spec(new/3 :: (string(), string(), string()) -> aws_config()).
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                ddb_host=Host}.

-spec(configure/2 :: (string(), string()) -> ok).
configure(AccessKeyID, SecretAccessKey) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey)),
    ok.

-spec(configure/3 :: (string(), string(), string()) -> ok).
configure(AccessKeyID, SecretAccessKey, Host) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host)),
    ok.

default_config() -> erlcloud_aws:default_config().

%%%------------------------------------------------------------------------------
%%% Shared Types
%%%------------------------------------------------------------------------------

-type table_name() :: binary().
-type attr_type() :: s | n | b | ss | ns | bs.
-type attr_name() :: binary().
-type maybe_list(T) :: T | [T].

-type in_attr_data_scalar() :: iolist() | binary() | number().
-type in_attr_data_set() :: [iolist() | binary()] | [number()].
-type in_attr_data() :: in_attr_data_scalar() | in_attr_data_set().
-type in_attr_typed_value() :: {attr_type(), in_attr_data()}.
-type in_attr_value() :: in_attr_data() | in_attr_typed_value().
-type in_attr() :: {attr_name(), in_attr_value()}.
-type in_expected_item() :: {attr_name(), false | in_attr_value()}.
-type in_expected() :: maybe_list(in_expected_item()).
-type in_item() :: [in_attr()].

-type json_pair() :: {binary(), jsx:json_term()}.
-type json_attr_type() :: binary().
-type json_attr_data() :: binary() | [binary()].
-type json_attr_value() :: {json_attr_type(), json_attr_data()}.
-type json_attr() :: {attr_name(), [json_attr_value()]}.
-type json_item() :: [json_attr()].
-type json_expected() :: [{attr_name(), [json_attr_value()] | [{binary(), boolean()}]}].
-type json_key() :: [json_attr(),...].

-type hash_key() :: in_attr().
-type range_key() :: in_attr().
-type hash_range_key() :: {hash_key(), range_key()}.
-type key() :: hash_key() | hash_range_key().
-type attr_defs() :: maybe_list({attr_name(), attr_type()}).
-type key_schema() :: attr_name() | {attr_name(), attr_name()}.

-type return_value() :: none | all_old | updated_old | all_new | updated_new.

-type comparison_op() :: eq | ne | le | lt | ge | gt | not_null | null | contains | not_contains | 
                         begins_with | in | between.

-type out_attr_value() :: binary() | number() | [binary()] | [number()].
-type out_attr() :: {attr_name(), out_attr_value()}.
-type out_item() :: [out_attr()].
-type ok_return(T) :: {ok, T} | {error, term()}.

%%%------------------------------------------------------------------------------
%%% Shared Dynamizers
%%%------------------------------------------------------------------------------

%% Convert terms into the form expected by erlcloud_ddb1

-spec dynamize_type(attr_type()) -> binary().
dynamize_type(s) ->
    <<"S">>;
dynamize_type(n) ->
    <<"N">>;
dynamize_type(b) ->
    <<"B">>.

-spec dynamize_number(number()) -> binary().
dynamize_number(Value) when is_integer(Value) ->
    list_to_binary(integer_to_list(Value));
dynamize_number(Value) when is_float(Value) ->
    %% Note that float_to_list produces overly precise and long string
    [String] = io_lib:format("~p", [Value]),
    list_to_binary(String).

-spec dynamize_set(attr_type(), in_attr_data_set()) -> [binary()].
dynamize_set(ss, Values) ->
    [iolist_to_binary(Value) || Value <- Values];
dynamize_set(ns, Values) ->
    [dynamize_number(Value) || Value <- Values];
dynamize_set(bs, Values) ->
    [base64:encode(Value) || Value <- Values].

-spec dynamize_value(in_attr_value()) -> json_attr_value().
dynamize_value({s, Value}) when is_binary(Value) ->
    {<<"S">>, Value};
dynamize_value({s, Value}) when is_list(Value) ->
    {<<"S">>, list_to_binary(Value)};
dynamize_value({n, Value}) when is_number(Value) ->
    {<<"N">>, dynamize_number(Value)};
dynamize_value({b, Value}) when is_binary(Value) orelse is_list(Value) ->
    {<<"B">>, base64:encode(Value)};

dynamize_value({ss, Value}) when is_list(Value) ->
    {<<"SS">>, dynamize_set(ss, Value)};
dynamize_value({ns, Value}) when is_list(Value) ->
    {<<"NS">>, dynamize_set(ns, Value)};
dynamize_value({bs, Value}) when is_list(Value) ->
    {<<"BS">>, dynamize_set(bs, Value)};

dynamize_value(Value) when is_binary(Value) ->
    dynamize_value({s, Value});
dynamize_value(Value) when is_list(Value) ->
    dynamize_value({s, Value});
dynamize_value(Value) when is_number(Value) ->
    dynamize_value({n, Value});
dynamize_value(Value) ->
    error({erlcloud_ddb, {invalid_attr_value, Value}}).

-spec dynamize_attr(in_attr()) -> json_attr().
dynamize_attr({Name, Value}) when is_binary(Name) ->
    {Name, [dynamize_value(Value)]};
dynamize_attr({Name, _}) ->
    error({erlcloud_ddb, {invalid_attr_name, Name}});
dynamize_attr(Attr) ->
    error({erlcloud_ddb, {invalid_attr, Attr}}).

-spec dynamize_key(key()) -> jsx:json_term().
dynamize_key({HashKey, RangeKey}) when is_tuple(HashKey) ->
    [dynamize_attr(HashKey), dynamize_attr(RangeKey)];
dynamize_key(HashKey) ->
    [dynamize_attr(HashKey)].

-spec dynamize_attr_defs(attr_defs()) -> jsx:json_term().
dynamize_attr_defs({Name, Type}) ->
    [[{<<"AttributeName">>, Name},
      {<<"AttributeType">>, dynamize_type(Type)}]];
dynamize_attr_defs(AttrDefs) ->
    [[{<<"AttributeName">>, Name},
      {<<"AttributeType">>, dynamize_type(Type)}]
     || {Name, Type} <- AttrDefs].

-spec dynamize_key_schema(key_schema()) -> jsx:json_term().
dynamize_key_schema({HashKey, RangeKey}) ->
    [[{<<"AttributeName">>, HashKey}, {<<"KeyType">>, <<"HASH">>}],
     [{<<"AttributeName">>, RangeKey}, {<<"KeyType">>, <<"RANGE">>}]];
dynamize_key_schema(HashKey) ->
    [[{<<"AttributeName">>, HashKey}, {<<"KeyType">>, <<"HASH">>}]].

-spec dynamize_maybe_list(fun((A) -> B), maybe_list(A)) -> [B].
dynamize_maybe_list(DynamizeItem, List) when is_list(List) ->
    [DynamizeItem(I) || I <- List];
dynamize_maybe_list(DynamizeItem, Item) ->
    [DynamizeItem(Item)].

-spec dynamize_expected_item(in_expected()) -> json_pair().
dynamize_expected_item({Name, false}) ->
    {Name, [{<<"Exists">>, false}]};
dynamize_expected_item({Name, Value}) ->
    {Name, [{<<"Value">>, [dynamize_value(Value)]}]}.

-spec dynamize_expected(in_expected()) -> json_expected().
dynamize_expected(Expected) ->
    dynamize_maybe_list(fun dynamize_expected_item/1, Expected).

-spec dynamize_return_value(return_value()) -> binary().
dynamize_return_value(none) ->
    <<"NONE">>;
dynamize_return_value(all_old) ->
    <<"ALL_OLD">>;
dynamize_return_value(updated_old) ->
    <<"UPDATED_OLD">>;
dynamize_return_value(all_new) ->
    <<"ALL_NEW">>;
dynamize_return_value(updated_new) ->
    <<"UPDATED_NEW">>.

-spec dynamize_item(in_item()) -> json_item().
dynamize_item(Item) when is_list(Item) ->
    [dynamize_attr(Attr) || Attr <- Item];
dynamize_item(Item) ->
    error({erlcloud_ddb, {invalid_item, Item}}).

-spec dynamize_comparison(comparison_op()) -> {binary(), binary()}.
dynamize_comparison(eq) ->
    {<<"ComparisonOperator">>, <<"EQ">>};
dynamize_comparison(ne) ->
    {<<"ComparisonOperator">>, <<"NE">>};
dynamize_comparison(le) ->
    {<<"ComparisonOperator">>, <<"LE">>};
dynamize_comparison(lt) ->
    {<<"ComparisonOperator">>, <<"LT">>};
dynamize_comparison(ge) ->
    {<<"ComparisonOperator">>, <<"GE">>};
dynamize_comparison(gt) ->
    {<<"ComparisonOperator">>, <<"GT">>};
dynamize_comparison(not_null) ->
    {<<"ComparisonOperator">>, <<"NOT_NULL">>};
dynamize_comparison(null) ->
    {<<"ComparisonOperator">>, <<"NULL">>};
dynamize_comparison(contains) ->
    {<<"ComparisonOperator">>, <<"CONTAINS">>};
dynamize_comparison(not_contains) ->
    {<<"ComparisonOperator">>, <<"NOT_CONTAINS">>};
dynamize_comparison(begins_with) ->
    {<<"ComparisonOperator">>, <<"BEGINS_WITH">>};
dynamize_comparison(in) ->
    {<<"ComparisonOperator">>, <<"IN">>};
dynamize_comparison(between) ->
    {<<"ComparisonOperator">>, <<"BETWEEN">>}.

-type return_consumed_capacity() :: none | total.
-type return_consumed_capacity_opt() :: {return_consumed_capacity, return_consumed_capacity()}.
-spec dynamize_return_consumed_capacity(return_consumed_capacity()) -> binary().
dynamize_return_consumed_capacity(none) ->
    <<"NONE">>;
dynamize_return_consumed_capacity(total) ->
    <<"TOTAL">>.

-type return_item_collection_metrics() :: none | size.
-type return_item_collection_metrics_opt() :: {return_item_collection_metrics, return_item_collection_metrics()}.
-spec dynamize_return_item_collection_metrics(return_item_collection_metrics()) -> binary().
dynamize_return_item_collection_metrics(none) ->
    <<"NONE">>;
dynamize_return_item_collection_metrics(size) ->
    <<"SIZE">>.

%%%------------------------------------------------------------------------------
%%% Shared Undynamizers
%%%------------------------------------------------------------------------------

-spec undynamize_type(json_attr_type()) -> attr_type().
undynamize_type(<<"S">>) ->
    s;
undynamize_type(<<"N">>) ->
    n;
undynamize_type(<<"B">>) ->
    b.

-spec undynamize_number(binary()) -> number().
undynamize_number(Value) ->
    String = binary_to_list(Value),
    case lists:member($., String) of
        true ->
            list_to_float(String);
        false ->
            list_to_integer(String)
    end.
            
-spec undynamize_value(json_attr_value()) -> out_attr_value().
undynamize_value({<<"S">>, Value}) when is_binary(Value) ->
    Value;
undynamize_value({<<"N">>, Value}) ->
    undynamize_number(Value);
undynamize_value({<<"B">>, Value}) ->
    base64:decode(Value);
undynamize_value({<<"SS">>, Values}) when is_list(Values) ->
    Values;
undynamize_value({<<"NS">>, Values}) ->
    [undynamize_number(Value) || Value <- Values];
undynamize_value({<<"BS">>, Values}) ->
    [base64:decode(Value) || Value <- Values].

-spec undynamize_attr(json_attr()) -> out_attr().
undynamize_attr({Name, [ValueJson]}) ->
    {Name, undynamize_value(ValueJson)}.

-spec undynamize_object(fun((json_pair()) -> A), [json_pair()] | [{}]) -> [A].
undynamize_object(_, [{}]) ->
    %% jsx returns [{}] for empty objects
    [];
undynamize_object(PairFun, List) ->
    [PairFun(I) || I <- List].

-spec undynamize_item(json_item()) -> out_item().
undynamize_item(Json) ->
    undynamize_object(fun undynamize_attr/1, Json).

-spec undynamize_items([json_item()]) -> [out_item()].
undynamize_items(Items) ->
    [undynamize_item(I) || I <- Items].

-spec undynamize_value_typed(json_attr_value()) -> in_attr_typed_value().
undynamize_value_typed({<<"S">>, Value}) ->
    {s, Value};
undynamize_value_typed({<<"N">>, Value}) ->
    {n, undynamize_number(Value)};
undynamize_value_typed({<<"B">>, Value}) ->
    {b, base64:decode(Value)}.

-spec undynamize_typed_key(json_key()) -> key().
undynamize_typed_key([HashKey]) ->
    undynamize_attr_typed(HashKey);
undynamize_typed_key([HashKey, RangeKey]) ->
    {undynamize_attr_typed(HashKey), undynamize_attr_typed(RangeKey)}.

-spec undynamize_attr_defs([json_item()]) -> attr_defs().
undynamize_attr_defs(V) ->
    [{proplists:get_value(<<"AttributeName">>, I),
      undynamize_type(proplists:get_value(<<"AttributeType">>, I))}
     || I <- V].
    
key_name(Key) ->
    proplists:get_value(<<"AttributeName">>, Key).
    
-spec undynamize_key_schema([json_item()]) -> key_schema().
undynamize_key_schema([HashKey]) ->
    key_name(HashKey);
undynamize_key_schema([Key1, Key2]) ->
    case proplists:get_value(<<"KeyType">>, Key1) of
        <<"HASH">> ->
            {key_name(Key1), key_name(Key2)};
        <<"RANGE">> ->
            {key_name(Key2), key_name(Key1)}
    end.

-spec undynamize_table_status(binary()) -> table_status().
undynamize_table_status(<<"CREATING">>) -> creating;
undynamize_table_status(<<"UPDATING">>) -> updating;
undynamize_table_status(<<"DELETING">>) -> deleting;
undynamize_table_status(<<"ACTIVE">>)   -> active.
    
-type field_table() :: [{binary(), pos_integer(), fun((jsx:json_term()) -> term())}].

-spec undynamize_folder(field_table(), json_pair(), tuple()) -> tuple().
undynamize_folder(Table, {Key, Value}, A) ->
    case lists:keyfind(Key, 1, Table) of
        {Key, Index, ValueFun} ->
            setelement(Index, A, ValueFun(Value));
        false ->
            A
    end.

-type record_desc() :: {tuple(), field_table()}.

-spec undynamize_record(record_desc(), jsx:json_term()) -> tuple().
undynamize_record({Record, _}, [{}]) ->
    %% jsx returns [{}] for empty objects
    Record;
undynamize_record({Record, Table}, Json) ->
    lists:foldl(fun(Pair, A) -> undynamize_folder(Table, Pair, A) end, Record, Json).

%%%------------------------------------------------------------------------------
%%% Shared Options
%%%------------------------------------------------------------------------------

-spec id(X) -> X.
id(X) -> X.

-type out_type() :: json | record | simple.
-type out_opt() :: {out, out_type()}.
-type boolean_opt(Name) :: Name | {Name, boolean()}.
-type property() :: proplists:property().

-type aws_opts() :: [json_pair()].
-type ddb_opts() :: [out_opt()].
-type opts() :: {aws_opts(), ddb_opts()}.

-spec verify_ddb_opt(atom(), term()) -> ok.
verify_ddb_opt(out, Value) ->
    case lists:member(Value, [json, record, simple]) of
        true ->
            ok;
        false ->
            error({erlcloud_ddb, {invalid_opt, {out, Value}}})
    end;
verify_ddb_opt(Name, Value) ->
    error({erlcloud_ddb, {invalid_opt, {Name, Value}}}).

-type opt_table_entry() :: {atom(), binary(), fun((_) -> jsx:json_term())}.
-type opt_table() :: [opt_table_entry()].
-spec opt_folder(opt_table(), property(), opts()) -> opts().
opt_folder(_, {_, undefined}, Opts) ->
    %% ignore options set to undefined
    Opts;
opt_folder(Table, {Name, Value}, {AwsOpts, DdbOpts}) ->
    case lists:keyfind(Name, 1, Table) of
        {Name, Key, ValueFun} ->
            {[{Key, ValueFun(Value)} | AwsOpts], DdbOpts};
        false ->
            verify_ddb_opt(Name, Value),
            {AwsOpts, [{Name, Value} | DdbOpts]}
    end.

-spec opts(opt_table(), proplist()) -> opts().
opts(Table, Opts) when is_list(Opts) ->
    %% remove duplicate options
    Opts1 = lists:ukeysort(1, proplists:unfold(Opts)),
    lists:foldl(fun(Opt, A) -> opt_folder(Table, Opt, A) end, {[], []}, Opts1);
opts(_, _) ->
    error({erlcloud_ddb, opts_not_list}).

-spec return_consumed_capacity_opt() -> opt_table_entry().
return_consumed_capacity_opt() ->
    {return_consumed_capacity, <<"ReturnConsumedCapacity">>, fun dynamize_return_consumed_capacity/1}.

-spec return_item_collection_metrics_opt() -> opt_table_entry().
return_item_collection_metrics_opt() ->
    {return_item_collection_metrics, <<"ReturnItemCollectionMetrics">>, 
     fun dynamize_return_item_collection_metrics/1}.

-type get_item_opt() :: {attributes_to_get, [binary()]} | 
                        boolean_opt(consistent_read) |
                        return_consumed_capacity_opt() |
                        out_opt().
-type get_item_opts() :: [get_item_opt()].

-spec get_item_opts() -> opt_table().
get_item_opts() ->
    [{attributes_to_get, <<"AttributesToGet">>, fun id/1},
     {consistent_read, <<"ConsistentRead">>, fun id/1},
     return_consumed_capacity_opt()].

%%%------------------------------------------------------------------------------
%%% Output
%%%------------------------------------------------------------------------------
-type ddb_return(Record, Simple) :: {ok, jsx:json_term() | Record | Simple} | {error, term()}.
-type item_return() :: ok_return(out_item()).
-type undynamize_fun() :: fun((jsx:json_term()) -> tuple()).

-spec out(erlcloud_ddb1:json_return(), undynamize_fun(), ddb_opts()) 
         -> {ok, jsx:json_term() | tuple()} |
            {simple, term()} |
            {error, term()}.
out({error, Reason}, _, _) ->
    {error, Reason};
out({ok, Json}, Undynamize, Opts) ->
    case proplists:get_value(out, Opts, simple) of
        json ->
            {ok, Json};
        record ->
            {ok, Undynamize(Json)};
        simple ->
            {simple, Undynamize(Json)}
    end.

%% Returns specified field of tuple for simple return
-spec out(erlcloud_ddb1:json_return(), undynamize_fun(), ddb_opts(), pos_integer()) 
         -> ok_return(term()).
out(Result, Undynamize, Opts, Index) ->
    out(Result, Undynamize, Opts, Index, {error, no_return}).

-spec out(erlcloud_ddb1:json_return(), undynamize_fun(), ddb_opts(), pos_integer(), ok_return(term())) 
         -> ok_return(term()).
out(Result, Undynamize, Opts, Index, Default) ->
    case out(Result, Undynamize, Opts) of
        {simple, Record} ->
            case element(Index, Record) of
                undefined ->
                    Default;
                Element ->
                    {ok, Element}
            end;
        Else ->
            Else
    end.

%%%------------------------------------------------------------------------------
%%% Shared Records
%%%------------------------------------------------------------------------------

-spec consumed_capacity_record() -> record_desc().
consumed_capacity_record() ->
    {#ddb_consumed_capacity{},
     [{<<"CapacityUnits">>, #ddb_consumed_capacity.capacity_units, fun id/1},
      {<<"TableName">>, #ddb_consumed_capacity.table_name, fun id/1}]}.

undynamize_consumed_capacity(V) ->
    undynamize_record(consumed_capacity_record(), V).

undynamize_consumed_capacity_list(V) ->
    [undynamize_record(consumed_capacity_record(), I) || I <- V].

-spec item_collection_metrics_record() -> record_desc().
item_collection_metrics_record() ->
    {#ddb_item_collection_metrics{},
     [{<<"ItemCollectionKey">>, #ddb_item_collection_metrics.item_collection_key,
       fun([V]) ->
               {_Name, Value} = undynamize_attr(V),
               Value
       end},
      {<<"SizeEstimateRangeGB">>, #ddb_item_collection_metrics.size_estimate_range_gb,
       fun([L, H]) -> {L, H} end}]}.

undynamize_item_collection_metrics(V) ->
    undynamize_record(item_collection_metrics_record(), V).

undynamize_item_collection_metric_list(Table, V) ->
    {Table, [undynamize_item_collection_metrics(I) || I <- V]}.

undynamize_projection(V) ->
    case proplists:get_value(<<"ProjectionType">>, V) of
        <<"KEYS_ONLY">> ->
            keys_only;
        <<"ALL">> ->
            all;
        <<"INCLUDE">> ->
            {include, proplists:get_value(<<"NonKeyAttributes">>, V)}
    end.
    
-spec local_secondary_index_description_record() -> record_desc().
local_secondary_index_description_record() ->
    {#ddb_local_secondary_index_description{},
     [{<<"IndexName">>, #ddb_local_secondary_index_description.index_name, fun id/1},
      {<<"IndexSizeBytes">>, #ddb_local_secondary_index_description.index_size_bytes, fun id/1},
      {<<"ItemCount">>, #ddb_local_secondary_index_description.item_count, fun id/1},
      {<<"KeySchema">>, #ddb_local_secondary_index_description.key_schema, fun undynamize_key_schema/1},
      {<<"Projection">>, #ddb_local_secondary_index_description.projection, fun undynamize_projection/1}
     ]}.

-spec provisioned_throughput_description_record() -> record_desc().
provisioned_throughput_description_record() ->
    {#ddb_provisioned_throughput_description{},
     [{<<"LastDecreaseDateTime">>, #ddb_provisioned_throughput_description.last_decrease_date_time, fun id/1},
      {<<"LastIncreaseDateTime">>, #ddb_provisioned_throughput_description.last_increase_date_time, fun id/1},
      {<<"NumberOfDecreasesToday">>, #ddb_provisioned_throughput_description.number_of_decreases_today, fun id/1},
      {<<"ReadCapacityUnits">>, #ddb_provisioned_throughput_description.read_capacity_units, fun id/1},
      {<<"WriteCapacityUnits">>, #ddb_provisioned_throughput_description.write_capacity_units, fun id/1}
     ]}.

-spec table_description_record() -> record_desc().
table_description_record() ->
    {#ddb_table_description{},
     [{<<"AttributeDefinitions">>, #ddb_table_description.attribute_definitions, fun undynamize_attr_defs/1},
      {<<"CreationDateTime">>, #ddb_table_description.creation_date_time, fun id/1},
      {<<"ItemCount">>, #ddb_table_description.item_count, fun id/1},
      {<<"KeySchema">>, #ddb_table_description.key_schema, fun undynamize_key_schema/1},
      {<<"LocalSecondaryIndexes">>, #ddb_table_description.local_secondary_indexes,
       fun(V) -> [undynamize_record(local_secondary_index_description_record(), I) || I <- V] end},
      {<<"ProvisionedThroughput">>, #ddb_table_description.provisioned_throughput,
       fun(V) -> undynamize_record(provisioned_throughput_description_record(), V) end},
      {<<"TableName">>, #ddb_table_description.table_name, fun id/1},
      {<<"TableSizeBytes">>, #ddb_table_description.table_size_bytes, fun id/1},
      {<<"TableStatus">>, #ddb_table_description.table_status, fun undynamize_table_status/1}
     ]}.

%%%------------------------------------------------------------------------------
%%% BatchGetItem
%%%------------------------------------------------------------------------------

-type batch_get_item_opt() :: return_consumed_capacity_opt() |
                              out_opt().
-type batch_get_item_opts() :: [batch_get_item_opt()].

-spec batch_get_item_opts() -> opt_table().
batch_get_item_opts() ->
    [return_consumed_capacity_opt()].

-type batch_get_item_request_item() :: {table_name(), [key(),...], get_item_opts()} | {table_name(), [key(),...]}.

-spec dynamize_batch_get_item_request_item(batch_get_item_request_item()) 
                                          -> {binary(), jsx:json_term(), jsx:json_term()}.
dynamize_batch_get_item_request_item({Table, Keys}) ->
    dynamize_batch_get_item_request_item({Table, Keys, []});
dynamize_batch_get_item_request_item({Table, Keys, Opts}) ->
    {AwsOpts, []} = opts(get_item_opts(), Opts),
    {Table, [{<<"Keys">>, [dynamize_key(K) || K <- Keys]}] ++ AwsOpts}.

-type batch_get_item_request_items() :: maybe_list(batch_get_item_request_item()).
-spec dynamize_batch_get_item_request_items(batch_get_item_request_items()) -> [tuple()].
dynamize_batch_get_item_request_items(Request) ->
    dynamize_maybe_list(fun dynamize_batch_get_item_request_item/1, Request).

-spec batch_get_item_request_item_folder({binary(), term()}, batch_get_item_request_item()) 
                                        -> batch_get_item_request_item().
batch_get_item_request_item_folder({<<"Keys">>, Keys}, {Table, _, Opts}) ->
    {Table, [undynamize_typed_key(K) || K <- Keys], Opts};
batch_get_item_request_item_folder({<<"AttributesToGet">>, Value}, {Table, Keys, Opts}) ->
    {Table, Keys, [{attributes_to_get, Value} | Opts]};
batch_get_item_request_item_folder({<<"ConsistentRead">>, Value}, {Table, Keys, Opts}) ->
    {Table, Keys, [{consistent_read, Value} | Opts]}.

-spec undynamize_batch_get_item_request_item(table_name(), jsx:json_term()) -> batch_get_item_request_item().
undynamize_batch_get_item_request_item(Table, Json) ->
    lists:foldl(fun batch_get_item_request_item_folder/2, {Table, [], []}, Json).

undynamize_batch_get_item_response({Table, Json}) ->
    #ddb_batch_get_item_response{
       table = Table,
       items = undynamize_items(Json)}.

undynamize_batch_get_item_responses(Response) ->
    undynamize_object(fun undynamize_batch_get_item_response/1, Response).

-spec batch_get_item_record() -> record_desc().    
batch_get_item_record() ->
    {#ddb_batch_get_item{},
     [{<<"ConsumedCapacity">>, #ddb_batch_get_item.consumed_capacity, fun undynamize_consumed_capacity_list/1},
      {<<"Responses">>, #ddb_batch_get_item.responses, fun undynamize_batch_get_item_responses/1},
      {<<"UnprocessedKeys">>, #ddb_batch_get_item.unprocessed_keys,
       fun(V) -> undynamize_object(fun({Table, Json}) ->
                                           undynamize_batch_get_item_request_item(Table, Json)
                                   end, V)
       end}
     ]}.

-type batch_get_item_return() :: ddb_return(#ddb_batch_get_item{}, [erlcloud_ddb:out_item()]).

-spec batch_get_item([batch_get_item_request_item()]) -> batch_get_item_return().
batch_get_item(RequestItems) ->
    batch_get_item(RequestItems, [], default_config()).

-spec batch_get_item([batch_get_item_request_item()], batch_get_item_opts()) -> batch_get_item_return().
batch_get_item(RequestItems, Opts) ->
    batch_get_item(RequestItems, Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc 
%% DynamoDB API:
%% [http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_BatchGetItems.html]
%%
%% ===Example===
%%
%% Get attributes "user" and "friends" from items with keys "Julie"
%% and "Mingus" in table "comp2" and attributes "user" and "status"
%% from three items in table "comp1".
%%
%% `
%% {ok, Items} = erlcloud_ddb:batch_get_item(
%%                    [{<<"comp2">>, [<<"Julie">>, 
%%                                    <<"Mingus">>], 
%%                      [{attributes_to_get, [<<"user">>, <<"friends">>]}]},
%%                     {<<"comp1">>, [{<<"Casey">>, 1319509152},
%%                                    {<<"Dave">>, 1319509155},
%%                                    {<<"Riley">>, 1319509158}],
%%                      [{attributes_to_get, [<<"user">>, <<"status">>]}]}])
%% '
%% @end
%%------------------------------------------------------------------------------
-spec batch_get_item([batch_get_item_request_item()], batch_get_item_opts(), aws_config()) -> 
                            batch_get_item_return().
batch_get_item(RequestItems, Opts, Config) ->
    {AwsOpts, DdbOpts} = opts(batch_get_item_opts(), Opts),
    Return = erlcloud_ddb1:batch_get_item(dynamize_batch_get_item_request_items(RequestItems), AwsOpts, Config),
    case out(Return, fun(Json) -> undynamize_record(batch_get_item_record(), Json) end, DdbOpts) of
        {simple, #ddb_batch_get_item{unprocessed_keys = [_|_]}} ->
            %% TODO resend unprocessed keys automatically (or controlled by option). 
            %% For now return an error - you can handle manually if you don't use simple.
            {error, unprocessed};
        {simple, #ddb_batch_get_item{unprocessed_keys = [], responses = Responses}} ->
            %% Simple return for batch_get_item is all items from all tables in a single list
            {ok, lists:flatmap(fun(#ddb_batch_get_item_response{items = I}) -> I end, Responses)};
        {ok, _} = Out -> Out;
        {error, _} = Out -> Out
    end.

%%%------------------------------------------------------------------------------
%%% BatchWriteItem
%%%------------------------------------------------------------------------------

-type batch_write_item_opt() :: return_consumed_capacity_opt() |
                                return_item_collection_metrics_opt() |
                                out_opt().
-type batch_write_item_opts() :: [batch_write_item_opt()].

-spec batch_write_item_opts() -> opt_table().
batch_write_item_opts() ->
    [return_consumed_capacity_opt(),
     return_item_collection_metrics_opt()].

-type batch_write_item_put() :: {put, in_item()}.
-type batch_write_item_delete() :: {delete, key()}.
-type batch_write_item_request() :: batch_write_item_put() | batch_write_item_delete().
-type batch_write_item_request_item() :: {table_name(), [batch_write_item_request()]}.

-spec dynamize_batch_write_item_request(batch_write_item_request()) -> jsx:json_term().
dynamize_batch_write_item_request({put, Item}) ->
    [{<<"PutRequest">>, [{<<"Item">>, dynamize_item(Item)}]}];
dynamize_batch_write_item_request({delete, Key}) ->
    [{<<"DeleteRequest">>, [{<<"Key">>, dynamize_key(Key)}]}].

-spec dynamize_batch_write_item_request_item(batch_write_item_request_item()) 
                                          -> json_pair().
dynamize_batch_write_item_request_item({Table, Requests}) ->
    {Table, [dynamize_batch_write_item_request(R) || R <- Requests]}.

-type batch_write_item_request_items() :: maybe_list(batch_write_item_request_item()).
-spec dynamize_batch_write_item_request_items(batch_write_item_request_items()) -> [tuple()].
dynamize_batch_write_item_request_items(Request) ->
    dynamize_maybe_list(fun dynamize_batch_write_item_request_item/1, Request).

-spec undynamize_attr_typed(json_attr()) -> in_attr().
undynamize_attr_typed({Name, [ValueJson]}) ->
    {Name, undynamize_value_typed(ValueJson)}.

-spec undynamize_item_typed(json_item()) -> in_item().
undynamize_item_typed(Json) ->
    undynamize_object(fun undynamize_attr_typed/1, Json).

-spec batch_write_item_request_folder([{binary(), term()}], batch_write_item_request_item()) 
                                     -> batch_write_item_request_item().
batch_write_item_request_folder([{<<"PutRequest">>, [{<<"Item">>, Item}]}], {Table, Requests}) ->
    {Table, [{put, undynamize_item_typed(Item)} | Requests]};
batch_write_item_request_folder([{<<"DeleteRequest">>, [{<<"Key">>, Key}]}], {Table, Requests}) ->
    {Table, [{delete, undynamize_typed_key(Key)} | Requests]}.

-spec undynamize_batch_write_item_request_item(table_name(), jsx:json_term()) -> batch_write_item_request_item().
undynamize_batch_write_item_request_item(Table, Json) ->
    {Table, Requests} = lists:foldl(fun batch_write_item_request_folder/2, {Table, []}, Json),
    {Table, lists:reverse(Requests)}.

-spec batch_write_item_record() -> record_desc().
batch_write_item_record() ->
    {#ddb_batch_write_item{},
     [{<<"ConsumedCapacity">>, #ddb_batch_write_item.consumed_capacity, fun undynamize_consumed_capacity_list/1},
      {<<"ItemCollectionMetrics">>, #ddb_batch_write_item.item_collection_metrics,
       fun(V) -> undynamize_object(fun({Table, Json}) ->
                                           undynamize_item_collection_metric_list(Table, Json)
                                   end, V)
       end},
      {<<"UnprocessedItems">>, #ddb_batch_write_item.unprocessed_items,
       fun(V) -> undynamize_object(fun({Table, Json}) ->
                                           undynamize_batch_write_item_request_item(Table, Json)
                                   end, V)
       end}
     ]}.

-type batch_write_item_return() :: ddb_return(#ddb_batch_write_item{}, #ddb_batch_write_item{}).

-spec batch_write_item([batch_write_item_request_item()]) -> batch_write_item_return().
batch_write_item(RequestItems) ->
    batch_write_item(RequestItems, [], default_config()).

-spec batch_write_item([batch_write_item_request_item()], batch_write_item_opts()) -> batch_write_item_return().
batch_write_item(RequestItems, Opts) ->
    batch_write_item(RequestItems, Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc 
%% DynamoDB API:
%% [http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_BatchWriteItem.html]
%%
%% ===Example===
%%
%% Put and delete an item in the "Reply" table and put an item in the "Thread" table.
%%
%% `
%% {ok, _} = erlcloud_ddb:batch_write_item(
%%                    [{<<"Reply">>, [{put, [{<<"ReplyDateTime">>, <<"2012-04-03T11:04:47.034Z">>},
%%                                           {<<"Id">>, <<"Amazon DynamoDB#DynamoDB Thread 5">>}]},
%%                                    {delete, {<<"Amazon DynamoDB#DynamoDB Thread 4">>,
%%                                              <<"oops - accidental row">>}}]},
%%                     {<<"Thread">>, [{put, [{<<"ForumName">>, <<"Amazon DynamoDB">>},
%%                                            {<<"Subject">>, <<"DynamoDB Thread 5">>}]}]}])
%% '
%% @end
%%------------------------------------------------------------------------------
-spec batch_write_item([batch_write_item_request_item()], batch_write_item_opts(), aws_config()) -> 
                              batch_write_item_return().
batch_write_item(RequestItems, Opts, Config) ->
    {AwsOpts, DdbOpts} = opts(batch_write_item_opts(), Opts),
    Return = erlcloud_ddb1:batch_write_item(dynamize_batch_write_item_request_items(RequestItems), AwsOpts, Config),
    case out(Return, fun(Json) -> undynamize_record(batch_write_item_record(), Json) end, DdbOpts) of
        {simple, #ddb_batch_write_item{unprocessed_items = [_|_]}} ->
            %% TODO resend unprocessed items automatically (or controlled by option). 
            %% For now return an error - you can handle manually if you don't use simple.
            {error, unprocessed};
        {simple, Record} -> {ok, Record};
        {ok, _} = Out -> Out;
        {error, _} = Out -> Out
    end.

%%%------------------------------------------------------------------------------
%%% CreateTable
%%%------------------------------------------------------------------------------

-type index_name() :: binary().
-type range_key_name() :: attr_name().
-type projection() :: keys_only |
                      {include, [attr_name()]} |
                      all.
-type local_secondary_index_def() :: {index_name(), range_key_name(), projection()}.

dynamize_projection(keys_only) ->
    [{<<"ProjectionType">>, <<"KEYS_ONLY">>}];
dynamize_projection(all) ->
    [{<<"ProjectionType">>, <<"ALL">>}];
dynamize_projection({include, AttrNames}) ->
    [{<<"ProjectionType">>, <<"INCLUDE">>},
     {<<"NonKeyAttributes">>, AttrNames}].

dynamize_local_secondary_index(HashKey, {IndexName, RangeKey, Projection}) ->
    [{<<"IndexName">>, IndexName},
     {<<"KeySchema">>, [[{<<"AttributeName">>, HashKey},
                         {<<"KeyType">>, <<"HASH">>}],
                        [{<<"AttributeName">>, RangeKey},
                         {<<"KeyType">>, <<"RANGE">>}]]},
     {<<"Projection">>, dynamize_projection(Projection)}].

dynamize_local_secondary_indexes({HashKey, _RangeKey}, Value) ->
    [dynamize_local_secondary_index(HashKey, I) || I <- Value].

-type create_table_opt() :: {local_secondary_indexes, [local_secondary_index_def()]}.
-type create_table_opts() :: [create_table_opt()].

-spec create_table_opts(key_schema()) -> opt_table().
create_table_opts(KeySchema) ->
    [{local_secondary_indexes, <<"LocalSecondaryIndexes">>, 
      fun(V) -> dynamize_local_secondary_indexes(KeySchema, V) end}].

-spec create_table_record() -> record_desc().
create_table_record() ->
    {#ddb_create_table{},
     [{<<"TableDescription">>, #ddb_create_table.table_description, 
       fun(V) -> undynamize_record(table_description_record(), V) end}
     ]}. 

-type create_table_return() :: ddb_return(#ddb_create_table{}, #ddb_table_description{}).

-spec create_table(table_name(), attr_defs(), key_schema(), non_neg_integer(), non_neg_integer()) 
                  -> create_table_return().
create_table(Table, AttrDefs, KeySchema, ReadUnits, WriteUnits) ->
    create_table(Table, AttrDefs, KeySchema, ReadUnits, WriteUnits, [], default_config()).

-spec create_table(table_name(), attr_defs(), key_schema(), non_neg_integer(), non_neg_integer(), 
                   create_table_opts())
                  -> create_table_return().
create_table(Table, AttrDefs, KeySchema, ReadUnits, WriteUnits, Opts) ->
    create_table(Table, AttrDefs, KeySchema, ReadUnits, WriteUnits, Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc 
%% DynamoDB API:
%% [http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_CreateTable.html]
%%
%% ===Example===
%%
%% Create "comp-table" with "user" as a string hash key and "time" as
%% a number range key and provisioned throughput of 5 read units and
%% 10 write units.
%%
%% `
%% {ok, _} = erlcloud_ddb:create_table(<<"comp-table">>, {{<<"user">>, s}, {<<"time">>, n}}, 5, 10)
%% '
%% @end
%%------------------------------------------------------------------------------
-spec create_table(table_name(), attr_defs(), key_schema(), non_neg_integer(), non_neg_integer(), 
                   create_table_opts(), aws_config()) 
                  -> create_table_return().
create_table(Table, AttrDefs, KeySchema, ReadUnits, WriteUnits, Opts, Config) ->
    {AwsOpts, DdbOpts} = opts(create_table_opts(KeySchema), Opts),
    Return = erlcloud_ddb1:create_table(Table, dynamize_attr_defs(AttrDefs), dynamize_key_schema(KeySchema), 
                                        ReadUnits, WriteUnits, AwsOpts, Config),
    out(Return, fun(Json) -> undynamize_record(create_table_record(), Json) end, DdbOpts, 
        #ddb_create_table.table_description).

%%%------------------------------------------------------------------------------
%%% DeleteItem
%%%------------------------------------------------------------------------------

-type delete_item_opt() :: {expected, in_expected()} | 
                           {return_values, none | all_old} |
                           return_consumed_capacity_opt() |
                           return_item_collection_metrics_opt() |
                           out_opt().
-type delete_item_opts() :: [delete_item_opt()].

-spec delete_item_opts() -> opt_table().
delete_item_opts() ->
    [{expected, <<"Expected">>, fun dynamize_expected/1},
     {return_values, <<"ReturnValues">>, fun dynamize_return_value/1},
     return_consumed_capacity_opt(),
     return_item_collection_metrics_opt()].

-spec delete_item_record() -> record_desc().
delete_item_record() ->
    {#ddb_delete_item{},
     [{<<"Attributes">>, #ddb_delete_item.attributes, fun undynamize_item/1},
      {<<"ConsumedCapacity">>, #ddb_delete_item.consumed_capacity, fun undynamize_consumed_capacity/1},
      {<<"ItemCollectionMetrics">>, #ddb_delete_item.item_collection_metrics, 
       fun undynamize_item_collection_metrics/1}
     ]}.

-type delete_item_return() :: ddb_return(#ddb_delete_item{}, out_item()).

-spec delete_item(table_name(), key()) -> delete_item_return().
delete_item(Table, Key) ->
    delete_item(Table, Key, [], default_config()).

-spec delete_item(table_name(), key(), delete_item_opts()) -> delete_item_return().
delete_item(Table, Key, Opts) ->
    delete_item(Table, Key, Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc 
%% DynamoDB API:
%% [http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_DeleteItem.html]
%%
%% ===Example===
%%
%% Delete item with hash key "Mingus" and range key 200 from
%% "comp-table" if the "status" field is "shopping". Return all old
%% values.
%%
%% `
%% {ok, OldValues} = erlcloud_ddb:delete_item(<<"comp-table">>, {"Mingus", 200},
%%                                           [{return_values, all_old},
%%                                            {expected, {<<"status">>, "shopping"}}])
%% '
%% @end
%%------------------------------------------------------------------------------
-spec delete_item(table_name(), key(), delete_item_opts(), aws_config()) -> delete_item_return().
delete_item(Table, Key, Opts, Config) ->
    {AwsOpts, DdbOpts} = opts(delete_item_opts(), Opts),
    Return = erlcloud_ddb1:delete_item(Table, dynamize_key(Key), AwsOpts, Config),
    out(Return, fun(Json) -> undynamize_record(delete_item_record(), Json) end, DdbOpts, 
        #ddb_delete_item.attributes, {ok, []}).

%%%------------------------------------------------------------------------------
%%% DeleteTable
%%%------------------------------------------------------------------------------

-spec delete_table_record() -> record_desc().
delete_table_record() ->
    {#ddb_delete_table{},
     [{<<"TableDescription">>, #ddb_create_table.table_description, 
       fun(V) -> undynamize_record(table_description_record(), V) end}
     ]}. 

-type delete_table_return() :: ddb_return(#ddb_delete_table{}, #ddb_table_description{}).

-spec delete_table(table_name()) -> delete_table_return().
delete_table(Table) ->
    delete_table(Table, [], default_config()).

-spec delete_table(table_name(), ddb_opts()) -> delete_table_return().
delete_table(Table, Opts) ->
    delete_table(Table, Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc 
%% DynamoDB API:
%% [http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_DeleteTable.html]
%%
%% ===Example===
%%
%% Delete "Table1".
%%
%% `
%% {ok, _} = erlcloud_ddb:delete_table(<<"Table1">>)
%% '
%% @end
%%------------------------------------------------------------------------------
-spec delete_table(table_name(), ddb_opts(), aws_config()) -> delete_table_return().
delete_table(Table, Opts, Config) ->
    {[], DdbOpts} = opts([], Opts),
    Return = erlcloud_ddb1:delete_table(Table, Config),
    out(Return, fun(Json) -> undynamize_record(delete_table_record(), Json) end, DdbOpts, 
        #ddb_delete_table.table_description).

%%%------------------------------------------------------------------------------
%%% DescribeTable
%%%------------------------------------------------------------------------------

-spec describe_table_record() -> record_desc().
describe_table_record() ->
    {#ddb_describe_table{},
     [{<<"Table">>, #ddb_describe_table.table, fun(V) -> undynamize_record(table_description_record(), V) end}
     ]}. 

-type describe_table_return() :: ddb_return(#ddb_describe_table{}, #ddb_table_description{}).

-spec describe_table(table_name()) -> describe_table_return().
describe_table(Table) ->
    describe_table(Table, [], default_config()).

-spec describe_table(table_name(), ddb_opts()) -> describe_table_return().
describe_table(Table, Opts) ->
    describe_table(Table, Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc 
%% DynamoDB API:
%% [http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_DescribeTables.html]
%%
%% ===Example===
%%
%% Describe "Table1".
%%
%% `
%% {ok, Table} = erlcloud_ddb:describe_table(<<"Table1">>)
%% '
%% @end
%%------------------------------------------------------------------------------
-spec describe_table(table_name(), ddb_opts(), aws_config()) -> describe_table_return().
describe_table(Table, Opts, Config) ->
    {[], DdbOpts} = opts([], Opts),
    Return = erlcloud_ddb1:describe_table(Table, Config),
    out(Return, fun(Json) -> undynamize_record(describe_table_record(), Json) end, DdbOpts, 
        #ddb_describe_table.table).

%%%------------------------------------------------------------------------------
%%% GetItem
%%%------------------------------------------------------------------------------

-spec get_item_record() -> record_desc().
get_item_record() ->
    {#ddb_get_item{},
     [{<<"Item">>, #ddb_get_item.item, fun undynamize_item/1},
      {<<"ConsumedCapacity">>, #ddb_get_item.consumed_capacity, fun undynamize_consumed_capacity/1}
     ]}.

-spec get_item(table_name(), key()) -> item_return().
get_item(Table, Key) ->
    get_item(Table, Key, [], default_config()).

-spec get_item(table_name(), key(), get_item_opts()) -> item_return().
get_item(Table, Key, Opts) ->
    get_item(Table, Key, Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc 
%% DynamoDB API:
%% [http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_GetItem.html]
%%
%% ===Example===
%%
%% Get attributes "status" and "friends" from the item with hash key
%% "Julie" and range key 1307654345 in the table "comptable" using a
%% consistent read.
%%
%% `
%% {ok, Item} = erlcloud_ddb:get_item(<<"comptable">>, {"Julie", 1307654345}, 
%%                                        [consistent_read, 
%%                                         {attributes_to_get, [<<"status">>, <<"friends">>]}])
%% '
%% @end
%%------------------------------------------------------------------------------
-spec get_item(table_name(), key(), get_item_opts(), aws_config()) -> item_return().
get_item(Table, Key, Opts, Config) ->
    {AwsOpts, DdbOpts} = opts(get_item_opts(), Opts),
    Return = erlcloud_ddb1:get_item(Table, dynamize_key(Key), AwsOpts, Config),
    out(Return, fun(Json) -> undynamize_record(get_item_record(), Json) end, DdbOpts, 
        #ddb_get_item.item, {ok, []}).

%%%------------------------------------------------------------------------------
%%% ListTables
%%%------------------------------------------------------------------------------

-type list_tables_opt() :: {limit, pos_integer()} | 
                           {exclusive_start_table_name, binary()} |
                           out_opt().
-type list_tables_opts() :: [list_tables_opt()].

-spec list_tables_opts() -> opt_table().
list_tables_opts() ->
    [{limit, <<"Limit">>, fun id/1},
     {exclusive_start_table_name, <<"ExclusiveStartTableName">>, fun id/1}].

-spec list_tables_record() -> record_desc().
list_tables_record() ->
    {#ddb_list_tables{},
     [{<<"TableNames">>, #ddb_list_tables.table_names, fun id/1},
      {<<"LastEvaluatedTableName">>, #ddb_list_tables.last_evaluated_table_name, fun id/1}
     ]}.

-type list_tables_return() :: ddb_return(#ddb_list_tables{}, [table_name()]).

-spec list_tables() -> list_tables_return().
list_tables() ->
    list_tables([], default_config()).

-spec list_tables(list_tables_opts()) -> list_tables_return().
list_tables(Opts) ->
    list_tables(Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc 
%% DynamoDB API:
%% [http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_ListTables.html]
%%
%% ===Example===
%%
%% Get the next 3 table names after "comp2".
%%
%% `
%% {ok, TableNames} = erlcloud_ddb:list_tables([{limit, 3}, {exclusive_start_table_name, <<"comp2">>}])
%% '
%% @end
%%------------------------------------------------------------------------------
-spec list_tables(list_tables_opts(), aws_config()) -> list_tables_return().
list_tables(Opts, Config) ->
    {AwsOpts, DdbOpts} = opts(list_tables_opts(), Opts),
    Return = erlcloud_ddb1:list_tables(AwsOpts, Config),
    out(Return, fun(Json) -> undynamize_record(list_tables_record(), Json) end, DdbOpts, 
        #ddb_list_tables.table_names, {ok, []}).

%%%------------------------------------------------------------------------------
%%% PutItem
%%%------------------------------------------------------------------------------

-type put_item_opt() :: {expected, in_expected()} | 
                        {return_values, none | all_old} |
                        out_opt().
-type put_item_opts() :: [put_item_opt()].

-spec put_item_opts() -> opt_table().
put_item_opts() ->
    [{expected, <<"Expected">>, fun dynamize_expected/1},
     {return_values, <<"ReturnValues">>, fun dynamize_return_value/1}].

-spec put_item_record() -> record_desc().
put_item_record() ->
    {#ddb_put_item{},
     [{<<"Attributes">>, #ddb_put_item.attributes, fun undynamize_item/1},
      {<<"ConsumedCapacityUnits">>, #ddb_put_item.consumed_capacity_units, fun id/1}
     ]}.

-type put_item_return() :: ddb_return(#ddb_put_item{}, out_item()).

-spec put_item(table_name(), in_item()) -> put_item_return().
put_item(Table, Item) ->
    put_item(Table, Item, [], default_config()).

-spec put_item(table_name(), in_item(), put_item_opts()) -> put_item_return().
put_item(Table, Item, Opts) ->
    put_item(Table, Item, Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc 
%% DynamoDB API:
%% [http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_PutItem.html]
%%
%% ===Example===
%%
%% Put item with attributes "time" of 300, "feeling" of "not
%% surprised" and "user" of "Riley" into table "comp5", but only if an
%% item with the same key exists and has field "feeling" set to
%% "surprised". Return all the old attributes.
%%
%% `
%% {ok, OldItem} = erlcloud_ddb:put_item(<<"comp5">>, 
%%                                        [{<<"time">>, 300}, 
%%                                         {<<"feeling">>, <<"not surprised">>},
%%                                         {<<"user">>, <<"Riley">>}],
%%                                        [{return_values, all_old},
%%                                         {expected, {<<"feeling">>, <<"surprised">>}}])
%% '
%% @end
%%------------------------------------------------------------------------------
-spec put_item(table_name(), in_item(), put_item_opts(), aws_config()) -> put_item_return().
put_item(Table, Item, Opts, Config) ->
    {AwsOpts, DdbOpts} = opts(put_item_opts(), Opts),
    Return = erlcloud_ddb1:put_item(Table, dynamize_item(Item), AwsOpts, Config),
    out(Return, fun(Json) -> undynamize_record(put_item_record(), Json) end, DdbOpts, 
        #ddb_put_item.attributes, {ok, []}).

%%%------------------------------------------------------------------------------
%%% Queue
%%%------------------------------------------------------------------------------

-type json_range_key_condition() :: jsx:json_term().
-type range_key_condition() :: {in_attr_value(), comparison_op()} | 
                               {{in_attr_value(), in_attr_value()}, between}.

-spec dynamize_range_key_condition(range_key_condition()) -> json_range_key_condition().
dynamize_range_key_condition({{Value1, Value2}, between}) ->
    [{<<"AttributeValueList">>, [[dynamize_value(Value1)], [dynamize_value(Value2)]]},
      dynamize_comparison(between)];
dynamize_range_key_condition({Value, Comparison}) ->
    [{<<"AttributeValueList">>, [[dynamize_value(Value)]]}, dynamize_comparison(Comparison)].

-type q_opt() :: {attributes_to_get, [binary()]} | 
                 {limit, pos_integer()} |
                 boolean_opt(consistent_read) | 
                 boolean_opt(count) |
                 {range_key_condition, range_key_condition()} |
                 boolean_opt(scan_index_forward) |
                 {exclusive_start_key, key() | undefined} |
                 out_opt().
-type q_opts() :: [q_opt()].

-spec q_opts() -> opt_table().
q_opts() ->
    [{attributes_to_get, <<"AttributesToGet">>, fun id/1},
     {limit, <<"Limit">>, fun id/1},
     {consistent_read, <<"ConsistentRead">>, fun id/1},
     {count, <<"Count">>, fun id/1},
     {range_key_condition, <<"RangeKeyCondition">>, fun dynamize_range_key_condition/1},
     {scan_index_forward, <<"ScanIndexForward">>, fun id/1},
     {exclusive_start_key, <<"ExclusiveStartKey">>, fun(V) -> erlcloud_ddb1:key_value(dynamize_key(V)) end}].

-spec q_record() -> record_desc().
q_record() ->
    {#ddb_q{},
     [{<<"Items">>, #ddb_q.items, fun(V) -> [undynamize_item(I) || I <- V] end},
      {<<"Count">>, #ddb_q.count, fun id/1},
      {<<"LastEvaluatedKey">>, #ddb_q.last_evaluated_key, fun undynamize_typed_key/1},
      {<<"ConsumedCapacityUnits">>, #ddb_q.consumed_capacity_units, fun id/1}
     ]}.

-type q_return() :: ddb_return(#ddb_q{}, [out_item()]).

-spec q(table_name(), hash_key()) -> q_return().
q(Table, HashKey) ->
    q(Table, HashKey, [], default_config()).

-spec q(table_name(), hash_key(), q_opts()) -> q_return().
q(Table, HashKey, Opts) ->
    q(Table, HashKey, Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc 
%% DynamoDB API:
%% [http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_Query.html]
%%
%% ===Example===
%%
%% Get up to 2 items with hash key "John" and with range keys coming
%% before "The Matrix" from table "1-hash-rangetable".
%%
%% `
%% {ok, Items} = erlcloud_ddb:q(<<"1-hash-rangetable">>, <<"John">>,
%%                                 [{exclusive_start_key, {{s, <<"John">>}, {s, <<"The Matrix">>}}},
%%                                  {scan_index_forward, false},
%%                                  {limit, 2}])
%% '
%% @end
%%------------------------------------------------------------------------------
-spec q(table_name(), hash_key(), q_opts(), aws_config()) -> q_return().
q(Table, HashKey, Opts, Config) ->
    {AwsOpts, DdbOpts} = opts(q_opts(), Opts),
    Return = erlcloud_ddb1:q(Table, dynamize_key(HashKey), AwsOpts, Config),
    out(Return, fun(Json) -> undynamize_record(q_record(), Json) end, DdbOpts, 
        #ddb_q.items, {ok, []}).

%%%------------------------------------------------------------------------------
%%% Scan
%%%------------------------------------------------------------------------------

-type scan_filter_item() :: {attr_name(), [in_attr_value()], in} |
                            {attr_name(), {in_attr_value(), in_attr_value()}, between} |
                            {attr_name(), in_attr_value(), comparison_op()}.
-type scan_filter() :: maybe_list(scan_filter_item()).

-spec dynamize_scan_filter_item(scan_filter_item()) -> json_pair().
dynamize_scan_filter_item({Name, AttrValueList, in}) ->
    {Name, [{<<"AttributeValueList">>, [[dynamize_value(A)] || A <- AttrValueList]},
            dynamize_comparison(in)]};
dynamize_scan_filter_item({Name, {AttrValue1, AttrValue2}, between}) ->
    {Name, [{<<"AttributeValueList">>, [[dynamize_value(AttrValue1)], [dynamize_value(AttrValue2)]]},
            dynamize_comparison(between)]};
dynamize_scan_filter_item({Name, AttrValue, Op}) ->
    {Name, [{<<"AttributeValueList">>, [[dynamize_value(AttrValue)]]},
            dynamize_comparison(Op)]}.

-spec dynamize_scan_filter(scan_filter()) -> [json_pair()].
dynamize_scan_filter(Filter) ->
    dynamize_maybe_list(fun dynamize_scan_filter_item/1, Filter).

-type scan_opt() :: {attributes_to_get, [binary()]} | 
                    {limit, pos_integer()} |
                    boolean_opt(count) |
                    {scan_filter, scan_filter()} |
                    {exclusive_start_key, key()} |
                    out_opt().
-type scan_opts() :: [scan_opt()].

-spec scan_opts() -> opt_table().
scan_opts() ->
    [{attributes_to_get, <<"AttributesToGet">>, fun id/1},
     {limit, <<"Limit">>, fun id/1},
     {count, <<"Count">>, fun id/1},
     {scan_filter, <<"ScanFilter">>, fun dynamize_scan_filter/1},
     {exclusive_start_key, <<"ExclusiveStartKey">>, fun(V) -> erlcloud_ddb1:key_value(dynamize_key(V)) end}].

-spec scan_record() -> record_desc().
scan_record() ->
    {#ddb_scan{},
     [{<<"Items">>, #ddb_scan.items, fun(V) -> [undynamize_item(I) || I <- V] end},
      {<<"Count">>, #ddb_scan.count, fun id/1},
      {<<"ScannedCount">>, #ddb_scan.scanned_count, fun id/1},
      {<<"LastEvaluatedKey">>, #ddb_scan.last_evaluated_key, fun undynamize_typed_key/1},
      {<<"ConsumedCapacityUnits">>, #ddb_scan.consumed_capacity_units, fun id/1}
     ]}.

-type scan_return() :: ddb_return(#ddb_scan{}, [out_item()]).

-spec scan(table_name()) -> scan_return().
scan(Table) ->
    scan(Table, [], default_config()).

-spec scan(table_name(), scan_opts()) -> scan_return().
scan(Table, Opts) ->
    scan(Table, Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc 
%% DynamoDB API:
%% [http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_Scan.html]
%%
%% ===Example===
%%
%% Return all items from table "comp5" with "time" greater than 400.
%%
%% `
%% {ok, Items} = erlcloud_ddb:scan(<<"comp5">>, [{scan_filter, [{<<"time">>, 400, gt}]}])
%% '
%% @end
%%------------------------------------------------------------------------------
-spec scan(table_name(), scan_opts(), aws_config()) -> scan_return().
scan(Table, Opts, Config) ->
    {AwsOpts, DdbOpts} = opts(scan_opts(), Opts),
    Return = erlcloud_ddb1:scan(Table, AwsOpts, Config),
    out(Return, fun(Json) -> undynamize_record(scan_record(), Json) end, DdbOpts, 
        #ddb_scan.items, {ok, []}).

%%%------------------------------------------------------------------------------
%%% UpdateItem
%%%------------------------------------------------------------------------------

-type update_action() :: put | add | delete.
-type in_update() :: {attr_name(), in_attr_value(), update_action()} | in_attr() | {attr_name(), delete}.
-type in_updates() :: maybe_list(in_update()).
-type json_update_action() :: {binary(), binary()}.
-type json_update() :: {attr_name(), [{binary(), json_attr_value()} | json_update_action()]}.
-spec dynamize_action(update_action()) -> json_update_action().
dynamize_action(put) ->
    {<<"Action">>, <<"PUT">>};
dynamize_action(add) ->
    {<<"Action">>, <<"ADD">>};
dynamize_action(delete) ->
    {<<"Action">>, <<"DELETE">>}.

-spec dynamize_update(in_update()) -> json_update().
dynamize_update({Name, Value, Action}) ->
    {Name, [{<<"Value">>, [dynamize_value(Value)]}, dynamize_action(Action)]};
dynamize_update({Name, delete}) ->
    {Name, [dynamize_action(delete)]};
dynamize_update({Name, Value}) ->
    %% Uses the default action of put
    {Name, [{<<"Value">>, [dynamize_value(Value)]}]}.

-spec dynamize_updates(in_updates()) -> [json_update()].
dynamize_updates(Updates) ->
    dynamize_maybe_list(fun dynamize_update/1, Updates).

-type update_item_opt() :: {expected, in_expected()} | 
                           {return_values, return_value()} |
                           out_opt().
-type update_item_opts() :: [update_item_opt()].

-spec update_item_opts() -> opt_table().
update_item_opts() ->
    [{expected, <<"Expected">>, fun dynamize_expected/1},
     {return_values, <<"ReturnValues">>, fun dynamize_return_value/1}].

-spec update_item_record() -> record_desc().
update_item_record() ->
    {#ddb_update_item{},
     [{<<"Attributes">>, #ddb_update_item.attributes, fun undynamize_item/1},
      {<<"ConsumedCapacityUnits">>, #ddb_update_item.consumed_capacity_units, fun id/1}
     ]}.

-type update_item_return() :: ddb_return(#ddb_update_item{}, out_item()).

-spec update_item(table_name(), key(), in_updates()) -> update_item_return().
update_item(Table, Key, Updates) ->
    update_item(Table, Key, Updates, [], default_config()).

-spec update_item(table_name(), key(), in_updates(), update_item_opts()) -> update_item_return().
update_item(Table, Key, Updates, Opts) ->
    update_item(Table, Key, Updates, Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc 
%% DynamoDB API:
%% [http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_UpdateItem.html]
%%
%% ===Example===
%%
%% Update item with hash key "Julie" and range key 1307654350 in table
%% "comp5" by changing the status from "offline" to "online" and
%% return the new item.
%%
%% `
%% {ok, NewItem} = erlcloud_ddb:update_item(<<"comp5">>, {"Julie", 1307654350},
%%                                           [{<<"status">>, <<"online">>, put}],
%%                                           [{return_values, all_new},
%%                                            {expected, {<<"status">>, "offline"}}])
%% '
%% @end
%%------------------------------------------------------------------------------
-spec update_item(table_name(), key(), in_updates(), update_item_opts(), aws_config()) -> update_item_return().
update_item(Table, Key, Updates, Opts, Config) ->
    {AwsOpts, DdbOpts} = opts(update_item_opts(), Opts),
    Return = erlcloud_ddb1:update_item(Table, dynamize_key(Key), dynamize_updates(Updates),
                                       AwsOpts, Config),
    out(Return, fun(Json) -> undynamize_record(update_item_record(), Json) end, DdbOpts, 
        #ddb_update_item.attributes, {ok, []}).

%%%------------------------------------------------------------------------------
%%% UpdateTable
%%%------------------------------------------------------------------------------

-spec update_table_record() -> record_desc().
update_table_record() ->
    {#ddb_update_table{},
     [{<<"TableDescription">>, #ddb_create_table.table_description, 
       fun(V) -> undynamize_record(table_description_record(), V) end}
     ]}. 

-type update_table_return() :: ddb_return(#ddb_update_table{}, #ddb_table_description{}).

-spec update_table(table_name(), non_neg_integer(), non_neg_integer()) -> update_table_return().
update_table(Table, ReadUnits, WriteUnits) ->
    update_table(Table, ReadUnits, WriteUnits, [], default_config()).

-spec update_table(table_name(), non_neg_integer(), non_neg_integer(), ddb_opts()) -> update_table_return().
update_table(Table, ReadUnits, WriteUnits, Opts) ->
    update_table(Table, ReadUnits, WriteUnits, Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc 
%% DynamoDB API:
%% [http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_UpdateTable.html]
%%
%% ===Example===
%%
%% Update table "comp1" to have provisioned capacity of 5 read units and 15 write units.
%%
%% `
%% {ok, _} = erlcloud_ddb:update_table(<<"comp1">>, 5, 15)
%% '
%% @end
%%------------------------------------------------------------------------------
-spec update_table(table_name(), non_neg_integer(), non_neg_integer(), ddb_opts(), aws_config()) 
                  -> update_table_return().
update_table(Table, ReadUnits, WriteUnits, Opts, Config) ->
    {[], DdbOpts} = opts([], Opts),
    Return = erlcloud_ddb1:update_table(Table, ReadUnits, WriteUnits, Config),
    out(Return, fun(Json) -> undynamize_record(update_table_record(), Json) end, DdbOpts, 
        #ddb_update_table.table_description).
