%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-

%%% erlcloud_ddb is a wrapper around erlcloud_ddb1 that provides a more natural
%%% Erlang API, including auto type inference. 
%%% It is similar to the layer2 API in boto.
-module(erlcloud_ddb).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").
-include("erlcloud_ddb.hrl").

%%% Library initialization.
-export([configure/2, configure/3, new/2, new/3]).

%%% DynamoDB API
-export([batch_get_item/1, batch_get_item/2,
         batch_write_item/1, batch_write_item/2,
         create_table/4, create_table/5,
         delete_item/2, delete_item/3, delete_item/4,
         delete_table/1, delete_table/2,
         describe_table/1, describe_table/2,
         get_item/2, get_item/3, get_item/4,
         list_tables/0, list_tables/1, list_tables/2,
         put_item/2, put_item/3, put_item/4,
         %% Note that query is a Erlang reserved word, so we use q instead
         q/2, q/3, q/4,
         update_item/3, update_item/4, update_item/5
        ]).

-export_type([table_name/0, hash_range_key/0, out_item/0, key_schema/0,
              batch_get_item_request_item/0,
              batch_write_item_request_item/0
             ]).

%%% Library initialization.
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


%% DDB API Functions
-type table_name() :: binary().
-type attr_type() :: s | n | b | ss | ns | bs.
-type attr_name() :: binary().
-type update_action() :: put | add | delete.

%% TODO decimal support
-type in_attr_data_scalar() :: iolist() | binary() | integer().
-type in_attr_data_set() :: [iolist() | binary()] | [integer()].
-type in_attr_data() :: in_attr_data_scalar() | in_attr_data_set().
-type in_attr_typed_value() :: {attr_type(), in_attr_data()}.
-type in_attr_value() :: in_attr_data() | in_attr_typed_value().
-type in_attr() :: {attr_name(), in_attr_value()}.
-type in_expected() :: {attr_name(), false | in_attr_value()}.
-type in_item() :: [in_attr()].
-type in_update() :: {attr_name(), in_attr_value(), update_action()} | in_attr() | {attr_name(), delete}.

-type json_attr_type() :: binary().
-type json_attr_data() :: binary() | [binary()].
-type json_attr_value() :: {json_attr_type(), json_attr_data()}.
-type json_attr() :: {attr_name(), [json_attr_value()]}.
-type json_item() :: [json_attr()].
-type json_expected() :: {attr_name(), [json_attr_value()] | [{binary(), boolean()}]}.
-type json_update_action() :: {binary(), binary()}.
-type json_update() :: {attr_name(), [{binary(), json_attr_value()} | json_update_action()]}.
-type json_range_key_condition() :: jsx:json_term().
-type json_key() :: [json_attr(),...].

-type hash_key() :: in_attr_value().
-type range_key() :: in_attr_value().
-type hash_range_key() :: {hash_key(), range_key()}.
-type key() :: hash_key() | hash_range_key().
-type key_schema_value() :: {attr_name(), attr_type()}.
-type key_schema() :: key_schema_value() | {key_schema_value(), key_schema_value()}.

-type comparison_operator() :: eq | le | lt | ge | gt | begins_with | between.
-type range_key_condition() :: {in_attr_value(), comparison_operator()} | 
                               {in_attr_value(), in_attr_value(), between}.

-type out_attr_value() :: binary() | integer() | [binary()] | [integer()].
-type out_attr() :: {attr_name(), out_attr_value()}.
-type out_item() :: [out_attr()].
-type item_return() :: {ok, out_item()} | {error, term()}.
-type q_return() :: {ok, #ddb_q{}} | {error, term()}.


%% Dynamize does type inference.
%% Binaries are assumed to be strings.
%% You must explicitly specify the type: {b, <<1,2,3>>} to get a binary
%% All lists are assumed to be strings.
%% You must explicitly specify the type: {ss, ["one", "two"]} to get a set

-spec dynamize_type(attr_type()) -> binary().
dynamize_type(s) ->
    <<"S">>;
dynamize_type(n) ->
    <<"N">>;
dynamize_type(b) ->
    <<"B">>.

-spec dynamize_set(attr_type(), in_attr_data_set()) -> [binary()].
dynamize_set(ss, Values) ->
    [iolist_to_binary(Value) || Value <- Values];
dynamize_set(ns, Values) ->
    [list_to_binary(integer_to_list(Value)) || Value <- Values];
dynamize_set(bs, Values) ->
    [base64:encode(Value) || Value <- Values].

-spec dynamize_value(in_attr_value()) -> json_attr_value().
dynamize_value({s, Value}) when is_binary(Value) ->
    {<<"S">>, Value};
dynamize_value({s, Value}) when is_list(Value) ->
    {<<"S">>, list_to_binary(Value)};
dynamize_value({n, Value}) when is_integer(Value) ->
    {<<"N">>, list_to_binary(integer_to_list(Value))};
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
dynamize_value(Value) when is_integer(Value) ->
    dynamize_value({n, Value});
dynamize_value(Value) ->
    throw({erlcloud_ddb_error, {invalid_attr_value, Value}}).

-spec dynamize_attr(in_attr()) -> json_attr().
dynamize_attr({Name, Value}) ->
    {Name, [dynamize_value(Value)]}.

-spec dynamize_key(key()) -> erlcloud_ddb1:key().
dynamize_key({HashType, _} = HashKey) when is_atom(HashType) ->
    dynamize_value(HashKey);
dynamize_key({HashKey, RangeKey}) ->
    {dynamize_value(HashKey), dynamize_value(RangeKey)};
dynamize_key(HashKey) ->
    dynamize_value(HashKey).

-spec dynamize_key_schema_value(key_schema_value()) -> erlcloud_ddb1:key_schema_value().
dynamize_key_schema_value({Name, Type}) ->
    {Name, dynamize_type(Type)}.

-spec dynamize_key_schema(key_schema()) -> erlcloud_ddb1:key_schema().
dynamize_key_schema({{_, _} = HashKey, {_, _} = RangeKey}) ->
    {dynamize_key_schema_value(HashKey), dynamize_key_schema_value(RangeKey)};
dynamize_key_schema(HashKey) ->
    dynamize_key_schema_value(HashKey).

-spec dynamize_expected(in_expected()) -> json_expected().
dynamize_expected({Name, false}) ->
    {Name, [{<<"Exists">>, false}]};
dynamize_expected({Name, Value}) ->
    {Name, [{<<"Value">>, [dynamize_value(Value)]}]}.

-spec dynamize_item(in_item()) -> json_item().
dynamize_item(Item) when is_list(Item) ->
    [dynamize_attr(Attr) || Attr <- Item];
dynamize_item(Item) ->
    throw({erlcloud_ddb_error, {invalid_item, Item}}).

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

-spec dynamize_comparison(comparison_operator()) -> {binary(), binary()}.
dynamize_comparison(eq) ->
    {<<"ComparisonOperator">>, <<"EQ">>};
dynamize_comparison(le) ->
    {<<"ComparisonOperator">>, <<"LE">>};
dynamize_comparison(lt) ->
    {<<"ComparisonOperator">>, <<"LT">>};
dynamize_comparison(ge) ->
    {<<"ComparisonOperator">>, <<"GE">>};
dynamize_comparison(gt) ->
    {<<"ComparisonOperator">>, <<"GT">>};
dynamize_comparison(begins_with) ->
    {<<"ComparisonOperator">>, <<"BEGINS_WITH">>};
dynamize_comparison(between) ->
    {<<"ComparisonOperator">>, <<"BETWEEN">>}.

-spec dynamize_range_key_condition(range_key_condition()) -> json_range_key_condition().
dynamize_range_key_condition({Value1, Value2, between}) ->
    [{<<"AttributeValueList">>, [[dynamize_value(Value1)], [dynamize_value(Value2)]],
      dynamize_comparison(between)}];
dynamize_range_key_condition({Value, Comparison}) ->
    [{<<"AttributeValueList">>, [[dynamize_value(Value)]]}, dynamize_comparison(Comparison)].

-spec undynamize_type(json_attr_type()) -> attr_type().
undynamize_type(<<"S">>) ->
    s;
undynamize_type(<<"N">>) ->
    n;
undynamize_type(<<"B">>) ->
    b.

-spec json_value_to_value(json_attr_value()) -> out_attr_value().
json_value_to_value({<<"S">>, Value}) when is_binary(Value) ->
    Value;
json_value_to_value({<<"N">>, Value}) ->
    list_to_integer(binary_to_list(Value));
json_value_to_value({<<"B">>, Value}) ->
    base64:decode(Value);
json_value_to_value({<<"SS">>, Values}) when is_list(Values) ->
    Values;
json_value_to_value({<<"NS">>, Values}) ->
    [list_to_integer(binary_to_list(Value)) || Value <- Values];
json_value_to_value({<<"BS">>, Values}) ->
    [base64:decode(Value) || Value <- Values].

-spec json_attr_to_attr(json_attr()) -> out_attr().
json_attr_to_attr({Name, [ValueJson]}) ->
    {Name, json_value_to_value(ValueJson)}.

-spec json_term_to_item(json_item()) -> out_item().
json_term_to_item([{}]) ->
    %% jsx returns [{}] for {} (as in "Item":{}), which is the JSON returned if none of the items attributes match
    %% the attributes to get.
    [];
json_term_to_item(Json) ->
    [json_attr_to_attr(Attr) || Attr <- Json].

-spec json_attr_value_to_typed_value(json_attr_value()) -> in_attr_typed_value().
json_attr_value_to_typed_value({<<"S">>, Value}) ->
    {s, Value};
json_attr_value_to_typed_value({<<"N">>, Value}) ->
    {n, list_to_integer(binary_to_list(Value))};
json_attr_value_to_typed_value({<<"B">>, Value}) ->
    {b, base64:decode(Value)}.

-spec json_key_to_key(json_key()) -> key().
json_key_to_key([{<<"HashKeyElement">>, [HashKey]}]) ->
    {json_attr_value_to_typed_value(HashKey)};
json_key_to_key([{<<"HashKeyElement">>, [HashKey]}, {<<"RangeKeyElement">>, [RangeKey]}]) ->
    {json_attr_value_to_typed_value(HashKey), json_attr_value_to_typed_value(RangeKey)}.

-spec json_attr_to_typed_attr(json_attr()) -> in_attr().
json_attr_to_typed_attr({Name, [ValueJson]}) ->
    {Name, json_attr_value_to_typed_value(ValueJson)}.

-spec json_term_to_typed_item(json_item()) -> in_item().
json_term_to_typed_item([{}]) ->
    %% jsx bug
    [];
json_term_to_typed_item(Json) ->
    [json_attr_to_typed_attr(Attr) || Attr <- Json].

-spec undynamize_key_schema_value(jsx:json_term()) -> key_schema().
undynamize_key_schema_value([{<<"AttributeName">>, Name}, {<<"AttributeType">>, Type}]) ->
    {Name, undynamize_type(Type)}.

-spec undynamize_key_schema(jsx:json_term()) -> key_schema().
undynamize_key_schema([{<<"HashKeyElement">>, HashKey}]) ->
    undynamize_key_schema_value(HashKey);
undynamize_key_schema([{<<"HashKeyElement">>, HashKey}, {<<"RangeKeyElement">>, RangeKey}]) ->
    {undynamize_key_schema_value(HashKey), undynamize_key_schema_value(RangeKey)}.

-spec provisioned_throughput_folder({binary(), term()}, #ddb_provisioned_throughput{}) 
                                   -> #ddb_provisioned_throughput{}.
provisioned_throughput_folder({<<"ReadCapacityUnits">>, Units}, A) ->
    A#ddb_provisioned_throughput{read_capacity_units = Units};
provisioned_throughput_folder({<<"WriteCapacityUnits">>, Units}, A) ->
    A#ddb_provisioned_throughput{write_capacity_units = Units};
provisioned_throughput_folder({<<"LastDecreaseDateTime">>, Time}, A) ->
    A#ddb_provisioned_throughput{last_decrease_date_time = Time};
provisioned_throughput_folder({<<"LastIncreaseDateTime">>, Time}, A) ->
    A#ddb_provisioned_throughput{last_increase_date_time = Time};
provisioned_throughput_folder(_, A) ->
    A.

-spec undynamize_provisioned_throughput(jsx:json_term()) -> #ddb_provisioned_throughput{}.
undynamize_provisioned_throughput(Json) ->
    lists:foldl(fun provisioned_throughput_folder/2, #ddb_provisioned_throughput{}, Json).

-spec table_description_folder({binary(), term()}, #ddb_table_description{}) -> #ddb_table_description{}.
table_description_folder({<<"CreationDateTime">>, Time}, A) ->
    A#ddb_table_description{creation_date_time = Time};
table_description_folder({<<"KeySchema">>, KeySchema}, A) ->
    A#ddb_table_description{key_schema = undynamize_key_schema(KeySchema)};
table_description_folder({<<"ProvisionedThroughput">>, ProvisionedThroughput}, A) ->
    A#ddb_table_description{provisioned_throughput = undynamize_provisioned_throughput(ProvisionedThroughput)};
table_description_folder({<<"TableName">>, Name}, A) ->
    A#ddb_table_description{name = Name};
table_description_folder({<<"TableStatus">>, Status}, A) ->
    A#ddb_table_description{status = Status};
table_description_folder(_, A) ->
    A.

-spec undynamize_table_description(jsx:json_term()) -> #ddb_table_description{}.
undynamize_table_description(Json) ->
    lists:foldl(fun table_description_folder/2, #ddb_table_description{}, Json).

-type table_description_return() :: {ok, #ddb_table_description{}} | {error, term()}.
-spec table_description_return(jsx:json_term()) -> table_description_return().
table_description_return(Json) ->
    case proplists:get_value(<<"TableDescription">>, Json) of
        undefined ->
            {error, no_table_description};
        Description ->
            {ok, undynamize_table_description(Description)}
    end.

-type get_item_opt() :: {attributes_to_get, [binary()]} | 
                        {consistent_read, boolean()}.
-type get_item_opts() :: [get_item_opt()].

-spec get_item_opt(get_item_opt()) -> {binary(), jsx:json_term()}.
get_item_opt({attributes_to_get, Value}) ->
    {<<"AttributesToGet">>, Value};
get_item_opt({consistent_read, Value}) ->
    {<<"ConsistentRead">>, Value}.

-type batch_get_item_request_item() :: {table_name(), [key(),...], get_item_opts()} | {table_name(), [key(),...]}.

-spec dynamize_batch_get_item_request_item(batch_get_item_request_item()) 
                                          -> {binary(), jsx:json_term(), jsx:json_term()}.
dynamize_batch_get_item_request_item({Table, Keys}) ->
    dynamize_batch_get_item_request_item({Table, Keys, []});
dynamize_batch_get_item_request_item({Table, Keys, Opts}) ->
    {Table, [dynamize_key(K) || K <- Keys], lists:map(fun get_item_opt/1, Opts)}.

-spec batch_get_item_response_folder({binary(), term()}, #ddb_batch_get_item_response{}) ->
                                            #ddb_batch_get_item_response{}.
batch_get_item_response_folder({<<"Items">>, ItemList}, A) ->
    A#ddb_batch_get_item_response{items = [json_term_to_item(I) || I <- ItemList]};
batch_get_item_response_folder({<<"ConsumedCapacityUnits">>, Units}, A) ->
    A#ddb_batch_get_item_response{consumed_capacity_units = Units};
batch_get_item_response_folder(_, A) ->
    A.

-spec undynamize_batch_get_item_response({table_name(), jsx:json_term()}) -> #ddb_batch_get_item_response{}.
undynamize_batch_get_item_response({Table, Json}) ->
    lists:foldl(fun batch_get_item_response_folder/2, #ddb_batch_get_item_response{table = Table}, Json).

-spec batch_get_item_request_item_folder({binary(), term()}, batch_get_item_request_item()) 
                                        -> batch_get_item_request_item().
batch_get_item_request_item_folder({<<"Keys">>, Keys}, {Table, _, Opts}) ->
    {Table, [json_key_to_key(K) || K <- Keys], Opts};
batch_get_item_request_item_folder({<<"AttributesToGet">>, Value}, {Table, Keys, Opts}) ->
    {Table, Keys, [{attributes_to_get, Value} | Opts]};
batch_get_item_request_item_folder({<<"ConsistentRead">>, Value}, {Table, Keys, Opts}) ->
    {Table, Keys, [{consistent_read, Value} | Opts]}.

-spec undynamize_batch_get_item_request_item({table_name(), jsx:json_term()}) -> batch_get_item_request_item().
undynamize_batch_get_item_request_item({Table, Json}) ->
    lists:foldl(fun batch_get_item_request_item_folder/2, {Table, [], []}, Json).

-spec batch_get_item_folder({binary(), term()}, #ddb_batch_get_item{}) -> #ddb_batch_get_item{}.
batch_get_item_folder({<<"Responses">>, [{}]}, A) ->
    %% Work around jsx bug
    A#ddb_batch_get_item{responses = []};
batch_get_item_folder({<<"Responses">>, Responses}, A) ->
    A#ddb_batch_get_item{responses = [undynamize_batch_get_item_response(R) || R <- Responses]};
batch_get_item_folder({<<"UnprocessedKeys">>, [{}]}, A) ->
    %% Work around jsx bug
    A#ddb_batch_get_item{unprocessed_keys = []};
batch_get_item_folder({<<"UnprocessedKeys">>, Keys}, A) ->
    A#ddb_batch_get_item{unprocessed_keys = [undynamize_batch_get_item_request_item(K) || K <- Keys]};
batch_get_item_folder(_, A) ->
    A.

-type batch_get_item_return() :: {ok, [#ddb_batch_get_item{}]} | {error, term()}.
-spec batch_get_item([batch_get_item_request_item()]) -> batch_get_item_return().
batch_get_item(RequestItems) ->
    batch_get_item(RequestItems, default_config()).

-spec batch_get_item([batch_get_item_request_item()], aws_config()) -> batch_get_item_return().
batch_get_item(RequestItems, Config) ->
    %% TODO unprocessed item handling
    %% TODO simple return
    case erlcloud_ddb1:batch_get_item([dynamize_batch_get_item_request_item(R) || R <- RequestItems], Config) of 
        {error, Reason} ->
            {error, Reason};
        {ok, Json} ->
            {ok, lists:foldl(fun batch_get_item_folder/2, #ddb_batch_get_item{}, Json)}
    end.


-type batch_write_item_put() :: {put, in_item()}.
-type batch_write_item_delete() :: {delete, key()}.
-type batch_write_item_request() :: batch_write_item_put() | batch_write_item_delete().
-type batch_write_item_request_item() :: {table_name(), [batch_write_item_request()]}.

-spec dynamize_batch_write_item_request(batch_write_item_request()) -> erlcloud_ddb1:batch_write_item_request().
dynamize_batch_write_item_request({put, Item}) ->
    {put, dynamize_item(Item)};
dynamize_batch_write_item_request({delete, Key}) ->
    {delete, dynamize_key(Key)}.

-spec dynamize_batch_write_item_request_item(batch_write_item_request_item()) 
                                          -> {binary(), jsx:json_term()}.
dynamize_batch_write_item_request_item({Table, Requests}) ->
    {Table, [dynamize_batch_write_item_request(R) || R <- Requests]}.

-spec batch_write_item_response_folder({binary(), term()}, #ddb_batch_write_item_response{}) ->
                                            #ddb_batch_write_item_response{}.
batch_write_item_response_folder({<<"ConsumedCapacityUnits">>, Units}, A) ->
    A#ddb_batch_write_item_response{consumed_capacity_units = Units};
batch_write_item_response_folder(_, A) ->
    A.

-spec undynamize_batch_write_item_response({table_name(), jsx:json_term()}) -> #ddb_batch_write_item_response{}.
undynamize_batch_write_item_response({Table, Json}) ->
    lists:foldl(fun batch_write_item_response_folder/2, #ddb_batch_write_item_response{table = Table}, Json).

-spec batch_write_item_request_folder([{binary(), term()}], batch_write_item_request_item()) 
                                     -> batch_write_item_request_item().
batch_write_item_request_folder([{<<"PutRequest">>, [{<<"Item">>, Item}]}], {Table, Requests}) ->
    {Table, [{put, json_term_to_typed_item(Item)} | Requests]};
batch_write_item_request_folder([{<<"DeleteRequest">>, [{<<"Key">>, Key}]}], {Table, Requests}) ->
    {Table, [{delete, json_key_to_key(Key)} | Requests]}.

-spec undynamize_batch_write_item_request_item({table_name(), jsx:json_term()}) -> batch_write_item_request_item().
undynamize_batch_write_item_request_item({Table, Json}) ->
    {Table, Requests} = lists:foldl(fun batch_write_item_request_folder/2, {Table, []}, Json),
    {Table, lists:reverse(Requests)}.

-spec batch_write_item_folder({binary(), term()}, #ddb_batch_write_item{}) -> #ddb_batch_write_item{}.
batch_write_item_folder({<<"Responses">>, [{}]}, A) ->
    %% Work around jsx bug
    A#ddb_batch_write_item{responses = []};
batch_write_item_folder({<<"Responses">>, Responses}, A) ->
    A#ddb_batch_write_item{responses = [undynamize_batch_write_item_response(R) || R <- Responses]};
batch_write_item_folder({<<"UnprocessedItems">>, [{}]}, A) ->
    %% Work around jsx bug
    A#ddb_batch_write_item{unprocessed_items = []};
batch_write_item_folder({<<"UnprocessedItems">>, Items}, A) ->
    A#ddb_batch_write_item{unprocessed_items = [undynamize_batch_write_item_request_item(I) || I <- Items]};
batch_write_item_folder(_, A) ->
    A.

-spec batch_write_item([batch_write_item_request_item()]) -> batch_write_item_return().
batch_write_item(RequestItems) ->
    batch_write_item(RequestItems, default_config()).

-type batch_write_item_return() :: {ok, [#ddb_batch_write_item{}]} | {error, term()}.
-spec batch_write_item([batch_write_item_request_item()], aws_config()) -> batch_write_item_return().
batch_write_item(RequestItems, Config) ->
    case erlcloud_ddb1:batch_write_item([dynamize_batch_write_item_request_item(R) || R <- RequestItems], 
                                        Config) of 
        {error, Reason} ->
            {error, Reason};
        {ok, Json} ->
            {ok, lists:foldl(fun batch_write_item_folder/2, #ddb_batch_write_item{}, Json)}
    end.


-spec create_table(table_name(), key_schema(), non_neg_integer(), non_neg_integer()) -> table_description_return().
create_table(Table, KeySchema, ReadUnits, WriteUnits) ->
    create_table(Table, KeySchema, ReadUnits, WriteUnits, default_config()).

-spec create_table(table_name(), key_schema(), non_neg_integer(), non_neg_integer(), aws_config()) 
                  -> table_description_return().
create_table(Table, KeySchema, ReadUnits, WriteUnits, Config) ->
    case erlcloud_ddb1:create_table(Table, dynamize_key_schema(KeySchema), ReadUnits, WriteUnits, Config) of
        {error, Reason} ->
            {error, Reason};
        {ok, Json} ->
            table_description_return(Json)
    end.


-type delete_item_opt() :: {expected, in_expected()} | 
                           {return_values, none | all_old}.
-type delete_item_opts() :: [delete_item_opt()].

-spec delete_item_opt(delete_item_opt()) -> {binary(), jsx:json_term()}.
delete_item_opt({expected, Value}) ->
    {<<"Expected">>, [dynamize_expected(Value)]};
delete_item_opt({return_values, none}) ->
    {<<"ReturnValues">>, <<"NONE">>};
delete_item_opt({return_values, all_old}) ->
    {<<"ReturnValues">>, <<"ALL_OLD">>}.

-spec delete_item(table_name(), key()) -> item_return().
delete_item(Table, Key) ->
    delete_item(Table, Key, [], default_config()).

-spec delete_item(table_name(), key(), delete_item_opts()) -> item_return().
delete_item(Table, Key, Opts) ->
    delete_item(Table, Key, Opts, default_config()).

-spec delete_item(table_name(), key(), delete_item_opts(), aws_config()) -> item_return().
delete_item(Table, Key, Opts, Config) ->
    case erlcloud_ddb1:delete_item(Table, dynamize_key(Key), lists:map(fun delete_item_opt/1, Opts), Config) of
        {error, Reason} ->
            {error, Reason};
        {ok, Json} ->
            case proplists:get_value(<<"Attributes">>, Json) of
                undefined ->
                    {ok, []};
                Item ->
                    {ok, json_term_to_item(Item)}
            end
    end.


-spec delete_table(table_name()) -> table_description_return().
delete_table(Table) ->
    delete_table(Table, default_config()).

-spec delete_table(table_name(), aws_config()) -> table_description_return().
delete_table(Table, Config) ->
    case erlcloud_ddb1:delete_table(Table, Config) of
        {error, Reason} ->
            {error, Reason};
        {ok, Json} ->
            table_description_return(Json)
    end.


-spec table_folder({binary(), term()}, #ddb_table{}) -> #ddb_table{}.
table_folder({<<"CreationDateTime">>, Time}, A) ->
    A#ddb_table{creation_date_time = Time};
table_folder({<<"ItemCount">>, Count}, A) ->
    A#ddb_table{item_count = Count};
table_folder({<<"KeySchema">>, KeySchema}, A) ->
    A#ddb_table{key_schema = undynamize_key_schema(KeySchema)};
table_folder({<<"ProvisionedThroughput">>, ProvisionedThroughput}, A) ->
    A#ddb_table{provisioned_throughput = undynamize_provisioned_throughput(ProvisionedThroughput)};
table_folder({<<"TableName">>, Name}, A) ->
    A#ddb_table{name = Name};
table_folder({<<"TableSizeBytes">>, Size}, A) ->
    A#ddb_table{size_bytes = Size};
table_folder({<<"TableStatus">>, Status}, A) ->
    A#ddb_table{status = Status};
table_folder(_, A) ->
    A.

-spec undynamize_table(jsx:json_term()) -> #ddb_table{}.
undynamize_table(Json) ->
    lists:foldl(fun table_folder/2, #ddb_table{}, Json).

-type table_return() :: {ok, #ddb_table{}} | {error, term()}.
-spec table_return(jsx:json_term()) -> table_return().
table_return(Json) ->
    case proplists:get_value(<<"Table">>, Json) of
        undefined ->
            {error, no_table};
        Description ->
            {ok, undynamize_table(Description)}
    end.

-spec describe_table(table_name()) -> table_return().
describe_table(Table) ->
    describe_table(Table, default_config()).

-spec describe_table(table_name(), aws_config()) -> table_return().
describe_table(Table, Config) ->
    case erlcloud_ddb1:describe_table(Table, Config) of
        {error, Reason} ->
            {error, Reason};
        {ok, Json} ->
            table_return(Json)
    end.


-spec get_item(table_name(), key()) -> item_return().
get_item(Table, Key) ->
    get_item(Table, Key, [], default_config()).

-spec get_item(table_name(), key(), get_item_opts()) -> item_return().
get_item(Table, Key, Opts) ->
    get_item(Table, Key, Opts, default_config()).

-spec get_item(table_name(), key(), get_item_opts(), aws_config()) -> item_return().
get_item(Table, Key, Opts, Config) ->
    case erlcloud_ddb1:get_item(Table, dynamize_key(Key), lists:map(fun get_item_opt/1, Opts), Config) of
        {error, Reason} ->
            {error, Reason};
        {ok, Json} ->
            case proplists:get_value(<<"Item">>, Json) of
                undefined ->
                    {error, no_item};
                Item ->
                    {ok, json_term_to_item(Item)}
            end
    end.


-type list_tables_opt() :: {limit, pos_integer()} | 
                           {exclusive_start_table_name, binary()}.
-type list_tables_opts() :: [list_tables_opt()].

-spec list_tables_opt(list_tables_opt()) -> {binary(), jsx:json_term()}.
list_tables_opt({limit, Value}) ->
    {<<"Limit">>, Value};
list_tables_opt({exclusive_start_table_name, Name}) ->
    {<<"ExclusiveStartTableName">>, Name}.

-spec list_tables_folder({binary(), term()}, #ddb_list_tables{}) -> #ddb_list_tables{}.
list_tables_folder({<<"TableNames">>, Names}, A) ->
    A#ddb_list_tables{table_names = Names};
list_tables_folder({<<"LastEvaluatedTableName">>, Name}, A) ->
    A#ddb_list_tables{last_evaluated_table_name = Name};
list_tables_folder(_, A) ->
    A.

-type list_tables_return() :: {ok, #ddb_list_tables{}} | {error, term()}.

-spec list_tables() -> list_tables_return().
list_tables() ->
    list_tables([], default_config()).

-spec list_tables(list_tables_opts()) -> list_tables_return().
list_tables(Opts) ->
    list_tables(Opts, default_config()).

-spec list_tables(list_tables_opts(), aws_config()) -> list_tables_return().
list_tables(Opts, Config) ->
    case erlcloud_ddb1:list_tables(lists:map(fun list_tables_opt/1, Opts), Config) of
        {error, Reason} ->
            {error, Reason};
        {ok, Json} ->
            {ok, lists:foldl(fun list_tables_folder/2, #ddb_list_tables{}, Json)}
    end.


-type put_item_opt() :: {expected, in_expected()} | 
                        {return_values, none | all_old}.
-type put_item_opts() :: [put_item_opt()].

-spec put_item_opt(put_item_opt()) -> {binary(), jsx:json_term()}.
put_item_opt({expected, Value}) ->
    {<<"Expected">>, [dynamize_expected(Value)]};
put_item_opt({return_values, none}) ->
    {<<"ReturnValues">>, <<"NONE">>};
put_item_opt({return_values, all_old}) ->
    {<<"ReturnValues">>, <<"ALL_OLD">>}.

-spec put_item(table_name(), in_item()) -> item_return().
put_item(Table, Item) ->
    put_item(Table, Item, [], default_config()).

-spec put_item(table_name(), in_item(), put_item_opts()) -> item_return().
put_item(Table, Item, Opts) ->
    put_item(Table, Item, Opts, default_config()).

-spec put_item(table_name(), in_item(), put_item_opts(), aws_config()) -> item_return().
put_item(Table, Item, Opts, Config) ->
    case erlcloud_ddb1:put_item(Table, dynamize_item(Item), lists:map(fun put_item_opt/1, Opts), Config) of
        {error, Reason} ->
            {error, Reason};
        {ok, Json} ->
            case proplists:get_value(<<"Attributes">>, Json) of
                undefined ->
                    {ok, []};
                Return ->
                    {ok, json_term_to_item(Return)}
            end
    end.


-type q_opt() :: {attributes_to_get, [binary()]} | 
                 {limit, pos_integer()} |
                 {consistent_read, boolean()} |
                 {count, boolean()} |
                 {range_key_condition, range_key_condition()} |
                 {scan_index_forward, boolean()} |
                 {exclusive_start_key, key()}.
-type q_opts() :: [q_opt()].

-spec q_opt(q_opt()) -> {binary(), jsx:json_term()}.
q_opt({attributes_to_get, Value}) ->
    {<<"AttributesToGet">>, Value};
q_opt({limit, Value}) ->
    {<<"Limit">>, Value};
q_opt({consistent_read, Value}) ->
    {<<"ConsistentRead">>, Value};
q_opt({count, Value}) ->
    {<<"Count">>, Value};
q_opt({range_key_condition, Value}) ->
    {<<"RangeKeyCondition">>, dynamize_range_key_condition(Value)};
q_opt({scan_index_forward, Value}) ->
    {<<"ScanIndexForward">>, Value};
q_opt({exclusive_start_key, Value}) ->
    {<<"ExclusiveStartKey">>, erlcloud_ddb1:key_value(dynamize_key(Value))}.

-spec q_folder({binary(), term()}, #ddb_q{}) -> #ddb_q{}.
q_folder({<<"Items">>, ItemList}, A) ->
    A#ddb_q{items = [json_term_to_item(I) || I <- ItemList]};
q_folder({<<"Count">>, Count}, A) ->
    A#ddb_q{count = Count};
q_folder({<<"LastEvaluatedKey">>, Key}, A) ->
    A#ddb_q{last_evaluated_key = json_key_to_key(Key)};
q_folder({<<"ConsumedCapacityUnits">>, Units}, A) ->
    A#ddb_q{consumed_capacity_units = Units};
q_folder(_, A) ->
    A.

-spec q(table_name(), hash_key()) -> q_return().
q(Table, HashKey) ->
    q(Table, HashKey, [], default_config()).

-spec q(table_name(), hash_key(), q_opts()) -> q_return().
q(Table, HashKey, Opts) ->
    q(Table, HashKey, Opts, default_config()).

-spec q(table_name(), hash_key(), q_opts(), aws_config()) -> q_return().
q(Table, HashKey, Opts, Config) ->
    case erlcloud_ddb1:q(Table, dynamize_key(HashKey), lists:map(fun q_opt/1, Opts), Config) of
        {error, Reason} ->
            {error, Reason};
        {ok, Json} ->
            {ok, lists:foldl(fun q_folder/2, #ddb_q{}, Json)}
    end.


-type update_item_opt() :: {expected, in_expected()} | 
                           {return_values, none | all_old | updated_old | all_new | updated_new}.
-type update_item_opts() :: [update_item_opt()].

-spec update_item_opt(update_item_opt()) -> {binary(), jsx:json_term()}.
update_item_opt({expected, Value}) ->
    {<<"Expected">>, [dynamize_expected(Value)]};
update_item_opt({return_values, none}) ->
    {<<"ReturnValues">>, <<"NONE">>};
update_item_opt({return_values, all_old}) ->
    {<<"ReturnValues">>, <<"ALL_OLD">>};
update_item_opt({return_values, updated_old}) ->
    {<<"ReturnValues">>, <<"UPDATED_OLD">>};
update_item_opt({return_values, all_new}) ->
    {<<"ReturnValues">>, <<"ALL_NEW">>};
update_item_opt({return_values, updated_new}) ->
    {<<"ReturnValues">>, <<"UPDATED_NEW">>}.

-spec update_item(table_name(), key(), [in_update()]) -> item_return().
update_item(Table, Key, Updates) ->
    update_item(Table, Key, Updates, [], default_config()).

-spec update_item(table_name(), key(), [in_update()], update_item_opts()) -> item_return().
update_item(Table, Key, Updates, Opts) ->
    update_item(Table, Key, Updates, Opts, default_config()).

-spec update_item(table_name(), key(), [in_update()], update_item_opts(), aws_config()) -> item_return().
update_item(Table, Key, Updates, Opts, Config) ->
    case erlcloud_ddb1:update_item(Table, dynamize_key(Key), lists:map(fun dynamize_update/1, Updates), 
                                   lists:map(fun update_item_opt/1, Opts), Config) of
        {error, Reason} ->
            {error, Reason};
        {ok, Json} ->
            case proplists:get_value(<<"Attributes">>, Json) of
                undefined ->
                    {ok, []};
                Item ->
                    {ok, json_term_to_item(Item)}
            end
    end.


default_config() -> erlcloud_aws:default_config().
