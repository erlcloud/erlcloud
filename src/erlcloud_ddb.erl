%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-

%%% erlcloud_ddb is a wrapper around erlcloud_ddb1 that provides a more natural
%%% Erlang API, including auto type inference. 
%%% It is similar to the layer2 API in boto.
-module(erlcloud_ddb).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

%%% Library initialization.
-export([configure/2, configure/3, new/2, new/3]).

-export([delete_item/2, delete_item/3, delete_item/4,
         get_item/2, get_item/3, get_item/4
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
%% TODO decimal support
-type in_attr_data_scalar() :: iolist() | binary() | integer().
-type in_attr_data_set() :: [iolist() | binary()] | [integer()].
-type in_attr_data() :: in_attr_data_scalar() | in_attr_data_set().
-type in_attr_typed_value() :: {attr_type(), in_attr_data()}.
-type in_attr_value() :: in_attr_data() | in_attr_typed_value().
-type in_attr() :: {attr_name(), in_attr_value()}.
-type in_expected() :: {attr_name(), false | in_attr_value()}.

-type json_attr_type() :: binary().
-type json_attr_data() :: binary() | [binary()].
-type json_attr_value() :: {json_attr_type(), json_attr_data()}.
-type json_attr() :: {attr_name(), [json_attr_value()]}.
-type json_item() :: [json_attr()].
-type json_expected() :: {attr_name(), [json_attr_value()] | [{binary(), boolean()}]}.

-type hash_key() :: in_attr_value().
-type range_key() :: in_attr_value().
-type hash_range_key() :: {hash_key(), range_key()}.
-type key() :: hash_key() | hash_range_key().

-type out_attr_value() :: binary() | integer() | [binary()] | [integer()].
-type out_attr() :: {attr_name(), out_attr_value()}.
-type out_item() :: [out_attr()].
-type item_return() :: {ok, out_item()} | {error, term()}.


%% Dynamize does type inference.
%% Binaries are assumed to be strings.
%% You must explicitly specify the type: {b, <<1,2,3>>} to get a binary
%% All lists are assumed to be strings.
%% You must explicitly specify the type: {ss, ["one", "two"]} to get a set

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
    throw({erlcould_ddb_error, {invalid_attr_value, Value}}).

%% -spec dynamize_attr(in_attr()) -> json_attr().
%% dynamize_attr({Name, Value}) ->
%%     {Name, [dynamize_value(Value)]}.

-spec dynamize_key(key()) -> erlcloud_ddb1:key().
dynamize_key({HashType, _} = HashKey) when is_atom(HashType) ->
    dynamize_value(HashKey);
dynamize_key({HashKey, RangeKey}) ->
    {dynamize_value(HashKey), dynamize_value(RangeKey)};
dynamize_key(HashKey) ->
    dynamize_value(HashKey).

-spec dynamize_expected(in_expected()) -> json_expected().
dynamize_expected({Name, false}) ->
    {Name, [{<<"Exists">>, false}]};
dynamize_expected({Name, Value}) ->
    {Name, [{<<"Value">>, [dynamize_value(Value)]}]}.


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
json_term_to_item(Json) ->
    [json_attr_to_attr(Attr) || Attr <- Json].


-type delete_item_opt() :: {expected, in_attr()} | 
                           {return_values, none | all_old}.
-type delete_item_opts() :: [delete_item_opt()].

-spec delete_item_opt(delete_item_opt()) -> {binary(), jsx:json_term()}.
delete_item_opt({expected, Value}) ->
    {<<"Expected">>, [dynamize_expected(Value)]};
delete_item_opt({return_values, none}) ->
    {<<"ReturnValues">>, <<"NONE">>};
delete_item_opt({return_values, all_old}) ->
    {<<"ReturnValues">>, <<"ALL_OLD">>}.

-spec delete_item_opts(delete_item_opts()) -> jsx:json_term().
delete_item_opts(Opts) ->
    [delete_item_opt(Opt) || Opt <- Opts].

-spec delete_item(table_name(), key()) -> item_return().
delete_item(Table, Key) ->
    delete_item(Table, Key, [], default_config()).

-spec delete_item(table_name(), key(), delete_item_opts()) -> item_return().
delete_item(Table, Key, Opts) ->
    delete_item(Table, Key, Opts, default_config()).

-spec delete_item(table_name(), key(), delete_item_opts(), aws_config()) -> item_return().
delete_item(Table, Key, Opts, Config) ->
    case erlcloud_ddb1:delete_item(Table, dynamize_key(Key), delete_item_opts(Opts), Config) of
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


-type get_item_opt() :: {attributes_to_get, [binary()]} | 
                        {consistent_read, boolean()}.
-type get_item_opts() :: [get_item_opt()].

-spec get_item_opt(get_item_opt()) -> {binary(), jsx:json_term()}.
get_item_opt({attributes_to_get, Value}) ->
    {<<"AttributesToGet">>, Value};
get_item_opt({consistent_read, Value}) ->
    {<<"ConsistentRead">>, Value}.

-spec get_item_opts(get_item_opts()) -> jsx:json_term().
get_item_opts(Opts) ->
    [get_item_opt(Opt) || Opt <- Opts].

-spec get_item(table_name(), key()) -> item_return().
get_item(Table, Key) ->
    get_item(Table, Key, [], default_config()).

-spec get_item(table_name(), key(), get_item_opts()) -> item_return().
get_item(Table, Key, Opts) ->
    get_item(Table, Key, Opts, default_config()).

-spec get_item(table_name(), key(), get_item_opts(), aws_config()) -> item_return().
get_item(Table, Key, Opts, Config) ->
    case erlcloud_ddb1:get_item(Table, dynamize_key(Key), get_item_opts(Opts), Config) of
        {error, Reason} ->
            {error, Reason};
        {ok, Json} ->
            {ok, json_term_to_item(proplists:get_value(<<"Item">>, Json))}
    end.

default_config() -> erlcloud_aws:default_config().
