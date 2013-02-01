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
         get_item/2, get_item/3, get_item/4,
         put_item/2, put_item/3, put_item/4,
         update_item/3, update_item/4, update_item/5
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

-spec dynamize_expected(in_expected()) -> json_expected().
dynamize_expected({Name, false}) ->
    {Name, [{<<"Exists">>, false}]};
dynamize_expected({Name, Value}) ->
    {Name, [{<<"Value">>, [dynamize_value(Value)]}]}.

-spec dynamize_item(in_item()) -> json_item().
dynamize_item(Item) ->
    [dynamize_attr(Attr) || Attr <- Item].

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


-type get_item_opt() :: {attributes_to_get, [binary()]} | 
                        {consistent_read, boolean()}.
-type get_item_opts() :: [get_item_opt()].

-spec get_item_opt(get_item_opt()) -> {binary(), jsx:json_term()}.
get_item_opt({attributes_to_get, Value}) ->
    {<<"AttributesToGet">>, Value};
get_item_opt({consistent_read, Value}) ->
    {<<"ConsistentRead">>, Value}.

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
            {ok, json_term_to_item(proplists:get_value(<<"Item">>, Json))}
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
