%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-

%% @author Smiler Lee <smilerlee@live.com>
%% @author Nicholas Lundgaard <nalundgaard@gmail.com>
%% @doc
%% An Erlang interface to Amazon's DynamoDB Streams.
%%
%% [http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Streams.html]
%%
%% Method names match DynamoDB Streams operations converted to
%% lower_case_with_underscores.
%%
%% Required parameters are passed as function arguments. In addition
%% all methods take an options proplist argument which can be used to
%% pass optional parameters.
%%
%% Output is in the form of `{ok, Value}' or `{error, Reason}'. The
%% format of `Value' is controlled by the `out' option, which defaults
%% to `simple'. The possible values are:
%%
%% * `simple' - The most interesting part of the output. For example
%% `list_streams' will return the streams, `describe_stream' will return
%% the stream description.
%%
%% * `record' - A record containing all the information from the
%% DynamoDB Streams response except field types. This is useful if you
%% need more detailed information than what is returned with `simple'.
%% For example, with `list_streams' the record will contain the last
%% evaluated stream ARN which can be used to continue the operation.
%%
%% * `typed_record' - A record containing all the information from the
%% DynamoDB Streams response. All field values are returned with type
%% information. This option only makes sense to `get_records'.
%%
%% * `json' - The output from DynamoDB Streams as processed by
%% `jsx:decode' but with no further manipulation. This would rarely be
%% useful, unless the DynamoDB Streams API is updated to include data
%% that is not yet parsed correctly.
%%
%% DynamoDB Streams errors are return in the form `{error, {ErrorCode,
%% Message}}' where `ErrorCode' and 'Message' are both binary
%% strings. So
%% to handle limit exceeded exception, match `{error,
%% {<<"LimitExceededException">>, _}}'.
%%
%% As to the error retries, we use `erlcloud_retry:default_retry/1'
%% as the default retry strategy, this behaviour can be changed through
%% `aws_config()'.
%%
%% @end

-module(erlcloud_ddb_streams).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").
-include("erlcloud_ddb_streams.hrl").

%%% Library initialization.
-export([configure/2, configure/3, new/2, new/3]).

%%% DynamoDB Streams API
-export([describe_stream/1, describe_stream/2, describe_stream/3,
         get_records/1, get_records/2, get_records/3,
         get_shard_iterator/3, get_shard_iterator/4, get_shard_iterator/5,
         list_streams/0, list_streams/1, list_streams/2
        ]).

-export_type(
   [aws_region/0,
    event_id/0,
    event_name/0,
    event_source/0,
    event_version/0,
    item/0,
    key/0,
    key_schema/0,
    sequence_number/0,
    sequence_number_range/0,
    shard_id/0,
    shard_iterator/0,
    shard_iterator_type/0,
    stream_arn/0,
    stream_label/0,
    stream_status/0,
    stream_view_type/0,
    table_name/0
   ]).

%%%------------------------------------------------------------------------------
%%% Library initialization.
%%%------------------------------------------------------------------------------

-spec new(string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                retry = fun erlcloud_retry:default_retry/1}.

-spec new(string(), string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                ddb_streams_host=Host,
                retry = fun erlcloud_retry:default_retry/1}.

-spec configure(string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey) ->
    erlcloud_config:configure(AccessKeyID, SecretAccessKey, fun new/2).

-spec configure(string(), string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host) ->
    erlcloud_config:configure(AccessKeyID, SecretAccessKey, Host, fun new/3).

default_config() -> erlcloud_aws:default_config().

%%%------------------------------------------------------------------------------
%%% Shared Types
%%%------------------------------------------------------------------------------

-type table_name() :: binary().

-type attr_name() :: binary().
-type hash_key_name() :: attr_name().
-type range_key_name() :: attr_name().
-type key_schema() :: hash_key_name() | {hash_key_name(), range_key_name()}.

-type stream_arn() :: binary().
-type stream_label() :: binary().

-type stream_status() :: enabling | enabled | disabling | disabled.
-type stream_view_type() :: keys_only | new_image | old_image | new_and_old_images.

-type shard_id() :: binary().

-type shard_iterator() :: binary().
-type shard_iterator_type() :: trim_horizon | latest | at_sequence_number | after_sequence_number.

-type sequence_number() :: binary().
-type sequence_number_range() :: {sequence_number(), sequence_number()} |
                                 {sequence_number(), undefined}.

-type aws_region() :: binary().
-type event_id() :: binary().
-type event_name() :: insert | modify | remove.
-type event_source() :: binary().
-type event_version() :: binary().

-type untyped_attr_value() :: binary() | number() | boolean() | undefined |
                              [binary()] | [number()] | [untyped_attr_value()] | [untyped_attr()].
-type typed_attr_value() :: {s, binary()} |
                            {n, number()} |
                            {b, binary()} |
                            {bool, boolean()} |
                            {null, true} |
                            {ss, [binary(),...]} |
                            {ns, [number(),...]} |
                            {bs, [binary(),...]} |
                            {l, [typed_attr_value()]} |
                            {m, [typed_attr()]}.
-type untyped_attr() :: {attr_name(), untyped_attr_value()}.
-type typed_attr() :: {attr_name(), typed_attr_value()}.
-type attr() :: untyped_attr() | typed_attr().
-type item() :: [attr()].
-type key() :: [attr(),...].

-type json_pair() :: {binary(), json_term()}.
-type json_term() :: jsx:json_term().

-type ok_return(T) :: {ok, T} | {error, term()}.

%%%------------------------------------------------------------------------------
%%% Shared Dynamizers
%%%------------------------------------------------------------------------------

-spec dynamize_shard_iterator_type(shard_iterator_type()) -> binary().
dynamize_shard_iterator_type(trim_horizon) -> <<"TRIM_HORIZON">>;
dynamize_shard_iterator_type(latest) -> <<"LATEST">>;
dynamize_shard_iterator_type(at_sequence_number) -> <<"AT_SEQUENCE_NUMBER">>;
dynamize_shard_iterator_type(after_sequence_number) -> <<"AFTER_SEQUENCE_NUMBER">>.

%%%------------------------------------------------------------------------------
%%% Shared Undynamizers
%%%------------------------------------------------------------------------------

-type undynamize_opt() :: {typed, boolean()}.
-type undynamize_opts() :: [undynamize_opt()].

-spec id(X, undynamize_opts()) -> X.
id(X, _) -> X.

-spec undynamize_number(binary(), undynamize_opts()) -> number().
undynamize_number(Value, _) ->
    String = binary_to_list(Value),
    case lists:member($., String) of
        true ->
            list_to_float(String);
        false ->
            list_to_integer(String)
    end.
            
-spec undynamize_value_untyped(json_pair(), undynamize_opts()) -> untyped_attr_value().
undynamize_value_untyped({<<"S">>, Value}, _) when is_binary(Value) ->
    Value;
undynamize_value_untyped({<<"N">>, Value}, Opts) ->
    undynamize_number(Value, Opts);
undynamize_value_untyped({<<"B">>, Value}, _) ->
    base64:decode(Value);
undynamize_value_untyped({<<"BOOL">>, Value}, _) when is_boolean(Value) ->
    Value;
undynamize_value_untyped({<<"NULL">>, true}, _) ->
    undefined;
undynamize_value_untyped({<<"SS">>, Values}, _) when is_list(Values) ->
    Values;
undynamize_value_untyped({<<"NS">>, Values}, Opts) ->
    [undynamize_number(Value, Opts) || Value <- Values];
undynamize_value_untyped({<<"BS">>, Values}, _) ->
    [base64:decode(Value) || Value <- Values];
undynamize_value_untyped({<<"L">>, List}, Opts) ->
    [undynamize_value_untyped(Value, Opts) || [Value] <- List];
undynamize_value_untyped({<<"M">>, Map}, Opts) ->
    [undynamize_attr_untyped(Attr, Opts) || Attr <- Map].

-spec undynamize_attr_untyped(json_pair(), undynamize_opts()) -> untyped_attr().
undynamize_attr_untyped({Name, [ValueJson]}, Opts) ->
    {Name, undynamize_value_untyped(ValueJson, Opts)}.

-spec undynamize_object(fun((json_pair(), undynamize_opts()) -> A), 
                        [json_pair()] | [{}], undynamize_opts()) -> [A].
undynamize_object(_, [{}], _) ->
    %% jsx returns [{}] for empty objects
    [];
undynamize_object(PairFun, List, Opts) ->
    [PairFun(I, Opts) || I <- List].

-spec undynamize_item(json_term(), undynamize_opts()) -> item().
undynamize_item(Json, Opts) ->
    case lists:keyfind(typed, 1, Opts) of
        {typed, true} ->
            undynamize_object(fun undynamize_attr_typed/2, Json, Opts);
        _ ->
            undynamize_object(fun undynamize_attr_untyped/2, Json, Opts)
    end.

-spec undynamize_key(json_term(), undynamize_opts()) -> key().
undynamize_key(Json, Opts) ->
    case lists:keyfind(typed, 1, Opts) of
        {typed, true} ->
            [undynamize_attr_typed(I, Opts) || I <- Json];
        _ ->
            [undynamize_attr_untyped(I, Opts) || I <- Json]
    end.

-spec undynamize_value_typed(json_pair(), undynamize_opts()) -> typed_attr_value().
undynamize_value_typed({<<"S">>, Value}, _) when is_binary(Value) ->
    {s, Value};
undynamize_value_typed({<<"N">>, Value}, Opts) ->
    {n, undynamize_number(Value, Opts)};
undynamize_value_typed({<<"B">>, Value}, _) ->
    {b, base64:decode(Value)};
undynamize_value_typed({<<"BOOL">>, Value}, _) when is_boolean(Value) ->
    {bool, Value};
undynamize_value_typed({<<"NULL">>, true}, _) ->
    {null, true};
undynamize_value_typed({<<"SS">>, Values}, _) when is_list(Values) ->
    {ss, Values};
undynamize_value_typed({<<"NS">>, Values}, Opts) ->
    {ns, [undynamize_number(Value, Opts) || Value <- Values]};
undynamize_value_typed({<<"BS">>, Values}, _) ->
    {bs, [base64:decode(Value) || Value <- Values]};
undynamize_value_typed({<<"L">>, List}, Opts) ->
    {l, [undynamize_value_typed(Value, Opts) || [Value] <- List]};
undynamize_value_typed({<<"M">>, Map}, Opts) ->
    {m, [undynamize_attr_typed(Attr, Opts) || Attr <- Map]}.

-spec undynamize_attr_typed(json_pair(), undynamize_opts()) -> typed_attr().
undynamize_attr_typed({Name, [ValueJson]}, Opts) ->
    {Name, undynamize_value_typed(ValueJson, Opts)}.

key_name(Key) ->
    proplists:get_value(<<"AttributeName">>, Key).

-spec undynamize_key_schema(json_term(), undynamize_opts()) -> key_schema().
undynamize_key_schema([HashKey], _) ->
    key_name(HashKey);
undynamize_key_schema([Key1, Key2], _) ->
    case proplists:get_value(<<"KeyType">>, Key1) of
        <<"HASH">> ->
            {key_name(Key1), key_name(Key2)};
        <<"RANGE">> ->
            {key_name(Key2), key_name(Key1)}
    end.

-spec undynamize_stream_status(binary(), undynamize_opts()) -> stream_status().
undynamize_stream_status(<<"ENABLING">>, _) -> enabling;
undynamize_stream_status(<<"ENABLED">>, _) -> enabled;
undynamize_stream_status(<<"DISABLING">>, _) -> disabling;
undynamize_stream_status(<<"DISABLED">>, _) -> disabled.

-spec undynamize_stream_view_type(binary(), undynamize_opts()) -> stream_view_type().
undynamize_stream_view_type(<<"KEYS_ONLY">>, _) -> keys_only;
undynamize_stream_view_type(<<"NEW_IMAGE">>, _) -> new_image;
undynamize_stream_view_type(<<"OLD_IMAGE">>, _) -> old_image;
undynamize_stream_view_type(<<"NEW_AND_OLD_IMAGES">>, _) -> new_and_old_images.

-spec undynamize_sequence_number_range(json_term(), undynamize_opts()) -> sequence_number_range().
undynamize_sequence_number_range(Json, _) ->
    Starting = proplists:get_value(<<"StartingSequenceNumber">>, Json),
    Ending = proplists:get_value(<<"EndingSequenceNumber">>, Json),
    {Starting, Ending}.

-spec undynamize_event_name(binary(), undynamize_opts()) -> event_name().
undynamize_event_name(<<"INSERT">>, _) -> insert;
undynamize_event_name(<<"MODIFY">>, _) -> modify;
undynamize_event_name(<<"REMOVE">>, _) -> remove.

-type field_table() :: [{binary(), pos_integer(),
                         fun((json_term(), undynamize_opts()) -> term())}].

-spec undynamize_folder(field_table(), json_pair(), undynamize_opts(), tuple()) -> tuple().
undynamize_folder(Table, {Key, Value}, Opts, A) ->
    case lists:keyfind(Key, 1, Table) of
        {Key, Index, ValueFun} ->
            setelement(Index, A, ValueFun(Value, Opts));
        false ->
            A
    end.

-type record_desc() :: {tuple(), field_table()}.

-spec undynamize_record(record_desc(), json_term(), undynamize_opts()) -> tuple().
undynamize_record({Record, _}, [{}], _) ->
    %% jsx returns [{}] for empty objects
    Record;
undynamize_record({Record, Table}, Json, Opts) ->
    lists:foldl(fun(Pair, A) -> undynamize_folder(Table, Pair, Opts, A) end, Record, Json).

%%%------------------------------------------------------------------------------
%%% Shared Options
%%%------------------------------------------------------------------------------

-spec id(X) -> X.
id(X) -> X.

-type out_type() :: json | record | typed_record | simple.
-type out_opt() :: {out, out_type()}.
-type property() :: proplists:property().

-type aws_opts() :: [json_pair()].
-type ddb_streams_opts() :: [out_opt()].
-type opts() :: {aws_opts(), ddb_streams_opts()}.

-spec verify_ddb_streams_opt(atom(), term()) -> ok.
verify_ddb_streams_opt(out, Value) ->
    case lists:member(Value, [json, record, typed_record, simple]) of
        true ->
            ok;
        false ->
            error({erlcloud_ddb, {invalid_opt, {out, Value}}})
    end;
verify_ddb_streams_opt(Name, Value) ->
    error({erlcloud_ddb, {invalid_opt, {Name, Value}}}).

-type opt_table_entry() :: {atom(), binary(), fun((_) -> json_term())}.
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
            verify_ddb_streams_opt(Name, Value),
            {AwsOpts, [{Name, Value} | DdbOpts]}
    end.

-spec opts(opt_table(), proplist()) -> opts().
opts(Table, Opts) when is_list(Opts) ->
    %% remove duplicate options
    Opts1 = lists:ukeysort(1, proplists:unfold(Opts)),
    lists:foldl(fun(Opt, A) -> opt_folder(Table, Opt, A) end, {[], []}, Opts1);
opts(_, _) ->
    error({erlcloud_ddb, opts_not_list}).

%%%------------------------------------------------------------------------------
%%% Output
%%%------------------------------------------------------------------------------

-type ddb_streams_return(Record, Simple) :: {ok, json_term() | Record | Simple} | {error, term()}.
-type undynamize_fun() :: fun((json_term(), undynamize_opts()) -> tuple()).

-spec out(json_return(), undynamize_fun(), ddb_streams_opts())
         -> {ok, json_term() | tuple()} |
            {simple, term()} |
            {error, term()}.
out({error, Reason}, _, _) ->
    {error, Reason};
out({ok, Json}, Undynamize, Opts) ->
    case proplists:get_value(out, Opts, simple) of
        json ->
            {ok, Json};
        record ->
            {ok, Undynamize(Json, [])};
        typed_record ->
            {ok, Undynamize(Json, [{typed, true}])};
        simple ->
            {simple, Undynamize(Json, [])}
    end.

%% Returns specified field of tuple for simple return
-spec out(json_return(), undynamize_fun(), ddb_streams_opts(), pos_integer())
         -> ok_return(term()).
out(Result, Undynamize, Opts, Index) ->
    out(Result, Undynamize, Opts, Index, {error, no_return}).

-spec out(json_return(), undynamize_fun(), ddb_streams_opts(), pos_integer(), ok_return(term()))
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

-spec stream_record() -> record_desc().
stream_record() ->
    {#ddb_streams_stream{},
     [{<<"StreamArn">>, #ddb_streams_stream.stream_arn, fun id/2},
      {<<"StreamLabel">>, #ddb_streams_stream.stream_label, fun id/2},
      {<<"TableName">>, #ddb_streams_stream.table_name, fun id/2}
     ]}.

-spec shard_record() -> record_desc().
shard_record() ->
    {#ddb_streams_shard{},
     [{<<"ParentShardId">>, #ddb_streams_shard.parent_shard_id, fun id/2},
      {<<"SequenceNumberRange">>, #ddb_streams_shard.sequence_number_range, fun undynamize_sequence_number_range/2},
      {<<"ShardId">>, #ddb_streams_shard.shard_id, fun id/2}
     ]}.

-spec stream_description_record() -> record_desc().
stream_description_record() ->
    {#ddb_streams_stream_description{},
     [{<<"CreationRequestDateTime">>, #ddb_streams_stream_description.creation_request_date_time, fun id/2},
      {<<"KeySchema">>, #ddb_streams_stream_description.key_schema, fun undynamize_key_schema/2},
      {<<"LastEvaluatedShardId">>, #ddb_streams_stream_description.last_evaluated_shard_id, fun id/2},
      {<<"Shards">>, #ddb_streams_stream_description.shards,
       fun(V, Opts) -> [undynamize_record(shard_record(), I, Opts) || I <- V] end},
      {<<"StreamArn">>, #ddb_streams_stream_description.stream_arn, fun id/2},
      {<<"StreamLabel">>, #ddb_streams_stream_description.stream_label, fun id/2},
      {<<"StreamStatus">>, #ddb_streams_stream_description.stream_status, fun undynamize_stream_status/2},
      {<<"StreamViewType">>, #ddb_streams_stream_description.stream_view_type, fun undynamize_stream_view_type/2},
      {<<"TableName">>, #ddb_streams_stream_description.table_name, fun id/2}
     ]}.

-spec stream_record_record() -> record_desc().
stream_record_record() ->
    {#ddb_streams_stream_record{},
     [{<<"Keys">>, #ddb_streams_stream_record.keys, fun undynamize_key/2},
      {<<"NewImage">>, #ddb_streams_stream_record.new_image, fun undynamize_item/2},
      {<<"OldImage">>, #ddb_streams_stream_record.old_image, fun undynamize_item/2},
      {<<"SequenceNumber">>, #ddb_streams_stream_record.sequence_number, fun id/2},
      {<<"SizeBytes">>, #ddb_streams_stream_record.size_bytes, fun id/2},
      {<<"StreamViewType">>, #ddb_streams_stream_record.stream_view_type,
       fun undynamize_stream_view_type/2}]}.

-spec record_record() -> record_desc().
record_record() ->
    {#ddb_streams_record{},
     [{<<"awsRegion">>, #ddb_streams_record.aws_region, fun id/2},
      {<<"dynamodb">>, #ddb_streams_record.dynamodb,
       fun(V, Opts) -> undynamize_record(stream_record_record(), V, Opts) end},
      {<<"eventID">>, #ddb_streams_record.event_id, fun id/2},
      {<<"eventName">>, #ddb_streams_record.event_name, fun undynamize_event_name/2},
      {<<"eventSource">>, #ddb_streams_record.event_source, fun id/2},
      {<<"eventVersion">>, #ddb_streams_record.event_version, fun id/2}]}.

%%%------------------------------------------------------------------------------
%%% DescribeStream
%%%------------------------------------------------------------------------------

-type describe_stream_opt() :: {exclusive_start_shard_id, shard_id()} |
                               {limit, 1..100} |
                               out_opt().
-type describe_stream_opts() :: [describe_stream_opt()].

-spec describe_stream_opts() -> opt_table().
describe_stream_opts() ->
    [{exclusive_start_shard_id, <<"ExclusiveStartShardId">>, fun id/1},
     {limit, <<"Limit">>, fun id/1}].

-spec describe_stream_record() -> record_desc().    
describe_stream_record() ->
    {#ddb_streams_describe_stream{},
     [{<<"StreamDescription">>, #ddb_streams_describe_stream.stream_description,
       fun(V, Opts) -> undynamize_record(stream_description_record(), V, Opts) end}
     ]}.

-type describe_stream_return() :: ddb_streams_return(#ddb_streams_describe_stream{}, #ddb_streams_stream_description{}).

-spec describe_stream(stream_arn()) -> describe_stream_return().
describe_stream(StreamArn) ->
    describe_stream(StreamArn, [], default_config()).

-spec describe_stream(stream_arn(), describe_stream_opts()) -> describe_stream_return().
describe_stream(StreamArn, Opts) ->
    describe_stream(StreamArn, Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc 
%% DynamoDB Streams API:
%% [http://docs.aws.amazon.com/dynamodbstreams/latest/APIReference/API_DescribeStream.html]
%%
%% ===Example===
%%
%% Describe a stream.
%%
%% `
%% erlcloud_ddb_streams:describe_stream(<<"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252">>).
%%   {ok, #ddb_streams_stream_description{
%%          creation_request_date_time = 1437677671.062,
%%          key_schema = {<<"ForumName">>, <<"Subject">>},
%%          last_evaluated_shard_id = undefined,
%%          shards = [#ddb_streams_shard{
%%                      parent_shard_id = <<"shardId-00000001414562045508-2bac9cd1">>,
%%                      sequence_number_range = {<<"20500000000000000910398">>,
%%                                               <<"20500000000000000910398">>},
%%                      shard_id = <<"shardId-00000001414562045508-2bac9cd2">>}],
%%          stream_arn = <<"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252">>,
%%          stream_label = <<"2015-05-20T20:51:10.252">>,
%%          stream_status = disabled,
%%          stream_view_type = new_and_old_images,
%%          table_name = <<"Forum">>}}
%% '
%% @end
%%------------------------------------------------------------------------------
-spec describe_stream(stream_arn(), describe_stream_opts(), aws_config()) -> describe_stream_return().
describe_stream(StreamArn, Opts, Config) ->
    {AwsOpts, DdbStreamsOpts} = opts(describe_stream_opts(), Opts),
    Return = request(
               Config,
               "DynamoDBStreams_20120810.DescribeStream",
               [{<<"StreamArn">>, StreamArn}]
               ++ AwsOpts),
    out(Return,
        fun(Json, UOpts) -> undynamize_record(describe_stream_record(), Json, UOpts) end,
        DdbStreamsOpts, #ddb_streams_describe_stream.stream_description).

%%%------------------------------------------------------------------------------
%%% GetRecords
%%%------------------------------------------------------------------------------

-type get_records_opt() :: {limit, 1..1000} |
                           out_opt().
-type get_records_opts() :: [get_records_opt()].

-spec get_records_opts() -> opt_table().
get_records_opts() ->
    [{limit, <<"Limit">>, fun id/1}].

-spec get_records_record() -> record_desc().
get_records_record() ->
    {#ddb_streams_get_records{},
     [{<<"NextShardIterator">>, #ddb_streams_get_records.next_shard_iterator, fun id/2},
      {<<"Records">>, #ddb_streams_get_records.records,
       fun(V, Opts) -> [undynamize_record(record_record(), I, Opts) || I <- V] end}]}.

-type get_records_return() :: ddb_streams_return(#ddb_streams_get_records{}, [#ddb_streams_record{}]).

-spec get_records(shard_iterator()) -> get_records_return().
get_records(ShardIterator) ->
    get_records(ShardIterator, [], default_config()).

-spec get_records(shard_iterator(), get_records_opts()) -> get_records_return().
get_records(ShardIterator, Opts) ->
    get_records(ShardIterator, Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc
%% DynamoDB Streams API:
%% [http://docs.aws.amazon.com/dynamodbstreams/latest/APIReference/API_GetRecords.html]
%%
%% ===Example===
%%
%% Get the records from a given shard iterator.
%%
%% `
%% erlcloud_ddb_streams:get_records(<<"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252|1|AAAAAAAAAAEvJp6D+zaQ...  <remaining characters omitted> ...">>).
%%   {ok, [#ddb_streams_record{
%%           aws_region = <<"us-west-2">>,
%%           dynamodb = #ddb_streams_stream_record{
%%                        keys = [{<<"ForumName">>, <<"DynamoDB">>},
%%                                {<<"Subject">>, <<"DynamoDB Thread 3">>}],
%%                        new_image = undefined,
%%                        old_image = undefined,
%%                        sequence_number = <<"300000000000000499659">>,
%%                        size_bytes = 41,
%%                        stream_view_type = keys_only},
%%           event_id = <<"e2fd9c34eff2d779b297b26f5fef4206">>,
%%           event_name = insert,
%%           event_source = <<"aws:dynamodb">>,
%%           event_version = <<"1.0">>},
%%         #ddb_streams_record{
%%           aws_region = <<"us-west-2">>,
%%           dynamodb = #ddb_streams_stream_record{
%%                        keys = [{<<"ForumName">>, <<"DynamoDB">>},
%%                                {<<"Subject">>, <<"DynamoDB Thread 1">>}],
%%                        new_image = undefined,
%%                        old_image = undefined,
%%                        sequence_number = <<"400000000000000499660">>,
%%                        size_bytes = 41,
%%                        stream_view_type = keys_only},
%%           event_id = <<"4b25bd0da9a181a155114127e4837252">>,
%%           event_name = modify,
%%           event_source = <<"aws:dynamodb">>,
%%           event_version = <<"1.0">>},
%%         #ddb_streams_record{
%%           aws_region = <<"us-west-2">>,
%%           dynamodb = #ddb_streams_stream_record{
%%                        keys = [{<<"ForumName">>, <<"DynamoDB">>},
%%                                {<<"Subject">>, <<"DynamoDB Thread 2">>}],
%%                        new_image = undefined,
%%                        old_image = undefined,
%%                        sequence_number = <<"500000000000000499661">>,
%%                        size_bytes = 41,
%%                        stream_view_type = keys_only},
%%           event_id = <<"740280c73a3df7842edab3548a1b08ad">>,
%%           event_name = remove,
%%           event_source = <<"aws:dynamodb">>,
%%           event_version = <<"1.0">>}]}
%% '
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_records(shard_iterator(), get_records_opts(), aws_config()) -> get_records_return().
get_records(ShardIterator, Opts, Config) ->
    {AwsOpts, DdbStreamsOpts} = opts(get_records_opts(), Opts),
    Return = request(
               Config,
               "DynamoDBStreams_20120810.GetRecords",
               [{<<"ShardIterator">>, ShardIterator}]
               ++ AwsOpts),
    out(Return,
        fun(Json, UOpts) -> undynamize_record(get_records_record(), Json, UOpts) end,
        DdbStreamsOpts, #ddb_streams_get_records.records).

%%%------------------------------------------------------------------------------
%%% GetShardIterator
%%%------------------------------------------------------------------------------

-type get_shard_iterator_opt() :: {sequence_number, sequence_number()} |
                                  out_opt().
-type get_shard_iterator_opts() :: [get_shard_iterator_opt()].

-spec get_shard_iterator_opts() -> opt_table().
get_shard_iterator_opts() ->
    [{sequence_number, <<"SequenceNumber">>, fun id/1}].

-spec get_shard_iterator_record() -> record_desc().
get_shard_iterator_record() ->
    {#ddb_streams_get_shard_iterator{},
     [{<<"ShardIterator">>, #ddb_streams_get_shard_iterator.shard_iterator, fun id/2}
     ]}.

-type get_shard_iterator_return() :: ddb_streams_return(#ddb_streams_get_shard_iterator{}, shard_iterator()).

-spec get_shard_iterator(stream_arn(), shard_id(), shard_iterator_type()) -> get_shard_iterator_return().
get_shard_iterator(StreamArn, ShardId, ShardIteratorType) ->
    get_shard_iterator(StreamArn, ShardId, ShardIteratorType, [], default_config()).

-spec get_shard_iterator(stream_arn(), shard_id(), shard_iterator_type(), get_shard_iterator_opts())
                        -> get_shard_iterator_return().
get_shard_iterator(StreamArn, ShardId, ShardIteratorType, Opts) ->
    get_shard_iterator(StreamArn, ShardId, ShardIteratorType, Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc
%% DynamoDB API:
%% [http://docs.aws.amazon.com/dynamodbstreams/latest/APIReference/API_GetShardIterator.html]
%%
%% ===Example===
%%
%% Get a shard iterator.
%%
%% `
%% erlcloud_ddb_streams:get_shard_iterator(<<"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252">>,
%%                                         <<"shardId-00000001414576573621-f55eea83">>,
%%                                         trim_horizon).
%%   {ok, <<"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252|1|AAAAAAAAAAEvJp6D+zaQ...  <remaining characters omitted> ...">>}
%% '
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_shard_iterator(stream_arn(), shard_id(), shard_iterator_type(), get_shard_iterator_opts(),
                         aws_config())
                        -> get_shard_iterator_return().
get_shard_iterator(StreamArn, ShardId, ShardIteratorType, Opts, Config) ->
    {AwsOpts, DdbStreamsOpts} = opts(get_shard_iterator_opts(), Opts),
    Return = request(
               Config,
               "DynamoDBStreams_20120810.GetShardIterator",
               [{<<"StreamArn">>, StreamArn},
                {<<"ShardId">>, ShardId},
                {<<"ShardIteratorType">>, dynamize_shard_iterator_type(ShardIteratorType)}]
               ++ AwsOpts),
    out(Return, fun(Json, UOpts) -> undynamize_record(get_shard_iterator_record(), Json, UOpts) end,
        DdbStreamsOpts, #ddb_streams_get_shard_iterator.shard_iterator).

%%%------------------------------------------------------------------------------
%%% ListStreams
%%%------------------------------------------------------------------------------

-type list_streams_opt() :: {exclusive_start_stream_arn, stream_arn()} |
                            {limit, 1..100} |
                            {table_name, table_name()} |
                            out_opt().
-type list_streams_opts() :: [list_streams_opt()].

-spec list_streams_opts() -> opt_table().
list_streams_opts() ->
    [{exclusive_start_stream_arn, <<"ExclusiveStartStreamArn">>, fun id/1},
     {limit, <<"Limit">>, fun id/1},
     {table_name, <<"TableName">>, fun id/1}].

-spec list_streams_record() -> record_desc().
list_streams_record() ->
    {#ddb_streams_list_streams{},
     [{<<"LastEvaluatedStreamArn">>, #ddb_streams_list_streams.last_evaluated_stream_arn, fun id/2},
      {<<"Streams">>, #ddb_streams_list_streams.streams,
       fun(V, Opts) -> [undynamize_record(stream_record(), I, Opts) || I <- V] end}
     ]}.

-type list_streams_return() :: ddb_streams_return(#ddb_streams_list_streams{}, [#ddb_streams_stream{}]).

-spec list_streams() -> list_streams_return().
list_streams() ->
    list_streams([], default_config()).

-spec list_streams(list_streams_opts()) -> list_streams_return().
list_streams(Opts) ->
    list_streams(Opts, default_config()).

%%------------------------------------------------------------------------------
%% @doc
%% DynamoDB API:
%% [http://docs.aws.amazon.com/dynamodbstreams/latest/APIReference/API_ListStreams.html]
%%
%% ===Example===
%%
%% Get the list of streams with a limit of 3.
%%
%% `
%% erlcloud_ddb_streams:list_streams([{limit, 3}]).
%%   {ok, [#ddb_streams_stream{
%%           stream_arn = <<"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252">>,
%%           stream_label = <<"2015-05-20T20:51:10.252">>,
%%           table_name = <<"Forum">>},
%%         #ddb_streams_stream{
%%           stream_arn = <<"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:50:02.714">>,
%%           stream_label = <<"2015-05-20T20:50:02.714">>,
%%           table_name = <<"Forum">>},
%%         #ddb_streams_stream{
%%           stream_arn = <<"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-19T23:03:50.641">>,
%%           stream_label = <<"2015-05-19T23:03:50.641">>,
%%           table_name = <<"Forum">>}]}
%% '
%%
%% @end
%%------------------------------------------------------------------------------
-spec list_streams(list_streams_opts(), aws_config()) -> list_streams_return().
list_streams(Opts, Config) ->
    {AwsOpts, DdbStreamsOpts} = opts(list_streams_opts(), Opts),
    Return = request(
               Config,
               "DynamoDBStreams_20120810.ListStreams",
               AwsOpts),
    out(Return,
        fun(Json, UOpts) -> undynamize_record(list_streams_record(), Json, UOpts) end,
        DdbStreamsOpts, #ddb_streams_list_streams.streams).

%%%------------------------------------------------------------------------------
%%% Request
%%%------------------------------------------------------------------------------

-type operation() :: string().
-type json_return() :: {ok, json_term()} | {error, term()}.

-spec request(aws_config(), operation(), json_term()) -> json_return().
request(Config, Operation, Json) ->
    case erlcloud_aws:update_config(Config) of
        {ok, Config2} ->
            request2(Config2, Operation, Json);
        {error, _} = Error ->
            Error
    end.

-spec request2(aws_config(), operation(), json_term()) -> json_return().
request2(Config, Operation, Json) ->
    Body = case Json of
               [] -> <<"{}">>;
               _ -> jsx:encode(Json)
           end,
    Headers = headers(Config, Operation, Body),
    Request = #aws_request{service = ddb_streams,
                           uri = uri(Config),
                           method = post,
                           request_headers = Headers,
                           request_body = Body},
    request_to_return(erlcloud_retry:request(Config, Request, fun result_fun/1)).

-spec request_to_return(#aws_request{}) -> json_return().
request_to_return(#aws_request{response_type = ok,
                               response_body = Body}) ->
    %% TODO check crc
    {ok, jsx:decode(Body)};
request_to_return(#aws_request{response_type = error,
                               error_type = aws,
                               httpc_error_reason = undefined,
                               response_status = Status,
                               response_status_line = StatusLine,
                               response_body = Body}) ->
    {error, {http_error, Status, StatusLine, Body}};
request_to_return(#aws_request{response_type = error,
                               error_type = aws,
                               httpc_error_reason = Reason}) ->
    {error, Reason};
request_to_return(#aws_request{response_type = error,
                               error_type = httpc,
                               httpc_error_reason = Reason}) ->
    {error, Reason}.

-spec result_fun(#aws_request{}) -> #aws_request{}.
result_fun(#aws_request{response_type = ok} = Request) ->
    Request;
result_fun(#aws_request{response_type = error,
                        error_type = aws,
                        response_status = Status} = Request) ->
    %% == IMPLEMENTATION NOTES ==
    %% Set `httpc_error_reason` to `undefined` in case of retry
    Request2 = Request#aws_request{httpc_error_reason = undefined},
    if
        Status >= 400 andalso Status < 500 ->
            client_error(Request2);
        Status >= 500 ->
            Request2#aws_request{should_retry = true};
        Status < 400 ->
            Request2#aws_request{should_retry = false}
    end.

-spec client_error(#aws_request{}) -> #aws_request{}.
client_error(#aws_request{response_body = Body} = Request) ->
    %% == IMPLEMENTATION NOTES ==
    %% We store the error reason in `httpc_error_reason` for now,
    %% this may be changed at any time later
    case jsx:is_json(Body) of
        false ->
            Request#aws_request{should_retry = false};
        true ->
            Json = jsx:decode(Body),
            case proplists:get_value(<<"__type">>, Json) of
                undefined ->
                    Request#aws_request{should_retry = false};
                FullType ->
                    Message = proplists:get_value(<<"message">>, Json, <<>>),
                    case binary:split(FullType, <<"#">>) of
                        [_, Type] when
                              Type =:= <<"LimitExceededException">>;
                              Type =:= <<"ThrottlingException">> ->
                            Request#aws_request{httpc_error_reason = {Type, Message},
                                                should_retry = true};
                        [_, Type] ->
                            Request#aws_request{httpc_error_reason = {Type, Message},
                                                should_retry = false};
                        _ ->
                            Request#aws_request{should_retry = false}
                    end
            end
    end.

-spec headers(aws_config(), string(), binary()) -> [{string(), string()}].
headers(Config, Operation, Body) ->
    Headers = [{"host", Config#aws_config.ddb_streams_host},
               {"x-amz-target", Operation},
               {"content-type", "application/x-amz-json-1.0"}],
    Region = erlcloud_aws:aws_region_from_host(Config#aws_config.ddb_streams_host),
    erlcloud_aws:sign_v4_headers(Config, Headers, Body, Region, "dynamodb").

uri(#aws_config{ddb_streams_scheme = Scheme, ddb_streams_host = Host} = Config) ->
    lists:flatten([Scheme, Host, port_spec(Config)]).

port_spec(#aws_config{ddb_streams_port=80}) ->
    "";
port_spec(#aws_config{ddb_streams_port=Port}) ->
    [":", erlang:integer_to_list(Port)].
