%% Amazon Kinesis Service (Kinesis)

-module(erlcloud_kinesis).

%%% Library initialization.
-export([configure/2, configure/3, configure/4,  new/2, new/3]).

-export([create_stream/2, create_stream/3,
         delete_stream/1, delete_stream/2,
         list_streams/0, list_streams/1, list_streams/2, list_streams/3,
         describe_stream/1, describe_stream/2, describe_stream/3, describe_stream/4,
         describe_stream_summary/1, describe_stream_summary/2,
         enable_enhanced_monitoring/1, enable_enhanced_monitoring/2, enable_enhanced_monitoring/3,
         disable_enhanced_monitoring/1, disable_enhanced_monitoring/2, disable_enhanced_monitoring/3,
         get_shard_iterator/3, get_shard_iterator/4, get_shard_iterator/5,
         get_records/1, get_records/2, get_records/3, get_records/4,
         put_record/3, put_record/4, put_record/5, put_record/6, put_record/7,
         put_records/2, put_records/3, put_records/4,
         merge_shards/3, merge_shards/4,
         split_shards/3, split_shards/4,
         add_tags_to_stream/2, add_tags_to_stream/3,
         list_tags_for_stream/1, list_tags_for_stream/2,
         list_tags_for_stream/3, list_tags_for_stream/4,
         list_all_tags_for_stream/1, list_all_tags_for_stream/2,
         remove_tags_from_stream/2, remove_tags_from_stream/3
        ]).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

-type get_records_limit() :: 1..10000.

-spec new(string(), string()) -> aws_config().

new(AccessKeyID, SecretAccessKey) ->
    #aws_config{
       access_key_id=AccessKeyID,
       secret_access_key=SecretAccessKey
      }.

-spec new(string(), string(), string()) -> aws_config().

new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{
       access_key_id=AccessKeyID,
       secret_access_key=SecretAccessKey,
       kinesis_host=Host
      }.


-spec new(string(), string(), string(), non_neg_integer()) -> aws_config().

new(AccessKeyID, SecretAccessKey, Host, Port) ->
    #aws_config{
       access_key_id=AccessKeyID,
       secret_access_key=SecretAccessKey,
       kinesis_host=Host,
       kinesis_port=Port
      }.

-spec configure(string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey) ->
    erlcloud_config:configure(AccessKeyID, SecretAccessKey, fun new/2).

-spec configure(string(), string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host) ->
    erlcloud_config:configure(AccessKeyID, SecretAccessKey, Host, fun new/3).

-spec configure(string(), string(), string(), non_neg_integer()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host, Port) ->
    erlcloud_config:configure(AccessKeyID, SecretAccessKey, Host, Port, fun new/4).

default_config() -> erlcloud_aws:default_config().

%%------------------------------------------------------------------------------
%% @doc
%% Kinesis API:
%% [http://docs.aws.amazon.com/kinesis/latest/APIReference/API_CreateStream.html]
%%
%% ===Example===
%%
%% This operation adds a new Amazon Kinesis stream to your AWS account.
%%
%% `
%% erlcloud_kinesis:create_stream(<<"test">>, 2).
%%   {ok,{incomplete,#Fun<jsx_decoder.1.688044>}}
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec create_stream(binary(), 1..100000) -> erlcloud_kinesis_impl:json_return().

create_stream(StreamName, ShardCount) when is_integer(ShardCount), ShardCount > 0, ShardCount =< 100000 ->
   Json = [{<<"StreamName">>, StreamName}, {<<"ShardCount">>, ShardCount}],
   erlcloud_kinesis_impl:request(default_config(), "Kinesis_20131202.CreateStream", Json).

-spec create_stream(binary(), 1..100000, aws_config()) -> erlcloud_kinesis_impl:json_return().

create_stream(StreamName, ShardCount, Config) when is_record(Config, aws_config), is_integer(ShardCount), ShardCount > 0, ShardCount =< 100000 ->
   Json = [{<<"StreamName">>, StreamName}, {<<"ShardCount">>, ShardCount}],
   erlcloud_kinesis_impl:request(Config, "Kinesis_20131202.CreateStream", Json).

%%------------------------------------------------------------------------------
%% @doc
%% Kinesis API:
%% [http://docs.aws.amazon.com/kinesis/latest/APIReference/API_DeleteStream.html]
%%
%% ===Example===
%%
%% This operation deletes a stream and all of its shards and data.
%%
%% `
%% erlcloud_kinesis:delete_stream(<<"test">>).
%%   {ok,{incomplete,#Fun<jsx_decoder.1.688044>}}
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec delete_stream(binary()) -> erlcloud_kinesis_impl:json_return().

delete_stream(StreamName) ->
   Json = [{<<"StreamName">>, StreamName}],
   erlcloud_kinesis_impl:request(default_config(), "Kinesis_20131202.DeleteStream", Json).

-spec delete_stream(binary(), aws_config()) -> erlcloud_kinesis_impl:json_return().

delete_stream(StreamName, Config) when is_record(Config, aws_config) ->
   Json = [{<<"StreamName">>, StreamName}],
   erlcloud_kinesis_impl:request(Config, "Kinesis_20131202.DeleteStream", Json).

%%------------------------------------------------------------------------------
%% @doc
%% Kinesis API:
%% [http://docs.aws.amazon.com/kinesis/latest/APIReference/API_ListStreams.html]
%%
%% ===Example===
%%
%% This operation returns an array of the names of all the streams that are associated with the AWS account making the ListStreams request.
%%
%% `
%% erlcloud_kinesis:list_streams().
%%   {ok,[{<<"HasMoreStreams">>,false},
%%    {<<"StreamNames">>,[<<"staging">>]}]}
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec list_streams() -> erlcloud_kinesis_impl:json_return().

list_streams() ->
   list_streams(default_config()).

-spec list_streams(binary() | aws_config()) -> erlcloud_kinesis_impl:json_return().

list_streams(Config) when is_record(Config, aws_config) ->
   erlcloud_kinesis_impl:request(Config, "Kinesis_20131202.ListStreams", []);
list_streams(ExclusiveStartStreamName) ->
   Json = [{<<"ExclusiveStartStreamName">>, ExclusiveStartStreamName}],
   erlcloud_kinesis_impl:request(default_config(), "Kinesis_20131202.ListStreams", Json).

-spec list_streams(binary(), 1..100 | aws_config()) -> erlcloud_kinesis_impl:json_return().

list_streams(ExclusiveStartStreamName, Config) when is_record(Config, aws_config) ->
   Json = [{<<"ExclusiveStartStreamName">>, ExclusiveStartStreamName}],
   erlcloud_kinesis_impl:request(Config, "Kinesis_20131202.ListStreams", Json);
list_streams(ExclusiveStartStreamName, Limit) when is_integer(Limit), Limit > 0, Limit =< 100 ->
   Json = [{<<"ExclusiveStartStreamName">>, ExclusiveStartStreamName}, {<<"Limit">>, Limit}],
   erlcloud_kinesis_impl:request(default_config(), "Kinesis_20131202.ListStreams", Json).

-spec list_streams(binary(), 1..100, aws_config()) -> erlcloud_kinesis_impl:json_return().

list_streams(ExclusiveStartStreamName, Limit, Config) when is_record(Config, aws_config), is_integer(Limit), Limit > 0, Limit =< 100 ->
   Json = [{<<"ExclusiveStartStreamName">>, ExclusiveStartStreamName}, {<<"Limit">>, Limit}],
   erlcloud_kinesis_impl:request(Config, "Kinesis_20131202.ListStreams", Json).

%%------------------------------------------------------------------------------
%% @doc
%% Kinesis API:
%% [http://docs.aws.amazon.com/kinesis/latest/APIReference/API_DescribeStream.html]
%%
%% ===Example===
%%
%% This operation returns the following information about the stream: the current status of the stream, the stream Amazon Resource Name (ARN), and an array of shard objects that comprise the stream.
%%
%% `
%% erlcloud_kinesis:describe_stream(<<"staging">>).
%%   {ok,[{<<"StreamDescription">>,
%%    [{<<"HasMoreShards">>,false},
%%     {<<"Shards">>,
%%      [[{<<"HashKeyRange">>,
%%         [{<<"EndingHashKey">>,
%%           <<"170141183460469231731687303715884105727">>},
%%          {<<"StartingHashKey">>,<<"0">>}]},
%%        {<<"SequenceNumberRange">>,
%%         [{<<"StartingSequenceNumber">>,
%%           <<"495372647485535624187345081927970814089871018992"...>>}]},
%%        {<<"ShardId">>,<<"shardId-000000000000">>}],
%%       [{<<"HashKeyRange">>,
%%         [{<<"EndingHashKey">>,
%%           <<"340282366920938463463374607431768211455">>},
%%          {<<"StartingHashKey">>,
%%           <<"170141183460469231731687303715884105728">>}]},
%%        {<<"SequenceNumberRange">>,
%%         [{<<"StartingSequenceNumber">>,
%%           <<"49537264748575863163933038815938617127259750"...>>}]},
%%        {<<"ShardId">>,<<"shardId-000000000001">>}]]},
%%     {<<"StreamARN">>,
%%      <<"arn:aws:kinesis:us-east-1:821148768124:stream/staging">>},
%%     {<<"StreamName">>,<<"staging">>},
%%     {<<"StreamStatus">>,<<"ACTIVE">>}]}]}
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec describe_stream(binary()) -> erlcloud_kinesis_impl:json_return().
describe_stream(StreamName) ->
   describe_stream(StreamName, default_config()).

-spec describe_stream(binary(), get_records_limit() | aws_config()) -> erlcloud_kinesis_impl:json_return().
describe_stream(StreamName, Config) when is_record(Config, aws_config) ->
    Json = [{<<"StreamName">>, StreamName}],
    erlcloud_kinesis_impl:request(Config, "Kinesis_20131202.DescribeStream", Json);
describe_stream(StreamName, Limit) ->
    describe_stream(StreamName, Limit, default_config()).

-spec describe_stream(binary(), get_records_limit(), string() | aws_config()) -> erlcloud_kinesis_impl:json_return().
describe_stream(StreamName, Limit, Config)
  when is_record(Config, aws_config),
       is_integer(Limit),
       Limit >= 1, Limit =< 10000 ->
    Json = [{<<"StreamName">>, StreamName}, {<<"Limit">>, Limit}],
    erlcloud_kinesis_impl:request(Config, "Kinesis_20131202.DescribeStream", Json);
describe_stream(StreamName, Limit, ExcludeShard) ->
    describe_stream(StreamName, Limit, ExcludeShard, default_config()).

-spec describe_stream(binary(), get_records_limit(), string(), aws_config()) -> erlcloud_kinesis_impl:json_return().
describe_stream(StreamName, Limit, ExcludeShard, Config)
  when is_record(Config, aws_config),
       is_integer(Limit),
       Limit >= 1, Limit =< 10000 ->
    Json = [{<<"StreamName">>, StreamName}, {<<"Limit">>, Limit}, {<<"ExclusiveStartShardId">>, ExcludeShard}],
    erlcloud_kinesis_impl:request(Config, "Kinesis_20131202.DescribeStream", Json).


%%------------------------------------------------------------------------------
%% @doc
%% Kinesis API:
%% [http://docs.aws.amazon.com/kinesis/latest/APIReference/API_DescribeStreamSummary.html]
%%
%% ===Example===
%%
%% Provides a summarized description of the specified Kinesis data stream without the shard list.
%%
%% `
%% erlcloud_kinesis:describe_stream_summary(<<"staging">>).
%%   {ok,[{<<"StreamDescriptionSummary">>,
%%    [{<<EncryptionType">>,<<"NONE">>},
%%     {<<"EnhancedMonitoring">>,[[{<<"ShardLevelMetrics">>,[<<"ALL">>]}]]},
%%     {<<"KeyId">>,<<"staging">>},
%%     {<<"OpenShardCount">>,10},
%%     {<<"RetentionPeriodHours">>,24},
%%     {<<"StreamARN">>,
%%      <<"arn:aws:kinesis:us-east-1:821148768124:stream/staging">>},
%%     {<<"StreamName">>,<<"staging">>},
%%     {<<"StreamStatus">>,<<"ACTIVE">>}]}]}
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec describe_stream_summary(binary()) -> erlcloud_kinesis_impl:json_return().
describe_stream_summary(StreamName) ->
    describe_stream_summary(StreamName, default_config()).

-spec describe_stream_summary(binary(), aws_config()) -> erlcloud_kinesis_impl:json_return().
describe_stream_summary(StreamName, Config) when is_record(Config, aws_config) ->
    Json = [{<<"StreamName">>, StreamName}],
    erlcloud_kinesis_impl:request(Config, "Kinesis_20131202.DescribeStreamSummary", Json).

%%------------------------------------------------------------------------------
%% @doc
%% Kinesis API:
%% [https://docs.aws.amazon.com/kinesis/latest/APIReference/API_EnableEnhancedMonitoring.html]
%%
%% ===Example===
%%
%% nables enhanced Kinesis data stream monitoring for shard-level metrics.
%%
%% `
%% erlcloud_kinesis:enable_enhanced_monitoring(<<"staging">>, [<<"ALL">>]).
%%   {ok,[
%%     {<<"CurrentShardLevelMetrics">>,[]},
%%     {<<"DesiredShardLevelMetrics">>,[{<<"ShardLevelMetrics">>,[<<"ALL">>]}]},
%%     {<<"StreamName">>,<<"staging">>}
%%    ]}
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec enable_enhanced_monitoring(binary()) -> erlcloud_kinesis_impl:json_return().
enable_enhanced_monitoring(StreamName) ->
    enable_enhanced_monitoring(StreamName, default_config()).

-spec enable_enhanced_monitoring(binary(), list(binary()) | aws_config()) -> erlcloud_kinesis_impl:json_return().
enable_enhanced_monitoring(StreamName, Config) when is_record(Config, aws_config) ->
    enable_enhanced_monitoring(StreamName, [<<"ALL">>], Config);
enable_enhanced_monitoring(StreamName, Metrics) when is_list(Metrics) ->
    enable_enhanced_monitoring(StreamName, Metrics, default_config()).

-spec enable_enhanced_monitoring(binary(), list(binary()), aws_config()) -> erlcloud_kinesis_impl:json_return().
enable_enhanced_monitoring(StreamName, Metrics, Config)
        when is_record(Config, aws_config), is_list(Metrics) ->
    Json = [{<<"StreamName">>, StreamName}, {<<"ShardLevelMetrics">>, Metrics}],
    erlcloud_kinesis_impl:request(Config, "Kinesis_20131202.EnableEnhancedMonitoring", Json).

%%------------------------------------------------------------------------------
%% @doc
%% Kinesis API:
%% [https://docs.aws.amazon.com/kinesis/latest/APIReference/API_DisableEnhancedMonitoring.html]
%%
%% ===Example===
%%
%% Disables enhanced monitoring.
%%
%% `
%% erlcloud_kinesis:disable_enhanced_monitoring(<<"staging">>, [<<"ALL">>]).
%%   {ok,[
%%     {<<"CurrentShardLevelMetrics">>,[{<<"ShardLevelMetrics">>,[<<"ALL">>]}]},
%%     {<<"DesiredShardLevelMetrics">>,[]},
%%     {<<"StreamName">>,<<"staging">>}
%%    ]}
%% '
%% @end
%%------------------------------------------------------------------------------

-spec disable_enhanced_monitoring(binary()) -> erlcloud_kinesis_impl:json_return().
disable_enhanced_monitoring(StreamName) ->
    disable_enhanced_monitoring(StreamName, default_config()).

-spec disable_enhanced_monitoring(binary(), list(binary()) | aws_config()) -> erlcloud_kinesis_impl:json_return().
disable_enhanced_monitoring(StreamName, Config) when is_record(Config, aws_config) ->
    disable_enhanced_monitoring(StreamName, [<<"ALL">>], Config);
disable_enhanced_monitoring(StreamName, Metrics) when is_list(Metrics) ->
    disable_enhanced_monitoring(StreamName, Metrics, default_config()).

-spec disable_enhanced_monitoring(binary(), list(binary()), aws_config()) -> erlcloud_kinesis_impl:json_return().
disable_enhanced_monitoring(StreamName, Metrics, Config)
        when is_record(Config, aws_config), is_list(Metrics)  ->
    Json = [{<<"StreamName">>, StreamName}, {<<"ShardLevelMetrics">>, Metrics}],
    erlcloud_kinesis_impl:request(Config, "Kinesis_20131202.DisableEnhancedMonitoring", Json).

%%------------------------------------------------------------------------------
%% @doc
%% Kinesis API:
%% [http://docs.aws.amazon.com/kinesis/latest/APIReference/API_GetShardIterator.html]
%%
%% ===Example===
%%
%% This operation returns a shard iterator in ShardIterator. The shard iterator specifies the position in the shard from which you want to start reading data records sequentially.
%%
%% `
%% erlcloud_kinesis:get_shard_iterator(<<"test">>, <<"shardId-000000000001">>, <<"TRIM_HORIZON">>).
%%   {ok,[{<<"ShardIterator">>,
%%    <<"AAAAAAAAAAFHJejL6/AjDShV3pIXsxYZT7Xj2G6EHxokHqT2D1stIOVYUEyprlUGWUepKqUDaR0+hB6qTlKvZa+fsBRqgHi4"...>>}]}
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec get_shard_iterator(binary(), binary(), binary()) ->
    erlcloud_kinesis_impl:json_return().
get_shard_iterator(StreamName, ShardId, ShardIteratorType) ->
    get_shard_iterator(StreamName, ShardId, ShardIteratorType, default_config()).

-spec get_shard_iterator(binary(), binary(), binary(),
                         binary() | integer() | float() | aws_config()) ->
    erlcloud_kinesis_impl:json_return().
get_shard_iterator(StreamName, ShardId, ShardIteratorType, Config)
    when is_record(Config, aws_config) ->
    Json = [{<<"StreamName">>,        StreamName},
            {<<"ShardId">>,           ShardId},
            {<<"ShardIteratorType">>, ShardIteratorType}],
    get_shard_iterator_request(Config, Json);
get_shard_iterator(StreamName, ShardId, ShardIteratorType, SeqNOrTs) ->
    get_shard_iterator(StreamName, ShardId, ShardIteratorType, SeqNOrTs,
                       default_config()).

-spec get_shard_iterator(binary(), binary(), binary(),
                         binary() | integer() | float(), aws_config()) ->
    erlcloud_kinesis_impl:json_return().
get_shard_iterator(StreamName, ShardId, ShardIteratorType = <<"AT_TIMESTAMP">>,
                   Timestamp, Config) ->
    Json = [{<<"StreamName">>,        StreamName},
            {<<"ShardId">>,           ShardId},
            {<<"ShardIteratorType">>, ShardIteratorType},
            {<<"Timestamp">>,         Timestamp}],
    get_shard_iterator_request(Config, Json);
get_shard_iterator(StreamName, ShardId, ShardIteratorType,
                   StartingSequenceNumber, Config) ->
    Json = [{<<"StreamName">>,             StreamName},
            {<<"ShardId">>,                ShardId},
            {<<"ShardIteratorType">>,      ShardIteratorType},
            {<<"StartingSequenceNumber">>, StartingSequenceNumber}],
    get_shard_iterator_request(Config, Json).

get_shard_iterator_request(Config, Json) ->
    Operation = "Kinesis_20131202.GetShardIterator",
    erlcloud_kinesis_impl:request(Config, Operation, Json).

%%------------------------------------------------------------------------------
%% @doc
%% Kinesis API:
%% [http://docs.aws.amazon.com/kinesis/latest/APIReference/API_GetRecords.html]
%%
%% ===Example===
%%
%% This operation returns one or more data records from a shard. A GetRecords operation request can retrieve up to 10 MB of data.
%%
%% `
%% {ok, [{_, A2}]} = erlcloud_kinesis:get_shard_terator(<<"test">>, <<"shardId-000000000000">>, <<"TRIM_HORIZON">>).
%% {ok,[{<<"ShardIterator">>,
%%      <<"AAAAAAAAAAEuncwaAk+GTC2TIdmdg5w6dIuZ4Scu6vaMGPtaPUfopvw9cBm2NM3Rlj9WyI5JFJr2ahuSh3Z187AdW4Lug86E"...>>}]}
%% erlcloud_kinesis:get_records(A2).
%%  {ok,[{<<"NextShardIterator">>,
%%      <<"AAAAAAAAAAEkuCmrC+QDW1gUywyu7G8GxvRyM6GSMkcHQ9wrvCJBW87mjn9C8YEckkipaoJySwgKXMmn1BwSPjnjiUCsu6pc"...>>},
%%    {<<"Records">>,
%%      [[{<<"Data">>,<<"asdasd">>},
%%        {<<"PartitionKey">>,<<"key">>},
%%        {<<"SequenceNumber">>,
%%         <<"49537292605574028653758531131893428543501381406818304001">>}],
%%       [{<<"Data">>,<<"asdasd 213123123">>},
%%        {<<"PartitionKey">>,<<"key">>},
%%        {<<"SequenceNumber">>,
%%         <<"49537292605574028653758541428570459745183078607853977601">>}]]}]}
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec get_records(string()) -> erlcloud_kinesis_impl:json_return().
get_records(ShardIterator) ->
    Json = [{<<"ShardIterator">>, ShardIterator}],
    get_normalized_records(default_config(), Json).

-spec get_records(string(), get_records_limit()| aws_config()) -> erlcloud_kinesis_impl:json_return().
get_records(ShardIterator, Config) when is_record(Config, aws_config) ->
    Json = [{<<"ShardIterator">>, ShardIterator}],
    get_normalized_records(Config, Json);
get_records(ShardIterator, Limit)
  when is_integer(Limit), Limit > 0, Limit =< 10000 ->
    get_records(ShardIterator, Limit, default_config()).

-spec get_records(binary(), get_records_limit(), aws_config()) ->
    erlcloud_kinesis_impl:json_return().
get_records(ShardIterator, Limit, Config) ->
    get_records(ShardIterator, Limit, [], Config).

-spec get_records(binary(), get_records_limit(), proplist(), aws_config()) ->
    erlcloud_kinesis_impl:json_return().
get_records(ShardIterator, Limit, Options, Config)
  when is_record(Config, aws_config),
       is_integer(Limit),
       Limit > 0, Limit =< 10000 ->
    Json = [{<<"ShardIterator">>, ShardIterator},
            {<<"Limit">>, Limit}],
    ShouldDecode = proplists:get_value(decode, Options, true),
    get_normalized_records(Config, Json, ShouldDecode).

get_normalized_records(Config, Json) ->
    get_normalized_records(Config, Json, true).

get_normalized_records(Config, Json, ShouldDecode)
  when is_record(Config, aws_config) ->
    Request = "Kinesis_20131202.GetRecords",
    case erlcloud_kinesis_impl:request(Config, Request, Json, ShouldDecode) of
        {ok, Resp} when is_binary(Resp) -> {ok, Resp};
        {ok, Resp} -> {ok, normalize_response(Resp)};
        {error, Reason} -> {error, Reason}
    end.


normalize_record([{K,V} | T]) when K == <<"Data">> -> [ {K, base64:decode(V)} | normalize_record(T) ];
normalize_record([K | T]) -> [K | normalize_record(T) ];
normalize_record([]) -> [].

normalize_records([K | V]) -> [ normalize_record(K) | normalize_records(V) ];
normalize_records([]) -> [].

normalize_response([{K,V} | T]) when K == <<"Records">> -> [ {K, normalize_records(V)} | normalize_response(T)];
normalize_response([K | T]) -> [K | normalize_response(T)];
normalize_response([]) -> [].


%%------------------------------------------------------------------------------
%% @doc
%% Kinesis API:
%% [http://docs.aws.amazon.com/kinesis/latest/APIReference/API_PutRecord.html]
%%
%% ===Example===
%%
%% This operation puts a data record into an Amazon Kinesis stream from a producer.
%%
%% `
%% erlcloud_kinesis:put_record(<<"test">>, <<"key">>, <<"asdasd">>).
%% {ok,[{<<"SequenceNumber">>,
%%    <<"49537292605574028653758531131893428543501381406818304001">>},
%%    {<<"ShardId">>,<<"shardId-000000000000">>}]}
%% erlcloud_kinesis:put_record(<<"test">>, <<"key">>, <<"asdasd 213123123">>).
%% {ok,[{<<"SequenceNumber">>,
%%    <<"49537292605574028653758541428570459745183078607853977601">>},
%%    {<<"ShardId">>,<<"shardId-000000000000">>}]}
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-type partition_key()     :: binary().
-type payload()           :: binary() | string().
-type explicit_hash_key() :: binary() | undefined.
-type ordering()          :: binary() | undefined.

-spec put_record(binary(), partition_key(), payload()) ->
    erlcloud_kinesis_impl:json_return().
put_record(StreamName, PartitionKey, Data) ->
    put_record(StreamName, PartitionKey, Data, undefined).

-spec put_record(binary(), partition_key(), payload(), explicit_hash_key() | aws_config()) ->
    erlcloud_kinesis_impl:json_return().
put_record(StreamName, PartitionKey, Data, Config) when is_record(Config, aws_config) ->
    put_record(StreamName, PartitionKey, Data, undefined, Config);
put_record(StreamName, PartitionKey, Data, ExplicitHashKey) ->
    put_record(StreamName, PartitionKey, Data, ExplicitHashKey, undefined).

-spec put_record(binary(), partition_key(), payload(), explicit_hash_key(), ordering() | aws_config()) ->
    erlcloud_kinesis_impl:json_return().
put_record(StreamName, PartitionKey, Data, ExplicitHashKey, Config) when is_record(Config, aws_config) ->
    put_record(StreamName, PartitionKey, Data, ExplicitHashKey, undefined, Config);
put_record(StreamName, PartitionKey, Data, ExplicitHashKey, Ordering) ->
    put_record(StreamName, PartitionKey, Data, ExplicitHashKey, Ordering, []).

-spec put_record(binary(), partition_key(), payload(), explicit_hash_key(), ordering(), proplist() | aws_config()) ->
    erlcloud_kinesis_impl:json_return().
put_record(StreamName, PartitionKey, Data, ExplicitHashKey, Ordering, Config) when is_record(Config, aws_config) ->
    put_record(StreamName, PartitionKey, Data, ExplicitHashKey, Ordering, [], Config);
put_record(StreamName, PartitionKey, Data, ExplicitHashKey, Ordering, Options) ->
    put_record(StreamName, PartitionKey, Data, ExplicitHashKey, Ordering, Options, default_config()).

-spec put_record(binary(), partition_key(), payload(), explicit_hash_key(), ordering(), proplist(), aws_config()) ->
    erlcloud_kinesis_impl:json_return().
put_record(StreamName, PartitionKey, Data, ExplicitHashKey, Ordering, Options, Config) when is_record(Config, aws_config) ->
    Encoded  = case proplists:get_value(encode, Options, true) of
                   true  -> base64:encode(Data);
                   false -> Data
               end,
    Optional = [{<<"ExplicitHashKey">>, ExplicitHashKey},
                {<<"SequenceNumberForOrdering">>, Ordering}],
    Json = [{<<"StreamName">>, StreamName},
            {<<"PartitionKey">>, PartitionKey},
            {<<"Data">>, Encoded}
           |[ KV || {_, V} = KV <- Optional, V /= undefined ]],
    erlcloud_kinesis_impl:request(Config, "Kinesis_20131202.PutRecord", Json).

%%------------------------------------------------------------------------------
%% Kinesis API:
%% [https://docs.aws.amazon.com/kinesis/latest/APIReference/API_PutRecords.html]
%%
%% ===Example===
%%
%% This operation puts data records into an Amazon Kinesis stream from a producer.
%%
%% `
%% erlcloud_kinesis:put_records(<<"StreamName">>, [{<<"data">>, <<"partitionKey1">>}]).
%% {ok, [{<<"FailedRecordCount">>, 0},
%%      {<<"Records">>,
%%       [
%%        [{<<"SequenceNumber">>,
%%          <<"49537292605574028653758531131893428543501381406818304001">>},
%%         {<<"ShardId">>,<<"shardId-000000000000">>}]
%%       ]}
%%     ]
%% }
%% erlcloud_kinesis:put_records(<<"StreamName">>, [{<<"data">>, <<"explicitHashKey">>, <<"partitionKey1">>}]).
%% {ok, [{<<"FailedRecordCount">>, 0},
%%      {<<"Records">>,
%%       [
%%        [{<<"SequenceNumber">>,
%%          <<"49537292605574028653758531131893428543501381406818304001">>},
%%         {<<"ShardId">>,<<"shardId-000000000000">>}]
%%       ]}
%%     ]
%% }
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-type put_records_item() :: {Data :: string(), PartitionKey :: string()}
                          | {Data :: string(), ExplicitHashKey :: string(), PartitionKey :: string()}.
-type put_records_items() :: [put_records_item()].

-spec put_records(binary(), put_records_items()) -> erlcloud_kinesis_impl:json_return().

put_records(StreamName, Items) ->
    put_records(StreamName, Items, default_config()).

-spec put_records(binary(), put_records_items(), function() | aws_config()) -> erlcloud_kinesis_impl:json_return().

put_records(StreamName, Items, EncodingFun) when is_function(EncodingFun, 1) ->
    put_records(StreamName, Items, EncodingFun, default_config());

put_records(StreamName, Items, Config) ->
    put_records(StreamName, Items, fun default_put_encoding/1, Config).

-spec put_records(binary(), put_records_items(),
                  function(), Config :: aws_config()) -> erlcloud_kinesis_impl:json_return().

put_records(StreamName, Items, EncodingFun, Config) when is_function(EncodingFun, 1) ->
    Operation = put_records_operation(),
    Json = prepare_put_records_data(StreamName, Items, EncodingFun),
    erlcloud_kinesis_impl:request(Config, Operation, Json).

default_put_encoding(D) -> base64:encode(D).

put_records_operation() ->
    "Kinesis_20131202.PutRecords".

prepare_put_records_data(StreamName, Items, Fun) ->
    Records = [prepare_put_records_item(X, Fun) || X <- Items],
    [{<<"StreamName">>, StreamName}, {<<"Records">>, Records}].

prepare_put_records_item({Data, PartitionKey}, Fun) ->
    [{<<"PartitionKey">>, PartitionKey},
     {<<"Data">>, Fun(Data)}];
prepare_put_records_item({Data, ExplicitHashKey, PartitionKey}, Fun) ->
    [{<<"PartitionKey">>, PartitionKey},
     {<<"ExplicitHashKey">>, ExplicitHashKey},
     {<<"Data">>, Fun(Data)}].

%%------------------------------------------------------------------------------
%% @doc
%% Kinesis API:
%% [http://docs.aws.amazon.com/kinesis/latest/APIReference/API_MergeShards.html]
%%
%% ===Example===
%%
%% This operation merges two adjacent shards in a stream and combines them into a single shard to reduce the stream's capacity to ingest and transport data. Two shards are considered adjacent if the union of the hash key ranges for the two shards form a contiguous set with no gaps.
%%
%% `
%% erlcloud_kinesis:merge_shards(<<"test">>, <<"shardId-000000000001">>, <<"shardId-000000000003">>).
%% {ok,{incomplete,#Fun<jsx_decoder.1.688044>}}
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec merge_shards(binary(), string(), string()) -> erlcloud_kinesis_impl:json_return().

merge_shards(StreamName, AdjacentShardToMerge, ShardToMerge) ->
  Json = [{<<"StreamName">>, StreamName}, {<<"AdjacentShardToMerge">>, AdjacentShardToMerge}, {<<"ShardToMerge">>, ShardToMerge}],
  erlcloud_kinesis_impl:request(default_config(), "Kinesis_20131202.MergeShards", Json).

-spec merge_shards(binary(), string(), string(), aws_config()) -> erlcloud_kinesis_impl:json_return().

merge_shards(StreamName, AdjacentShardToMerge, ShardToMerge, Config) when is_record(Config, aws_config) ->
  Json = [{<<"StreamName">>, StreamName}, {<<"AdjacentShardToMerge">>, AdjacentShardToMerge}, {<<"ShardToMerge">>, ShardToMerge}],
  erlcloud_kinesis_impl:request(Config, "Kinesis_20131202.MergeShards", Json).

%%------------------------------------------------------------------------------
%% @doc
%% Kinesis API:
%% [http://docs.aws.amazon.com/kinesis/latest/APIReference/API_SplitShard.html]
%%
%% ===Example===
%%
%% This operation splits a shard into two new shards in the stream, to increase the stream's capacity to ingest and transport data.
%%
%% `
%%  erlcloud_kinesis:split_shards(<<"test">>, <<"shardId-000000000000">>, <<"10">>).
%%  {ok,{incomplete,#Fun<jsx_decoder.1.688044>}}
%% '
%%
%% @end
%%------------------------------------------------------------------------------

-spec split_shards(binary(), string(), string()) -> erlcloud_kinesis_impl:json_return().

split_shards(StreamName, ShardToSplit, NewStartingHashKey) ->
  Json = [{<<"StreamName">>, StreamName}, {<<"ShardToSplit">>, ShardToSplit}, {<<"NewStartingHashKey">>, NewStartingHashKey}],
  erlcloud_kinesis_impl:request(default_config(), "Kinesis_20131202.SplitShard", Json).

-spec split_shards(binary(), string(), string(), aws_config()) -> erlcloud_kinesis_impl:json_return().

split_shards(StreamName, ShardToSplit, NewStartingHashKey, Config) when is_record(Config, aws_config) ->
  Json = [{<<"StreamName">>, StreamName}, {<<"ShardToSplit">>, ShardToSplit}, {<<"NewStartingHashKey">>, NewStartingHashKey}],
  erlcloud_kinesis_impl:request(Config, "Kinesis_20131202.SplitShard", Json).

%%------------------------------------------------------------------------------
%% @doc
%% Kinesis API:
%% [http://docs.aws.amazon.com/kinesis/latest/APIReference/API_AddTagsToStream.html]
%%
%% Adds or updates tags for the specified Amazon Kinesis stream.
%%
%% erlcloud_kinesis:add_tags_to_stream(<<"stream_name">>,
%%                                     [{<<"tag_key">>, <<"tag_value">>}]).
%% ok.
%%
%% @end
%%------------------------------------------------------------------------------
-spec add_tags_to_stream(binary(), [{binary(), binary()}, ...]) ->
    ok | {error, any()}.
add_tags_to_stream(StreamName, Tags) ->
    add_tags_to_stream(StreamName, Tags, default_config()).

-spec add_tags_to_stream(binary(), [{binary(), binary()}, ...], aws_config()) ->
    ok | {error, any()}.
add_tags_to_stream(StreamName, Tags, Config)
  when is_record(Config, aws_config) ->
    Json      = [{<<"StreamName">>, StreamName},
                 {<<"Tags">>,       Tags}],
    Operation = "Kinesis_20131202.AddTagsToStream",
    Response  = erlcloud_kinesis_impl:request(Config, Operation, Json),
    normalize_tags_response(Response).

%%------------------------------------------------------------------------------
%% @doc
%% Kinesis API:
%% [http://docs.aws.amazon.com/kinesis/latest/APIReference/API_ListTagsForStream.html]
%%
%% Lists the tags for the specified Amazon Kinesis stream.
%%
%% erlcloud_kinesis:list_tags_for_stream(<<"stream_name">>, <<"key2">>, 2).
%% {ok, [
%%       {<<"HasMoreTags">>, true},
%%       {<<"Tags">>,        [[{<<"Key">>, <<"k1">>}, {<<"Value">>, <<"v1">>}],
%%                            [{<<"Key">>, <<"k2">>}, {<<"Value">>, <<"v2">>}]]}
%%      ]
%% }.
%%
%% @end
%%------------------------------------------------------------------------------
-spec list_tags_for_stream(binary()) -> {ok, proplist()} | {error, any()}.
list_tags_for_stream(StreamName) ->
    list_tags_for_stream(StreamName, default_config()).

-spec list_tags_for_stream(binary(), aws_config()) ->
    {ok, proplist()} | {error, any()}.
list_tags_for_stream(StreamName, Config) when is_record(Config, aws_config) ->
    list_tags_for_stream(StreamName, undefined, undefined, Config).

-spec list_tags_for_stream(binary(),
                           binary() | undefined,
                           integer() | undefined) ->
    {ok, proplist()} | {error, any()}.
list_tags_for_stream(StreamName, ESK, Limit) ->
    list_tags_for_stream(StreamName, ESK, Limit, default_config()).

-spec list_tags_for_stream(binary(),
                           binary() | undefined,
                           integer() | undefined,
                           aws_config()) ->
    erlcloud_kinesis_impl:json_return().
list_tags_for_stream(StreamName, ESK, Limit, Config)
  when is_record(Config, aws_config) ->
    Json      = [{<<"StreamName">>,           StreamName},
                 {<<"ExclusiveStartTagKey">>, ESK},
                 {<<"Limit">>,                Limit}],
    JsonUpd   = lists:filter(fun filter_undefined_params/1, Json),
    Operation = "Kinesis_20131202.ListTagsForStream",
    erlcloud_kinesis_impl:request(Config, Operation, JsonUpd).

filter_undefined_params({_, undefined}) -> false;
filter_undefined_params(_)              -> true.

%%------------------------------------------------------------------------------
%% @doc
%% Wrapper around 'list_tags_for_stream' to return {ok, <list_of_all_tags>}.
%%
%% erlcloud_kinesis:list_all_tags_for_stream(<<"stream_name">>).
%% {ok, [{<<"k1">>, <<"v1">>},
%%       {<<"k2">>, <<"v2">>}]
%% }.
%%
%% @end
%%------------------------------------------------------------------------------
-spec list_all_tags_for_stream(binary()) -> {ok, proplist()} | {error, any()}.
list_all_tags_for_stream(StreamName) ->
    list_all_tags_for_stream(StreamName, default_config()).

-spec list_all_tags_for_stream(binary(), aws_config()) ->
    {ok, proplist()} | {error, any()}.
list_all_tags_for_stream(StreamName, Config) ->
    list_all_tags_for_stream(StreamName, undefined, undefined, Config, []).

list_all_tags_for_stream(StreamName, ESK, Limit, Config, Acc) ->
    case list_tags_for_stream(StreamName, ESK, Limit, Config) of
        {ok, [{<<"HasMoreTags">>, true}, {<<"Tags">>, Tags}]} ->
            NewESK = get_last_tag_key(Tags),
            UpdAcc = Acc ++ Tags,
            list_all_tags_for_stream(StreamName, NewESK, Limit, Config, UpdAcc);
        {ok, [{<<"HasMoreTags">>, false}, {<<"Tags">>, Tags}]} ->
            {ok, [ {K, V} || [{<<"Key">>, K}, {<<"Value">>, V}] <- Acc ++ Tags ]};
        Error ->
            Error
    end.

get_last_tag_key(Tags) ->
    [{<<"Key">>, Key}, _] = lists:last(Tags),
    Key.

%%------------------------------------------------------------------------------
%% @doc
%% Kinesis API:
%% [http://docs.aws.amazon.com/kinesis/latest/APIReference/API_RemoveTagsFromStream.html]
%%
%% Removes tags from the specified Amazon Kinesis stream.
%%
%% erlcloud_kinesis:remove_tags_from_stream(<<"stream_name">>, [<<"tag_key">>]).
%% ok.
%%
%% @end
%%------------------------------------------------------------------------------
-spec remove_tags_from_stream(binary(), [binary(), ...]) -> ok | {error, any()}.
remove_tags_from_stream(StreamName, TagKeys) ->
    remove_tags_from_stream(StreamName, TagKeys, default_config()).

-spec remove_tags_from_stream(binary(), [binary(), ...], aws_config()) ->
    ok | {error, any()}.
remove_tags_from_stream(StreamName, TagKeys, Config)
  when is_record(Config, aws_config) ->
    Json      = [{<<"StreamName">>, StreamName},
                 {<<"TagKeys">>,    TagKeys}],
    Operation = "Kinesis_20131202.RemoveTagsFromStream",
    Response  = erlcloud_kinesis_impl:request(Config, Operation, Json),
    normalize_tags_response(Response).

normalize_tags_response({ok, []}) -> ok;
normalize_tags_response(Error)    -> Error.

%%------------------------------------------------------------------------------
%% Unit Tests
%%------------------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

list_all_tags_pagination_test_() ->
    meck:new(EK = erlcloud_kinesis_impl, [passthrough]),
    Tags1 = [[{<<"Key">>, <<"k1">>}, {<<"Value">>, <<"v1">>}],
             [{<<"Key">>, <<"k2">>}, {<<"Value">>, <<"v2">>}]],
    Tags2 = [[{<<"Key">>, <<"k3">>}, {<<"Value">>, <<"v3">>}]],
    meck:sequence(EK, request, 3,
                  [{ok, [{<<"HasMoreTags">>, true},  {<<"Tags">>, Tags1}]},
                   {ok, [{<<"HasMoreTags">>, false}, {<<"Tags">>, Tags2}]}]),
    Result = erlcloud_kinesis:list_all_tags_for_stream(<<"stream">>),
    meck:unload(EK),
    ?_assertEqual({ok, [{<<"k1">>, <<"v1">>},
                        {<<"k2">>, <<"v2">>},
                        {<<"k3">>, <<"v3">>}]},
                  Result).

-endif.
