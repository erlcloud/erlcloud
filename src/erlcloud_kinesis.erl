%% Amazon Kinesis Service (Kinesis)

-module(erlcloud_kinesis).

%%% Library initialization.
-export([configure/2, configure/3, configure/4,  new/2, new/3]).

-export([create_stream/2, create_stream/3,
         delete_stream/1, delete_stream/2,
         list_streams/0, list_streams/1, list_streams/2, list_streams/3,
         describe_stream/1, describe_stream/2, describe_stream/3, describe_stream/4,
         get_shard_iterator/3, get_shard_iterator/4, get_shard_iterator/5,
         get_records/1, get_records/2, get_records/3,
         put_record/3, put_record/4, put_record/5, put_record/6,
         merge_shards/3, merge_shards/4,
         split_shards/3, split_shards/4
        ]).

-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

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
    put(aws_config, new(AccessKeyID, SecretAccessKey)),
    ok.

-spec configure(string(), string(), string()) -> ok.

configure(AccessKeyID, SecretAccessKey, Host) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host)),
    ok.

-spec configure(string(), string(), string(), non_neg_integer()) -> ok.

configure(AccessKeyID, SecretAccessKey, Host, Port) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host, Port)),
    ok.

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

-spec create_stream/2 :: (string(), 1..100) -> proplist().

create_stream(StreamName, ShardCount) when is_integer(ShardCount), ShardCount > 0, ShardCount =< 100 ->
   Json = [{<<"StreamName">>, StreamName}, {<<"ShardCount">>, ShardCount}],
   erlcloud_kinesis_impl:request(default_config(), "Kinesis_20131202.CreateStream", Json).

-spec create_stream/3 :: (string(), 1..100, aws_config()) -> proplist().

create_stream(StreamName, ShardCount, Config) when is_record(Config, aws_config), is_integer(ShardCount), ShardCount > 0, ShardCount =< 100 ->
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

-spec delete_stream/1 :: (string()) -> proplist().

delete_stream(StreamName) ->
   Json = [{<<"StreamName">>, StreamName}],
   erlcloud_kinesis_impl:request(default_config(), "Kinesis_20131202.DeleteStream", Json).

-spec delete_stream/2 :: (string(), aws_config()) -> proplist().

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

-spec list_streams/0 :: () -> proplist().

list_streams() ->
   list_streams(default_config()).

-spec list_streams/1 :: (string() | aws_config()) -> proplist().

list_streams(Config) when is_record(Config, aws_config) ->
   erlcloud_kinesis_impl:request(Config, "Kinesis_20131202.ListStreams", []);
list_streams(ExclusiveStartStreamName) ->
   Json = [{<<"ExclusiveStartStreamName">>, ExclusiveStartStreamName}],
   erlcloud_kinesis_impl:request(default_config(), "Kinesis_20131202.ListStreams", Json).

-spec list_streams/2 :: (string(), 1..100 | aws_config()) -> proplist().

list_streams(ExclusiveStartStreamName, Config) when is_record(Config, aws_config) ->
   Json = [{<<"ExclusiveStartStreamName">>, ExclusiveStartStreamName}],
   erlcloud_kinesis_impl:request(Config, "Kinesis_20131202.ListStreams", Json);
list_streams(ExclusiveStartStreamName, Limit) when is_integer(Limit), Limit > 0, Limit =< 100 ->
   Json = [{<<"ExclusiveStartStreamName">>, ExclusiveStartStreamName}, {<<"Limit">>, Limit}],
   erlcloud_kinesis_impl:request(default_config(), "Kinesis_20131202.ListStreams", Json).

-spec list_streams/3 :: (string(), 1..100, aws_config()) -> proplist().

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

-spec describe_stream/1 :: (string()) -> proplist().

describe_stream(StreamName) ->
   describe_stream(StreamName, default_config()).

-spec describe_stream/2 :: (string(), 1..100 | aws_config()) -> proplist().

describe_stream(StreamName, Config) when is_record(Config, aws_config) ->
   Json = [{<<"StreamName">>, StreamName}],
   erlcloud_kinesis_impl:request(Config, "Kinesis_20131202.DescribeStream", Json);
describe_stream(StreamName, Limit) when is_integer(Limit), Limit > 0, Limit =< 100 ->
   Json = [{<<"StreamName">>, StreamName}, {<<"Limit">>, Limit}],
   erlcloud_kinesis_impl:request(default_config(), "Kinesis_20131202.DescribeStream", Json).

-spec describe_stream/3 :: (string(), 1..100, string() | aws_config()) -> proplist().

describe_stream(StreamName, Limit, Config) when is_record(Config, aws_config) ->
   Json = [{<<"StreamName">>, StreamName}, {<<"Limit">>, Limit}],
   erlcloud_kinesis_impl:request(Config, "Kinesis_20131202.DescribeStream", Json);
describe_stream(StreamName, Limit, ExcludeShard) when is_integer(Limit), Limit > 0, Limit =< 100 ->
   Json = [{<<"StreamName">>, StreamName}, {<<"Limit">>, Limit}, {<<"ExclusiveStartShardId">>, ExcludeShard}],
   erlcloud_kinesis_impl:request(default_config(), "Kinesis_20131202.DescribeStream", Json).

-spec describe_stream/4 :: (string(), 1..100, string(), aws_config()) -> proplist().

describe_stream(StreamName, Limit, ExcludeShard, Config) when is_record(Config, aws_config), is_integer(Limit), Limit > 0, Limit =< 100 ->
   Json = [{<<"StreamName">>, StreamName}, {<<"Limit">>, Limit}, {<<"ExclusiveStartShardId">>, ExcludeShard}],
   erlcloud_kinesis_impl:request(default_config(), "Kinesis_20131202.DescribeStream", Json).


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

-spec get_shard_iterator/3 :: (string(), string(), string()) -> proplist().

get_shard_iterator(StreamName, ShardId, ShardIteratorType) ->
  Json = [{<<"StreamName">>, StreamName}, {<<"ShardId">>, ShardId}, {<<"ShardIteratorType">>, ShardIteratorType}],
  erlcloud_kinesis_impl:request(default_config(), "Kinesis_20131202.GetShardIterator", Json).

-spec get_shard_iterator/4 :: (string(), string(), string(), string() | aws_config()) -> proplist().

get_shard_iterator(StreamName, ShardId, ShardIteratorType, Config) when is_record(Config, aws_config) ->
  Json = [{<<"StreamName">>, StreamName}, {<<"ShardId">>, ShardId}, {<<"ShardIteratorType">>, ShardIteratorType}],
  erlcloud_kinesis_impl:request(Config, "Kinesis_20131202.GetShardIterator", Json);
get_shard_iterator(StreamName, ShardId, ShardIteratorType, StartingSequenceNumber) ->
  Json = [{<<"StreamName">>, StreamName}, {<<"ShardId">>, ShardId}, {<<"ShardIteratorType">>, ShardIteratorType}, {<<"StartingSequenceNumber">>, StartingSequenceNumber}],
  erlcloud_kinesis_impl:request(default_config(), "Kinesis_20131202.GetShardIterator", Json).

-spec get_shard_iterator/5 :: (string(), string(), string(), string(), aws_config()) -> proplist().

get_shard_iterator(StreamName, ShardId, ShardIteratorType, StartingSequenceNumber, Config) when is_record(Config, aws_config) ->
  Json = [{<<"StreamName">>, StreamName}, {<<"ShardId">>, ShardId}, {<<"ShardIteratorType">>, ShardIteratorType}, {<<"StartingSequenceNumber">>, StartingSequenceNumber}],
  erlcloud_kinesis_impl:request(Config, "Kinesis_20131202.GetShardIterator", Json).


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

-spec get_records/1 :: (string()) -> proplist().

get_records(ShardIterator) ->
  Json = [{<<"ShardIterator">>, ShardIterator}],
  get_normalized_records(default_config(), Json).

-spec get_records/2 :: (string(), 1..100 | aws_config()) -> proplist().

get_records(ShardIterator, Config) when is_record(Config, aws_config) ->
  Json = [{<<"ShardIterator">>, ShardIterator}],
  erlcloud_kinesis_impl:request(Config, "Kinesis_20131202.GetRecords", Json);
get_records(ShardIterator, Limit) when is_integer(Limit), Limit > 0, Limit =< 100 ->
  Json = [{<<"ShardIterator">>, ShardIterator}, {<<"Limit">>, Limit}],
  get_normalized_records(default_config(), Json).

-spec get_records/3 :: (string(), 1..100, aws_config()) -> proplist().

get_records(ShardIterator, Limit, Config) when is_record(Config, aws_config), is_integer(Limit), Limit > 0, Limit =< 100 ->
  Json = [{<<"ShardIterator">>, ShardIterator}, {<<"Limit">>, Limit}],
  get_normalized_records(Config, Json).

%% Normalize records from Kinesis

get_normalized_records(Config, Json) when is_record(Config, aws_config) ->
  case erlcloud_kinesis_impl:request(Config, "Kinesis_20131202.GetRecords", Json) of
    {ok, Response} -> {ok, normalize_response(Response)};
    {error, Msg} -> {error, Msg}
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

-spec put_record/3 :: (string(), string(), string()) -> proplist().

put_record(StreamName, PartitionKey, Data) ->
  Json = [{<<"StreamName">>, StreamName}, {<<"PartitionKey">>, PartitionKey}, {<<"Data">>, base64:encode(Data)}],
  erlcloud_kinesis_impl:request(default_config(), "Kinesis_20131202.PutRecord", Json).

-spec put_record/4 :: (string(), string(), string(), string() | aws_config()) -> proplist().

put_record(StreamName, PartitionKey, Data, Config) when is_record(Config, aws_config) ->
  Json = [{<<"StreamName">>, StreamName}, {<<"PartitionKey">>, PartitionKey}, {<<"Data">>, base64:encode(Data)}],
  erlcloud_kinesis_impl:request(Config, "Kinesis_20131202.PutRecord", Json);
put_record(StreamName, PartitionKey, Data, ExplicitHashKey) ->
  Json = [{<<"StreamName">>, StreamName}, {<<"PartitionKey">>, PartitionKey}, {<<"Data">>, base64:encode(Data)}, {<<"ExplicitHashKey">>, ExplicitHashKey}],
  erlcloud_kinesis_impl:request(default_config(), "Kinesis_20131202.PutRecord", Json).

-spec put_record/5 :: (string(), string(), string(), string(), string() | aws_config()) -> proplist().

put_record(StreamName, PartitionKey, Data, ExplicitHashKey, Config) when is_record(Config, aws_config) ->
  Json = [{<<"StreamName">>, StreamName}, {<<"PartitionKey">>, PartitionKey}, {<<"Data">>, base64:encode(Data)}, {<<"ExplicitHashKey">>, ExplicitHashKey}],
  erlcloud_kinesis_impl:request(Config, "Kinesis_20131202.PutRecord", Json);
put_record(StreamName, PartitionKey, Data, ExplicitHashKey, Ordering) ->
  Json = [{<<"StreamName">>, StreamName}, {<<"PartitionKey">>, PartitionKey}, {<<"Data">>, base64:encode(Data)}, {<<"ExplicitHashKey">>, ExplicitHashKey}, {<<"SequenceNumberForOrdering">>, Ordering}],
  erlcloud_kinesis_impl:request(default_config(), "Kinesis_20131202.PutRecord", Json).

-spec put_record/6 :: (string(), string(), string(), string(), string(), aws_config()) -> proplist().

put_record(StreamName, PartitionKey, Data, ExplicitHashKey, Ordering, Config) when is_record(Config, aws_config) ->
  Json = [{<<"StreamName">>, StreamName}, {<<"PartitionKey">>, PartitionKey}, {<<"Data">>, base64:encode(Data)}, {<<"ExplicitHashKey">>, ExplicitHashKey}, {<<"SequenceNumberForOrdering">>, Ordering}],
  erlcloud_kinesis_impl:request(Config, "Kinesis_20131202.PutRecord", Json).


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

-spec merge_shards/3 :: (string(), string(), string()) -> proplist().

merge_shards(StreamName, AdjacentShardToMerge, ShardToMerge) ->
  Json = [{<<"StreamName">>, StreamName}, {<<"AdjacentShardToMerge">>, AdjacentShardToMerge}, {<<"ShardToMerge">>, ShardToMerge}],
  erlcloud_kinesis_impl:request(default_config(), "Kinesis_20131202.MergeShards", Json).

-spec merge_shards/4 :: (string(), string(), string(), aws_config()) -> proplist().

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

-spec split_shards/3 :: (string(), string(), string()) -> proplist().

split_shards(StreamName, ShardToSplit, NewStartingHashKey) ->
  Json = [{<<"StreamName">>, StreamName}, {<<"ShardToSplit">>, ShardToSplit}, {<<"NewStartingHashKey">>, NewStartingHashKey}],
  erlcloud_kinesis_impl:request(default_config(), "Kinesis_20131202.SplitShard", Json).

-spec split_shards/4 :: (string(), string(), string(), aws_config()) -> proplist().

split_shards(StreamName, ShardToSplit, NewStartingHashKey, Config) when is_record(Config, aws_config) ->
  Json = [{<<"StreamName">>, StreamName}, {<<"ShardToSplit">>, ShardToSplit}, {<<"NewStartingHashKey">>, NewStartingHashKey}],
  erlcloud_kinesis_impl:request(Config, "Kinesis_20131202.SplitShard", Json).