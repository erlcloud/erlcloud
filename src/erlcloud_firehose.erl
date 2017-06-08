%%%-------------------------------------------------------------------
%%% Amazon Kinesis Service (Kinesis) - Firehose
%%% Created : 20. Mar 2017 10:39 AM
%%%-------------------------------------------------------------------
-module(erlcloud_firehose).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

-spec new(string(), string()) -> aws_config().
-type get_records_limit() :: 1..10000.
-type payload()           :: binary() | string().

%% API
-export([
  configure/2,
  configure/3,
  configure/4,
  new/2,
  new/3
]).

-export([
  list_streams/0,
  list_streams/1,
  describe_stream/1,
  get_shard_iterator/3,
  list_delivery_streams/0,
  list_delivery_streams/1,
  list_delivery_streams/2,
  list_delivery_streams/3,
  describe_delivery_stream/1,
  describe_delivery_stream/2,
  describe_delivery_stream/3,
  describe_delivery_stream/4,
  put_record/2,
  put_record/3,
  put_record/4,
  put_record_batch/2,
  put_record_batch/3,
  put_record_batch/4,
  update_destination/2,
  update_destination/3,
  update_destination/4
]).

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
    firehose_host=Host
  }.


-spec new(string(), string(), string(), non_neg_integer()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host, Port) ->
  #aws_config{
    access_key_id=AccessKeyID,
    secret_access_key=SecretAccessKey,
    firehose_host=Host,
    firehose_port=Port
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

list_streams() ->
  erlcloud_kinesis:list_streams().

list_streams(Criteria) ->
  erlcloud_kinesis:list_streams(Criteria).

describe_stream(StreamName) ->
  erlcloud_kinesis:describe_stream(StreamName).

get_shard_iterator(StreamName, ShardId, ShardIteratorType) ->
  erlcloud_kinesis:get_shard_iterator(StreamName, ShardId, ShardIteratorType).

-spec list_delivery_streams() -> proplist().
list_delivery_streams() ->
  list_delivery_streams(erlcloud_aws:default_config()).

-spec list_delivery_streams(string() | aws_config()) -> proplist().
list_delivery_streams(Config) when is_record(Config, aws_config) ->
  erlcloud_kinesis_impl:request(firehose, Config, "Firehose_20150804.ListDeliveryStreams", []);
list_delivery_streams(ExclusiveStartStreamName) ->
  Json = [{<<"ExclusiveStartDeliveryStreamName">>, ExclusiveStartStreamName}],
  erlcloud_kinesis_impl:request(firehose, erlcloud_aws:default_config(), "Firehose_20150804.ListDeliveryStreams", Json).

-spec list_delivery_streams(string(), 1..100 | aws_config()) -> proplist().
list_delivery_streams(ExclusiveStartStreamName, Config) when is_record(Config, aws_config) ->
  Json = [{<<"ExclusiveStartDeliveryStreamName">>, ExclusiveStartStreamName}],
  erlcloud_kinesis_impl:request(firehose, Config, "Firehose_20150804.ListDeliveryStreams", Json);
list_delivery_streams(ExclusiveStartStreamName, Limit) when is_integer(Limit), Limit > 0, Limit =< 100 ->
  Json = [{<<"ExclusiveStartDeliveryStreamName">>, ExclusiveStartStreamName}, {<<"Limit">>, Limit}],
  erlcloud_kinesis_impl:request(firehose, erlcloud_aws:default_config(), "Firehose_20150804.ListDeliveryStreams", Json).

-spec list_delivery_streams(string(), 1..100, aws_config()) -> proplist().
list_delivery_streams(ExclusiveStartStreamName, Limit, Config) when is_record(Config, aws_config), is_integer(Limit), Limit > 0, Limit =< 100 ->
  Json = [{<<"ExclusiveStartDeliveryStreamName">>, ExclusiveStartStreamName}, {<<"Limit">>, Limit}],
  erlcloud_kinesis_impl:request(firehose, Config, "Firehose_20150804.ListDeliveryStreams", Json).

-spec describe_delivery_stream(string()) -> proplist().
describe_delivery_stream(StreamName) ->
  describe_delivery_stream(StreamName, erlcloud_aws:default_config()).

-spec describe_delivery_stream(string(), get_records_limit() | aws_config()) -> proplist().
describe_delivery_stream(StreamName, Config) when is_record(Config, aws_config) ->
  Json = [{<<"DeliveryStreamName">>, StreamName}],
  erlcloud_kinesis_impl:request(firehose, Config, "Firehose_20150804.DescribeDeliveryStream", Json);
describe_delivery_stream(StreamName, Limit) ->
  erlcloud_kinesis_impl:request(firehose, StreamName, Limit, erlcloud_aws:default_config()).

-spec describe_delivery_stream(string(), get_records_limit(), string() | aws_config()) -> proplist().
describe_delivery_stream(StreamName, Limit, Config)
  when is_record(Config, aws_config),
  is_integer(Limit),
  Limit >= 1, Limit =< 10000 ->
  Json = [{<<"DeliveryStreamName">>, StreamName}, {<<"Limit">>, Limit}],
  erlcloud_kinesis_impl:request(firehose, Config, "Firehose_20150804.DescribeDeliveryStream", Json);
describe_delivery_stream(StreamName, Limit, ExcludeShard) ->
  erlcloud_kinesis_impl:request(firehose, StreamName, Limit, ExcludeShard, erlcloud_aws:default_config()).

-spec describe_delivery_stream(string(), get_records_limit(), string(), aws_config()) -> proplist().
describe_delivery_stream(StreamName, Limit, ExcludeStart, Config)
  when is_record(Config, aws_config),
  is_integer(Limit),
  Limit >= 1, Limit =< 10000 ->
  Json = [{<<"DeliveryStreamName">>, StreamName}, {<<"Limit">>, Limit}, {<<"ExclusiveStartDestinationId">>, ExcludeStart}],
  erlcloud_kinesis_impl:request(firehose, Config, "Firehose_20150804.DescribeDeliveryStream", Json).

-spec put_record(binary(), payload()) ->
  {ok, proplist()} | {error, any()}.
put_record(StreamName, Data) ->
  put_record(StreamName, Data, [], erlcloud_aws:default_config()).

-spec put_record(binary(), payload(), aws_config()) ->
  {ok, proplist()} | {error, any()}.
put_record(StreamName, Data, Config) when is_record(Config, aws_config) ->
  put_record(StreamName, Data, [], Config).

-spec put_record(binary(), payload(), proplist(), aws_config()) ->
  {ok, proplist()} | {error, any()}.
put_record(StreamName, Data, Options, Config) when is_record(Config, aws_config) ->
  Encoded  = case proplists:get_value(encode, Options, true) of
               true  -> base64:encode(Data);
               false -> Data
             end,

  Json = [{<<"DeliveryStreamName">>, StreamName},
    {<<"Record">>, [{<<"Data">>, Encoded}]}],

  erlcloud_kinesis_impl:request(firehose, Config, "Firehose_20150804.PutRecord", Json).

-spec put_record_batch(binary(), payload()) ->
  {ok, proplist()} | {error, any()}.
put_record_batch(StreamName, Data) ->
  put_record_batch(StreamName, Data, [], erlcloud_aws:default_config()).

-spec put_record_batch(binary(), payload(), aws_config()) ->
  {ok, proplist()} | {error, any()}.
put_record_batch(StreamName, Data, Config) when is_record(Config, aws_config) ->
  put_record_batch(StreamName, Data, [], Config).

-spec put_record_batch(binary(), payload(), proplist(), aws_config()) ->
  {ok, proplist()} | {error, any()}.
put_record_batch(StreamName, Data, Options, Config) when is_record(Config, aws_config) ->
  FormatData=[[{<<"Data">>, encode(Options, D)}] || D <- Data],
%%  [{<<"DeliveryStreamName">>, StreamName},{<<"Records">>,[[{#Bin<4>,#Bin<4>}],[{#Bin<4>,#Bin<4>}]]}]
  Json = [{<<"DeliveryStreamName">>, StreamName},
    {<<"Records">>, FormatData}],
  erlcloud_kinesis_impl:request(firehose, Config, "Firehose_20150804.PutRecordBatch", Json).

%%------------------------------------------------------------------------------
%% @doc
%% Kinesis API:
%% [http://docs.aws.amazon.com/firehose/latest/APIReference/API_UpdateDestination.html]
%%
%% Updates the specified destination of the specified delivery stream.
%%
%% erlcloud_firehose:update_destination(<<"stream_name">>, [
%%     {<<"RedshiftDestinationUpdate">>, [
%%         {<<"CopyCommand">>, [
%%             {<<"CopyOptions">>, <<"timeformat as 'epochmillisecs' json 's3://json/jsonpath.json'">>},
%%             {<<"DataTableColumns">>, <<"column_one, column_two, column_three">>},
%%             {<<"DataTableName">>,<<"preprocess_t">>}
%%         ]}
%%      ]}
%%    ]).
%%
%% @end
%%------------------------------------------------------------------------------
update_destination(DeliveryStreamName, Options) ->
  update_destination(DeliveryStreamName, Options, erlcloud_aws:default_config()).
update_destination(DeliveryStreamName, Options, Config) ->
  {ok, StreamDetails}=describe_delivery_stream(DeliveryStreamName),

  Description=proplists:get_value(<<"DeliveryStreamDescription">>, StreamDetails),
  StreamName=proplists:get_value(<<"DeliveryStreamName">>, Description),
  Destinations=proplists:get_value(<<"Destinations">>, Description),
  First=hd(Destinations),
  DestionationId=proplists:get_value(<<"DestinationId">>, First),
  VersionId=proplists:get_value(<<"VersionId">>, Description),

  update_destination(VersionId, StreamName, DestionationId, Options, Config).

update_destination(VersionId, DeliveryStreamName, DestionationId, Options) ->
  update_destination(VersionId, DeliveryStreamName, DestionationId, Options, erlcloud_aws:default_config()).
update_destination(VersionId, DeliveryStreamName, DestionationId, Options, Config) ->
  Json = [{<<"CurrentDeliveryStreamVersionId">>, VersionId},
    {<<"DeliveryStreamName">>, DeliveryStreamName},
    {<<"DestinationId">>, DestionationId}],

  JsonWOptions=Json ++ Options,

  erlcloud_kinesis_impl:request(firehose, Config, "Firehose_20150804.UpdateDestination", JsonWOptions).

encode(Options, Data) ->
  case proplists:get_value(encode, Options, true) of
    true  -> base64:encode(Data);
    false -> Data
  end.
