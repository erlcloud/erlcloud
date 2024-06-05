%-------------------------------------------------------------------
%%% Amazon Kinesis Service (Kinesis) - Firehose
%%% Created : 20. Mar 2017 10:39 AM
%%%-------------------------------------------------------------------
-module(erlcloud_firehose).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

-define(DEFAULT_CONFIG, erlcloud_aws:default_config()).

-define(OP_LIST_STREAMS,        "Firehose_20150804.ListDeliveryStreams").
-define(OP_DESCRIBE_STREAMS,    "Firehose_20150804.DescribeDeliveryStream").
-define(OP_PUT_ONE,             "Firehose_20150804.PutRecord").
-define(OP_PUT_BATCH,           "Firehose_20150804.PutRecordBatch").
-define(OP_UPDATE_DEST,         "Firehose_20150804.UpdateDestination").

-type get_records_limit() :: 1..10000.
-type payload()           :: binary() | string().

%% API
-export([configure/2,
         configure/3,
         configure/4,

         new/2,
         new/3]).

-export([list_streams/0,
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

         put_records/2,
         put_records/3,
         put_records/4,

         update_destination/2,
         update_destination/3,
         update_destination/4]).

-spec new(string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id = AccessKeyID,
                secret_access_key = SecretAccessKey}.

-spec new(string(), string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id = AccessKeyID,
                secret_access_key = SecretAccessKey,
                firehose_host = Host}.

-spec new(string(), string(), string(), non_neg_integer()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host, Port) ->
    #aws_config{access_key_id = AccessKeyID,
                secret_access_key = SecretAccessKey,
                firehose_host = Host,
                firehose_port = Port}.

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

-spec list_delivery_streams() -> erlcloud_kinesis_impl:json_return().
list_delivery_streams() ->
    list_delivery_streams(?DEFAULT_CONFIG).

-spec list_delivery_streams(string() | aws_config()) -> erlcloud_kinesis_impl:json_return().
list_delivery_streams(#aws_config{} = Config) ->
    erlcloud_kinesis_impl:request(firehose, Config, ?OP_LIST_STREAMS, []);
list_delivery_streams(ExclusiveStartStreamName) ->
    Json = [{<<"ExclusiveStartDeliveryStreamName">>, ExclusiveStartStreamName}],
    erlcloud_kinesis_impl:request(firehose, ?DEFAULT_CONFIG, ?OP_LIST_STREAMS, Json).

-spec list_delivery_streams(string(), 1..100 | aws_config()) -> erlcloud_kinesis_impl:json_return().
list_delivery_streams(ExclusiveStartStreamName, #aws_config{} = Config) ->
    Json = [{<<"ExclusiveStartDeliveryStreamName">>, ExclusiveStartStreamName}],
    erlcloud_kinesis_impl:request(firehose, Config, ?OP_LIST_STREAMS, Json);
list_delivery_streams(ExclusiveStartStreamName, Limit) when is_integer(Limit), Limit > 0, Limit =< 100 ->
    Json = [{<<"ExclusiveStartDeliveryStreamName">>, ExclusiveStartStreamName},
            {<<"Limit">>, Limit}],
    erlcloud_kinesis_impl:request(firehose, ?DEFAULT_CONFIG, ?OP_LIST_STREAMS, Json).

-spec list_delivery_streams(string(), 1..100, aws_config()) -> erlcloud_kinesis_impl:json_return().
list_delivery_streams(ExclusiveStartStreamName, Limit, #aws_config{} = Config)
  when is_integer(Limit)
       andalso Limit >= 1
       andalso Limit =< 100 ->
    Json = [{<<"ExclusiveStartDeliveryStreamName">>, ExclusiveStartStreamName},
            {<<"Limit">>, Limit}],
    erlcloud_kinesis_impl:request(firehose, Config, ?OP_LIST_STREAMS, Json).

-spec describe_delivery_stream(string()) -> erlcloud_kinesis_impl:json_return().
describe_delivery_stream(StreamName) ->
    describe_delivery_stream(StreamName, ?DEFAULT_CONFIG).

-spec describe_delivery_stream(string(), get_records_limit() | aws_config()) -> erlcloud_kinesis_impl:json_return().
describe_delivery_stream(StreamName, #aws_config{} = Config) ->
    Json = [{<<"DeliveryStreamName">>, StreamName}],
    erlcloud_kinesis_impl:request(firehose, Config, ?OP_DESCRIBE_STREAMS, Json);
describe_delivery_stream(StreamName, Limit) when is_integer(Limit) ->
    Json = [{<<"DeliveryStreamName">>, StreamName},
            {<<"limit">>, Limit}],
    erlcloud_kinesis_impl:request(firehose, ?DEFAULT_CONFIG, ?OP_DESCRIBE_STREAMS, Json).

-spec describe_delivery_stream(string(), get_records_limit(), string() | aws_config()) -> erlcloud_kinesis_impl:json_return().
describe_delivery_stream(StreamName, Limit, #aws_config{} = Config)
  when is_integer(Limit)
       andalso Limit >= 1
       andalso Limit =< 10000 ->
    Json = [{<<"DeliveryStreamName">>, StreamName},
            {<<"Limit">>, Limit}],
    erlcloud_kinesis_impl:request(firehose, Config, ?OP_DESCRIBE_STREAMS, Json);
describe_delivery_stream(StreamName, Limit, ExclusiveStartShardId) ->
    Json = [{<<"DeliveryStreamName">>, StreamName},
            {<<"ExclusiveStartShardId">>, ExclusiveStartShardId},
            {<<"limit">>, Limit}],
    erlcloud_kinesis_impl:request(firehose, ?DEFAULT_CONFIG, ?OP_DESCRIBE_STREAMS, Json).

-spec describe_delivery_stream(string(), get_records_limit(), string(), aws_config()) -> erlcloud_kinesis_impl:json_return().
describe_delivery_stream(StreamName, Limit, ExcludeStart, #aws_config{} = Config)
  when is_integer(Limit)
       andalso Limit >= 1
       andalso Limit =< 10000 ->
    Json = [{<<"DeliveryStreamName">>, StreamName},
            {<<"ExclusiveStartDestinationId">>, ExcludeStart},
            {<<"Limit">>, Limit}],
    erlcloud_kinesis_impl:request(firehose, Config, ?OP_DESCRIBE_STREAMS, Json).

-spec put_record(binary(), payload()) ->
                        {ok, proplist()} | {error, any()}.
put_record(StreamName, Data) ->
    put_record(StreamName, Data, [], ?DEFAULT_CONFIG).

-spec put_record(binary(), payload(), aws_config()) ->
                        {ok, proplist()} | {error, any()}.
put_record(StreamName, Data, #aws_config{} = Config) ->
    put_record(StreamName, Data, [], Config).

-spec put_record(binary(), payload(), proplist(), aws_config()) ->
                        {ok, proplist()} | {error, any()}.
put_record(StreamName, Data, Options, #aws_config{} = Config) ->
    Json = [{<<"DeliveryStreamName">>, StreamName},
            {<<"Record">>, [{<<"Data">>, encode(Options, Data)}]}],
    erlcloud_kinesis_impl:request(firehose, Config, ?OP_PUT_ONE, Json).

-spec put_records(binary(), payload()) ->
                              {ok, proplist()} | {error, any()}.
put_records(StreamName, Data) ->
    put_records(StreamName, Data, [], ?DEFAULT_CONFIG).

-spec put_records(binary(), payload(), aws_config()) ->
                              {ok, proplist()} | {error, any()}.
put_records(StreamName, Data, #aws_config{} = Config) ->
    put_records(StreamName, Data, [], Config).

-spec put_records(binary(), payload(), proplist(), aws_config()) ->
                              {ok, proplist()} | {error, any()}.
put_records(StreamName, Data, Options, #aws_config{} = Config) ->
    FormatData = [[{<<"Data">>, encode(Options, D)}] || D <- Data],
    Json = [{<<"DeliveryStreamName">>, StreamName},
            {<<"Records">>, FormatData}],
    erlcloud_kinesis_impl:request(firehose, Config, ?OP_PUT_BATCH, Json).

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
-spec update_destination(string(), list()) -> erlcloud_kinesis_impl:json_return().
update_destination(DeliveryStreamName, Options)
  when is_list(Options) ->
    update_destination(DeliveryStreamName, Options, ?DEFAULT_CONFIG).

-spec update_destination(string(), list(), aws_config()) -> erlcloud_kinesis_impl:json_return().
update_destination(DeliveryStreamName, Options, #aws_config{} = Config)
  when is_list(Options) ->
    {ok, StreamDetails} = describe_delivery_stream(DeliveryStreamName),
    Description = proplists:get_value(<<"DeliveryStreamDescription">>, StreamDetails),
    StreamName = proplists:get_value(<<"DeliveryStreamName">>, Description),
    Destinations = proplists:get_value(<<"Destinations">>, Description),
    First = hd(Destinations),
    DestinationId = proplists:get_value(<<"DestinationId">>, First),
    VersionId = proplists:get_value(<<"VersionId">>, Description),
    update_destination(VersionId, StreamName, DestinationId, Options, Config).

-spec update_destination(string(), string(), string(), list()) -> erlcloud_kinesis_impl:json_return().
update_destination(VersionId, DeliveryStreamName, DestinationId, Options)
  when is_list(Options) ->
    update_destination(VersionId, DeliveryStreamName, DestinationId, Options, ?DEFAULT_CONFIG).

-spec update_destination(string(), string(), string(), list(), aws_config()) -> erlcloud_kinesis_impl:json_return().
update_destination(VersionId, DeliveryStreamName, DestinationId, Options, #aws_config{} = Config)
  when is_list(Options) ->
    Json = [{<<"CurrentDeliveryStreamVersionId">>, VersionId},
            {<<"DeliveryStreamName">>, DeliveryStreamName},
            {<<"DestinationId">>, DestinationId} | Options],
    erlcloud_kinesis_impl:request(firehose, Config, ?OP_UPDATE_DEST, Json).

encode(Options, Data) ->
    case proplists:get_value(encode, Options, true) of
        true  -> base64:encode(Data);
        false -> Data
    end.
