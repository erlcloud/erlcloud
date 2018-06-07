%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_kinesis_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").
%%-include("erlcloud_kinesis.hrl").

%% Unit tests for kinesis.
%% These tests work by using meck to mock erlcloud_httpc. There are two classes of test: input and output.
%%
%% Input tests verify that different function args produce the desired JSON request.
%% An input test list provides a list of funs and the JSON that is expected to result.
%%
%% Output tests verify that the http response produces the correct return from the fun.
%% An output test lists provides a list of response bodies and the expected return.

%% The _ddb_test macro provides line number annotation to a test, similar to _test, but doesn't wrap in a fun
-define(_kinesis_test(T), {?LINE, T}).
%% The _f macro is a terse way to wrap code in a fun. Similar to _test but doesn't annotate with a line number
-define(_f(F), fun() -> F end).

-export([validate_body/2]).

%%%===================================================================
%%% Test entry points
%%%===================================================================

operation_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun create_stream_input_tests/1,
      fun create_stream_output_tests/1,
      fun delete_stream_input_tests/1,
      fun delete_stream_output_tests/1,
      fun list_streams_input_tests/1,
      fun list_streams_output_tests/1,
      fun describe_stream_input_tests/1,
      fun describe_stream_output_tests/1,
      fun describe_stream_summary_input_tests/1,
      fun describe_stream_summary_output_tests/1,
      fun enable_enchanced_monitoring_input_tests/1,
      fun enable_enchanced_monitoring_output_tests/1,
      fun disable_enchanced_monitoring_input_tests/1,
      fun disable_enchanced_monitoring_output_tests/1,
      fun get_shard_iterator_input_tests/1,
      fun get_shard_iterator_output_tests/1,
      fun get_records_input_tests/1,
      fun get_records_output_tests/1,
      fun get_records_no_decode_output_tests/1,
      fun put_record_input_tests/1,
      fun put_record_output_tests/1,
      fun put_records_input_tests/1,
      fun put_records_output_tests/1,
      fun merge_shards_input_tests/1,
      fun merge_shards_output_tests/1,
      fun split_shards_input_tests/1,
      fun split_shards_output_tests/1,
      fun add_tags_to_stream_input_tests/1,
      fun add_tags_to_stream_output_tests/1,
      fun list_tags_for_stream_input_tests/1,
      fun list_tags_for_stream_output_tests/1,
      fun list_all_tags_for_stream_input_tests/1,
      fun list_all_tags_for_stream_output_test/1,
      fun remove_tags_from_stream_input_tests/1,
      fun remove_tags_from_stream_output_tests/1
     ]}.

start() ->
    meck:new(erlcloud_httpc),
    ok.

stop(_) ->
    meck:unload(erlcloud_httpc).

%%%===================================================================
%%% Input test helpers
%%%===================================================================

config() ->
    #aws_config{
      access_key_id = string:copies("A", 20),
      secret_access_key = string:copies("a", 40)}.

-type expected_body() :: string().

sort_json([{_, _} | _] = Json) ->
    %% Value is an object
    SortedChildren = [{K, sort_json(V)} || {K,V} <- Json],
    lists:keysort(1, SortedChildren);
sort_json([_|_] = Json) ->
    %% Value is an array
    [sort_json(I) || I <- Json];
sort_json(V) ->
    V.

%% verifies that the parameters in the body match the expected parameters
-spec validate_body(binary(), expected_body()) -> ok.
validate_body(Body, Expected) ->
    Want = sort_json(jsx:decode(list_to_binary(Expected))),
    Actual = sort_json(jsx:decode(Body)),
    case Want =:= Actual of
        true -> ok;
        false ->
            ?debugFmt("~nEXPECTED~n~p~nACTUAL~n~p~n", [Want, Actual])
    end,
    ?assertEqual(Want, Actual).

%% returns the mock of the erlcloud_httpc function input tests expect to be called.
%% Validates the request body and responds with the provided response.
-spec input_expect(string(), expected_body()) -> fun().
input_expect(Response, Expected) ->
    fun(_Url, post, _Headers, Body, _Timeout, _Config) ->
            validate_body(Body, Expected),
            {ok, {{200, "OK"}, [], list_to_binary(Response)}}
    end.

%% input_test converts an input_test specifier into an eunit test generator
-type input_test_spec() :: {pos_integer(), {fun(), expected_body()} | {string(), fun(), expected_body()}}.
-spec input_test(string(), input_test_spec()) -> tuple().
input_test(Response, {Line, {Description, Fun, Expected}}) when
      is_list(Description) ->
    {Description,
     {Line,
      fun() ->
              meck:expect(erlcloud_httpc, request, input_expect(Response, Expected)),
              erlcloud_kinesis:configure(string:copies("A", 20), string:copies("a", 40)),
              Fun()
      end}}.
%% input_test(Response, {Line, {Fun, Params}}) ->
%%     input_test(Response, {Line, {"", Fun, Params}}).

%% input_tests converts a list of input_test specifiers into an eunit test generator
-spec input_tests(string(), [input_test_spec()]) -> [tuple()].
input_tests(Response, Tests) ->
    [input_test(Response, Test) || Test <- Tests].

%%%===================================================================
%%% Output test helpers
%%%===================================================================

%% returns the mock of the erlcloud_httpc function output tests expect to be called.
-spec output_expect(string()) -> fun().
output_expect(Response) ->
    fun(_Url, post, _Headers, _Body, _Timeout, _Config) ->
            {ok, {{200, "OK"}, [], list_to_binary(Response)}}
    end.

%% output_test converts an output_test specifier into an eunit test generator
-type output_test_spec() :: {pos_integer(), {string(), term()} | {string(), string(), term()}}.
-spec output_test(fun(), output_test_spec()) -> tuple().
output_test(Fun, {Line, {Description, Response, Result}}) ->
    {Description,
     {Line,
      fun() ->
              meck:expect(erlcloud_httpc, request, output_expect(Response)),
              erlcloud_kinesis:configure(string:copies("A", 20), string:copies("a", 40)),
              Actual = Fun(),
              case Result =:= Actual of
                  true -> ok;
                  false ->
                      ?debugFmt("~nEXPECTED~n~p~nACTUAL~n~p~n", [Result, Actual])
              end,
              ?assertEqual(Result, Actual)
      end}}.
%% output_test(Fun, {Line, {Response, Result}}) ->
%%     output_test(Fun, {Line, {"", Response, Result}}).

%% output_tests converts a list of output_test specifiers into an eunit test generator
-spec output_tests(fun(), [output_test_spec()]) -> [term()].
output_tests(Fun, Tests) ->
    [output_test(Fun, Test) || Test <- Tests].


%% CreateStream test based on the API examples:
%% http://docs.aws.amazon.com/kinesis/latest/APIReference/API_CreateStream.html
create_stream_input_tests(_) ->
    Tests =
        [?_kinesis_test(
            {"CreateStream example request",
             ?_f(erlcloud_kinesis:create_stream(<<"test">>, 3)), "
{
    \"StreamName\": \"test\",
    \"ShardCount\": 3
}"
            })
        ],

    Response = "{}",
    input_tests(Response, Tests).

create_stream_output_tests(_) ->
    Tests =
        [?_kinesis_test(
            {"CreateStream example response", "{}",
             {ok, jsx:decode(<<"{}">>)}})
        ],

    output_tests(?_f(erlcloud_kinesis:create_stream(<<"streamName">>, 2)), Tests).

%% DeleteStream test based on the API examples:
%% http://docs.aws.amazon.com/kinesis/latest/APIReference/API_DeleteStream.html
delete_stream_input_tests(_) ->
    Tests =
        [?_kinesis_test(
            {"DeleteStream example request",
             ?_f(erlcloud_kinesis:delete_stream(<<"test">>)), "
{
    \"StreamName\": \"test\"
}"
            })
        ],

    Response = "{}",
    input_tests(Response, Tests).

delete_stream_output_tests(_) ->
    Tests =
        [?_kinesis_test(
            {"DeleteStream example response", "{}",
             {ok, jsx:decode(<<"{}">>)}})
        ],

    output_tests(?_f(erlcloud_kinesis:delete_stream(<<"streamName">>)), Tests).

%% ListStreams test based on the API examples:
%% http://docs.aws.amazon.com/kinesis/latest/APIReference/API_ListStreams.html
list_streams_input_tests(_) ->
    Tests =
        [?_kinesis_test(
            {"ListStreams example request",
             ?_f(erlcloud_kinesis:list_streams()), "{}"
            })
        ],

    Response = "{
        \"HasMoreStreams\": false,
        \"StreamNames\": [
          \"streamName\"
        ]
    }",
    input_tests(Response, Tests).

list_streams_output_tests(_) ->
    Tests =
        [?_kinesis_test(
            {"ListStreams example response", "
{
    \"HasMoreStreams\": false,
    \"StreamNames\": [
      \"streamName\"
    ]
}",
             {ok,[{<<"HasMoreStreams">>,false},{<<"StreamNames">>,[<<"streamName">>]}]}}),
        ?_kinesis_test(
            {"ListStreams example response", "
{
    \"HasMoreStreams\": false,
    \"StreamNames\": [
      \"streamName\",
      \"test\"
    ]
}",
             {ok,[{<<"HasMoreStreams">>,false},{<<"StreamNames">>,[<<"streamName">>, <<"test">>]}]}})
        ],

    output_tests(?_f(erlcloud_kinesis:list_streams()), Tests).

%% DescribeStream test based on the API examples:
%% http://docs.aws.amazon.com/kinesis/latest/APIReference/API_DescribeStream.html
describe_stream_input_tests(_) ->
    Tests =
        [?_kinesis_test(
            {"DescribeStream example request",
             ?_f(erlcloud_kinesis:describe_stream(<<"test">>)), "
{
  \"StreamName\": \"test\"
}"
            })
        ],

    Response = "
{
\"StreamDescription\": {
    \"HasMoreShards\": false,
    \"Shards\": [
        {
            \"HashKeyRange\": {
                \"EndingHashKey\": \"340282366920938463463374607431768211455\",
                \"StartingHashKey\": \"49537264748575863163933038815938617127259750\"
            },
            \"SequenceNumberRange\": {
                \"EndingSequenceNumber\": \"340282366920938463463374607431768211455\",
                \"StartingSequenceNumber\": \"49537264748575863163933038815938617127259750\"
            },
            \"ShardId\": \"shardId-000000000000\"
        }
    ],
    \"StreamARN\": \"arn:aws:kinesis:us-east-1:821148768124:stream/test\",
    \"StreamName\": \"test\",
    \"StreamStatus\": \"ACTIVE\"
}
}",
    input_tests(Response, Tests).

describe_stream_output_tests(_) ->
    Tests =
        [?_kinesis_test(
            {"DescribeStream example response", "
{
\"StreamDescription\": {
    \"HasMoreShards\": false,
    \"Shards\": [
        {
            \"HashKeyRange\": {
                \"EndingHashKey\": \"170141183460469231731687303715884105727\",
                \"StartingHashKey\": \"0\"
            },
            \"SequenceNumberRange\": {
                \"StartingSequenceNumber\": \"495372647485535624187345081927970814089871018992\"
            },
            \"ShardId\": \"shardId-000000000000\"
        },
        {
            \"HashKeyRange\": {
                \"EndingHashKey\": \"340282366920938463463374607431768211455\",
                \"StartingHashKey\": \"170141183460469231731687303715884105728\"
            },
            \"SequenceNumberRange\": {
                \"StartingSequenceNumber\": \"49537264748575863163933038815938617127259750\"
            },
            \"ShardId\": \"shardId-000000000001\"
        }
    ],
    \"StreamARN\": \"arn:aws:kinesis:us-east-1:821148768124:stream/staging\",
    \"StreamName\": \"staging\",
    \"StreamStatus\": \"ACTIVE\"
}
}",
        {ok,[{<<"StreamDescription">>,
    [{<<"HasMoreShards">>,false},
     {<<"Shards">>,
      [[{<<"HashKeyRange">>,
         [{<<"EndingHashKey">>,
           <<"170141183460469231731687303715884105727">>},
          {<<"StartingHashKey">>,<<"0">>}]},
        {<<"SequenceNumberRange">>,
         [{<<"StartingSequenceNumber">>,
           <<"495372647485535624187345081927970814089871018992">>}]},
        {<<"ShardId">>,<<"shardId-000000000000">>}],
       [{<<"HashKeyRange">>,
         [{<<"EndingHashKey">>,
           <<"340282366920938463463374607431768211455">>},
          {<<"StartingHashKey">>,
           <<"170141183460469231731687303715884105728">>}]},
        {<<"SequenceNumberRange">>,
         [{<<"StartingSequenceNumber">>,
           <<"49537264748575863163933038815938617127259750">>}]},
        {<<"ShardId">>,<<"shardId-000000000001">>}]]},
     {<<"StreamARN">>,
      <<"arn:aws:kinesis:us-east-1:821148768124:stream/staging">>},
     {<<"StreamName">>,<<"staging">>},
     {<<"StreamStatus">>,<<"ACTIVE">>}]}]}
        })
        ],

    output_tests(?_f(erlcloud_kinesis:describe_stream(<<"staging">>)), Tests).

%% DescribeStream test based on the API examples:
%% http://docs.aws.amazon.com/kinesis/latest/APIReference/API_DescribeStreamSummary.html
describe_stream_summary_input_tests(_) ->
  Tests =
    [?_kinesis_test(
      {"DescribeStream example request",
        ?_f(erlcloud_kinesis:describe_stream_summary(<<"test">>)), "
{
  \"StreamName\": \"test\"
}"
      })
    ],

  Response = "
{
\"StreamDescription\": {
    \"EncryptionType\": \"NONE\",
    \"EnhancedMonitoring\": [
        {
            \"ShardLevelMetrics\": [ \"\OutgoingRecords\", \"IteratorAgeMilliseconds\", \"\IncomingRecords\"]
        }
    ],
    \"OpenShardCount\": 10,
    \"RetentionPeriodHours\": 24,
    \"StreamARN\": \"arn:aws:kinesis:us-east-1:821148768124:stream/staging\",
    \"StreamCreationTimestamp\": 1522843376,
    \"StreamName\": \"staging\",
    \"StreamStatus\": \"ACTIVE\"
}
}",
  input_tests(Response, Tests).

describe_stream_summary_output_tests(_) ->
  Tests =
    [?_kinesis_test(
      {"DescribeStream example response", "
{
\"StreamDescriptionSummary\": {
    \"EncryptionType\": \"NONE\",
    \"EnhancedMonitoring\": [
        {
            \"ShardLevelMetrics\": [ \"\OutgoingRecords\", \"IteratorAgeMilliseconds\", \"\IncomingRecords\"]
        }
    ],
    \"OpenShardCount\": 10,
    \"RetentionPeriodHours\": 24,
    \"StreamARN\": \"arn:aws:kinesis:us-west-2:821148768124:stream/test\",
    \"StreamCreationTimestamp\": 1522843376,
    \"StreamName\": \"test\",
    \"StreamStatus\": \"ACTIVE\"
}
}",
        {ok,[{<<"StreamDescriptionSummary">>,
          [{<<"EncryptionType">>,<<"NONE">>},
            {<<"EnhancedMonitoring">>,
              [[{<<"ShardLevelMetrics">>,
                [<<"OutgoingRecords">>,<<"IteratorAgeMilliseconds">>,
                  <<"IncomingRecords">>]}]]},
            {<<"OpenShardCount">>, 10},
            {<<"RetentionPeriodHours">>, 24},
            {<<"StreamARN">>,
              <<"arn:aws:kinesis:us-west-2:821148768124:stream/test">>},
            {<<"StreamCreationTimestamp">>, 1522843376},
            {<<"StreamName">>,<<"test">>},
            {<<"StreamStatus">>,<<"ACTIVE">>}]}]}
      })
    ],

  output_tests(?_f(erlcloud_kinesis:describe_stream_summary(<<"staging">>)), Tests).

%% EnableEnhancedMonitoring test based on the API examples:
%% http://docs.aws.amazon.com/kinesis/latest/APIReference/API_EnableEnhancedMonitoring.html
enable_enchanced_monitoring_input_tests(_) ->
  Stream = <<"test">>,
  Metric = [<<"IncomingBytes">>],
  Tests =
    [?_kinesis_test(
      {"EnableEnhancedMonitoring example request for ALL",
        ?_f(erlcloud_kinesis:enable_enhanced_monitoring(Stream)),
        "{\"StreamName\": \"test\",
          \"ShardLevelMetrics\": [ \"ALL\"]
         }"}
      ),
      ?_kinesis_test(
        {"EnableEnhancedMonitoring example request for specific metric",
          ?_f(erlcloud_kinesis:enable_enhanced_monitoring(Stream, Metric)),
          "{\"StreamName\": \"test\",
            \"ShardLevelMetrics\": [ \"IncomingBytes\"]
           }"}
      )
    ],
  Response = "{
    \"CurrentShardLevelMetrics\":[],
    \"DesiredShardLevelMetrics\":[\"IncomingBytes\",\"OutgoingRecords\",\"IteratorAgeMilliseconds\",
        \"IncomingRecords\",\"ReadProvisionedThroughputExceeded\",
        \"WriteProvisionedThroughputExceeded\",\"OutgoingBytes\"
    ],\"StreamName\":\"test\"}",
  input_tests(Response, Tests).

enable_enchanced_monitoring_output_tests(_) ->
  Tests =
    [?_kinesis_test(
      {"EnableEnhancedMonitoring example response", "
        {\"CurrentShardLevelMetrics\":[],
        \"DesiredShardLevelMetrics\":[\"IncomingBytes\",\"OutgoingRecords\",\"IteratorAgeMilliseconds\",\"IncomingRecords\",
            \"ReadProvisionedThroughputExceeded\",\"WriteProvisionedThroughputExceeded\",\"OutgoingBytes\"
        ],\"StreamName\":\"test\"}",
        {ok,[{<<"CurrentShardLevelMetrics">>, []},
          {<<"DesiredShardLevelMetrics">>,[
            <<"IncomingBytes">>,<<"OutgoingRecords">>,
            <<"IteratorAgeMilliseconds">>,<<"IncomingRecords">>,
            <<"ReadProvisionedThroughputExceeded">>,
            <<"WriteProvisionedThroughputExceeded">>,
            <<"OutgoingBytes">>]},
          {<<"StreamName">>,<<"test">>}]}
      })
    ],
  output_tests(?_f(erlcloud_kinesis:enable_enhanced_monitoring(<<"test">>)), Tests).

%% DisableEnhancedMonitoring test based on the API examples:
%% https://docs.aws.amazon.com/kinesis/latest/APIReference/API_DisableEnhancedMonitoring.html
disable_enchanced_monitoring_input_tests(_) ->
  Stream = <<"test">>,
  Metric = [<<"IncomingBytes">>],
  Tests =
    [?_kinesis_test(
      {"DisableEnhancedMonitoring example request for ALL",
        ?_f(erlcloud_kinesis:disable_enhanced_monitoring(Stream)),
        "{\"StreamName\": \"test\",
          \"ShardLevelMetrics\": [ \"ALL\"]
         }"}
    ),
      ?_kinesis_test(
        {"DisableEnhancedMonitoring example for specific",
          ?_f(erlcloud_kinesis:disable_enhanced_monitoring(Stream, Metric)),
          "{\"StreamName\": \"test\",
            \"ShardLevelMetrics\": [ \"IncomingBytes\"]
           }"}
      )
    ],
  Response = "{
    \"CurrentShardLevelMetrics\":[\"IncomingBytes\",\"OutgoingRecords\",\"IteratorAgeMilliseconds\",
        \"IncomingRecords\",\"ReadProvisionedThroughputExceeded\",
        \"WriteProvisionedThroughputExceeded\",\"OutgoingBytes\"
    ],
    \"DesiredShardLevelMetrics\":[],\"StreamName\":\"test\"}",
  input_tests(Response, Tests).

disable_enchanced_monitoring_output_tests(_) ->
  Tests =
    [?_kinesis_test(
      {"DisableEnhancedMonitoring example response", "
        {\"CurrentShardLevelMetrics\":[
            \"IncomingBytes\",\"OutgoingRecords\",\"IteratorAgeMilliseconds\",\"IncomingRecords\",
            \"ReadProvisionedThroughputExceeded\",\"WriteProvisionedThroughputExceeded\",\"OutgoingBytes\"
        ],
        \"DesiredShardLevelMetrics\":[],\"StreamName\":\"test\"}",
        {ok,[{<<"CurrentShardLevelMetrics">>, [
            <<"IncomingBytes">>,<<"OutgoingRecords">>,
            <<"IteratorAgeMilliseconds">>,<<"IncomingRecords">>,
            <<"ReadProvisionedThroughputExceeded">>,
            <<"WriteProvisionedThroughputExceeded">>,
            <<"OutgoingBytes">>]},
          {<<"DesiredShardLevelMetrics">>,[]},
          {<<"StreamName">>,<<"test">>}]}
      })
    ],
  output_tests(?_f(erlcloud_kinesis:disable_enhanced_monitoring(<<"test">>, [<<"ALL">>])), Tests).

%% GetShardIterator test based on the API examples:
%% http://docs.aws.amazon.com/kinesis/latest/APIReference/API_GetShardIterator.html
get_shard_iterator_input_tests(_) ->
    Stream = <<"test">>,
    Shard = <<"shardId-000000000000">>,
    Tests =
        [?_kinesis_test(
            {"GetShardIterator example request for TRIM_HORIZON",
             ?_f(erlcloud_kinesis:get_shard_iterator(Stream,
                                                     Shard,
                                                     <<"TRIM_HORIZON">>)),
             "{\"StreamName\": \"test\","
              "\"ShardId\": \"shardId-000000000000\","
              "\"ShardIteratorType\": \"TRIM_HORIZON\"}"}
        ),
         ?_kinesis_test(
             {"GetShardIterator example request for AT_TIMESTAMP",
              ?_f(erlcloud_kinesis:get_shard_iterator(Stream,
                                                      Shard,
                                                      <<"AT_TIMESTAMP">>,
                                                      1499263032)),
              "{\"StreamName\": \"test\","
               "\"ShardId\": \"shardId-000000000000\","
               "\"ShardIteratorType\": \"AT_TIMESTAMP\","
               "\"Timestamp\":1499263032}"}
         ),
         ?_kinesis_test(
             {"GetShardIterator example request for AT_SEQUENCE_NUMBER",
              ?_f(erlcloud_kinesis:get_shard_iterator(Stream,
                                                      Shard,
                                                      <<"AT_SEQUENCE_NUMBER">>,
                                                      <<"123">>)),
              "{\"StreamName\": \"test\","
               "\"ShardId\": \"shardId-000000000000\","
               "\"ShardIteratorType\": \"AT_SEQUENCE_NUMBER\","
               "\"StartingSequenceNumber\":\"123\"}"}
         )
        ],
    Response = "{\"ShardIterator\": \"AAAAAAAAAAFHJejL6/AjDShV3pIXsxYZT7Xj2G6EHxokHqT2D1stIOVYUEyprlUGWUepKqUDaR0+hB6qTlKvZa+fsBRqgHi4\"}",
    input_tests(Response, Tests).

get_shard_iterator_output_tests(_) ->
    Tests =
        [?_kinesis_test(
            {"GetShardIterator example response", "
{
  \"ShardIterator\": \"AAAAAAAAAAFHJejL6/AjDShV3pIXsxYZT7Xj2G6EHxokHqT2D1stIOVYUEyprlUGWUepKqUDaR0+hB6qTlKvZa+fsBRqgHi4\"
}",
        {ok,[{<<"ShardIterator">>,
    <<"AAAAAAAAAAFHJejL6/AjDShV3pIXsxYZT7Xj2G6EHxokHqT2D1stIOVYUEyprlUGWUepKqUDaR0+hB6qTlKvZa+fsBRqgHi4">>}]}
        })
        ],

    output_tests(?_f(erlcloud_kinesis:get_shard_iterator(<<"test">>, <<"shardId-000000000000">>, <<"TRIM_HORIZON">>)), Tests).


%% GetRecords test based on the API examples:
%% http://docs.aws.amazon.com/kinesis/latest/APIReference/API_GetRecords.html
get_records_input_tests(_) ->
    Tests =
        [?_kinesis_test(
            {"GetRecords example request",
             ?_f(erlcloud_kinesis:get_records(<<"AAAAAAAAAAEuncwaAk+GTC2TIdmdg5w6dIuZ4Scu6vaMGPtaPUfopvw9cBm2NM3Rlj9WyI5JFJr2ahuSh3Z187AdW4Lug86E">>)), "
{
  \"ShardIterator\": \"AAAAAAAAAAEuncwaAk+GTC2TIdmdg5w6dIuZ4Scu6vaMGPtaPUfopvw9cBm2NM3Rlj9WyI5JFJr2ahuSh3Z187AdW4Lug86E\"
}"
            })
        ],

    Response = "
{
    \"NextShardIterator\": \"AAAAAAAAAAEkuCmrC+QDW1gUywyu7G8GxvRyM6GSMkcHQ9wrvCJBW87mjn9C8YEckkipaoJySwgKXMmn1BwSPjnjiUCsu6pc\",
    \"Records\": [
        {
            \"Data\": \"YXNkYXNk\",
            \"PartitionKey\": \"key\",
            \"SequenceNumber\": \"49537292605574028653758531131893428543501381406818304001\"
        },
        {
            \"Data\": \"YXNkYXNkIDIxMzEyMzEyMw==\",
            \"PartitionKey\": \"key\",
            \"SequenceNumber\": \"49537292605574028653758541428570459745183078607853977601\"
        }
    ]
}",
    input_tests(Response, Tests).

get_records_output_tests(_) ->
    Tests =
        [?_kinesis_test(
            {"GetRecords example response", "
{
    \"NextShardIterator\": \"AAAAAAAAAAEkuCmrC+QDW1gUywyu7G8GxvRyM6GSMkcHQ9wrvCJBW87mjn9C8YEckkipaoJySwgKXMmn1BwSPjnjiUCsu6pc\",
    \"Records\": [
        {
            \"Data\": \"YXNkYXNk\",
            \"PartitionKey\": \"key\",
            \"SequenceNumber\": \"49537292605574028653758531131893428543501381406818304001\"
        },
        {
            \"Data\": \"YXNkYXNkIDIxMzEyMzEyMw==\",
            \"PartitionKey\": \"key\",
            \"SequenceNumber\": \"49537292605574028653758541428570459745183078607853977601\"
        }
    ]
}",
        {ok,[{<<"NextShardIterator">>,
      <<"AAAAAAAAAAEkuCmrC+QDW1gUywyu7G8GxvRyM6GSMkcHQ9wrvCJBW87mjn9C8YEckkipaoJySwgKXMmn1BwSPjnjiUCsu6pc">>},
    {<<"Records">>,
      [[{<<"Data">>,<<"asdasd">>},
        {<<"PartitionKey">>,<<"key">>},
        {<<"SequenceNumber">>,
         <<"49537292605574028653758531131893428543501381406818304001">>}],
       [{<<"Data">>,<<"asdasd 213123123">>},
        {<<"PartitionKey">>,<<"key">>},
        {<<"SequenceNumber">>,
         <<"49537292605574028653758541428570459745183078607853977601">>}]]}]}
        })
        ],

    output_tests(?_f(erlcloud_kinesis:get_records(<<"AAAAAAAAAAEuncwaAk+GTC2TIdmdg5w6dIuZ4Scu6vaMGPtaPUfopvw9cBm2NM3Rlj9WyI5JFJr2ahuSh3Z187AdW4Lug86E">>)), Tests).

get_records_no_decode_output_tests(_) ->
    Input = "
{
    \"NextShardIterator\": \"AAAAAAAAAAEkuCmrC+QDW1gUywyu7G8GxvRyM6GSMkcHQ9wrvCJBW87mjn9C8YEckkipaoJySwgKXMmn1BwSPjnjiUCsu6pc\",
    \"Records\": [
        {
            \"Data\": \"YXNkYXNk\",
            \"PartitionKey\": \"key\",
            \"SequenceNumber\": \"49537292605574028653758531131893428543501381406818304001\"
        },
        {
            \"Data\": \"YXNkYXNkIDIxMzEyMzEyMw==\",
            \"PartitionKey\": \"key\",
            \"SequenceNumber\": \"49537292605574028653758541428570459745183078607853977601\"
        }
    ]
}",
    Tests = [
        ?_kinesis_test({"GetRecords example response (no decode)", Input,
                        {ok, list_to_binary(Input)}})
    ],
    ShardIterator = <<"AAAAAAAAAAEuncwaAk+GTC2TIdmdg5w6dIuZ4Scu6vaMGPtaPUfopvw9cBm2NM3Rlj9WyI5JFJr2ahuSh3Z187AdW4Lug86E">>,
    Config = config(),
    output_tests(?_f(erlcloud_kinesis:get_records(ShardIterator, 10000, [{decode, false}], Config)), Tests).

%% PutRecord test based on the API examples:
%% ttp://docs.aws.amazon.com/kinesis/latest/APIReference/API_PutRecord.html
put_record_input_tests(_) ->
    Tests =
        [?_kinesis_test(
            {"PutRecord example request",
             ?_f(erlcloud_kinesis:put_record(<<"test">>, <<"key">>, <<"asdasd">>)), "
{
  \"Data\": \"YXNkYXNk\",
  \"StreamName\": \"test\",
  \"PartitionKey\": \"key\"
}"
            }),
         ?_kinesis_test(
             {"PutRecord example request",
              ?_f(erlcloud_kinesis:put_record(<<"test">>, <<"key">>, <<"abcdef">>, undefined, undefined, [{encode, false}])), "
{
  \"Data\": \"abcdef\",
  \"StreamName\": \"test\",
  \"PartitionKey\": \"key\"
}"
             }),
        ?_kinesis_test(
            {"PutRecord example request",
             ?_f(erlcloud_kinesis:put_record(<<"test">>, <<"key1">>, <<"asdasd 213123123">>)), "
{
  \"Data\": \"YXNkYXNkIDIxMzEyMzEyMw==\",
  \"StreamName\": \"test\",
  \"PartitionKey\": \"key1\"
}"
            })
        ],

    Response = "
{
    \"SequenceNumber\": \"49537292605574028653758531131893428543501381406818304001\",
    \"ShardId\": \"shardId-000000000000\"
}",
    input_tests(Response, Tests).

put_record_output_tests(_) ->
    Tests =
        [?_kinesis_test(
            {"PutRecord example response", "
{
    \"SequenceNumber\": \"49537292605574028653758531131893428543501381406818304001\",
    \"ShardId\": \"shardId-000000000000\"
}",
        {ok,[{<<"SequenceNumber">>,
    <<"49537292605574028653758531131893428543501381406818304001">>},
    {<<"ShardId">>,<<"shardId-000000000000">>}]}
        })
        ],

    output_tests(?_f(erlcloud_kinesis:put_record(<<"test">>, <<"key">>, <<"asdasd">>)), Tests).


put_records_input_tests(_) ->
    Tests =
        [?_kinesis_test(
            {"PutRecords example request",
             ?_f(erlcloud_kinesis:put_records(<<"test">>, [{<<"asdasd">>, <<"key">>}])), "
{
  \"StreamName\": \"test\",
  \"Records\": [
    {
      \"Data\": \"YXNkYXNk\",
      \"PartitionKey\": \"key\"
    }
  ]
}"
            }),
        ?_kinesis_test(
            {"PutRecords example request",
             ?_f(erlcloud_kinesis:put_records(<<"test">>, [{<<"asdasd 213123123">>, <<"hash_key1">>, <<"key1">>}])), "
{
  \"StreamName\": \"test\",
  \"Records\": [
    {
      \"Data\": \"YXNkYXNkIDIxMzEyMzEyMw==\",
      \"ExplicitHashKey\": \"hash_key1\",
      \"PartitionKey\": \"key1\"
    }
  ]
}"
            })
        ],

    Response = "
{
    \"SequenceNumber\": \"49537292605574028653758531131893428543501381406818304001\",
    \"ShardId\": \"shardId-000000000000\"
}",
    input_tests(Response, Tests).

put_records_output_tests(_) ->
    Tests =
        [?_kinesis_test(
            {"PutRecords example response", "
{
    \"FailedRecordCount\": 0,
    \"Records\": [
    {
      \"SequenceNumber\": \"49537292605574028653758531131893428543501381406818304001\",
      \"ShardId\": \"shardId-000000000000\"
    }
  ]
}",
        {ok, [{<<"FailedRecordCount">>, 0},
              {<<"Records">>,
               [
                [{<<"SequenceNumber">>,
                  <<"49537292605574028653758531131893428543501381406818304001">>},
                 {<<"ShardId">>,<<"shardId-000000000000">>}]
               ]}
             ]
         }
        })
        ],

    output_tests(?_f(erlcloud_kinesis:put_records(<<"test">>, [{<<"asdasd">>, <<"key">>}])), Tests).

%% MergeShards test based on the API examples:
%% http://docs.aws.amazon.com/kinesis/latest/APIReference/API_MergeShards.html
merge_shards_input_tests(_) ->
    Tests =
        [?_kinesis_test(
            {"MergeShards example request",
             ?_f(erlcloud_kinesis:merge_shards(<<"test">>, <<"shardId-000000000001">>, <<"shardId-000000000003">>)), "
{
    \"AdjacentShardToMerge\": \"shardId-000000000001\",
    \"ShardToMerge\": \"shardId-000000000003\",
    \"StreamName\": \"test\"
}"
            })
        ],

    Response = "{}",
    input_tests(Response, Tests).

merge_shards_output_tests(_) ->
    Tests =
        [?_kinesis_test(
            {"MergeShards example response", "{}",
             {ok, jsx:decode(<<"{}">>)}})
        ],

    output_tests(?_f(erlcloud_kinesis:merge_shards(<<"test">>, <<"shardId-000000000001">>, <<"shardId-000000000003">>)), Tests).

%% SplitShard test based on the API examples:
%% http://docs.aws.amazon.com/kinesis/latest/APIReference/API_SplitShard.html
split_shards_input_tests(_) ->
    Tests =
        [?_kinesis_test(
            {"SplitShard example request",
             ?_f(erlcloud_kinesis:split_shards(<<"test">>, <<"shardId-000000000000">>, <<"10">>)), "
{
    \"ShardToSplit\": \"shardId-000000000000\",
    \"NewStartingHashKey\": \"10\",
    \"StreamName\": \"test\"
}"
            })
        ],

    Response = "{}",
    input_tests(Response, Tests).

split_shards_output_tests(_) ->
    Tests =
        [?_kinesis_test(
            {"SplitShard example response", "{}",
             {ok, jsx:decode(<<"{}">>)}})
        ],

    output_tests(?_f(erlcloud_kinesis:split_shards(<<"test">>, <<"shardId-000000000000">>, <<"10">>)), Tests).

add_tags_to_stream_input_tests(_) ->
    Tests =
        [?_kinesis_test(
            {"Add tags request test",
             ?_f(erlcloud_kinesis:add_tags_to_stream(<<"stream">>,
                                                     [{<<"key">>, <<"val">>}])),
             "{\"StreamName\": \"stream\",
               \"Tags\": {\"key\": \"val\"}}"}
        )],
    input_tests("", Tests).

add_tags_to_stream_output_tests(_) ->
    Tests = [?_kinesis_test({"Add tags response test", "", ok})],
    output_tests(
        ?_f(erlcloud_kinesis:add_tags_to_stream(<<"stream">>,
                                                [{<<"key">>, <<"val">>}])),
        Tests
    ).

list_tags_for_stream_input_tests(_) ->
    Tests =
        [?_kinesis_test(
            {"List tags request test 1",
             ?_f(erlcloud_kinesis:list_tags_for_stream(<<"stream">>)),
             "{\"StreamName\": \"stream\"}"}
        ),
         ?_kinesis_test(
             {"List tags request test 2",
              ?_f(erlcloud_kinesis:list_tags_for_stream(<<"stream">>,
                                                        <<"key1">>,
                                                        1)),
              "{\"StreamName\": \"stream\",
                \"ExclusiveStartTagKey\": \"key1\",
                \"Limit\": 1}"}
         )],
    input_tests("", Tests).

list_tags_for_stream_output_tests(_) ->
    Response = "{\"HasMoreTags\": false,
                 \"Tags\": [{\"Key\":\"key1\",\"Value\":\"val1\"}]}",
    Tests = [?_kinesis_test({"List tags response test",
                             Response,
                             {ok, jsx:decode(list_to_binary(Response))}})],
    output_tests(
        ?_f(erlcloud_kinesis:list_tags_for_stream(<<"stream">>, <<"key1">>, 1)),
        Tests
    ).

list_all_tags_for_stream_input_tests(_) ->
    Tests =
        [?_kinesis_test(
            {"List all tags request test",
             ?_f(erlcloud_kinesis:list_all_tags_for_stream(<<"stream">>)),
             "{\"StreamName\": \"stream\"}"}
        )],
    input_tests("", Tests).

list_all_tags_for_stream_output_test(_) ->
    Response = "{\"HasMoreTags\": false,
                 \"Tags\": [{\"Key\":\"key1\",\"Value\":\"val1\"},
                            {\"Key\":\"key2\",\"Value\":\"val2\"}]}",
    Tests =
        [?_kinesis_test({"List all tags response test",
                         Response,
                         {ok, [{<<"key1">>, <<"val1">>},
                               {<<"key2">>, <<"val2">>}]}}
        )],
    output_tests(
        ?_f(erlcloud_kinesis:list_all_tags_for_stream(<<"stream">>)),
        Tests
    ).

remove_tags_from_stream_input_tests(_) ->
    Tests =
        [?_kinesis_test(
            {"Remove tags request test",
             ?_f(erlcloud_kinesis:remove_tags_from_stream(<<"stream">>,
                                                          [<<"key">>])),
             "{\"StreamName\": \"stream\",
               \"TagKeys\": [\"key\"]}"}
        )],
    input_tests("", Tests).

remove_tags_from_stream_output_tests(_) ->
    Tests = [?_kinesis_test({"Remove tags response test", "", ok})],
    output_tests(
        ?_f(erlcloud_kinesis:remove_tags_from_stream(<<"stream">>,
                                                     [<<"key">>])),
        Tests
    ).
