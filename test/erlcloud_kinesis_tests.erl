%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_kinesis_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").
%%-include("erlcloud_kinesis.hrl").

%% Unit tests for kinesis.
%% These tests work by using meck to mock httpc. There are two classes of test: input and output.
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
      fun get_shard_iterator_input_tests/1,
      fun get_shard_iterator_output_tests/1,
      fun get_records_input_tests/1,
      fun get_records_output_tests/1,
      fun put_record_input_tests/1,
      fun put_record_output_tests/1,
      fun merge_shards_input_tests/1,
      fun merge_shards_output_tests/1,
      fun split_shards_input_tests/1,
      fun split_shards_output_tests/1
     ]}.

start() ->
    meck:new(httpc, [unstick]),
    ok.

stop(_) ->
    meck:unload(httpc).

%%%===================================================================
%%% Input test helpers
%%%===================================================================

-type expected_body() :: string().

sort_object([{_, _} | _] = V) ->
    %% Value is an object
    lists:keysort(1, V);
sort_object(V) ->
    V.

%% verifies that the parameters in the body match the expected parameters
-spec validate_body(binary(), expected_body()) -> ok.
validate_body(Body, Expected) ->
    Want = jsx:decode(list_to_binary(Expected), [{post_decode, fun sort_object/1}]),
    Actual = jsx:decode(Body, [{post_decode, fun sort_object/1}]),
    case Want =:= Actual of
        true -> ok;
        false ->
            ?debugFmt("~nEXPECTED~n~p~nACTUAL~n~p~n", [Want, Actual])
    end,
    ?assertEqual(Want, Actual).

%% returns the mock of the httpc function input tests expect to be called.
%% Validates the request body and responds with the provided response.
-spec input_expect(string(), expected_body()) -> fun().
input_expect(Response, Expected) ->
    fun(post, {_Url, _Headers, _ContentType, Body}, _HTTPOpts, _Opts) ->
            validate_body(Body, Expected),
            {ok, {{0, 200, 0}, 0, list_to_binary(Response)}}
    end.

%% input_test converts an input_test specifier into an eunit test generator
-type input_test_spec() :: {pos_integer(), {fun(), expected_body()} | {string(), fun(), expected_body()}}.
-spec input_test(string(), input_test_spec()) -> tuple().
input_test(Response, {Line, {Description, Fun, Expected}}) when
      is_list(Description) ->
    {Description,
     {Line,
      fun() ->
              meck:expect(httpc, request, input_expect(Response, Expected)),
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

%% returns the mock of the httpc function output tests expect to be called.
-spec output_expect(string()) -> fun().
output_expect(Response) ->
    fun(post, {_Url, _Headers, _ContentType, _Body}, _HTTPOpts, _Opts) ->
            {ok, {{0, 200, 0}, 0, list_to_binary(Response)}}
    end.

%% output_test converts an output_test specifier into an eunit test generator
-type output_test_spec() :: {pos_integer(), {string(), term()} | {string(), string(), term()}}.
-spec output_test(fun(), output_test_spec()) -> tuple().
output_test(Fun, {Line, {Description, Response, Result}}) ->
    {Description,
     {Line,
      fun() ->
              meck:expect(httpc, request, output_expect(Response)),
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

    Response = "",
    input_tests(Response, Tests).

create_stream_output_tests(_) ->
    Tests =
        [?_kinesis_test(
            {"CreateStream example response", "",
             {ok, jsx:decode(<<"">>)}})
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

    Response = "",
    input_tests(Response, Tests).

delete_stream_output_tests(_) ->
    Tests =
        [?_kinesis_test(
            {"DeleteStream example response", "",
             {ok, jsx:decode(<<"">>)}})
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

%% GetShardIterator test based on the API examples:
%% http://docs.aws.amazon.com/kinesis/latest/APIReference/API_GetShardIterator.html
get_shard_iterator_input_tests(_) ->
    Tests =
        [?_kinesis_test(
            {"GetShardIterator example request",
             ?_f(erlcloud_kinesis:get_shard_iterator(<<"test">>, <<"shardId-000000000000">>, <<"TRIM_HORIZON">>)), "
{
  \"StreamName\": \"test\",
  \"ShardId\": \"shardId-000000000000\",
  \"ShardIteratorType\": \"TRIM_HORIZON\"
}"
            })
        ],

    Response = "
{
  \"ShardIterator\": \"AAAAAAAAAAFHJejL6/AjDShV3pIXsxYZT7Xj2G6EHxokHqT2D1stIOVYUEyprlUGWUepKqUDaR0+hB6qTlKvZa+fsBRqgHi4\"
}",
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

    Response = "",
    input_tests(Response, Tests).

merge_shards_output_tests(_) ->
    Tests =
        [?_kinesis_test(
            {"MergeShards example response", "",
             {ok, jsx:decode(<<"">>)}})
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

    Response = "",
    input_tests(Response, Tests).

split_shards_output_tests(_) ->
    Tests =
        [?_kinesis_test(
            {"SplitShard example response", "",
             {ok, jsx:decode(<<"">>)}})
        ],

    output_tests(?_f(erlcloud_kinesis:split_shards(<<"test">>, <<"shardId-000000000000">>, <<"10">>)), Tests).
