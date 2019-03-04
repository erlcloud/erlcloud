%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_ddb_streams_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").
-include("erlcloud_ddb_streams.hrl").

%% Unit tests for ddb streams.
%% These tests work by using meck to mock erlcloud_httpc. 

%% The _ddb_test macro provides line number annotation to a test, similar to _test, but doesn't wrap in a fun
-define(_ddb_streams_test(T), {?LINE, T}).
%% The _f macro is a terse way to wrap code in a fun. Similar to _test but doesn't annotate with a line number
-define(_f(F), fun() -> F end).
                            
%%%===================================================================
%%% Test entry points
%%%===================================================================

operation_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun describe_stream_input_tests/1,
      fun describe_stream_output_tests/1,
      fun get_records_input_tests/1,
      fun get_records_output_tests/1,
      fun get_shard_iterator_input_tests/1,
      fun get_shard_iterator_output_tests/1,
      fun list_streams_input_tests/1,
      fun list_streams_output_tests/1
     ]}.

start() ->
    meck:new(erlcloud_httpc),
    ok.

stop(_) ->
    meck:unload(erlcloud_httpc).

%%%===================================================================
%%% Input test helpers
%%%===================================================================

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
              erlcloud_ddb_streams:configure(string:copies("A", 20), string:copies("a", 40)),
              Fun()
      end}}.

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
              erlcloud_ddb_streams:configure(string:copies("A", 20), string:copies("a", 40)),
              Actual = Fun(),
              case Result =:= Actual of
                  true -> ok;
                  false ->
                      ?debugFmt("~nEXPECTED~n~p~nACTUAL~n~p~n", [Result, Actual])
              end,
              ?assertEqual(Result, Actual)
      end}}.

%% output_tests converts a list of output_test specifiers into an eunit test generator
-spec output_tests(fun(), [output_test_spec()]) -> [term()].       
output_tests(Fun, Tests) ->
    [output_test(Fun, Test) || Test <- Tests].


%%%===================================================================
%%% Actual test specifiers
%%%===================================================================

%% DescribeStream test based on the API examples:
%% http://docs.aws.amazon.com/dynamodbstreams/latest/APIReference/API_DescribeStream.html
describe_stream_input_tests(_) ->
    Tests =
        [?_ddb_streams_test(
            {"DescribeStream example request",
             ?_f(erlcloud_ddb_streams:describe_stream(<<"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252">>)), "
{
    \"StreamArn\": \"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252\"
}"
            }),
        ?_ddb_streams_test(
            {"DescribeStream example request with a limit of 1 shard and an exclusive start shard ID",
             ?_f(erlcloud_ddb_streams:describe_stream(<<"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252">>,
                                                      [{exclusive_start_shard_id, <<"shardId-00000001414562045508-2bac9cd2">>},
                                                       {limit, 1}])), "
{
    \"StreamArn\": \"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252\",
    \"ExclusiveStartShardId\": \"shardId-00000001414562045508-2bac9cd2\",
    \"Limit\": 1
}"
            })
        ],
    Response = "
{
    \"StreamDescription\": {
        \"StreamArn\": \"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252\",
        \"StreamLabel\": \"2015-05-20T20:51:10.252\",
        \"StreamStatus\": \"ENABLED\",
        \"StreamViewType\": \"NEW_AND_OLD_IMAGES\",
        \"CreationRequestDateTime\": 1437677671.062,
        \"TableName\": \"Forum\",
        \"KeySchema\": [
            {\"AttributeName\": \"ForumName\",\"KeyType\": \"HASH\"},
            {\"AttributeName\": \"Subject\",\"KeyType\": \"RANGE\"}
        ],
        \"Shards\": [
            {
                \"SequenceNumberRange\": {
                    \"EndingSequenceNumber\": \"20500000000000000910398\",
                    \"StartingSequenceNumber\": \"20500000000000000910398\"
                },
                \"ShardId\": \"shardId-00000001414562045508-2bac9cd2\"
            },
            {
                \"ParentShardId\": \"shardId-00000001414562045508-2bac9cd2\",
                \"SequenceNumberRange\": {
                    \"EndingSequenceNumber\": \"820400000000000001192334\",
                    \"StartingSequenceNumber\": \"820400000000000001192334\"
                },
                \"ShardId\": \"shardId-00000001414576573621-f55eea83\"
            },
            {
                \"ParentShardId\": \"shardId-00000001414576573621-f55eea83\",
                \"SequenceNumberRange\": {
                    \"EndingSequenceNumber\": \"1683700000000000001135967\",
                    \"StartingSequenceNumber\": \"1683700000000000001135967\"
                },
                \"ShardId\": \"shardId-00000001414592258131-674fd923\"
            },
            {
                \"ParentShardId\": \"shardId-00000001414592258131-674fd923\",
                \"SequenceNumberRange\": {\"StartingSequenceNumber\": \"2574600000000000000935255\"},
                \"ShardId\": \"shardId-00000001414608446368-3a1afbaf\"
            }
        ]
    }
}",
    input_tests(Response, Tests).

describe_stream_output_tests(_) ->
    Tests = 
        [?_ddb_streams_test(
            {"DescribeStream example response", "
{
    \"StreamDescription\": {
        \"StreamArn\": \"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252\",
        \"StreamLabel\": \"2015-05-20T20:51:10.252\",
        \"StreamStatus\": \"ENABLED\",
        \"StreamViewType\": \"NEW_AND_OLD_IMAGES\",
        \"CreationRequestDateTime\": 1437677671.062,
        \"TableName\": \"Forum\",
        \"KeySchema\": [
            {\"AttributeName\": \"ForumName\",\"KeyType\": \"HASH\"},
            {\"AttributeName\": \"Subject\",\"KeyType\": \"RANGE\"}
        ],
        \"Shards\": [
            {
                \"SequenceNumberRange\": {
                    \"EndingSequenceNumber\": \"20500000000000000910398\",
                    \"StartingSequenceNumber\": \"20500000000000000910398\"
                },
                \"ShardId\": \"shardId-00000001414562045508-2bac9cd2\"
            },
            {
                \"ParentShardId\": \"shardId-00000001414562045508-2bac9cd2\",
                \"SequenceNumberRange\": {
                    \"EndingSequenceNumber\": \"820400000000000001192334\",
                    \"StartingSequenceNumber\": \"820400000000000001192334\"
                },
                \"ShardId\": \"shardId-00000001414576573621-f55eea83\"
            },
            {
                \"ParentShardId\": \"shardId-00000001414576573621-f55eea83\",
                \"SequenceNumberRange\": {
                    \"EndingSequenceNumber\": \"1683700000000000001135967\",
                    \"StartingSequenceNumber\": \"1683700000000000001135967\"
                },
                \"ShardId\": \"shardId-00000001414592258131-674fd923\"
            },
            {
                \"ParentShardId\": \"shardId-00000001414592258131-674fd923\",
                \"SequenceNumberRange\": {\"StartingSequenceNumber\": \"2574600000000000000935255\"},
                \"ShardId\": \"shardId-00000001414608446368-3a1afbaf\"
            }
        ]
    }
}",
             {ok, #ddb_streams_stream_description{
                      creation_request_date_time = 1437677671.062,
                      key_schema = {<<"ForumName">>, <<"Subject">>},
                      last_evaluated_shard_id = undefined,
                      shards = [#ddb_streams_shard{
                                    parent_shard_id = undefined,
                                    sequence_number_range = {<<"20500000000000000910398">>,
                                                             <<"20500000000000000910398">>},
                                    shard_id = <<"shardId-00000001414562045508-2bac9cd2">>},
                                #ddb_streams_shard{
                                    parent_shard_id = <<"shardId-00000001414562045508-2bac9cd2">>,
                                    sequence_number_range = {<<"820400000000000001192334">>,
                                                             <<"820400000000000001192334">>},
                                    shard_id = <<"shardId-00000001414576573621-f55eea83">>},
                                #ddb_streams_shard{
                                    parent_shard_id = <<"shardId-00000001414576573621-f55eea83">>,
                                    sequence_number_range = {<<"1683700000000000001135967">>,
                                                             <<"1683700000000000001135967">>},
                                    shard_id = <<"shardId-00000001414592258131-674fd923">>},
                                #ddb_streams_shard{
                                    parent_shard_id = <<"shardId-00000001414592258131-674fd923">>,
                                    sequence_number_range = {<<"2574600000000000000935255">>,
                                                             undefined},
                                    shard_id = <<"shardId-00000001414608446368-3a1afbaf">>}],
                      stream_arn = <<"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252">>,
                      stream_label = <<"2015-05-20T20:51:10.252">>,
                      stream_status = enabled,
                      stream_view_type = new_and_old_images,
                      table_name = <<"Forum">>}}})
        ],
    output_tests(?_f(erlcloud_ddb_streams:describe_stream(<<"StreamArn">>)), Tests).


%% GetRecords test based on the API examples:
%% http://docs.aws.amazon.com/dynamodbstreams/latest/APIReference/API_GetRecords.html
get_records_input_tests(_) ->
    Tests =
        [?_ddb_streams_test(
            {"GetRecords example request",
             ?_f(erlcloud_ddb_streams:get_records(<<"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252|1|AAAAAAAAAAEvJp6D+zaQ...  <remaining characters omitted> ...">>)), "
{
    \"ShardIterator\": \"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252|1|AAAAAAAAAAEvJp6D+zaQ...  <remaining characters omitted> ...\"
}"
            }),
        ?_ddb_streams_test(
            {"GetRecords example request with a limit of 1",
             ?_f(erlcloud_ddb_streams:get_records(<<"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252|1|AAAAAAAAAAEvJp6D+zaQ...  <remaining characters omitted> ...">>,
                                                  [{limit, 1}])), "
{
    \"ShardIterator\": \"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252|1|AAAAAAAAAAEvJp6D+zaQ...  <remaining characters omitted> ...\",
    \"Limit\": 1
}"
            })
        ],
    Response = "
{
    \"NextShardIterator\": \"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252|1|AAAAAAAAAAGQBYshYDEe ... <remaining characters omitted> ...\",
    \"Records\": [
        {
            \"awsRegion\": \"us-west-2\",
            \"dynamodb\": {
                \"Keys\": {
                    \"ForumName\": {\"S\": \"DynamoDB\"},
                    \"Subject\": {\"S\": \"DynamoDB Thread 3\"}
                },
                \"SequenceNumber\": \"300000000000000499659\",
                \"SizeBytes\": 41,
                \"StreamViewType\": \"KEYS_ONLY\"
            },
            \"eventID\": \"e2fd9c34eff2d779b297b26f5fef4206\",
            \"eventName\": \"INSERT\",
            \"eventSource\": \"aws:dynamodb\",
            \"eventVersion\": \"1.0\"
        },
        {
            \"awsRegion\": \"us-west-2\",
            \"dynamodb\": {
                \"Keys\": {
                    \"ForumName\": {\"S\": \"DynamoDB\"},
                    \"Subject\": {\"S\": \"DynamoDB Thread 1\"}
                },
                \"SequenceNumber\": \"400000000000000499660\",
                \"SizeBytes\": 41,
                \"StreamViewType\": \"KEYS_ONLY\"
            },
            \"eventID\": \"4b25bd0da9a181a155114127e4837252\",
            \"eventName\": \"MODIFY\",
            \"eventSource\": \"aws:dynamodb\",
            \"eventVersion\": \"1.0\"
        },
        {
            \"awsRegion\": \"us-west-2\",
            \"dynamodb\": {
                \"Keys\": {
                    \"ForumName\": {\"S\": \"DynamoDB\"},
                    \"Subject\": {\"S\": \"DynamoDB Thread 2\"}
                },
                \"SequenceNumber\": \"500000000000000499661\",
                \"SizeBytes\": 41,
                \"StreamViewType\": \"KEYS_ONLY\"
            },
            \"eventID\": \"740280c73a3df7842edab3548a1b08ad\",
            \"eventName\": \"REMOVE\",
            \"eventSource\": \"aws:dynamodb\",
            \"eventVersion\": \"1.0\"
        }
    ]
}",
    input_tests(Response, Tests).

get_records_output_tests(_) ->
    Tests = 
        [?_ddb_streams_test(
            {"GetRecords example response", "
{
    \"NextShardIterator\": \"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252|1|AAAAAAAAAAGQBYshYDEe ... <remaining characters omitted> ...\",
    \"Records\": [
        {
            \"awsRegion\": \"us-west-2\",
            \"dynamodb\": {
                \"ApproximateCreationDateTime\": 1551727994,
                \"Keys\": {
                    \"ForumName\": {\"S\": \"DynamoDB\"},
                    \"Subject\": {\"S\": \"DynamoDB Thread 3\"}
                },
                \"SequenceNumber\": \"300000000000000499659\",
                \"SizeBytes\": 41,
                \"StreamViewType\": \"KEYS_ONLY\"
            },
            \"eventID\": \"e2fd9c34eff2d779b297b26f5fef4206\",
            \"eventName\": \"INSERT\",
            \"eventSource\": \"aws:dynamodb\",
            \"eventVersion\": \"1.0\"
        },
        {
            \"awsRegion\": \"us-west-2\",
            \"dynamodb\": {
                \"ApproximateCreationDateTime\": 1551727994,
                \"Keys\": {
                    \"ForumName\": {\"S\": \"DynamoDB\"},
                    \"Subject\": {\"S\": \"DynamoDB Thread 1\"}
                },
                \"SequenceNumber\": \"400000000000000499660\",
                \"SizeBytes\": 41,
                \"StreamViewType\": \"KEYS_ONLY\"
            },
            \"eventID\": \"4b25bd0da9a181a155114127e4837252\",
            \"eventName\": \"MODIFY\",
            \"eventSource\": \"aws:dynamodb\",
            \"eventVersion\": \"1.0\"
        },
        {
            \"awsRegion\": \"us-west-2\",
            \"dynamodb\": {
                \"ApproximateCreationDateTime\": 1551727994,
                \"Keys\": {
                    \"ForumName\": {\"S\": \"DynamoDB\"},
                    \"Subject\": {\"S\": \"DynamoDB Thread 2\"}
                },
                \"SequenceNumber\": \"500000000000000499661\",
                \"SizeBytes\": 41,
                \"StreamViewType\": \"KEYS_ONLY\"
            },
            \"eventID\": \"740280c73a3df7842edab3548a1b08ad\",
            \"eventName\": \"REMOVE\",
            \"eventSource\": \"aws:dynamodb\",
            \"eventVersion\": \"1.0\"
        }
    ]
}",
             {ok, [#ddb_streams_record{
                       aws_region = <<"us-west-2">>,
                       dynamodb = #ddb_streams_stream_record{
                                      approximate_creation_date_time = 1551727994,
                                      keys = [{<<"ForumName">>, <<"DynamoDB">>},
                                              {<<"Subject">>, <<"DynamoDB Thread 3">>}],
                                      new_image = undefined,
                                      old_image = undefined,
                                      sequence_number = <<"300000000000000499659">>,
                                      size_bytes = 41,
                                      stream_view_type = keys_only},
                       event_id = <<"e2fd9c34eff2d779b297b26f5fef4206">>,
                       event_name = insert,
                       event_source = <<"aws:dynamodb">>,
                       event_version = <<"1.0">>},
                   #ddb_streams_record{
                       aws_region = <<"us-west-2">>,
                       dynamodb = #ddb_streams_stream_record{
                                      approximate_creation_date_time = 1551727994,
                                      keys = [{<<"ForumName">>, <<"DynamoDB">>},
                                              {<<"Subject">>, <<"DynamoDB Thread 1">>}],
                                      new_image = undefined,
                                      old_image = undefined,
                                      sequence_number = <<"400000000000000499660">>,
                                      size_bytes = 41,
                                      stream_view_type = keys_only},
                       event_id = <<"4b25bd0da9a181a155114127e4837252">>,
                       event_name = modify,
                       event_source = <<"aws:dynamodb">>,
                       event_version = <<"1.0">>},
                   #ddb_streams_record{
                       aws_region = <<"us-west-2">>,
                       dynamodb = #ddb_streams_stream_record{
                                      approximate_creation_date_time = 1551727994,
                                      keys = [{<<"ForumName">>, <<"DynamoDB">>},
                                              {<<"Subject">>, <<"DynamoDB Thread 2">>}],
                                      new_image = undefined,
                                      old_image = undefined,
                                      sequence_number = <<"500000000000000499661">>,
                                      size_bytes = 41,
                                      stream_view_type = keys_only},
                       event_id = <<"740280c73a3df7842edab3548a1b08ad">>,
                       event_name = remove,
                       event_source = <<"aws:dynamodb">>,
                       event_version = <<"1.0">>}]}})
        ],
    output_tests(?_f(erlcloud_ddb_streams:get_records(<<"ShardIterator">>)), Tests).

%% GetShardIterator test based on the API examples:
%% http://docs.aws.amazon.com/dynamodbstreams/latest/APIReference/API_GetShardIterator.html
get_shard_iterator_input_tests(_) ->
    Tests =
        [?_ddb_streams_test(
            {"GetShardIterator example request",
             ?_f(erlcloud_ddb_streams:get_shard_iterator(<<"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252">>,
                                                         <<"00000001414576573621-f55eea83">>,
                                                         trim_horizon)), "
{
    \"StreamArn\": \"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252\",
    \"ShardId\": \"00000001414576573621-f55eea83\",
    \"ShardIteratorType\": \"TRIM_HORIZON\"
}"
            }),
        ?_ddb_streams_test(
            {"GetShardIterator example request with a sequence number",
             ?_f(erlcloud_ddb_streams:get_shard_iterator(<<"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252">>,
                                                         <<"00000001414576573621-f55eea83">>,
                                                         trim_horizon,
                                                         [{sequence_number, <<"20500000000000000910398">>}])), "
{
    \"StreamArn\": \"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252\",
    \"ShardId\": \"00000001414576573621-f55eea83\",
    \"ShardIteratorType\": \"TRIM_HORIZON\",
    \"SequenceNumber\": \"20500000000000000910398\"
}"
            })
        ],
    Response = "
{         
    \"ShardIterator\": \"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252|1|AAAAAAAAAAEvJp6D+zaQ...  <remaining characters omitted> ...\"
}",
    input_tests(Response, Tests).

get_shard_iterator_output_tests(_) ->
    Tests = 
        [?_ddb_streams_test(
            {"GetShardIterator example response", "
{         
    \"ShardIterator\": \"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252|1|AAAAAAAAAAEvJp6D+zaQ...  <remaining characters omitted> ...\"
}",
             {ok, <<"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252|1|AAAAAAAAAAEvJp6D+zaQ...  <remaining characters omitted> ...">>}})
        ],
    output_tests(?_f(erlcloud_ddb_streams:get_shard_iterator(<<"StreamArn">>, <<"ShardId">>, trim_horizon)), Tests).

%% ListStreams test based on the API examples:
%% http://docs.aws.amazon.com/dynamodbstreams/latest/APIReference/API_ListStreams.html
list_streams_input_tests(_) ->
    Tests =
        [?_ddb_streams_test(
            {"ListStreams example request",
             ?_f(erlcloud_ddb_streams:list_streams()), "
{}"
            }),
        ?_ddb_streams_test(
            {"ListStreams example request with all options provided",
             ?_f(erlcloud_ddb_streams:list_streams([{exclusive_start_stream_arn, <<"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252">>},
                                                    {limit, 1},
                                                    {table_name, <<"Forum">>}])), "
{
    \"ExclusiveStartStreamArn\": \"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252\",
    \"Limit\": 1,
    \"TableName\": \"Forum\"
}"
            })
        ],
    Response = "
{
    \"Streams\": [
        {
            \"StreamArn\": \"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252\",
            \"TableName\": \"Forum\",
            \"StreamLabel\": \"2015-05-20T20:51:10.252\"
        },{
            \"StreamArn\": \"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:50:02.714\",
            \"TableName\": \"Forum\",
            \"StreamLabel\": \"2015-05-20T20:50:02.714\"
        },{
            \"StreamArn\": \"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-19T23:03:50.641\",
            \"TableName\": \"Forum\",
            \"StreamLabel\": \"2015-05-19T23:03:50.641\"
        } 
    ]
}",
    input_tests(Response, Tests).

list_streams_output_tests(_) ->
    Tests = 
        [?_ddb_streams_test(
            {"ListStreams example response", "
{
    \"Streams\": [
        {
            \"StreamArn\": \"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252\",
            \"TableName\": \"Forum\",
            \"StreamLabel\": \"2015-05-20T20:51:10.252\"
        },{
            \"StreamArn\": \"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:50:02.714\",
            \"TableName\": \"Forum\",
            \"StreamLabel\": \"2015-05-20T20:50:02.714\"
        },{
            \"StreamArn\": \"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-19T23:03:50.641\",
            \"TableName\": \"Forum\",
            \"StreamLabel\": \"2015-05-19T23:03:50.641\"
        } 
    ]
}",
             {ok, [#ddb_streams_stream{
                       stream_arn = <<"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:51:10.252">>,
                       stream_label = <<"2015-05-20T20:51:10.252">>,
                       table_name = <<"Forum">>},
                   #ddb_streams_stream{stream_arn = <<"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-20T20:50:02.714">>,
                       stream_label = <<"2015-05-20T20:50:02.714">>,
                       table_name = <<"Forum">>},
                   #ddb_streams_stream{stream_arn = <<"arn:aws:dynamodb:us-west-2:111122223333:table/Forum/stream/2015-05-19T23:03:50.641">>,
                       stream_label = <<"2015-05-19T23:03:50.641">>,
                       table_name = <<"Forum">>}]}})
        ],
    output_tests(?_f(erlcloud_ddb_streams:list_streams()), Tests).
