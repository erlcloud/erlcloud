%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_ddb2_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").
-include("erlcloud_ddb2.hrl").

%% Unit tests for ddb.
%% These tests work by using meck to mock httpc. There are two classes of test: input and output.
%%
%% Input tests verify that different function args produce the desired JSON request.
%% An input test list provides a list of funs and the JSON that is expected to result.
%%
%% Output tests verify that the http response produces the correct return from the fun.
%% An output test lists provides a list of response bodies and the expected return.

%% The _ddb_test macro provides line number annotation to a test, similar to _test, but doesn't wrap in a fun
-define(_ddb_test(T), {?LINE, T}).
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
     [fun error_handling_tests/1,
      fun batch_get_item_input_tests/1,
      fun batch_get_item_output_tests/1,
      fun batch_write_item_input_tests/1,
      fun batch_write_item_output_tests/1,
      fun create_table_input_tests/1,
      fun create_table_output_tests/1,
      fun delete_item_input_tests/1,
      fun delete_item_output_tests/1,
      fun delete_table_input_tests/1,
      fun delete_table_output_tests/1,
      fun describe_table_input_tests/1,
      fun describe_table_output_tests/1,
      fun get_item_input_tests/1,
      fun get_item_output_tests/1,
      fun list_tables_input_tests/1,
      fun list_tables_output_tests/1,
      fun put_item_input_tests/1,
      fun put_item_output_tests/1,
      fun q_input_tests/1,
      fun q_output_tests/1,
      fun scan_input_tests/1,
      fun scan_output_tests/1,
      fun update_item_input_tests/1,
      fun update_item_output_tests/1,
      fun update_table_input_tests/1,
      fun update_table_output_tests/1
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
              erlcloud_ddb2:configure(string:copies("A", 20), string:copies("a", 40)),
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
              erlcloud_ddb2:configure(string:copies("A", 20), string:copies("a", 40)),
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


%%%===================================================================
%%% Error test helpers
%%%===================================================================

-spec httpc_response(pos_integer(), string()) -> tuple().
httpc_response(Code, Body) ->
    {ok, {{"", Code, ""}, [], list_to_binary(Body)}}.
    
-type error_test_spec() :: {pos_integer(), {string(), list(), term()}}.
-spec error_test(fun(), error_test_spec()) -> tuple().
error_test(Fun, {Line, {Description, Responses, Result}}) ->
    %% Add a bogus response to the end of the request to make sure we don't call too many times
    Responses1 = Responses ++ [httpc_response(200, "TOO MANY REQUESTS")],
    {Description,
     {Line,
      fun() ->
              meck:sequence(httpc, request, 4, Responses1),
              erlcloud_ddb2:configure(string:copies("A", 20), string:copies("a", 40)),
              Actual = Fun(),
              ?assertEqual(Result, Actual)
      end}}.
      
-spec error_tests(fun(), [error_test_spec()]) -> [term()].
error_tests(Fun, Tests) ->
    [error_test(Fun, Test) || Test <- Tests].

%%%===================================================================
%%% Actual test specifiers
%%%===================================================================

input_exception_test_() ->
    [?_assertError({erlcloud_ddb, {invalid_attr_value, {n, "string"}}},
                   erlcloud_ddb2:get_item(<<"Table">>, {<<"K">>, {n, "string"}})),
     %% This test causes an expected dialyzer error
     ?_assertError({erlcloud_ddb, {invalid_item, <<"Attr">>}},
                   erlcloud_ddb2:put_item(<<"Table">>, <<"Attr">>)),
     ?_assertError({erlcloud_ddb, {invalid_opt, {myopt, myval}}},
                   erlcloud_ddb2:list_tables([{myopt, myval}]))
    ].

%% Error handling tests based on:
%% http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ErrorHandling.html
error_handling_tests(_) ->
    OkResponse = httpc_response(200, "
{\"Item\":
	{\"friends\":{\"SS\":[\"Lynda\", \"Aaron\"]},
	 \"status\":{\"S\":\"online\"}
	},
\"ConsumedCapacityUnits\": 1
}"                                   
                                  ),
    OkResult = {ok, [{<<"friends">>, [<<"Lynda">>, <<"Aaron">>]},
                     {<<"status">>, <<"online">>}]},

    Tests = 
        [?_ddb_test(
            {"Test retry after ProvisionedThroughputExceededException",
             [httpc_response(400, "
{\"__type\":\"com.amazonaws.dynamodb.v20111205#ProvisionedThroughputExceededException\",
\"message\":\"The level of configured provisioned throughput for the table was exceeded. Consider increasing your provisioning level with the UpdateTable API\"}"
                            ),
              OkResponse],
             OkResult}),
         ?_ddb_test(
            {"Test ConditionalCheckFailed error",
             [httpc_response(400, "
{\"__type\":\"com.amazonaws.dynamodb.v20111205#ConditionalCheckFailedException\",
\"message\":\"The expected value did not match what was stored in the system.\"}"
                            )],
             {error, {<<"ConditionalCheckFailedException">>, <<"The expected value did not match what was stored in the system.">>}}}),
         ?_ddb_test(
            {"Test retry after 500",
             [httpc_response(500, ""),
              OkResponse],
             OkResult})
        ],
    
    error_tests(?_f(erlcloud_ddb2:get_item(<<"table">>, {<<"k">>, <<"v">>})), Tests).


%% BatchGetItem test based on the API examples:
%% http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_BatchGetItems.html
batch_get_item_input_tests(_) ->
    Tests =
        [?_ddb_test(
            {"BatchGetItem example request",
             ?_f(erlcloud_ddb2:batch_get_item(
                   [{<<"Forum">>, 
                     [{<<"Name">>, {s, <<"Amazon DynamoDB">>}},
                      {<<"Name">>, {s, <<"Amazon RDS">>}}, 
                      {<<"Name">>, {s, <<"Amazon Redshift">>}}],
                     [{attributes_to_get, [<<"Name">>, <<"Threads">>, <<"Messages">>, <<"Views">>]}]},
                    {<<"Thread">>, 
                     [[{<<"ForumName">>, {s, <<"Amazon DynamoDB">>}}, 
                       {<<"Subject">>, {s, <<"Concurrent reads">>}}]],
                     [{attributes_to_get, [<<"Tags">>, <<"Message">>]}]}],
                   [{return_consumed_capacity, total}])), "
{
    \"RequestItems\": {
        \"Forum\": {
            \"Keys\": [
                {
                    \"Name\":{\"S\":\"Amazon DynamoDB\"}
                },
                {
                    \"Name\":{\"S\":\"Amazon RDS\"}
                },
                {
                    \"Name\":{\"S\":\"Amazon Redshift\"}
                }
            ],
            \"AttributesToGet\": [
                \"Name\",\"Threads\",\"Messages\",\"Views\"
            ]
        },
        \"Thread\": {
            \"Keys\": [
                {
                    \"ForumName\":{\"S\":\"Amazon DynamoDB\"},
                    \"Subject\":{\"S\":\"Concurrent reads\"}
                }
            ],
            \"AttributesToGet\": [
                \"Tags\",\"Message\"
            ]
        }
    },
    \"ReturnConsumedCapacity\": \"TOTAL\"
}"
            })
        ],

    Response = "
{
    \"Responses\": {
        \"Forum\": [
            {
                \"Name\":{
                    \"S\":\"Amazon DynamoDB\"
                },
                \"Threads\":{
                    \"N\":\"5\"
                },
                \"Messages\":{
                    \"N\":\"19\"
                },
                \"Views\":{
                    \"N\":\"35\"
                }
            },
            {
                \"Name\":{
                    \"S\":\"Amazon RDS\"
                },
                \"Threads\":{
                    \"N\":\"8\"
                },
                \"Messages\":{
                    \"N\":\"32\"
                },
                \"Views\":{
                    \"N\":\"38\"
                }
            },
            {
                \"Name\":{
                    \"S\":\"Amazon Redshift\"
                },
                \"Threads\":{
                    \"N\":\"12\"
                },
                \"Messages\":{
                    \"N\":\"55\"
                },
                \"Views\":{
                    \"N\":\"47\"
                }
            }
        ],
        \"Thread\": [
            {
                \"Tags\":{
                    \"SS\":[\"Reads\",\"MultipleUsers\"]
                },
                \"Message\":{
                    \"S\":\"How many users can read a single data item at a time? Are there any limits?\"
                }
            }
        ]
    },
    \"UnprocessedKeys\": {
    },
    \"ConsumedCapacity\": [
        {
            \"TableName\": \"Forum\",
            \"CapacityUnits\": 3
        },
        {
            \"TableName\": \"Thread\",
            \"CapacityUnits\": 1
        }
    ]
}",
    input_tests(Response, Tests).

batch_get_item_output_tests(_) ->
    Tests = 
        [?_ddb_test(
            {"BatchGetItem example response", "
{
    \"Responses\": {
        \"Forum\": [
            {
                \"Name\":{
                    \"S\":\"Amazon DynamoDB\"
                },
                \"Threads\":{
                    \"N\":\"5\"
                },
                \"Messages\":{
                    \"N\":\"19\"
                },
                \"Views\":{
                    \"N\":\"35\"
                }
            },
            {
                \"Name\":{
                    \"S\":\"Amazon RDS\"
                },
                \"Threads\":{
                    \"N\":\"8\"
                },
                \"Messages\":{
                    \"N\":\"32\"
                },
                \"Views\":{
                    \"N\":\"38\"
                }
            },
            {
                \"Name\":{
                    \"S\":\"Amazon Redshift\"
                },
                \"Threads\":{
                    \"N\":\"12\"
                },
                \"Messages\":{
                    \"N\":\"55\"
                },
                \"Views\":{
                    \"N\":\"47\"
                }
            }
        ],
        \"Thread\": [
            {
                \"Tags\":{
                    \"SS\":[\"Reads\",\"MultipleUsers\"]
                },
                \"Message\":{
                    \"S\":\"How many users can read a single data item at a time? Are there any limits?\"
                }
            }
        ]
    },
    \"UnprocessedKeys\": {
    },
    \"ConsumedCapacity\": [
        {
            \"TableName\": \"Forum\",
            \"CapacityUnits\": 3
        },
        {
            \"TableName\": \"Thread\",
            \"CapacityUnits\": 1
        }
    ]
}",
             {ok, #ddb2_batch_get_item
              {consumed_capacity = 
                   [#ddb2_consumed_capacity{table_name = <<"Forum">>, capacity_units = 3},
                    #ddb2_consumed_capacity{table_name = <<"Thread">>, capacity_units = 1}],
               responses = 
                   [#ddb2_batch_get_item_response
                    {table = <<"Forum">>,
                     items = [[{<<"Name">>, <<"Amazon DynamoDB">>},
                               {<<"Threads">>, 5},
                               {<<"Messages">>, 19},
                               {<<"Views">>, 35}],
                              [{<<"Name">>, <<"Amazon RDS">>},
                               {<<"Threads">>, 8},
                               {<<"Messages">>, 32},
                               {<<"Views">>, 38}],
                              [{<<"Name">>, <<"Amazon Redshift">>},
                               {<<"Threads">>, 12},
                               {<<"Messages">>, 55},
                               {<<"Views">>, 47}]]},
                    #ddb2_batch_get_item_response
                    {table = <<"Thread">>,
                     items = [[{<<"Tags">>, [<<"Reads">>, <<"MultipleUsers">>]},
                               {<<"Message">>, <<"How many users can read a single data item at a time? Are there any limits?">>}]]}],
               unprocessed_keys = []}}}),
         ?_ddb_test(
            {"BatchGetItem unprocessed keys", "
{
    \"Responses\": {},
    \"UnprocessedKeys\": {
        \"Forum\": {
            \"Keys\": [
                {
                    \"Name\":{\"S\":\"Amazon DynamoDB\"}
                },
                {
                    \"Name\":{\"S\":\"Amazon RDS\"}
                },
                {
                    \"Name\":{\"S\":\"Amazon Redshift\"}
                }
            ],
            \"AttributesToGet\": [
                \"Name\",\"Threads\",\"Messages\",\"Views\"
            ]
        },
        \"Thread\": {
            \"Keys\": [
                {
                    \"ForumName\":{\"S\":\"Amazon DynamoDB\"},
                    \"Subject\":{\"S\":\"Concurrent reads\"}
                }
            ],
            \"AttributesToGet\": [
                \"Tags\",\"Message\"
            ]
        }
    }
}",
             {ok, #ddb2_batch_get_item
              {responses = [], 
               unprocessed_keys = 
                   [{<<"Forum">>, 
                     [[{<<"Name">>, {s, <<"Amazon DynamoDB">>}}],
                      [{<<"Name">>, {s, <<"Amazon RDS">>}}], 
                      [{<<"Name">>, {s, <<"Amazon Redshift">>}}]],
                     [{attributes_to_get, [<<"Name">>, <<"Threads">>, <<"Messages">>, <<"Views">>]}]},
                    {<<"Thread">>, 
                     [[{<<"ForumName">>, {s, <<"Amazon DynamoDB">>}}, 
                       {<<"Subject">>, {s, <<"Concurrent reads">>}}]],
                     [{attributes_to_get, [<<"Tags">>, <<"Message">>]}]}]}}})
        ],
    
    output_tests(?_f(erlcloud_ddb2:batch_get_item([{<<"table">>, [{<<"k">>, <<"v">>}]}], [{out, record}])), Tests).

%% BatchWriteItem test based on the API examples:
%% http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_BatchWriteItem.html
batch_write_item_input_tests(_) ->
    Tests =
        [?_ddb_test(
            {"BatchWriteItem example request",
             ?_f(erlcloud_ddb2:batch_write_item(
                   [{<<"Forum">>, [{put, [{<<"Name">>, {s, <<"Amazon DynamoDB">>}},
                                          {<<"Category">>, {s, <<"Amazon Web Services">>}}]},
                                   {put, [{<<"Name">>, {s, <<"Amazon RDS">>}},
                                          {<<"Category">>, {s, <<"Amazon Web Services">>}}]},
                                   {put, [{<<"Name">>, {s, <<"Amazon Redshift">>}},
                                          {<<"Category">>, {s, <<"Amazon Web Services">>}}]},
                                   {put, [{<<"Name">>, {s, <<"Amazon ElastiCache">>}},
                                          {<<"Category">>, {s, <<"Amazon Web Services">>}}]}
                                   ]}],
                  [{return_consumed_capacity, total}])), "
{
    \"RequestItems\": {
        \"Forum\": [
            {
                \"PutRequest\": {
                    \"Item\": {
                        \"Name\": {
                            \"S\": \"Amazon DynamoDB\"
                        },
                        \"Category\": {
                            \"S\": \"Amazon Web Services\"
                        }
                    }
                }
            },
            {
                \"PutRequest\": {
                    \"Item\": {
                        \"Name\": {
                            \"S\": \"Amazon RDS\"
                        },
                        \"Category\": {
                            \"S\": \"Amazon Web Services\"
                        }
                    }
                }
            },
            {
                \"PutRequest\": {
                    \"Item\": {
                        \"Name\": {
                            \"S\": \"Amazon Redshift\"
                        },
                        \"Category\": {
                            \"S\": \"Amazon Web Services\"
                        }
                    }
                }
            },
            {
                \"PutRequest\": {
                    \"Item\": {
                        \"Name\": {
                            \"S\": \"Amazon ElastiCache\"
                        },
                        \"Category\": {
                            \"S\": \"Amazon Web Services\"
                        }
                    }
                }
            }
        ]
    },
    \"ReturnConsumedCapacity\": \"TOTAL\"
}"
            }),
        ?_ddb_test(
            {"BatchWriteItem put, delete and item collection metrics",
             ?_f(erlcloud_ddb2:batch_write_item(
                   [{<<"Forum">>, [{put, [{<<"Name">>, {s, <<"Amazon DynamoDB">>}},
                                          {<<"Category">>, {s, <<"Amazon Web Services">>}}]},
                                   {delete, {<<"Name">>, {s, <<"Amazon RDS">>}}}
                                  ]}],
                  [{return_item_collection_metrics, size}])), "
{
    \"RequestItems\": {
        \"Forum\": [
            {
                \"PutRequest\": {
                    \"Item\": {
                        \"Name\": {
                            \"S\": \"Amazon DynamoDB\"
                        },
                        \"Category\": {
                            \"S\": \"Amazon Web Services\"
                        }
                    }
                }
            },
            {
                \"DeleteRequest\": {
                    \"Key\": {
                        \"Name\": {
                            \"S\": \"Amazon RDS\"
                        }
                    }
                }
            }
        ]
    },
    \"ReturnItemCollectionMetrics\": \"SIZE\"
}"
            })
        ],

    Response = "
{
    \"UnprocessedItems\": {
        \"Forum\": [
            {
                \"PutRequest\": {
                    \"Item\": {
                        \"Name\": {
                            \"S\": \"Amazon ElastiCache\"
                        },
                        \"Category\": {
                            \"S\": \"Amazon Web Services\"
                        }
                    }
                }
            }
        ]
    },
    \"ConsumedCapacity\": [
        {
            \"TableName\": \"Forum\",
            \"CapacityUnits\": 3
        }
    ]
}",
    input_tests(Response, Tests).

batch_write_item_output_tests(_) ->
    Tests = 
        [?_ddb_test(
            {"BatchWriteItem example response", "
{
    \"UnprocessedItems\": {
        \"Forum\": [
            {
                \"PutRequest\": {
                    \"Item\": {
                        \"Name\": {
                            \"S\": \"Amazon ElastiCache\"
                        },
                        \"Category\": {
                            \"S\": \"Amazon Web Services\"
                        }
                    }
                }
            }
        ]
    },
    \"ConsumedCapacity\": [
        {
            \"TableName\": \"Forum\",
            \"CapacityUnits\": 3
        }
    ]
}",
             {ok, #ddb2_batch_write_item
              {consumed_capacity = [#ddb2_consumed_capacity{table_name = <<"Forum">>, capacity_units = 3}],
               item_collection_metrics = undefined,
               unprocessed_items = [{<<"Forum">>, 
                                     [{put, [{<<"Name">>, {s, <<"Amazon ElastiCache">>}},
                                             {<<"Category">>, {s, <<"Amazon Web Services">>}}]}]}]}}}),
         ?_ddb_test(
            {"BatchWriteItem unprocessed response", "
{
   \"UnprocessedItems\":{
        \"Forum\": [
            {
                \"PutRequest\": {
                    \"Item\": {
                        \"Name\": {
                            \"S\": \"Amazon DynamoDB\"
                        },
                        \"Category\": {
                            \"S\": \"Amazon Web Services\"
                        }
                    }
                }
            },
            {
                \"DeleteRequest\": {
                    \"Key\": {
                        \"Name\": {
                            \"S\": \"Amazon RDS\"
                        }
                    }
                }
            }
        ]
    }
}",
             {ok, #ddb2_batch_write_item
              {consumed_capacity = undefined, 
               item_collection_metrics = undefined,
               unprocessed_items =
                   [{<<"Forum">>, [{put, [{<<"Name">>, {s, <<"Amazon DynamoDB">>}},
                                          {<<"Category">>, {s, <<"Amazon Web Services">>}}]},
                                   {delete, [{<<"Name">>, {s, <<"Amazon RDS">>}}]}
                                  ]}]}}}),
         ?_ddb_test(
            {"BatchWriteItem item collection metrics", "
{
    \"ItemCollectionMetrics\": {
        \"Table1\": [
            {
                \"ItemCollectionKey\": {
                    \"key\": {
                        \"S\": \"value1\"
                    }
                },
                \"SizeEstimateRangeGB\": [
                    2,
                    4
                ]
            },
            {
                \"ItemCollectionKey\": {
                    \"key\": {
                        \"S\": \"value2\"
                    }
                },
                \"SizeEstimateRangeGB\": [
                    0.1,
                    0.2
                ]
            }
        ],
        \"Table2\": [
            {
                \"ItemCollectionKey\": {
                    \"key2\": {
                        \"N\": \"3\"
                    }
                },
                \"SizeEstimateRangeGB\": [
                    1.2,
                    1.4
                ]
            }
        ]
    }
}",
             {ok, #ddb2_batch_write_item
              {consumed_capacity = undefined, 
               item_collection_metrics = 
                   [{<<"Table1">>,
                     [#ddb2_item_collection_metrics
                      {item_collection_key = <<"value1">>,
                       size_estimate_range_gb = {2, 4}},
                      #ddb2_item_collection_metrics
                      {item_collection_key = <<"value2">>,
                       size_estimate_range_gb = {0.1, 0.2}}
                     ]},
                    {<<"Table2">>,
                     [#ddb2_item_collection_metrics
                      {item_collection_key = 3,
                       size_estimate_range_gb = {1.2, 1.4}}
                     ]}],
               unprocessed_items = []}}})
        ],
    
    output_tests(?_f(erlcloud_ddb2:batch_write_item([], [{out, record}])), Tests).

%% CreateTable test based on the API examples:
%% http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_CreateTable.html
create_table_input_tests(_) ->
    Tests =
        [?_ddb_test(
            {"CreateTable example request",
             ?_f(erlcloud_ddb2:create_table(
                   <<"Thread">>,
                   [{<<"ForumName">>, s},
                    {<<"Subject">>, s},
                    {<<"LastPostDateTime">>, s}],
                   {<<"ForumName">>, <<"Subject">>},
                   5, 
                   5,
                   [{local_secondary_indexes,
                     [{<<"LastPostIndex">>, <<"LastPostDateTime">>, keys_only}]}]
                  )), "
{
    \"AttributeDefinitions\": [
        {
            \"AttributeName\": \"ForumName\",
            \"AttributeType\": \"S\"
        },
        {
            \"AttributeName\": \"Subject\",
            \"AttributeType\": \"S\"
        },
        {
            \"AttributeName\": \"LastPostDateTime\",
            \"AttributeType\": \"S\"
        }
    ],
    \"TableName\": \"Thread\",
    \"KeySchema\": [
        {
            \"AttributeName\": \"ForumName\",
            \"KeyType\": \"HASH\"
        },
        {
            \"AttributeName\": \"Subject\",
            \"KeyType\": \"RANGE\"
        }
    ],
    \"LocalSecondaryIndexes\": [
        {
            \"IndexName\": \"LastPostIndex\",
            \"KeySchema\": [
                {
                    \"AttributeName\": \"ForumName\",
                    \"KeyType\": \"HASH\"
                },
                {
                    \"AttributeName\": \"LastPostDateTime\",
                    \"KeyType\": \"RANGE\"
                }
            ],
            \"Projection\": {
                \"ProjectionType\": \"KEYS_ONLY\"
            }
        }
    ],
    \"ProvisionedThroughput\": {
        \"ReadCapacityUnits\": 5,
        \"WriteCapacityUnits\": 5
    }
}"
            }),
         ?_ddb_test(
            {"CreateTable with INCLUDE local secondary index",
             ?_f(erlcloud_ddb2:create_table(
                   <<"Thread">>,
                   [{<<"ForumName">>, s},
                    {<<"Subject">>, s},
                    {<<"LastPostDateTime">>, s}],
                   {<<"ForumName">>, <<"Subject">>},
                   5, 
                   5,
                   [{local_secondary_indexes,
                     [{<<"LastPostIndex">>, <<"LastPostDateTime">>, {include, [<<"Author">>, <<"Body">>]}}]}]
                  )), "
{
    \"AttributeDefinitions\": [
        {
            \"AttributeName\": \"ForumName\",
            \"AttributeType\": \"S\"
        },
        {
            \"AttributeName\": \"Subject\",
            \"AttributeType\": \"S\"
        },
        {
            \"AttributeName\": \"LastPostDateTime\",
            \"AttributeType\": \"S\"
        }
    ],
    \"TableName\": \"Thread\",
    \"KeySchema\": [
        {
            \"AttributeName\": \"ForumName\",
            \"KeyType\": \"HASH\"
        },
        {
            \"AttributeName\": \"Subject\",
            \"KeyType\": \"RANGE\"
        }
    ],
    \"LocalSecondaryIndexes\": [
        {
            \"IndexName\": \"LastPostIndex\",
            \"KeySchema\": [
                {
                    \"AttributeName\": \"ForumName\",
                    \"KeyType\": \"HASH\"
                },
                {
                    \"AttributeName\": \"LastPostDateTime\",
                    \"KeyType\": \"RANGE\"
                }
            ],
            \"Projection\": {
                \"ProjectionType\": \"INCLUDE\",
                \"NonKeyAttributes\": [\"Author\", \"Body\"]
            }
        }
    ],
    \"ProvisionedThroughput\": {
        \"ReadCapacityUnits\": 5,
        \"WriteCapacityUnits\": 5
    }
}"
            }),
         ?_ddb_test(
            {"CreateTable with only hash key",
             ?_f(erlcloud_ddb2:create_table(
                   <<"Thread">>,
                   {<<"ForumName">>, s},
                   <<"ForumName">>,
                   1, 
                   1
                  )), "
{
    \"AttributeDefinitions\": [
        {
            \"AttributeName\": \"ForumName\",
            \"AttributeType\": \"S\"
        }
    ],
    \"TableName\": \"Thread\",
    \"KeySchema\": [
        {
            \"AttributeName\": \"ForumName\",
            \"KeyType\": \"HASH\"
        }
    ],
    \"ProvisionedThroughput\": {
        \"ReadCapacityUnits\": 1,
        \"WriteCapacityUnits\": 1
    }
}"
            })
        ],

    Response = "
{
    \"TableDescription\": {
        \"AttributeDefinitions\": [
            {
                \"AttributeName\": \"ForumName\",
                \"AttributeType\": \"S\"
            },
            {
                \"AttributeName\": \"LastPostDateTime\",
                \"AttributeType\": \"S\"
            },
            {
                \"AttributeName\": \"Subject\",
                \"AttributeType\": \"S\"
            }
        ],
        \"CreationDateTime\": 1.36372808007E9,
        \"ItemCount\": 0,
        \"KeySchema\": [
            {
                \"AttributeName\": \"ForumName\",
                \"KeyType\": \"HASH\"
            },
            {
                \"AttributeName\": \"Subject\",
                \"KeyType\": \"RANGE\"
            }
        ],
        \"LocalSecondaryIndexes\": [
            {
                \"IndexName\": \"LastPostIndex\",
                \"IndexSizeBytes\": 0,
                \"ItemCount\": 0,
                \"KeySchema\": [
                    {
                        \"AttributeName\": \"ForumName\",
                        \"KeyType\": \"HASH\"
                    },
                    {
                        \"AttributeName\": \"LastPostDateTime\",
                        \"KeyType\": \"RANGE\"
                    }
                ],
                \"Projection\": {
                    \"ProjectionType\": \"KEYS_ONLY\"
                }
            }
        ],
        \"ProvisionedThroughput\": {
            \"NumberOfDecreasesToday\": 0,
            \"ReadCapacityUnits\": 5,
            \"WriteCapacityUnits\": 5
        },
        \"TableName\": \"Thread\",
        \"TableSizeBytes\": 0,
        \"TableStatus\": \"CREATING\"
    }
}",
    input_tests(Response, Tests).

create_table_output_tests(_) ->
    Tests = 
        [?_ddb_test(
            {"CreateTable example response", "
{
    \"TableDescription\": {
        \"AttributeDefinitions\": [
            {
                \"AttributeName\": \"ForumName\",
                \"AttributeType\": \"S\"
            },
            {
                \"AttributeName\": \"LastPostDateTime\",
                \"AttributeType\": \"S\"
            },
            {
                \"AttributeName\": \"Subject\",
                \"AttributeType\": \"S\"
            }
        ],
        \"CreationDateTime\": 1.36372808007E9,
        \"ItemCount\": 0,
        \"KeySchema\": [
            {
                \"AttributeName\": \"ForumName\",
                \"KeyType\": \"HASH\"
            },
            {
                \"AttributeName\": \"Subject\",
                \"KeyType\": \"RANGE\"
            }
        ],
        \"LocalSecondaryIndexes\": [
            {
                \"IndexName\": \"LastPostIndex\",
                \"IndexSizeBytes\": 0,
                \"ItemCount\": 0,
                \"KeySchema\": [
                    {
                        \"AttributeName\": \"ForumName\",
                        \"KeyType\": \"HASH\"
                    },
                    {
                        \"AttributeName\": \"LastPostDateTime\",
                        \"KeyType\": \"RANGE\"
                    }
                ],
                \"Projection\": {
                    \"ProjectionType\": \"KEYS_ONLY\"
                }
            }
        ],
        \"ProvisionedThroughput\": {
            \"NumberOfDecreasesToday\": 0,
            \"ReadCapacityUnits\": 5,
            \"WriteCapacityUnits\": 5
        },
        \"TableName\": \"Thread\",
        \"TableSizeBytes\": 0,
        \"TableStatus\": \"CREATING\"
    }
}",
             {ok, #ddb2_table_description
              {attribute_definitions = [{<<"ForumName">>, s},
                                        {<<"LastPostDateTime">>, s},
                                        {<<"Subject">>, s}],
               creation_date_time = 1363728080.07,
               item_count = 0,
               key_schema = {<<"ForumName">>, <<"Subject">>},
               local_secondary_indexes =
                   [#ddb2_local_secondary_index_description{
                       index_name = <<"LastPostIndex">>,
                       index_size_bytes = 0,
                       item_count = 0,
                       key_schema = {<<"ForumName">>, <<"LastPostDateTime">>},
                       projection = keys_only}],
               provisioned_throughput = 
                   #ddb2_provisioned_throughput_description{
                      last_decrease_date_time = undefined,
                      last_increase_date_time = undefined,
                      number_of_decreases_today = 0,
                      read_capacity_units = 5,
                      write_capacity_units = 5},
               table_name = <<"Thread">>,
               table_size_bytes = 0,
               table_status = creating}}}),
         ?_ddb_test(
            {"CreateTable response with INCLUDE projection", "
{
    \"TableDescription\": {
        \"AttributeDefinitions\": [
            {
                \"AttributeName\": \"ForumName\",
                \"AttributeType\": \"S\"
            },
            {
                \"AttributeName\": \"LastPostDateTime\",
                \"AttributeType\": \"S\"
            },
            {
                \"AttributeName\": \"Subject\",
                \"AttributeType\": \"S\"
            }
        ],
        \"CreationDateTime\": 1.36372808007E9,
        \"ItemCount\": 0,
        \"KeySchema\": [
            {
                \"AttributeName\": \"ForumName\",
                \"KeyType\": \"HASH\"
            },
            {
                \"AttributeName\": \"Subject\",
                \"KeyType\": \"RANGE\"
            }
        ],
        \"LocalSecondaryIndexes\": [
            {
                \"IndexName\": \"LastPostIndex\",
                \"IndexSizeBytes\": 0,
                \"ItemCount\": 0,
                \"KeySchema\": [
                    {
                        \"AttributeName\": \"ForumName\",
                        \"KeyType\": \"HASH\"
                    },
                    {
                        \"AttributeName\": \"LastPostDateTime\",
                        \"KeyType\": \"RANGE\"
                    }
                ],
                \"Projection\": {
                    \"ProjectionType\": \"INCLUDE\",
                    \"NonKeyAttributes\": [\"Author\", \"Body\"]
                }
            }
        ],
        \"ProvisionedThroughput\": {
            \"NumberOfDecreasesToday\": 0,
            \"ReadCapacityUnits\": 5,
            \"WriteCapacityUnits\": 5
        },
        \"TableName\": \"Thread\",
        \"TableSizeBytes\": 0,
        \"TableStatus\": \"CREATING\"
    }
}",
             {ok, #ddb2_table_description
              {attribute_definitions = [{<<"ForumName">>, s},
                                        {<<"LastPostDateTime">>, s},
                                        {<<"Subject">>, s}],
               creation_date_time = 1363728080.07,
               item_count = 0,
               key_schema = {<<"ForumName">>, <<"Subject">>},
               local_secondary_indexes =
                   [#ddb2_local_secondary_index_description{
                       index_name = <<"LastPostIndex">>,
                       index_size_bytes = 0,
                       item_count = 0,
                       key_schema = {<<"ForumName">>, <<"LastPostDateTime">>},
                       projection = {include, [<<"Author">>, <<"Body">>]}}],
               provisioned_throughput = 
                   #ddb2_provisioned_throughput_description{
                      last_decrease_date_time = undefined,
                      last_increase_date_time = undefined,
                      number_of_decreases_today = 0,
                      read_capacity_units = 5,
                      write_capacity_units = 5},
               table_name = <<"Thread">>,
               table_size_bytes = 0,
               table_status = creating}}}),
         ?_ddb_test(
            {"CreateTable response with hash key only", "
{
    \"TableDescription\": {
        \"AttributeDefinitions\": [
            {
                \"AttributeName\": \"ForumName\",
                \"AttributeType\": \"S\"
            }
        ],
        \"CreationDateTime\": 1.36372808007E9,
        \"ItemCount\": 0,
        \"KeySchema\": [
            {
                \"AttributeName\": \"ForumName\",
                \"KeyType\": \"HASH\"
            }
        ],
        \"ProvisionedThroughput\": {
            \"NumberOfDecreasesToday\": 0,
            \"ReadCapacityUnits\": 1,
            \"WriteCapacityUnits\": 1
        },
        \"TableName\": \"Thread\",
        \"TableSizeBytes\": 0,
        \"TableStatus\": \"CREATING\"
    }
}",
             {ok, #ddb2_table_description
              {attribute_definitions = [{<<"ForumName">>, s}],
               creation_date_time = 1363728080.07,
               item_count = 0,
               key_schema = <<"ForumName">>,
               local_secondary_indexes = undefined,
               provisioned_throughput = 
                   #ddb2_provisioned_throughput_description{
                      last_decrease_date_time = undefined,
                      last_increase_date_time = undefined,
                      number_of_decreases_today = 0,
                      read_capacity_units = 1,
                      write_capacity_units = 1},
               table_name = <<"Thread">>,
               table_size_bytes = 0,
               table_status = creating}}})
        ],
    
    output_tests(?_f(erlcloud_ddb2:create_table(<<"name">>, [{<<"key">>, s}], <<"key">>, 5, 10)), Tests).

%% DeleteItem test based on the API examples:
%% http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_DeleteItem.html
delete_item_input_tests(_) ->
    Tests =
        [?_ddb_test(
            {"DeleteItem example request",
             ?_f(erlcloud_ddb2:delete_item(<<"Thread">>, 
                                          [{<<"ForumName">>, {s, <<"Amazon DynamoDB">>}},
                                           {<<"Subject">>, {s, <<"How do I update multiple items?">>}}],
                                          [{return_values, all_old},
                                           {expected, {<<"Replies">>, false}}])), "
{
    \"TableName\": \"Thread\",
    \"Key\": {
        \"ForumName\": {
            \"S\": \"Amazon DynamoDB\"
        },
        \"Subject\": {
            \"S\": \"How do I update multiple items?\"
        }
    },
    \"Expected\": {
        \"Replies\": {
            \"Exists\": false
        }
    },
    \"ReturnValues\": \"ALL_OLD\"
}"
            }),
         ?_ddb_test(
            {"DeleteItem return metrics",
             ?_f(erlcloud_ddb2:delete_item(<<"Thread">>, 
                                          {<<"ForumName">>, {s, <<"Amazon DynamoDB">>}},
                                          [{return_consumed_capacity, total},
                                           {return_item_collection_metrics, size}])), "
{
    \"TableName\": \"Thread\",
    \"Key\": {
        \"ForumName\": {
            \"S\": \"Amazon DynamoDB\"
        }
    },
    \"ReturnConsumedCapacity\": \"TOTAL\",
    \"ReturnItemCollectionMetrics\": \"SIZE\"
}"
            })
        ],

    Response = "
{
    \"Attributes\": {
        \"LastPostedBy\": {
            \"S\": \"fred@example.com\"
        },
        \"ForumName\": {
            \"S\": \"Amazon DynamoDB\"
        },
        \"LastPostDateTime\": {
            \"S\": \"201303201023\"
        },
        \"Tags\": {
            \"SS\": [\"Update\",\"Multiple Items\",\"HelpMe\"]
        },
        \"Subject\": {
            \"S\": \"How do I update multiple items?\"
        },
        \"Message\": {
            \"S\": \"I want to update multiple items in a single API call. What's the best way to do that?\"
        }
    }
}",
    input_tests(Response, Tests).

delete_item_output_tests(_) ->
    Tests = 
        [?_ddb_test(
            {"DeleteItem example response", "
{
    \"Attributes\": {
        \"LastPostedBy\": {
            \"S\": \"fred@example.com\"
        },
        \"ForumName\": {
            \"S\": \"Amazon DynamoDB\"
        },
        \"LastPostDateTime\": {
            \"S\": \"201303201023\"
        },
        \"Tags\": {
            \"SS\": [\"Update\",\"Multiple Items\",\"HelpMe\"]
        },
        \"Subject\": {
            \"S\": \"How do I update multiple items?\"
        },
        \"Message\": {
            \"S\": \"I want to update multiple items in a single API call. What's the best way to do that?\"
        }
    }
}",
             {ok, #ddb2_delete_item{
                     attributes = [{<<"LastPostedBy">>, <<"fred@example.com">>},
                                   {<<"ForumName">>, <<"Amazon DynamoDB">>},
                                   {<<"LastPostDateTime">>, <<"201303201023">>},
                                   {<<"Tags">>, [<<"Update">>, <<"Multiple Items">>, <<"HelpMe">>]},
                                   {<<"Subject">>, <<"How do I update multiple items?">>},
                                   {<<"Message">>, <<"I want to update multiple items in a single API call. What's the best way to do that?">>}
                                  ]}}}),
         ?_ddb_test(
            {"DeleteItem metrics response", "
{
    \"ConsumedCapacity\": {
        \"CapacityUnits\": 1,
        \"TableName\": \"Thread\"
    },
    \"ItemCollectionMetrics\": {
        \"ItemCollectionKey\": {
            \"ForumName\": {
                \"S\": \"Amazon DynamoDB\"
            }
        },
        \"SizeEstimateRangeGB\": [1, 2]
    }
}",
             {ok, #ddb2_delete_item{
                     consumed_capacity =
                         #ddb2_consumed_capacity{
                            table_name = <<"Thread">>,
                            capacity_units = 1},
                     item_collection_metrics =
                         #ddb2_item_collection_metrics{
                            item_collection_key = <<"Amazon DynamoDB">>,
                            size_estimate_range_gb = {1,2}}
                     }}})
        ],
    
    output_tests(?_f(erlcloud_ddb2:delete_item(<<"table">>, {<<"k">>, <<"v">>}, [{out, record}])), Tests).

%% DeleteTable test based on the API examples:
%% http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_DeleteTable.html
delete_table_input_tests(_) ->
    Tests =
        [?_ddb_test(
            {"DeleteTable example request",
             ?_f(erlcloud_ddb2:delete_table(<<"Reply">>)), "
{
    \"TableName\": \"Reply\"
}"
            })
        ],

    Response = "
{
    \"TableDescription\": {
        \"ItemCount\": 0,
        \"ProvisionedThroughput\": {
            \"NumberOfDecreasesToday\": 0,
            \"ReadCapacityUnits\": 5,
            \"WriteCapacityUnits\": 5
        },
        \"TableName\": \"Reply\",
        \"TableSizeBytes\": 0,
        \"TableStatus\": \"DELETING\"
    }
}",
    input_tests(Response, Tests).

delete_table_output_tests(_) ->
    Tests = 
        [?_ddb_test(
            {"DeleteTable example response", "
{
    \"TableDescription\": {
        \"ItemCount\": 0,
        \"ProvisionedThroughput\": {
            \"NumberOfDecreasesToday\": 0,
            \"ReadCapacityUnits\": 5,
            \"WriteCapacityUnits\": 5
        },
        \"TableName\": \"Reply\",
        \"TableSizeBytes\": 0,
        \"TableStatus\": \"DELETING\"
    }
}",
             {ok, #ddb2_table_description
              {item_count = 0,
               provisioned_throughput = #ddb2_provisioned_throughput_description{
                                           read_capacity_units = 5,
                                           write_capacity_units = 5,
                                           number_of_decreases_today = 0},
               table_name = <<"Reply">>,
               table_size_bytes = 0,
               table_status = deleting}}})
        ],
    
    output_tests(?_f(erlcloud_ddb2:delete_table(<<"name">>)), Tests).

%% DescribeTable test based on the API examples:
%% http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_DescribeTables.html
describe_table_input_tests(_) ->
    Tests =
        [?_ddb_test(
            {"DescribeTable example request",
             ?_f(erlcloud_ddb2:describe_table(<<"Thread">>)), "
{
    \"TableName\":\"Thread\"
}"
            })
        ],

    Response = "
{
    \"Table\": {
        \"AttributeDefinitions\": [
            {
                \"AttributeName\": \"ForumName\",
                \"AttributeType\": \"S\"
            },
            {
                \"AttributeName\": \"LastPostDateTime\",
                \"AttributeType\": \"S\"
            },
            {
                \"AttributeName\": \"Subject\",
                \"AttributeType\": \"S\"
            }
        ],
        \"CreationDateTime\": 1.363729002358E9,
        \"ItemCount\": 0,
        \"KeySchema\": [
            {
                \"AttributeName\": \"ForumName\",
                \"KeyType\": \"HASH\"
            },
            {
                \"AttributeName\": \"Subject\",
                \"KeyType\": \"RANGE\"
            }
        ],
        \"LocalSecondaryIndexes\": [
            {
                \"IndexName\": \"LastPostIndex\",
                \"IndexSizeBytes\": 0,
                \"ItemCount\": 0,
                \"KeySchema\": [
                    {
                        \"AttributeName\": \"ForumName\",
                        \"KeyType\": \"HASH\"
                    },
                    {
                        \"AttributeName\": \"LastPostDateTime\",
                        \"KeyType\": \"RANGE\"
                    }
                ],
                \"Projection\": {
                    \"ProjectionType\": \"KEYS_ONLY\"
                }
            }
        ],
        \"ProvisionedThroughput\": {
            \"NumberOfDecreasesToday\": 0,
            \"ReadCapacityUnits\": 5,
            \"WriteCapacityUnits\": 5
        },
        \"TableName\": \"Thread\",
        \"TableSizeBytes\": 0,
        \"TableStatus\": \"ACTIVE\"
    }
}",
    input_tests(Response, Tests).

describe_table_output_tests(_) ->
    Tests = 
        [?_ddb_test(
            {"DescribeTable example response", "
{
    \"Table\": {
        \"AttributeDefinitions\": [
            {
                \"AttributeName\": \"ForumName\",
                \"AttributeType\": \"S\"
            },
            {
                \"AttributeName\": \"LastPostDateTime\",
                \"AttributeType\": \"S\"
            },
            {
                \"AttributeName\": \"Subject\",
                \"AttributeType\": \"S\"
            }
        ],
        \"CreationDateTime\": 1.363729002358E9,
        \"ItemCount\": 0,
        \"KeySchema\": [
            {
                \"AttributeName\": \"ForumName\",
                \"KeyType\": \"HASH\"
            },
            {
                \"AttributeName\": \"Subject\",
                \"KeyType\": \"RANGE\"
            }
        ],
        \"LocalSecondaryIndexes\": [
            {
                \"IndexName\": \"LastPostIndex\",
                \"IndexSizeBytes\": 0,
                \"ItemCount\": 0,
                \"KeySchema\": [
                    {
                        \"AttributeName\": \"ForumName\",
                        \"KeyType\": \"HASH\"
                    },
                    {
                        \"AttributeName\": \"LastPostDateTime\",
                        \"KeyType\": \"RANGE\"
                    }
                ],
                \"Projection\": {
                    \"ProjectionType\": \"KEYS_ONLY\"
                }
            }
        ],
        \"ProvisionedThroughput\": {
            \"NumberOfDecreasesToday\": 0,
            \"ReadCapacityUnits\": 5,
            \"WriteCapacityUnits\": 5
        },
        \"TableName\": \"Thread\",
        \"TableSizeBytes\": 0,
        \"TableStatus\": \"ACTIVE\"
    }
}",
             {ok, #ddb2_table_description
              {attribute_definitions = [{<<"ForumName">>, s},
                                        {<<"LastPostDateTime">>, s},
                                        {<<"Subject">>, s}],
               creation_date_time = 1363729002.358,
               item_count = 0,
               key_schema = {<<"ForumName">>, <<"Subject">>},
               local_secondary_indexes =
                   [#ddb2_local_secondary_index_description{
                       index_name = <<"LastPostIndex">>,
                       index_size_bytes = 0,
                       item_count = 0,
                       key_schema = {<<"ForumName">>, <<"LastPostDateTime">>},
                       projection = keys_only}],
               provisioned_throughput = 
                   #ddb2_provisioned_throughput_description{
                      last_decrease_date_time = undefined,
                      last_increase_date_time = undefined,
                      number_of_decreases_today = 0,
                      read_capacity_units = 5,
                      write_capacity_units = 5},
               table_name = <<"Thread">>,
               table_size_bytes = 0,
               table_status = active}}})
        ],
    
    output_tests(?_f(erlcloud_ddb2:describe_table(<<"name">>)), Tests).

%% GetItem test based on the API examples:
%% http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_GetItem.html
get_item_input_tests(_) ->
    Example1Response = "
{
    \"TableName\": \"Thread\",
    \"Key\": {
        \"ForumName\": {
            \"S\": \"Amazon DynamoDB\"
        },
        \"Subject\": {
            \"S\": \"How do I update multiple items?\"
        }
    },
    \"AttributesToGet\": [\"LastPostDateTime\",\"Message\",\"Tags\"],
    \"ConsistentRead\": true,
    \"ReturnConsumedCapacity\": \"TOTAL\"
}",

    Tests =
        [?_ddb_test(
            {"GetItem example request, with fully specified keys",
             ?_f(erlcloud_ddb2:get_item(<<"Thread">>,
                                       [{<<"ForumName">>, {s, <<"Amazon DynamoDB">>}}, 
                                        {<<"Subject">>, {s, <<"How do I update multiple items?">>}}],
                                       [{attributes_to_get, [<<"LastPostDateTime">>, <<"Message">>, <<"Tags">>]},
                                        consistent_read,
                                        {return_consumed_capacity, total},
                                        %% Make sure options at beginning of list override later options
                                        {consistent_read, false}]
                                      )),
             Example1Response}),
         ?_ddb_test(
            {"GetItem example request, with inferred key types",
             ?_f(erlcloud_ddb2:get_item(<<"Thread">>, 
                                       [{<<"ForumName">>, "Amazon DynamoDB"},
                                        {<<"Subject">>, <<"How do I update multiple items?">>}],
                                       [{attributes_to_get, [<<"LastPostDateTime">>, <<"Message">>, <<"Tags">>]},
                                        {consistent_read, true},
                                        {return_consumed_capacity, total}]
                                      )),
             Example1Response}),
         ?_ddb_test(
            {"GetItem Simple call with only hash key and no options",
             ?_f(erlcloud_ddb2:get_item(<<"TableName">>, {<<"HashKey">>, 1})), "
{\"TableName\":\"TableName\",
 \"Key\":{\"HashKey\":{\"N\":\"1\"}}
}"
             })
        ],

    Response = "
{
    \"ConsumedCapacity\": {
        \"CapacityUnits\": 1,
        \"TableName\": \"Thread\"
    },
    \"Item\": {
        \"Tags\": {
            \"SS\": [\"Update\",\"Multiple Items\",\"HelpMe\"]
        },
        \"LastPostDateTime\": {
            \"S\": \"201303190436\"
        },
        \"Message\": {
            \"S\": \"I want to update multiple items in a single API call. What's the best way to do that?\"
        }
    }
}",
    input_tests(Response, Tests).

get_item_output_tests(_) ->
    Tests = 
        [?_ddb_test(
            {"GetItem example response", "
{
    \"ConsumedCapacity\": {
        \"CapacityUnits\": 1,
        \"TableName\": \"Thread\"
    },
    \"Item\": {
        \"Tags\": {
            \"SS\": [\"Update\",\"Multiple Items\",\"HelpMe\"]
        },
        \"LastPostDateTime\": {
            \"S\": \"201303190436\"
        },
        \"Message\": {
            \"S\": \"I want to update multiple items in a single API call. What's the best way to do that?\"
        }
    }
}",
             {ok, #ddb2_get_item{
                     item = [{<<"Tags">>, [<<"Update">>, <<"Multiple Items">>, <<"HelpMe">>]},
                             {<<"LastPostDateTime">>, <<"201303190436">>},
                             {<<"Message">>, <<"I want to update multiple items in a single API call. What's the best way to do that?">>}],
                    consumed_capacity = #ddb2_consumed_capacity{
                                          capacity_units = 1,
                                          table_name = <<"Thread">>}}}}),
         ?_ddb_test(
            {"GetItem test all attribute types", "
{\"Item\":
	{\"ss\":{\"SS\":[\"Lynda\", \"Aaron\"]},
	 \"ns\":{\"NS\":[\"12\",\"13.0\",\"14.1\"]},
	 \"bs\":{\"BS\":[\"BbY=\"]},
	 \"es\":{\"SS\":[]},
	 \"s\":{\"S\":\"Lynda\"},
	 \"n\":{\"N\":\"12\"},
	 \"f\":{\"N\":\"12.34\"},
	 \"b\":{\"B\":\"BbY=\"},
	 \"empty\":{\"S\":\"\"}
	}
}",
             {ok, #ddb2_get_item{
                     item = [{<<"ss">>, [<<"Lynda">>, <<"Aaron">>]},
                             {<<"ns">>, [12,13.0,14.1]},
                             {<<"bs">>, [<<5,182>>]},
                             {<<"es">>, []},
                             {<<"s">>, <<"Lynda">>},
                             {<<"n">>, 12},
                             {<<"f">>, 12.34},
                             {<<"b">>, <<5,182>>},
                             {<<"empty">>, <<>>}],
                    consumed_capacity = undefined}}}),
         ?_ddb_test(
            {"GetItem item not found", 
             "{}",
             {ok, #ddb2_get_item{item = undefined}}}),
         ?_ddb_test(
            {"GetItem no attributes returned", 
             "{\"Item\":{}}",
             {ok, #ddb2_get_item{item = []}}})
        ],
    
    output_tests(?_f(erlcloud_ddb2:get_item(<<"table">>, {<<"k">>, <<"v">>}, [{out, record}])), Tests).

%% ListTables test based on the API examples:
%% http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_ListTables.html
list_tables_input_tests(_) ->
    Tests =
        [?_ddb_test(
            {"ListTables example request",
             ?_f(erlcloud_ddb2:list_tables([{limit, 3}, {exclusive_start_table_name, <<"Forum">>}])), "
{
    \"ExclusiveStartTableName\": \"Forum\",
    \"Limit\": 3
}"
            }),
         ?_ddb_test(
            {"ListTables empty request",
             ?_f(erlcloud_ddb2:list_tables()), 
             "{}"
            })

        ],

    Response = "
{
    \"LastEvaluatedTableName\": \"Thread\",
    \"TableNames\": [\"Forum\",\"Reply\",\"Thread\"]
}",
    input_tests(Response, Tests).

list_tables_output_tests(_) ->
    Tests = 
        [?_ddb_test(
            {"ListTables example response", "
{
    \"LastEvaluatedTableName\": \"Thread\",
    \"TableNames\": [\"Forum\",\"Reply\",\"Thread\"]
}",
             {ok, #ddb2_list_tables
              {last_evaluated_table_name = <<"Thread">>,
               table_names = [<<"Forum">>, <<"Reply">>, <<"Thread">>]}}})
        ],
    
    output_tests(?_f(erlcloud_ddb2:list_tables([{out, record}])), Tests).

%% PutItem test based on the API examples:
%% http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_PutItem.html
put_item_input_tests(_) ->
    Tests =
        [?_ddb_test(
            {"PutItem example request",
             ?_f(erlcloud_ddb2:put_item(<<"Thread">>, 
                                       [{<<"LastPostedBy">>, <<"fred@example.com">>},
                                        {<<"ForumName">>, <<"Amazon DynamoDB">>},
                                        {<<"LastPostDateTime">>, <<"201303190422">>},
                                        {<<"Tags">>, {ss, [<<"Update">>, <<"Multiple Items">>, <<"HelpMe">>]}},
                                        {<<"Subject">>, <<"How do I update multiple items?">>},
                                        {<<"Message">>, <<"I want to update multiple items in a single API call. What's the best way to do that?">>}],
                                       [{expected, [{<<"ForumName">>, false}, {<<"Subject">>, false}]}])), "
{
    \"TableName\": \"Thread\",
    \"Item\": {
        \"LastPostDateTime\": {
            \"S\": \"201303190422\"
        },
        \"Tags\": {
            \"SS\": [\"Update\",\"Multiple Items\",\"HelpMe\"]
        },
        \"ForumName\": {
            \"S\": \"Amazon DynamoDB\"
        },
        \"Message\": {
            \"S\": \"I want to update multiple items in a single API call. What's the best way to do that?\"
        },
        \"Subject\": {
            \"S\": \"How do I update multiple items?\"
        },
        \"LastPostedBy\": {
            \"S\": \"fred@example.com\"
        }
    },
    \"Expected\": {
        \"ForumName\": {
            \"Exists\": false
        },
        \"Subject\": {
            \"Exists\": false
        }
    }
}"
            }),
         ?_ddb_test(
            {"PutItem float inputs",
             ?_f(erlcloud_ddb2:put_item(<<"Thread">>, 
                                       [{<<"typed float">>, {n, 1.2}},
                                        {<<"untyped float">>, 3.456},
                                        {<<"mixed set">>, {ns, [7.8, 9.0, 10]}}],
                                       [])), "
{
    \"TableName\": \"Thread\",
    \"Item\": {
        \"typed float\": {
            \"N\": \"1.2\"
        },
        \"untyped float\": {
            \"N\": \"3.456\"
        },
        \"mixed set\": {
            \"NS\": [\"7.8\", \"9.0\", \"10\"]
        }
    }
}"
            })
        ],

    Response = "
{
}",
    input_tests(Response, Tests).

put_item_output_tests(_) ->
    Tests = 
        [?_ddb_test(
            {"PutItem example response", "
{
}",
             {ok, #ddb2_put_item{}}}),
         ?_ddb_test(
            {"PutItem complete response", "
{
    \"Attributes\": {
        \"LastPostedBy\": {
            \"S\": \"fred@example.com\"
        },
        \"ForumName\": {
            \"S\": \"Amazon DynamoDB\"
        },
        \"LastPostDateTime\": {
            \"S\": \"201303201023\"
        },
        \"Tags\": {
            \"SS\": [\"Update\",\"Multiple Items\",\"HelpMe\"]
        },
        \"Subject\": {
            \"S\": \"How do I update multiple items?\"
        },
        \"Message\": {
            \"S\": \"I want to update multiple items in a single API call. What's the best way to do that?\"
        }
    },
    \"ConsumedCapacity\": {
        \"CapacityUnits\": 1,
        \"TableName\": \"Thread\"
    },
    \"ItemCollectionMetrics\": {
        \"ItemCollectionKey\": {
            \"ForumName\": {
                \"S\": \"Amazon DynamoDB\"
            }
        },
        \"SizeEstimateRangeGB\": [1, 2]
    }
}",

             {ok, #ddb2_put_item{
                     attributes = [{<<"LastPostedBy">>, <<"fred@example.com">>},
                                   {<<"ForumName">>, <<"Amazon DynamoDB">>},
                                   {<<"LastPostDateTime">>, <<"201303201023">>},
                                   {<<"Tags">>, [<<"Update">>, <<"Multiple Items">>, <<"HelpMe">>]},
                                   {<<"Subject">>, <<"How do I update multiple items?">>},
                                   {<<"Message">>, <<"I want to update multiple items in a single API call. What's the best way to do that?">>}],
                     consumed_capacity =
                         #ddb2_consumed_capacity{
                            table_name = <<"Thread">>,
                            capacity_units = 1},
                     item_collection_metrics =
                         #ddb2_item_collection_metrics{
                            item_collection_key = <<"Amazon DynamoDB">>,
                            size_estimate_range_gb = {1,2}}
                     }}})
        ],
    
    output_tests(?_f(erlcloud_ddb2:put_item(<<"table">>, [], [{out, record}])), Tests).

%% Query test based on the API examples:
%% http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_Query.html
q_input_tests(_) ->
    Tests =
        [?_ddb_test(
            {"Query example 1 request",
             ?_f(erlcloud_ddb2:q(<<"Thread">>,
                                [{<<"LastPostDateTime">>, {{s, <<"20130101">>}, {s, <<"20130115">>}}, between},
                                 {<<"ForumName">>, {s, <<"Amazon DynamoDB">>}}],
                                [{index_name, <<"LastPostIndex">>},
                                 {select, all_attributes},
                                 {limit, 3},
                                 {consistent_read, true}])), "
{
    \"TableName\": \"Thread\",
    \"IndexName\": \"LastPostIndex\",
    \"Select\": \"ALL_ATTRIBUTES\",
    \"Limit\":3,
    \"ConsistentRead\": true,
    \"KeyConditions\": {
        \"LastPostDateTime\": {
            \"AttributeValueList\": [
                {
                    \"S\": \"20130101\"
                },
                {
                    \"S\": \"20130115\"
                }
            ],
            \"ComparisonOperator\": \"BETWEEN\"
        },
        \"ForumName\": {
            \"AttributeValueList\": [
                {
                    \"S\": \"Amazon DynamoDB\"
                }
            ],
            \"ComparisonOperator\": \"EQ\"
        }
    }
}"
            }),
         ?_ddb_test(
            {"Query example 2 request",
             ?_f(erlcloud_ddb2:q(<<"Thread">>,
                                {<<"ForumName">>, <<"Amazon DynamoDB">>, eq},
                                [{select, count},
                                 {consistent_read, true}])), "
{
    \"TableName\": \"Thread\",
    \"Select\": \"COUNT\",
    \"ConsistentRead\": true,
    \"KeyConditions\": {
        \"ForumName\": {
            \"AttributeValueList\": [
                {
                    \"S\": \"Amazon DynamoDB\"
                }
            ],
            \"ComparisonOperator\": \"EQ\"
        }
    }
}"
            }),
         ?_ddb_test(
            {"Query exclusive start key",
             ?_f(erlcloud_ddb2:q(<<"Thread">>,
                                [{<<"ForumName">>, <<"Amazon DynamoDB">>, eq}],
                                [{select, count},
                                 {index_name, <<"LastPostIndex">>},
                                 {exclusive_start_key, [{<<"ForumName">>, {s, <<"Amazon DynamoDB">>}},
                                                        {<<"Subject">>, {s, <<"Exclusive key can have 3 parts">>}},
                                                        {<<"LastPostDateTime">>, {s, <<"20130102054211">>}}]}])), "
{
    \"TableName\": \"Thread\",
    \"IndexName\": \"LastPostIndex\",
    \"Select\": \"COUNT\",
    \"ExclusiveStartKey\": {
        \"ForumName\": {
            \"S\": \"Amazon DynamoDB\"
        },
        \"Subject\": {
            \"S\": \"Exclusive key can have 3 parts\"
        },
        \"LastPostDateTime\": {
            \"S\": \"20130102054211\"
        }
    },
    \"KeyConditions\": {
        \"ForumName\": {
            \"AttributeValueList\": [
                {
                    \"S\": \"Amazon DynamoDB\"
                }
            ],
            \"ComparisonOperator\": \"EQ\"
        }
    }
}"
            })
        ],

    Response = "
{
    \"Count\":17
}",
    input_tests(Response, Tests).

q_output_tests(_) ->
    Tests = 
        [?_ddb_test(
            {"Query example 1 response", "
{
    \"Count\": 3,
    \"Items\": [
        {
            \"LastPostedBy\": {
                \"S\": \"fred@example.com\"
            },
            \"ForumName\": {
                \"S\": \"Amazon DynamoDB\"
            },
            \"LastPostDateTime\": {
                \"S\": \"20130102054211\"
            },
            \"Tags\": {
                \"SS\": [\"Problem\",\"Question\"]
            }
        },
        {
            \"LastPostedBy\": {
                \"S\": \"alice@example.com\"
            },
            \"ForumName\": {
                \"S\": \"Amazon DynamoDB\"
            },
            \"LastPostDateTime\": {
                    \"S\": \"20130105111307\"
            },
            \"Tags\": {
                \"SS\": [\"Idea\"]
            }
        },
        {
            \"LastPostedBy\": {
                \"S\": \"bob@example.com\"
            },
            \"ForumName\": {
                \"S\": \"Amazon DynamoDB\"
            },
            \"LastPostDateTime\": {
                \"S\": \"20130108094417\"
            },
            \"Tags\": {
                \"SS\": [\"AppDesign\", \"HelpMe\"]
            }
        }
    ],
    \"ConsumedCapacity\": {
        \"CapacityUnits\": 2,
        \"TableName\": \"Thread\"
    }
}",
             {ok, #ddb2_q{count = 3,
                         items = [[{<<"LastPostedBy">>, <<"fred@example.com">>},
                                   {<<"ForumName">>, <<"Amazon DynamoDB">>},
                                   {<<"LastPostDateTime">>, <<"20130102054211">>},
                                   {<<"Tags">>, [<<"Problem">>,<<"Question">>]}],
                                  [{<<"LastPostedBy">>, <<"alice@example.com">>},
                                   {<<"ForumName">>, <<"Amazon DynamoDB">>},
                                   {<<"LastPostDateTime">>, <<"20130105111307">>},
                                   {<<"Tags">>, [<<"Idea">>]}],
                                  [{<<"LastPostedBy">>, <<"bob@example.com">>},
                                   {<<"ForumName">>, <<"Amazon DynamoDB">>},
                                   {<<"LastPostDateTime">>, <<"20130108094417">>},
                                   {<<"Tags">>, [<<"AppDesign">>, <<"HelpMe">>]}]],
                         consumed_capacity =
                             #ddb2_consumed_capacity{
                                capacity_units = 2,
                                table_name = <<"Thread">>}}}}),
         ?_ddb_test(
            {"Query example 2 response", "
{
    \"Count\":17
}",
             {ok, #ddb2_q{count = 17}}}),
         ?_ddb_test(
            {"Query last evaluated key", "
{
    \"Count\":17,
    \"LastEvaluatedKey\": {
        \"ForumName\": {
            \"S\": \"Amazon DynamoDB\"
        },
        \"Subject\": {
            \"S\": \"Exclusive key can have 3 parts\"
        },
        \"LastPostDateTime\": {
            \"S\": \"20130102054211\"
        }
    }
}",
             {ok, #ddb2_q{count = 17,
                         last_evaluated_key = 
                             [{<<"ForumName">>, {s, <<"Amazon DynamoDB">>}},
                              {<<"Subject">>, {s, <<"Exclusive key can have 3 parts">>}},                                                        {<<"LastPostDateTime">>, {s, <<"20130102054211">>}}]
                         }}})
        ],
    
    output_tests(?_f(erlcloud_ddb2:q(<<"table">>, [{<<"k">>, <<"v">>, eq}], [{out, record}])), Tests).

%% Scan test based on the API examples:
%% http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_Scan.html
scan_input_tests(_) ->
    Tests =
        [?_ddb_test(
            {"Scan example 1 request",
             ?_f(erlcloud_ddb2:scan(<<"Reply">>, [{return_consumed_capacity, total}])), "
{
    \"TableName\": \"Reply\",
    \"ReturnConsumedCapacity\": \"TOTAL\"
}"
            }),
         ?_ddb_test(
            {"Scan example 2 request",
             ?_f(erlcloud_ddb2:scan(<<"Reply">>, 
                                   [{scan_filter, [{<<"PostedBy">>, <<"joe@example.com">>, eq}]},
                                    {return_consumed_capacity, total}])), "
{
    \"TableName\": \"Reply\",
    \"ScanFilter\": {
        \"PostedBy\": {
            \"AttributeValueList\": [
                {
                    \"S\": \"joe@example.com\"
                }
            ],
            \"ComparisonOperator\": \"EQ\"
        }
    },
    \"ReturnConsumedCapacity\": \"TOTAL\"
}"
            }),
         ?_ddb_test(
            {"Scan exclusive start key",
             ?_f(erlcloud_ddb2:scan(<<"Reply">>, 
                                   [{exclusive_start_key, [{<<"ForumName">>, {s, <<"Amazon DynamoDB">>}},
                                                           {<<"LastPostDateTime">>, {n, 20130102054211}}]}])), "
{
    \"TableName\": \"Reply\",
    \"ExclusiveStartKey\": {
        \"ForumName\": {
            \"S\": \"Amazon DynamoDB\"
        },
        \"LastPostDateTime\": {
            \"N\": \"20130102054211\"
        }
    }
}"
            }),
         ?_ddb_test(
            {"Parallel Scan",
             ?_f(erlcloud_ddb2:scan(<<"Reply">>,
                                    [{segment, 1},
                                     {total_segments, 2}])), "
{
    \"TableName\": \"Reply\",
    \"Segment\": 1,
    \"TotalSegments\": 2
}"
            })
        ],

    Response = "
{
    \"ConsumedCapacity\": {
        \"CapacityUnits\": 0.5,
        \"TableName\": \"Reply\"
    },
    \"Count\": 4,
    \"Items\": [
        {
            \"PostedBy\": {
                \"S\": \"joe@example.com\"
            },
            \"ReplyDateTime\": {
                \"S\": \"20130320115336\"
            },
            \"Id\": {
                \"S\": \"Amazon DynamoDB#How do I update multiple items?\"
            },
            \"Message\": {
                \"S\": \"Have you looked at the BatchWriteItem API?\"
            }
        },
        {
            \"PostedBy\": {
                \"S\": \"fred@example.com\"
            },
            \"ReplyDateTime\": {
                \"S\": \"20130320115342\"
            },
            \"Id\": {
                \"S\": \"Amazon DynamoDB#How do I update multiple items?\"
            },
            \"Message\": {
                \"S\": \"No, I didn't know about that.  Where can I find more information?\"
            }
        },
        {
            \"PostedBy\": {
                \"S\": \"joe@example.com\"
            },
            \"ReplyDateTime\": {
                \"S\": \"20130320115347\"
            },
            \"Id\": {
                \"S\": \"Amazon DynamoDB#How do I update multiple items?\"
            },
            \"Message\": {
                \"S\": \"BatchWriteItem is documented in the Amazon DynamoDB API Reference.\"
            }
        },
        {
            \"PostedBy\": {
                \"S\": \"fred@example.com\"
            },
            \"ReplyDateTime\": {
                \"S\": \"20130320115352\"
            },
            \"Id\": {
                \"S\": \"Amazon DynamoDB#How do I update multiple items?\"
            },
            \"Message\": {
                \"S\": \"OK, I'll take a look at that.  Thanks!\"
            }
        }
    ],
    \"ScannedCount\": 4
}",
    input_tests(Response, Tests).

scan_output_tests(_) ->
    Tests = 
        [?_ddb_test(
            {"Scan example 1 response", "
{
    \"ConsumedCapacity\": {
        \"CapacityUnits\": 0.5,
        \"TableName\": \"Reply\"
    },
    \"Count\": 4,
    \"Items\": [
        {
            \"PostedBy\": {
                \"S\": \"joe@example.com\"
            },
            \"ReplyDateTime\": {
                \"S\": \"20130320115336\"
            },
            \"Id\": {
                \"S\": \"Amazon DynamoDB#How do I update multiple items?\"
            },
            \"Message\": {
                \"S\": \"Have you looked at the BatchWriteItem API?\"
            }
        },
        {
            \"PostedBy\": {
                \"S\": \"fred@example.com\"
            },
            \"ReplyDateTime\": {
                \"S\": \"20130320115342\"
            },
            \"Id\": {
                \"S\": \"Amazon DynamoDB#How do I update multiple items?\"
            },
            \"Message\": {
                \"S\": \"No, I didn't know about that.  Where can I find more information?\"
            }
        },
        {
            \"PostedBy\": {
                \"S\": \"joe@example.com\"
            },
            \"ReplyDateTime\": {
                \"S\": \"20130320115347\"
            },
            \"Id\": {
                \"S\": \"Amazon DynamoDB#How do I update multiple items?\"
            },
            \"Message\": {
                \"S\": \"BatchWriteItem is documented in the Amazon DynamoDB API Reference.\"
            }
        },
        {
            \"PostedBy\": {
                \"S\": \"fred@example.com\"
            },
            \"ReplyDateTime\": {
                \"S\": \"20130320115352\"
            },
            \"Id\": {
                \"S\": \"Amazon DynamoDB#How do I update multiple items?\"
            },
            \"Message\": {
                \"S\": \"OK, I'll take a look at that.  Thanks!\"
            }
        }
    ],
    \"ScannedCount\": 4
}",
             {ok, #ddb2_scan
              {count = 4,
               items = [[{<<"PostedBy">>, <<"joe@example.com">>},
                         {<<"ReplyDateTime">>, <<"20130320115336">>},
                         {<<"Id">>, <<"Amazon DynamoDB#How do I update multiple items?">>},
                         {<<"Message">>, <<"Have you looked at the BatchWriteItem API?">>}],
                        [{<<"PostedBy">>, <<"fred@example.com">>},
                         {<<"ReplyDateTime">>, <<"20130320115342">>},
                         {<<"Id">>, <<"Amazon DynamoDB#How do I update multiple items?">>},
                         {<<"Message">>, <<"No, I didn't know about that.  Where can I find more information?">>}],
                        [{<<"PostedBy">>, <<"joe@example.com">>},
                         {<<"ReplyDateTime">>, <<"20130320115347">>},
                         {<<"Id">>, <<"Amazon DynamoDB#How do I update multiple items?">>},
                         {<<"Message">>, <<"BatchWriteItem is documented in the Amazon DynamoDB API Reference.">>}],
                        [{<<"PostedBy">>, <<"fred@example.com">>},
                         {<<"ReplyDateTime">>, <<"20130320115352">>},
                         {<<"Id">>, <<"Amazon DynamoDB#How do I update multiple items?">>},
                         {<<"Message">>, <<"OK, I'll take a look at that.  Thanks!">>}]],
               scanned_count = 4,
               consumed_capacity = #ddb2_consumed_capacity{table_name = <<"Reply">>,
                                                          capacity_units = 0.5}}}}),
         ?_ddb_test(
            {"Scan example 2 response", "
{
    \"ConsumedCapacity\": {
        \"CapacityUnits\": 0.5,
        \"TableName\": \"Reply\"
    },
    \"Count\": 2,
    \"Items\": [
        {
            \"PostedBy\": {
                \"S\": \"joe@example.com\"
            },
            \"ReplyDateTime\": {
                \"S\": \"20130320115336\"
            },
            \"Id\": {
                \"S\": \"Amazon DynamoDB#How do I update multiple items?\"
            },
            \"Message\": {
                \"S\": \"Have you looked at the BatchWriteItem API?\"
            }
        },
        {
            \"PostedBy\": {
                \"S\": \"joe@example.com\"
            },
            \"ReplyDateTime\": {
                \"S\": \"20130320115347\"
            },
            \"Id\": {
                \"S\": \"Amazon DynamoDB#How do I update multiple items?\"
            },
            \"Message\": {
                \"S\": \"BatchWriteItem is documented in the Amazon DynamoDB API Reference.\"
            }
        }
    ],
    \"ScannedCount\": 4
}",
             {ok, #ddb2_scan
              {count = 2,
               items = [[{<<"PostedBy">>, <<"joe@example.com">>},
                         {<<"ReplyDateTime">>, <<"20130320115336">>},
                         {<<"Id">>, <<"Amazon DynamoDB#How do I update multiple items?">>},
                         {<<"Message">>, <<"Have you looked at the BatchWriteItem API?">>}],
                        [{<<"PostedBy">>, <<"joe@example.com">>},
                         {<<"ReplyDateTime">>, <<"20130320115347">>},
                         {<<"Id">>, <<"Amazon DynamoDB#How do I update multiple items?">>},
                         {<<"Message">>, <<"BatchWriteItem is documented in the Amazon DynamoDB API Reference.">>}]
],
               scanned_count = 4,
               consumed_capacity = #ddb2_consumed_capacity{table_name = <<"Reply">>,
                                                          capacity_units = 0.5}}}}),
         ?_ddb_test(
            {"Scan last evaluated key", "

{
    \"Count\":2,
    \"Items\": [
        {
            \"PostedBy\": {
                \"S\": \"joe@example.com\"
            },
            \"ReplyDateTime\": {
                \"S\": \"20130320115336\"
            },
            \"Id\": {
                \"S\": \"Amazon DynamoDB#How do I update multiple items?\"
            },
            \"Message\": {
                \"S\": \"Have you looked at the BatchWriteItem API?\"
            }
        },
        {
            \"PostedBy\": {
                \"S\": \"joe@example.com\"
            },
            \"ReplyDateTime\": {
                \"S\": \"20130320115347\"
            },
            \"Id\": {
                \"S\": \"Amazon DynamoDB#How do I update multiple items?\"
            },
            \"Message\": {
                \"S\": \"BatchWriteItem is documented in the Amazon DynamoDB API Reference.\"
            }
        }
    ],
    \"LastEvaluatedKey\": {
        \"ForumName\": {
            \"S\": \"Amazon DynamoDB\"
        },
        \"LastPostDateTime\": {
            \"N\": \"20130102054211\"
        }
    },
    \"ScannedCount\":4
}",
             {ok, #ddb2_scan
              {count = 2,
               items = [[{<<"PostedBy">>, <<"joe@example.com">>},
                         {<<"ReplyDateTime">>, <<"20130320115336">>},
                         {<<"Id">>, <<"Amazon DynamoDB#How do I update multiple items?">>},
                         {<<"Message">>, <<"Have you looked at the BatchWriteItem API?">>}],
                        [{<<"PostedBy">>, <<"joe@example.com">>},
                         {<<"ReplyDateTime">>, <<"20130320115347">>},
                         {<<"Id">>, <<"Amazon DynamoDB#How do I update multiple items?">>},
                         {<<"Message">>, <<"BatchWriteItem is documented in the Amazon DynamoDB API Reference.">>}]
],
               last_evaluated_key = [{<<"ForumName">>, {s, <<"Amazon DynamoDB">>}},
                                     {<<"LastPostDateTime">>, {n, 20130102054211}}],
               scanned_count = 4}}})
        ],
    
    output_tests(?_f(erlcloud_ddb2:scan(<<"name">>, [{out, record}])), Tests).

%% UpdateItem test based on the API examples:
%% http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_UpdateItem.html
update_item_input_tests(_) ->
    Tests =
        [?_ddb_test(
            {"UpdateItem example request",
             ?_f(erlcloud_ddb2:update_item(<<"Thread">>, 
                                          [{<<"ForumName">>, {s, <<"Amazon DynamoDB">>}},
                                           {<<"Subject">>, {s, <<"How do I update multiple items?">>}}],
                                          [{<<"LastPostedBy">>, {s, <<"alice@example.com">>}, put}],
                                          [{expected, {<<"LastPostedBy">>, {s, <<"fred@example.com">>}}},
                                           {return_values, all_new}])), "
{
    \"TableName\": \"Thread\",
    \"Key\": {
        \"ForumName\": {
            \"S\": \"Amazon DynamoDB\"
        },
        \"Subject\": {
            \"S\": \"How do I update multiple items?\"
        }
    },
    \"AttributeUpdates\": {
        \"LastPostedBy\": {
            \"Value\": {
                \"S\": \"alice@example.com\"
            },
            \"Action\": \"PUT\"
        }
    },
    \"Expected\": {
        \"LastPostedBy\": {
            \"Value\": {
                \"S\": \"fred@example.com\"
            }
        }
    },
    \"ReturnValues\": \"ALL_NEW\"
}"
            }),
         ?_ddb_test(
            {"UpdateItem example request 2",
             ?_f(erlcloud_ddb2:update_item(<<"Thread">>,
                                          [{<<"ForumName">>, {s, <<"Amazon DynamoDB">>}},
                                           {<<"Subject">>, {s, <<"A question about updates">>}}],
                                          [{<<"Replies">>, {n, 1}, add}],
                                          [{return_values, none}])), "
{
    \"TableName\": \"Thread\",
    \"Key\": {
        \"ForumName\": {
            \"S\": \"Amazon DynamoDB\"
        },
        \"Subject\": {
            \"S\": \"A question about updates\"
        }
    },
    \"AttributeUpdates\": {
        \"Replies\": {
            \"Action\": \"ADD\",
            \"Value\": {
                \"N\": \"1\"
            }
        }
    },
    \"ReturnValues\" : \"NONE\"
}"
            }),
         ?_ddb_test(
            {"UpdateItem no attribute updates",
             ?_f(erlcloud_ddb2:update_item(<<"Thread">>,
                                          [{<<"ForumName">>, {s, <<"Amazon DynamoDB">>}},
                                           {<<"Subject">>, {s, <<"A question about updates">>}}],
                                          [])), "
{
    \"TableName\": \"Thread\",
    \"Key\": {
        \"ForumName\": {
            \"S\": \"Amazon DynamoDB\"
        },
        \"Subject\": {
            \"S\": \"A question about updates\"
        }
    }
}"
            })
        ],

    Response = "
{
}",
    input_tests(Response, Tests).

update_item_output_tests(_) ->
    Tests = 
        [?_ddb_test(
            {"UpdateItem example response", "
{
    \"Attributes\": {
        \"LastPostedBy\": {
            \"S\": \"alice@example.com\"
        },
        \"ForumName\": {
            \"S\": \"Amazon DynamoDB\"
        },
        \"LastPostDateTime\": {
            \"S\": \"20130320010350\"
        },
        \"Tags\": {
            \"SS\": [\"Update\",\"Multiple Items\",\"HelpMe\"]
        },
        \"Subject\": {
            \"S\": \"Maximum number of items?\"
        },
        \"Views\": {
            \"N\": \"5\"
        },
        \"Message\": {
            \"S\": \"I want to put 10 million data items to an Amazon DynamoDB table.  Is there an upper limit?\"
        }
    }
}",
             {ok, [{<<"LastPostedBy">>, <<"alice@example.com">>},
                   {<<"ForumName">>, <<"Amazon DynamoDB">>},
                   {<<"LastPostDateTime">>, <<"20130320010350">>},
                   {<<"Tags">>, [<<"Update">>,<<"Multiple Items">>,<<"HelpMe">>]},
                   {<<"Subject">>, <<"Maximum number of items?">>},
                   {<<"Views">>, 5},
                   {<<"Message">>, <<"I want to put 10 million data items to an Amazon DynamoDB table.  Is there an upper limit?">>}]}}),
         ?_ddb_test(
            {"UpdateItem example response 2", "
{
}",
             {ok, []}})
        ],
    
    output_tests(?_f(erlcloud_ddb2:update_item(<<"table">>, {<<"k">>, <<"v">>}, [])), Tests).

%% UpdateTable test based on the API examples:
%% http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_UpdateTable.html
update_table_input_tests(_) ->
    Tests =
        [?_ddb_test(
            {"UpdateTable example request",
             ?_f(erlcloud_ddb2:update_table(<<"Thread">>, 10, 10)), "
{
    \"TableName\": \"Thread\",
    \"ProvisionedThroughput\": {
        \"ReadCapacityUnits\": 10,
        \"WriteCapacityUnits\": 10
    }
}"
            })
        ],

    Response = "
{          
    \"TableDescription\": {
        \"AttributeDefinitions\": [
            {
                \"AttributeName\": \"ForumName\",
                \"AttributeType\": \"S\"
            },
            {
                \"AttributeName\": \"LastPostDateTime\",
                \"AttributeType\": \"S\"
            },
            {
                \"AttributeName\": \"Subject\",
                \"AttributeType\": \"S\"
            }
        ],
        \"CreationDateTime\": 1.363801528686E9,
        \"ItemCount\": 0,
        \"KeySchema\": [
            {
                \"AttributeName\": \"ForumName\",
                \"KeyType\": \"HASH\"
            },
            {
                \"AttributeName\": \"Subject\",
                \"KeyType\": \"RANGE\"
            }
        ],
        \"LocalSecondaryIndexes\": [
            {
                \"IndexName\": \"LastPostIndex\",
                \"IndexSizeBytes\": 0,
                \"ItemCount\": 0,
                \"KeySchema\": [
                    {
                        \"AttributeName\": \"ForumName\",
                        \"KeyType\": \"HASH\"
                    },
                    {
                        \"AttributeName\": \"LastPostDateTime\",
                        \"KeyType\": \"RANGE\"
                    }
                ],
                \"Projection\": {
                    \"ProjectionType\": \"KEYS_ONLY\"
                }
            }
        ],
        \"ProvisionedThroughput\": {
            \"LastIncreaseDateTime\": 1.363801701282E9,
            \"NumberOfDecreasesToday\": 0,
            \"ReadCapacityUnits\": 5,
            \"WriteCapacityUnits\": 5
        },
        \"TableName\": \"Thread\",
        \"TableSizeBytes\": 0,
        \"TableStatus\": \"UPDATING\"
    }
}",
    input_tests(Response, Tests).

update_table_output_tests(_) ->
    Tests = 
        [?_ddb_test(
            {"UpdateTable example response", "
{          
    \"TableDescription\": {
        \"AttributeDefinitions\": [
            {
                \"AttributeName\": \"ForumName\",
                \"AttributeType\": \"S\"
            },
            {
                \"AttributeName\": \"LastPostDateTime\",
                \"AttributeType\": \"S\"
            },
            {
                \"AttributeName\": \"Subject\",
                \"AttributeType\": \"S\"
            }
        ],
        \"CreationDateTime\": 1.363801528686E9,
        \"ItemCount\": 0,
        \"KeySchema\": [
            {
                \"AttributeName\": \"ForumName\",
                \"KeyType\": \"HASH\"
            },
            {
                \"AttributeName\": \"Subject\",
                \"KeyType\": \"RANGE\"
            }
        ],
        \"LocalSecondaryIndexes\": [
            {
                \"IndexName\": \"LastPostIndex\",
                \"IndexSizeBytes\": 0,
                \"ItemCount\": 0,
                \"KeySchema\": [
                    {
                        \"AttributeName\": \"ForumName\",
                        \"KeyType\": \"HASH\"
                    },
                    {
                        \"AttributeName\": \"LastPostDateTime\",
                        \"KeyType\": \"RANGE\"
                    }
                ],
                \"Projection\": {
                    \"ProjectionType\": \"KEYS_ONLY\"
                }
            }
        ],
        \"ProvisionedThroughput\": {
            \"LastIncreaseDateTime\": 1.363801701282E9,
            \"NumberOfDecreasesToday\": 0,
            \"ReadCapacityUnits\": 5,
            \"WriteCapacityUnits\": 5
        },
        \"TableName\": \"Thread\",
        \"TableSizeBytes\": 0,
        \"TableStatus\": \"UPDATING\"
    }
}",
             {ok, #ddb2_table_description
              {attribute_definitions = [{<<"ForumName">>, s},
                                        {<<"LastPostDateTime">>, s},
                                        {<<"Subject">>, s}],
               creation_date_time = 1363801528.686,
               item_count = 0,
               key_schema = {<<"ForumName">>, <<"Subject">>},
               local_secondary_indexes =
                   [#ddb2_local_secondary_index_description{
                       index_name = <<"LastPostIndex">>,
                       index_size_bytes = 0,
                       item_count = 0,
                       key_schema = {<<"ForumName">>, <<"LastPostDateTime">>},
                       projection = keys_only}],
               provisioned_throughput = 
                   #ddb2_provisioned_throughput_description{
                      last_decrease_date_time = undefined,
                      last_increase_date_time = 1363801701.282,
                      number_of_decreases_today = 0,
                      read_capacity_units = 5,
                      write_capacity_units = 5},
               table_name = <<"Thread">>,
               table_size_bytes = 0,
               table_status = updating}}})
        ],
    
    output_tests(?_f(erlcloud_ddb2:update_table(<<"name">>, 5, 15)), Tests).

