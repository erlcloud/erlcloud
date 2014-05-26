%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_ddb_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").
-include("erlcloud_ddb.hrl").

%% Unit tests for ddb.
%% These tests work by using meck to mock erlcloud_httpc. There are two classes of test: input and output.
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
    meck:new(erlcloud_httpc),
    ok.

stop(_) ->
    meck:unload(erlcloud_httpc).

%%%===================================================================
%%% Input test helpers
%%%===================================================================

-type expected_body() :: string().

%% verifies that the parameters in the body match the expected parameters
-spec validate_body(binary(), expected_body()) -> ok.
validate_body(Body, Expected) ->
    Want = jsx:decode(list_to_binary(Expected)), 
    Actual = jsx:decode(Body),
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
              erlcloud_ddb:configure(string:copies("A", 20), string:copies("a", 40)),
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
              erlcloud_ddb:configure(string:copies("A", 20), string:copies("a", 40)),
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
    {ok, {{Code, ""}, [], list_to_binary(Body)}}.
    
-type error_test_spec() :: {pos_integer(), {string(), list(), term()}}.
-spec error_test(fun(), error_test_spec()) -> tuple().
error_test(Fun, {Line, {Description, Responses, Result}}) ->
    %% Add a bogus response to the end of the request to make sure we don't call too many times
    Responses1 = Responses ++ [httpc_response(200, "TOO MANY REQUESTS")],
    {Description,
     {Line,
      fun() ->
              meck:sequence(erlcloud_httpc, request, 6, Responses1),
              erlcloud_ddb:configure(string:copies("A", 20), string:copies("a", 40)),
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
                   erlcloud_ddb:get_item(<<"Table">>, {n, "string"})),
     %% This test causes an expected dialyzer error
     ?_assertError({erlcloud_ddb, {invalid_item, <<"Attr">>}},
                   erlcloud_ddb:put_item(<<"Table">>, <<"Attr">>)),
     ?_assertError({erlcloud_ddb, {invalid_opt, {myopt, myval}}},
                   erlcloud_ddb:list_tables([{myopt, myval}]))
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
    
    error_tests(?_f(erlcloud_ddb:get_item(<<"table">>, <<"key">>)), Tests).


%% BatchGetItem test based on the API examples:
%% http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_BatchGetItems.html
batch_get_item_input_tests(_) ->
    Tests =
        [?_ddb_test(
            {"BatchGetItem example request",
             ?_f(erlcloud_ddb:batch_get_item(
                   [{<<"comp2">>, [<<"Julie">>, 
                                   <<"Mingus">>], 
                     [{attributes_to_get, [<<"user">>, <<"friends">>]}]},
                    {<<"comp1">>, [{<<"Casey">>, 1319509152},
                                   {<<"Dave">>, 1319509155},
                                   {<<"Riley">>, 1319509158}],
                     [{attributes_to_get, [<<"user">>, <<"status">>]}]}])), "
{\"RequestItems\":
    {\"comp2\":
        {\"Keys\":
            [{\"HashKeyElement\":{\"S\":\"Julie\"}},{\"HashKeyElement\":{\"S\":\"Mingus\"}}],
        \"AttributesToGet\":[\"user\",\"friends\"]},
    \"comp1\":
        {\"Keys\":
            [{\"HashKeyElement\":{\"S\":\"Casey\"},\"RangeKeyElement\":{\"N\":\"1319509152\"}},
            {\"HashKeyElement\":{\"S\":\"Dave\"},\"RangeKeyElement\":{\"N\":\"1319509155\"}},
            {\"HashKeyElement\":{\"S\":\"Riley\"},\"RangeKeyElement\":{\"N\":\"1319509158\"}}],
        \"AttributesToGet\":[\"user\",\"status\"]}
    }
}"
            })
        ],

    Response = "
{\"Responses\":
    {\"comp1\":
        {\"Items\":
            [{\"status\":{\"S\":\"online\"},\"user\":{\"S\":\"Casey\"}},
            {\"status\":{\"S\":\"working\"},\"user\":{\"S\":\"Riley\"}},
            {\"status\":{\"S\":\"running\"},\"user\":{\"S\":\"Dave\"}}],
        \"ConsumedCapacityUnits\":1.5},
    \"comp2\":
        {\"Items\":
            [{\"friends\":{\"SS\":[\"Elisabeth\", \"Peter\"]},\"user\":{\"S\":\"Mingus\"}},
            {\"friends\":{\"SS\":[\"Dave\", \"Peter\"]},\"user\":{\"S\":\"Julie\"}}],
        \"ConsumedCapacityUnits\":1}
    },
    \"UnprocessedKeys\":{}
}",
    input_tests(Response, Tests).

batch_get_item_output_tests(_) ->
    Tests = 
        [?_ddb_test(
            {"BatchGetItem example response", "
{\"Responses\":
    {\"comp1\":
        {\"Items\":
            [{\"status\":{\"S\":\"online\"},\"user\":{\"S\":\"Casey\"}},
            {\"status\":{\"S\":\"working\"},\"user\":{\"S\":\"Riley\"}},
            {\"status\":{\"S\":\"running\"},\"user\":{\"S\":\"Dave\"}}],
        \"ConsumedCapacityUnits\":1.5},
    \"comp2\":
        {\"Items\":
            [{\"friends\":{\"SS\":[\"Elisabeth\", \"Peter\"]},\"user\":{\"S\":\"Mingus\"}},
            {\"friends\":{\"SS\":[\"Dave\", \"Peter\"]},\"user\":{\"S\":\"Julie\"}}],
        \"ConsumedCapacityUnits\":1}
    },
    \"UnprocessedKeys\":{}
}",
             {ok, #ddb_batch_get_item
              {responses = 
                   [#ddb_batch_get_item_response
                    {table = <<"comp1">>,
                     items = [[{<<"status">>, <<"online">>},
                               {<<"user">>, <<"Casey">>}],
                              [{<<"status">>, <<"working">>},
                               {<<"user">>, <<"Riley">>}],
                              [{<<"status">>, <<"running">>},
                               {<<"user">>, <<"Dave">>}]],
                     consumed_capacity_units = 1.5},
                    #ddb_batch_get_item_response
                    {table = <<"comp2">>,
                     items = [[{<<"friends">>, [<<"Elisabeth">>, <<"Peter">>]},
                               {<<"user">>, <<"Mingus">>}],
                              [{<<"friends">>, [<<"Dave">>, <<"Peter">>]},
                               {<<"user">>, <<"Julie">>}]],
                     consumed_capacity_units = 1}],
               unprocessed_keys = []}}}),
         ?_ddb_test(
            {"BatchGetItem unprocessed keys", "
{\"Responses\":{},
 \"UnprocessedKeys\":
   {\"comp2\":
        {\"Keys\":
            [{\"HashKeyElement\":{\"S\":\"Julie\"}},{\"HashKeyElement\":{\"S\":\"Mingus\"}}],
        \"AttributesToGet\":[\"user\",\"friends\"]},
    \"comp1\":
        {\"Keys\":
            [{\"HashKeyElement\":{\"S\":\"Casey\"},\"RangeKeyElement\":{\"N\":\"1319509152\"}},
            {\"HashKeyElement\":{\"S\":\"Dave\"},\"RangeKeyElement\":{\"N\":\"1319509155\"}},
            {\"HashKeyElement\":{\"S\":\"Riley\"},\"RangeKeyElement\":{\"N\":\"1319509158\"}}],
        \"AttributesToGet\":[\"user\",\"status\"]}
    }
}",
             {ok, #ddb_batch_get_item
              {responses = [], 
               unprocessed_keys = 
                   [{<<"comp2">>, [{s, <<"Julie">>}, 
                                   {s, <<"Mingus">>}], 
                     [{attributes_to_get, [<<"user">>, <<"friends">>]}]},
                    {<<"comp1">>, [{{s, <<"Casey">>}, {n, 1319509152}},
                                   {{s, <<"Dave">>}, {n, 1319509155}},
                                   {{s, <<"Riley">>}, {n, 1319509158}}],
                     [{attributes_to_get, [<<"user">>, <<"status">>]}]}]}}})
        ],
    
    output_tests(?_f(erlcloud_ddb:batch_get_item([{<<"table">>, [<<"key">>]}], [{out, record}])), Tests).

%% BatchWriteItem test based on the API examples:
%% http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_BatchWriteItem.html
batch_write_item_input_tests(_) ->
    Tests =
        [?_ddb_test(
            {"BatchWriteItem example request",
             ?_f(erlcloud_ddb:batch_write_item(
                   [{<<"Reply">>, [{put, [{<<"ReplyDateTime">>, <<"2012-04-03T11:04:47.034Z">>},
                                          {<<"Id">>, <<"Amazon DynamoDB#DynamoDB Thread 5">>}]},
                                   {delete, {<<"Amazon DynamoDB#DynamoDB Thread 4">>,
                                             <<"oops - accidental row">>}}]},
                    {<<"Thread">>, [{put, [{<<"ForumName">>, <<"Amazon DynamoDB">>},
                                           {<<"Subject">>, <<"DynamoDB Thread 5">>}]}]}])), "
{
  \"RequestItems\":{
    \"Reply\":[
      {
        \"PutRequest\":{
          \"Item\":{
            \"ReplyDateTime\":{
              \"S\":\"2012-04-03T11:04:47.034Z\"
            },
            \"Id\":{
              \"S\":\"Amazon DynamoDB#DynamoDB Thread 5\"
            }
          }
        }
      },
      {
        \"DeleteRequest\":{
          \"Key\":{
            \"HashKeyElement\":{
              \"S\":\"Amazon DynamoDB#DynamoDB Thread 4\"
            },
            \"RangeKeyElement\":{
              \"S\":\"oops - accidental row\"
            }
          }
        }
      }
    ],
    \"Thread\":[
      {
        \"PutRequest\":{
          \"Item\":{
            \"ForumName\":{
              \"S\":\"Amazon DynamoDB\"
            },
            \"Subject\":{
              \"S\":\"DynamoDB Thread 5\"
            }
          }
        }
      }
    ]
  }
}"
            })
        ],

    Response = "
{
   \"Responses\":{
      \"Thread\":{
         \"ConsumedCapacityUnits\":1.0
      },
      \"Reply\":{
         \"ConsumedCapacityUnits\":1.0
      }
   },
   \"UnprocessedItems\":{
      \"Reply\":[
         {
            \"DeleteRequest\":{
               \"Key\":{
                  \"HashKeyElement\":{
                     \"S\":\"Amazon DynamoDB#DynamoDB Thread 4\"
                  },
                  \"RangeKeyElement\":{
                     \"S\":\"oops - accidental row\"
                  }
               }
            }
         }
      ]
   }
}",
    input_tests(Response, Tests).

batch_write_item_output_tests(_) ->
    Tests = 
        [?_ddb_test(
            {"BatchWriteItem example response", "
{
   \"Responses\":{
      \"Thread\":{
         \"ConsumedCapacityUnits\":1.0
      },
      \"Reply\":{
         \"ConsumedCapacityUnits\":1.0
      }
   },
   \"UnprocessedItems\":{
      \"Reply\":[
         {
            \"DeleteRequest\":{
               \"Key\":{
                  \"HashKeyElement\":{
                     \"S\":\"Amazon DynamoDB#DynamoDB Thread 4\"
                  },
                  \"RangeKeyElement\":{
                     \"S\":\"oops - accidental row\"
                  }
               }
            }
         }
      ]
   }
}",
             {ok, #ddb_batch_write_item
              {responses = 
                   [#ddb_batch_write_item_response
                    {table = <<"Thread">>,
                     consumed_capacity_units = 1.0},
                    #ddb_batch_write_item_response
                    {table = <<"Reply">>,
                     consumed_capacity_units = 1.0}],
               unprocessed_items = [{<<"Reply">>, [{delete, {{s, <<"Amazon DynamoDB#DynamoDB Thread 4">>},
                                                             {s, <<"oops - accidental row">>}}}]}]}}}),
         ?_ddb_test(
            {"BatchWriteItem unprocessed response", "
{
   \"Responses\":{},
   \"UnprocessedItems\":{
    \"Reply\":[
      {
        \"PutRequest\":{
          \"Item\":{
            \"ReplyDateTime\":{
              \"S\":\"2012-04-03T11:04:47.034Z\"
            },
            \"Id\":{
              \"S\":\"Amazon DynamoDB#DynamoDB Thread 5\"
            }
          }
        }
      },
      {
        \"DeleteRequest\":{
          \"Key\":{
            \"HashKeyElement\":{
              \"S\":\"Amazon DynamoDB#DynamoDB Thread 4\"
            },
            \"RangeKeyElement\":{
              \"S\":\"oops - accidental row\"
            }
          }
        }
      }
    ],
    \"Thread\":[
      {
        \"PutRequest\":{
          \"Item\":{
            \"ForumName\":{
              \"S\":\"Amazon DynamoDB\"
            },
            \"Subject\":{
              \"S\":\"DynamoDB Thread 5\"
            }
          }
        }
      }
    ]
  }
}",
             {ok, #ddb_batch_write_item
              {responses = [],
               unprocessed_items =
                   [{<<"Reply">>, [{put, [{<<"ReplyDateTime">>, {s, <<"2012-04-03T11:04:47.034Z">>}},
                                          {<<"Id">>, {s, <<"Amazon DynamoDB#DynamoDB Thread 5">>}}]},
                                   {delete, {{s, <<"Amazon DynamoDB#DynamoDB Thread 4">>},
                                             {s, <<"oops - accidental row">>}}}]},
                    {<<"Thread">>, [{put, [{<<"ForumName">>, {s, <<"Amazon DynamoDB">>}},
                                           {<<"Subject">>, {s, <<"DynamoDB Thread 5">>}}]}]}]}}})
        ],
    
    output_tests(?_f(erlcloud_ddb:batch_write_item([], [{out, record}])), Tests).

%% CreateTable test based on the API examples:
%% http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_CreateTable.html
create_table_input_tests(_) ->
    Tests =
        [?_ddb_test(
            {"CreateTable example request",
             ?_f(erlcloud_ddb:create_table(<<"comp-table">>, {{<<"user">>, s}, {<<"time">>, n}}, 5, 10)), "
{\"TableName\":\"comp-table\",
    \"KeySchema\":
        {\"HashKeyElement\":{\"AttributeName\":\"user\",\"AttributeType\":\"S\"},
        \"RangeKeyElement\":{\"AttributeName\":\"time\",\"AttributeType\":\"N\"}},
    \"ProvisionedThroughput\":{\"ReadCapacityUnits\":5,\"WriteCapacityUnits\":10}
}"
            })
        ],

    Response = "
{\"TableDescription\":
    {\"CreationDateTime\":1.310506263362E9,
    \"KeySchema\":
        {\"HashKeyElement\":{\"AttributeName\":\"user\",\"AttributeType\":\"S\"},
        \"RangeKeyElement\":{\"AttributeName\":\"time\",\"AttributeType\":\"N\"}},
    \"ProvisionedThroughput\":{\"ReadCapacityUnits\":5,\"WriteCapacityUnits\":10},
    \"TableName\":\"comp-table\",
    \"TableStatus\":\"CREATING\"
    }
}",
    input_tests(Response, Tests).

create_table_output_tests(_) ->
    Tests = 
        [?_ddb_test(
            {"CreateTable example response", "
{\"TableDescription\":
    {\"CreationDateTime\":1.310506263362E9,
    \"KeySchema\":
        {\"HashKeyElement\":{\"AttributeName\":\"user\",\"AttributeType\":\"S\"},
        \"RangeKeyElement\":{\"AttributeName\":\"time\",\"AttributeType\":\"N\"}},
    \"ProvisionedThroughput\":{\"ReadCapacityUnits\":5,\"WriteCapacityUnits\":10},
    \"TableName\":\"comp-table\",
    \"TableStatus\":\"CREATING\"
    }
}",
             {ok, #ddb_table_description
              {creation_date_time = 1310506263.362,
               key_schema = {{<<"user">>, s}, {<<"time">>, n}},
               provisioned_throughput = #ddb_provisioned_throughput{
                                           read_capacity_units = 5,
                                           write_capacity_units = 10,
                                           last_decrease_date_time = undefined,
                                           last_increase_date_time = undefined},
               table_name = <<"comp-table">>,
               table_status = <<"CREATING">>}}})
        ],
    
    output_tests(?_f(erlcloud_ddb:create_table(<<"name">>, {<<"key">>, s}, 5, 10)), Tests).

%% DeleteItem test based on the API examples:
%% http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_DeleteItem.html
delete_item_input_tests(_) ->
    Tests =
        [?_ddb_test(
            {"DeleteItem example request",
             ?_f(erlcloud_ddb:delete_item(<<"comp-table">>, {"Mingus", 200},
                                          [{return_values, all_old},
                                           {expected, {<<"status">>, "shopping"}}])), "
{\"TableName\":\"comp-table\",
    \"Key\":
        {\"HashKeyElement\":{\"S\":\"Mingus\"},\"RangeKeyElement\":{\"N\":\"200\"}},
    \"Expected\":
        {\"status\":{\"Value\":{\"S\":\"shopping\"}}},
    \"ReturnValues\":\"ALL_OLD\"
}"
            })
        ],

    Response = "
{\"Attributes\":
    {\"friends\":{\"SS\":[\"Dooley\",\"Ben\",\"Daisy\"]},
    \"status\":{\"S\":\"shopping\"},
    \"time\":{\"N\":\"200\"},
    \"user\":{\"S\":\"Mingus\"}
    },
\"ConsumedCapacityUnits\":1
}",
    input_tests(Response, Tests).

delete_item_output_tests(_) ->
    Tests = 
        [?_ddb_test(
            {"DeleteItem example response", "
{\"Attributes\":
    {\"friends\":{\"SS\":[\"Dooley\",\"Ben\",\"Daisy\"]},
    \"status\":{\"S\":\"shopping\"},
    \"time\":{\"N\":\"200\"},
    \"user\":{\"S\":\"Mingus\"}
    },
\"ConsumedCapacityUnits\":1
}",
             {ok, [{<<"friends">>, [<<"Dooley">>, <<"Ben">>, <<"Daisy">>]},
                   {<<"status">>, <<"shopping">>},
                   {<<"time">>, 200},
                   {<<"user">>, <<"Mingus">>}]}})
        ],
    
    output_tests(?_f(erlcloud_ddb:delete_item(<<"table">>, <<"key">>)), Tests).

%% DeleteTable test based on the API examples:
%% http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_DeleteTable.html
delete_table_input_tests(_) ->
    Tests =
        [?_ddb_test(
            {"DeleteTable example request",
             ?_f(erlcloud_ddb:delete_table(<<"Table1">>)), 
             "{\"TableName\":\"Table1\"}"
            })
        ],

    Response = "
{\"TableDescription\":
    {\"CreationDateTime\":1.313362508446E9,
    \"KeySchema\":
        {\"HashKeyElement\":{\"AttributeName\":\"user\",\"AttributeType\":\"S\"},
        \"RangeKeyElement\":{\"AttributeName\":\"time\",\"AttributeType\":\"N\"}},
    \"ProvisionedThroughput\":{\"ReadCapacityUnits\":10,\"WriteCapacityUnits\":10},
    \"TableName\":\"Table1\",
    \"TableStatus\":\"DELETING\"
    }
}",
    input_tests(Response, Tests).

delete_table_output_tests(_) ->
    Tests = 
        [?_ddb_test(
            {"DeleteTable example response", "
{\"TableDescription\":
    {\"CreationDateTime\":1.313362508446E9,
    \"KeySchema\":
        {\"HashKeyElement\":{\"AttributeName\":\"user\",\"AttributeType\":\"S\"},
        \"RangeKeyElement\":{\"AttributeName\":\"time\",\"AttributeType\":\"N\"}},
    \"ProvisionedThroughput\":{\"ReadCapacityUnits\":10,\"WriteCapacityUnits\":10},
    \"TableName\":\"Table1\",
    \"TableStatus\":\"DELETING\"
    }
}",
             {ok, #ddb_table_description
              {creation_date_time = 1313362508.446,
               key_schema = {{<<"user">>, s}, {<<"time">>, n}},
               provisioned_throughput = #ddb_provisioned_throughput{
                                           read_capacity_units = 10,
                                           write_capacity_units = 10,
                                           last_decrease_date_time = undefined,
                                           last_increase_date_time = undefined},
               table_name = <<"Table1">>,
               table_status = <<"DELETING">>}}})
        ],
    
    output_tests(?_f(erlcloud_ddb:delete_table(<<"name">>)), Tests).

%% DescribeTable test based on the API examples:
%% http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_DescribeTables.html
describe_table_input_tests(_) ->
    Tests =
        [?_ddb_test(
            {"DescribeTable example request",
             ?_f(erlcloud_ddb:describe_table(<<"Table1">>)), 
             "{\"TableName\":\"Table1\"}"
            })
        ],

    Response = "
{\"Table\":
    {\"CreationDateTime\":1.309988345372E9,
    \"ItemCount\":23,
    \"KeySchema\":
        {\"HashKeyElement\":{\"AttributeName\":\"user\",\"AttributeType\":\"S\"},
        \"RangeKeyElement\":{\"AttributeName\":\"time\",\"AttributeType\":\"N\"}},
    \"ProvisionedThroughput\":{\"LastIncreaseDateTime\": 1.309988345384E9, \"ReadCapacityUnits\":10,\"WriteCapacityUnits\":10},
    \"TableName\":\"users\",
    \"TableSizeBytes\":949,
    \"TableStatus\":\"ACTIVE\"
    }
}",
    input_tests(Response, Tests).

describe_table_output_tests(_) ->
    Tests = 
        [?_ddb_test(
            {"DescribeTable example response", "
{\"Table\":
    {\"CreationDateTime\":1.309988345372E9,
    \"ItemCount\":23,
    \"KeySchema\":
        {\"HashKeyElement\":{\"AttributeName\":\"user\",\"AttributeType\":\"S\"},
        \"RangeKeyElement\":{\"AttributeName\":\"time\",\"AttributeType\":\"N\"}},
    \"ProvisionedThroughput\":{\"LastIncreaseDateTime\": 1.309988345384E9, \"ReadCapacityUnits\":10,\"WriteCapacityUnits\":10},
    \"TableName\":\"users\",
    \"TableSizeBytes\":949,
    \"TableStatus\":\"ACTIVE\"
    }
}",
             {ok, #ddb_table
              {creation_date_time = 1309988345.372,
               item_count = 23,
               key_schema = {{<<"user">>, s}, {<<"time">>, n}},
               provisioned_throughput = #ddb_provisioned_throughput{
                                           read_capacity_units = 10,
                                           write_capacity_units = 10,
                                           last_decrease_date_time = undefined,
                                           last_increase_date_time = 1309988345.384},
               table_name = <<"users">>,
               table_size_bytes = 949,
               table_status = <<"ACTIVE">>}}})
        ],
    
    output_tests(?_f(erlcloud_ddb:describe_table(<<"name">>)), Tests).

%% GetItem test based on the API examples:
%% http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_GetItem.html
get_item_input_tests(_) ->
    Example1Response = "
{\"TableName\":\"comptable\",
	\"Key\":
		{\"HashKeyElement\":{\"S\":\"Julie\"},
		\"RangeKeyElement\":{\"N\":\"1307654345\"}},
	\"AttributesToGet\":[\"status\",\"friends\"],
	\"ConsistentRead\":true
}",

    Tests =
        [?_ddb_test(
            {"GetItem example request, with fully specified keys",
             ?_f(erlcloud_ddb:get_item(<<"comptable">>, {{s, <<"Julie">>}, {n, 1307654345}}, 
                                       [consistent_read,
                                        {attributes_to_get, [<<"status">>, <<"friends">>]}])),
             Example1Response}),
         ?_ddb_test(
            {"GetItem example request, with inferred key types",
             ?_f(erlcloud_ddb:get_item(<<"comptable">>, {"Julie", 1307654345}, 
                                       [consistent_read, 
                                        {attributes_to_get, [<<"status">>, <<"friends">>]}])),
             Example1Response}),
         ?_ddb_test(
            {"GetItem Simple call with only hash key and no options",
             ?_f(erlcloud_ddb:get_item(<<"comptable">>, {s, "Julie"})), "
{\"TableName\":\"comptable\",
	\"Key\":
		{\"HashKeyElement\":{\"S\":\"Julie\"}}
}"
             })
        ],

    Response = "
{\"Item\":
	{\"friends\":{\"SS\":[\"Lynda, Aaron\"]},
	\"status\":{\"S\":\"online\"}
	},
\"ConsumedCapacityUnits\": 1
}",
    input_tests(Response, Tests).

get_item_output_tests(_) ->
    Tests = 
        [?_ddb_test(
            {"GetItem example response", "
{\"Item\":
	{\"friends\":{\"SS\":[\"Lynda\", \"Aaron\"]},
	 \"status\":{\"S\":\"online\"}
	},
\"ConsumedCapacityUnits\": 1
}",
             {ok, [{<<"friends">>, [<<"Lynda">>, <<"Aaron">>]},
                   {<<"status">>, <<"online">>}]}}),
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
	},
\"ConsumedCapacityUnits\": 1
}",
             {ok, [{<<"ss">>, [<<"Lynda">>, <<"Aaron">>]},
                   {<<"ns">>, [12,13.0,14.1]},
                   {<<"bs">>, [<<5,182>>]},
                   {<<"es">>, []},
                   {<<"s">>, <<"Lynda">>},
                   {<<"n">>, 12},
                   {<<"f">>, 12.34},
                   {<<"b">>, <<5,182>>},
                   {<<"empty">>, <<>>}]}}),
         ?_ddb_test(
            {"GetItem item not found", 
             "{\"ConsumedCapacityUnits\": 0.5}",
             {ok, []}}),
         ?_ddb_test(
            {"GetItem no attributes returned", 
             "{\"ConsumedCapacityUnits\":0.5,\"Item\":{}}",
             {ok, []}})
        ],
    
    output_tests(?_f(erlcloud_ddb:get_item(<<"table">>, <<"key">>)), Tests).

%% ListTables test based on the API examples:
%% http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_ListTables.html
list_tables_input_tests(_) ->
    Tests =
        [?_ddb_test(
            {"ListTables example request",
             ?_f(erlcloud_ddb:list_tables([{limit, 3}, {exclusive_start_table_name, <<"comp2">>}])), 
             "{\"ExclusiveStartTableName\":\"comp2\",\"Limit\":3}"
            }),
         ?_ddb_test(
            {"ListTables empty request",
             ?_f(erlcloud_ddb:list_tables()), 
             "{}"
            })

        ],

    Response = "{\"LastEvaluatedTableName\":\"comp5\",\"TableNames\":[\"comp3\",\"comp4\",\"comp5\"]}",
    input_tests(Response, Tests).

list_tables_output_tests(_) ->
    Tests = 
        [?_ddb_test(
            {"ListTables example response",
             "{\"LastEvaluatedTableName\":\"comp5\",\"TableNames\":[\"comp3\",\"comp4\",\"comp5\"]}",
             {ok, #ddb_list_tables
              {last_evaluated_table_name = <<"comp5">>,
               table_names = [<<"comp3">>, <<"comp4">>, <<"comp5">>]}}})
        ],
    
    output_tests(?_f(erlcloud_ddb:list_tables([{out, record}])), Tests).

%% PutItem test based on the API examples:
%% http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_PutItem.html
put_item_input_tests(_) ->
    Tests =
        [?_ddb_test(
            {"PutItem example request",
             ?_f(erlcloud_ddb:put_item(<<"comp5">>, 
                                       [{<<"time">>, 300}, 
                                        {<<"feeling">>, <<"not surprised">>},
                                        {<<"user">>, <<"Riley">>}],
                                       [{return_values, all_old},
                                        {expected, {<<"feeling">>, <<"surprised">>}}])), "
{\"TableName\":\"comp5\",
	\"Item\":
		{\"time\":{\"N\":\"300\"},
		 \"feeling\":{\"S\":\"not surprised\"},
	  	 \"user\":{\"S\":\"Riley\"}
		},
	\"Expected\":
		{\"feeling\":{\"Value\":{\"S\":\"surprised\"}}},
	\"ReturnValues\":\"ALL_OLD\"
}"
            }),
         ?_ddb_test(
            {"PutItem float inputs",
             ?_f(erlcloud_ddb:put_item(<<"comp5">>, 
                                       [{<<"time">>, 300}, 
                                        {<<"typed float">>, {n, 1.2}},
                                        {<<"untyped float">>, 3.456},
                                        {<<"mixed set">>, {ns, [7.8, 9.0, 10]}}],
                                       [])), "
{\"TableName\":\"comp5\",
	\"Item\":
		{\"time\":{\"N\":\"300\"},
		 \"typed float\":{\"N\":\"1.2\"},
		 \"untyped float\":{\"N\":\"3.456\"},
		 \"mixed set\":{\"NS\":[\"7.8\", \"9.0\", \"10\"]}
		}
}"
            })
        ],

    Response = "
{\"Attributes\":
	{\"feeling\":{\"S\":\"surprised\"},
	\"time\":{\"N\":\"300\"},
	\"user\":{\"S\":\"Riley\"}},
\"ConsumedCapacityUnits\":1
}",
    input_tests(Response, Tests).

put_item_output_tests(_) ->
    Tests = 
        [?_ddb_test(
            {"PutItem example response", "
{\"Attributes\":
	{\"feeling\":{\"S\":\"surprised\"},
	\"time\":{\"N\":\"300\"},
	\"user\":{\"S\":\"Riley\"}},
\"ConsumedCapacityUnits\":1
}",
             {ok, [{<<"feeling">>, <<"surprised">>},
                   {<<"time">>, 300},
                   {<<"user">>, <<"Riley">>}]}})
        ],
    
    output_tests(?_f(erlcloud_ddb:put_item(<<"table">>, [])), Tests).

%% Query test based on the API examples:
%% http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_Query.html
q_input_tests(_) ->
    Tests =
        [?_ddb_test(
            {"Query example 1 request",
             ?_f(erlcloud_ddb:q(<<"1-hash-rangetable">>, <<"John">>,
                                [{exclusive_start_key, {{s, <<"John">>}, {s, <<"The Matrix">>}}},
                                 {scan_index_forward, false},
                                 {limit, 2}])), "
{\"TableName\":\"1-hash-rangetable\",
	\"HashKeyValue\":{\"S\":\"John\"},
	\"Limit\":2,
	\"ScanIndexForward\":false,
	\"ExclusiveStartKey\":{
		\"HashKeyElement\":{\"S\":\"John\"},
		\"RangeKeyElement\":{\"S\":\"The Matrix\"}
	}
}"
            }),
         ?_ddb_test(
            {"Query example 2 request",
             ?_f(erlcloud_ddb:q(<<"1-hash-rangetable">>, <<"Airplane">>,
                                [{scan_index_forward, false},
                                 {range_key_condition, {1980, eq}},
                                 {limit, 2}])), "
{\"TableName\":\"1-hash-rangetable\",
	\"HashKeyValue\":{\"S\":\"Airplane\"},
	\"Limit\":2,
	\"RangeKeyCondition\":{\"AttributeValueList\":[{\"N\":\"1980\"}],\"ComparisonOperator\":\"EQ\"},
	\"ScanIndexForward\":false}"
            }),
         ?_ddb_test(
            {"Query between test",
             ?_f(erlcloud_ddb:q(<<"table">>, <<"key">>,
                                [{exclusive_start_key, undefined},
                                 {range_key_condition, {{1980, 1990}, between}}])), "
{       \"TableName\":\"table\",
	\"HashKeyValue\":{\"S\":\"key\"},
	\"RangeKeyCondition\":{\"AttributeValueList\":[{\"N\":\"1980\"},{\"N\":\"1990\"}],
                               \"ComparisonOperator\":\"BETWEEN\"}}"
            })
        ],

    Response = "
{\"Count\":1,\"Items\":[{
	\"fans\":{\"SS\":[\"Dave\",\"Aaron\"]},
	\"name\":{\"S\":\"Airplane\"},
	\"rating\":{\"S\":\"***\"},
	\"year\":{\"N\":\"1980\"}
	}],
\"ConsumedCapacityUnits\":1
}",
    input_tests(Response, Tests).

q_output_tests(_) ->
    Tests = 
        [?_ddb_test(
            {"Query example 1 response", "
{\"Count\":2,\"Items\":[{
	\"fans\":{\"SS\":[\"Jody\",\"Jake\"]},
	\"name\":{\"S\":\"John\"},
	\"rating\":{\"S\":\"***\"},
	\"title\":{\"S\":\"The End\"}
	},{
	\"fans\":{\"SS\":[\"Jody\",\"Jake\"]},
	\"name\":{\"S\":\"John\"},
	\"rating\":{\"S\":\"***\"},
	\"title\":{\"S\":\"The Beatles\"}
	}],
	\"LastEvaluatedKey\":{\"HashKeyElement\":{\"S\":\"John\"},\"RangeKeyElement\":{\"S\":\"The Beatles\"}},
\"ConsumedCapacityUnits\":1
}",
             {ok, #ddb_q{count = 2,
                         items = [[{<<"fans">>, [<<"Jody">>, <<"Jake">>]},
                                   {<<"name">>, <<"John">>},
                                   {<<"rating">>, <<"***">>},
                                   {<<"title">>, <<"The End">>}],
                                  [{<<"fans">>, [<<"Jody">>, <<"Jake">>]},
                                   {<<"name">>, <<"John">>},
                                   {<<"rating">>, <<"***">>},
                                   {<<"title">>, <<"The Beatles">>}]],
                         last_evaluated_key = {{s, <<"John">>}, {s, <<"The Beatles">>}},
                         consumed_capacity_units = 1}}}),
         ?_ddb_test(
            {"Query example 2 response", "
{\"Count\":1,\"Items\":[{
	\"fans\":{\"SS\":[\"Dave\",\"Aaron\"]},
	\"name\":{\"S\":\"Airplane\"},
	\"rating\":{\"S\":\"***\"},
	\"year\":{\"N\":\"1980\"}
	}],
\"ConsumedCapacityUnits\":1
}",
             {ok, #ddb_q{count = 1,
                         items = [[{<<"fans">>, [<<"Dave">>, <<"Aaron">>]},
                                   {<<"name">>, <<"Airplane">>},
                                   {<<"rating">>, <<"***">>},
                                   {<<"year">>, 1980}]],
                         last_evaluated_key = undefined,
                         consumed_capacity_units = 1}}})
        ],
    
    output_tests(?_f(erlcloud_ddb:q(<<"table">>, <<"key">>, [{out, record}])), Tests).

%% Scan test based on the API examples:
%% http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_Scan.html
scan_input_tests(_) ->
    Tests =
        [?_ddb_test(
            {"Scan example 1 request",
             ?_f(erlcloud_ddb:scan(<<"1-hash-rangetable">>)), 
             "{\"TableName\":\"1-hash-rangetable\"}"
            }),
         ?_ddb_test(
            {"Scan example 2 request",
             ?_f(erlcloud_ddb:scan(<<"comp5">>, [{scan_filter, [{<<"time">>, 400, gt}]}])), "
{\"TableName\":\"comp5\",
	\"ScanFilter\":
		{\"time\":
			{\"AttributeValueList\":[{\"N\":\"400\"}],
			\"ComparisonOperator\":\"GT\"}
	}
}"
            }),
         ?_ddb_test(
            {"Scan example 3 request",
             ?_f(erlcloud_ddb:scan(<<"comp5">>, [{exclusive_start_key, {<<"Fredy">>, 2000}},
                                                 {scan_filter, [{<<"time">>, 400, gt}]},
                                                 {limit, 2}])), "
{\"TableName\":\"comp5\",
	\"Limit\":2,
	\"ScanFilter\":
		{\"time\":
			{\"AttributeValueList\":[{\"N\":\"400\"}],
			\"ComparisonOperator\":\"GT\"}
	},
	\"ExclusiveStartKey\":
		{\"HashKeyElement\":{\"S\":\"Fredy\"},\"RangeKeyElement\":{\"N\":\"2000\"}}
}"
            })
        ],

    Response = "
{\"Count\":1,
	\"Items\":[
		{\"friends\":{\"SS\":[\"Jane\",\"James\",\"John\"]},
		\"status\":{\"S\":\"exercising\"},
		\"time\":{\"N\":\"2200\"},
		\"user\":{\"S\":\"Roger\"}}
	],
	\"LastEvaluatedKey\":{\"HashKeyElement\":{\"S\":\"Riley\"},\"RangeKeyElement\":{\"N\":\"250\"}},
\"ConsumedCapacityUnits\":0.5,
\"ScannedCount\":2
}",
    input_tests(Response, Tests).

scan_output_tests(_) ->
    Tests = 
        [?_ddb_test(
            {"Scan example 1 response", "
{\"Count\":4,\"Items\":[{
	\"date\":{\"S\":\"1980\"},
	\"fans\":{\"SS\":[\"Dave\",\"Aaron\"]},
	\"name\":{\"S\":\"Airplane\"},
	\"rating\":{\"S\":\"***\"}
	},{
	\"date\":{\"S\":\"1999\"},
	\"fans\":{\"SS\":[\"Ziggy\",\"Laura\",\"Dean\"]},
	\"name\":{\"S\":\"Matrix\"},
	\"rating\":{\"S\":\"*****\"}
	},{
	\"date\":{\"S\":\"1976\"},
	\"fans\":{\"SS\":[\"Riley\"]},
	\"name\":{\"S\":\"The Shaggy D.A.\"},
	\"rating\":{\"S\":\"**\"}
	},{
	\"date\":{\"S\":\"1989\"},
	\"fans\":{\"SS\":[\"Alexis\",\"Keneau\"]},
	\"name\":{\"S\":\"Bill & Ted's Excellent Adventure\"},
	\"rating\":{\"S\":\"****\"}
	}],
    \"ConsumedCapacityUnits\":0.5,
	\"ScannedCount\":4
}",
             {ok, #ddb_scan
              {count = 4,
               items = [[{<<"date">>, <<"1980">>},
                         {<<"fans">>, [<<"Dave">>, <<"Aaron">>]},
                         {<<"name">>, <<"Airplane">>},
                         {<<"rating">>, <<"***">>}],
                        [{<<"date">>, <<"1999">>},
                         {<<"fans">>, [<<"Ziggy">>, <<"Laura">>, <<"Dean">>]},
                         {<<"name">>, <<"Matrix">>},
                         {<<"rating">>, <<"*****">>}],
                        [{<<"date">>, <<"1976">>},
                         {<<"fans">>, [<<"Riley">>]},
                         {<<"name">>, <<"The Shaggy D.A.">>},
                         {<<"rating">>, <<"**">>}],
                        [{<<"date">>, <<"1989">>},
                         {<<"fans">>, [<<"Alexis">>, <<"Keneau">>]},
                         {<<"name">>, <<"Bill & Ted's Excellent Adventure">>},
                         {<<"rating">>, <<"****">>}]],
               scanned_count = 4,
               consumed_capacity_units = 0.5}}}),
         ?_ddb_test(
            {"Scan example 2 response", "
{\"Count\":2,
	\"Items\":[
		{\"friends\":{\"SS\":[\"Dave\",\"Ziggy\",\"Barrie\"]},
		\"status\":{\"S\":\"chatting\"},
		\"time\":{\"N\":\"2000\"},
		\"user\":{\"S\":\"Casey\"}},
		{\"friends\":{\"SS\":[\"Dave\",\"Ziggy\",\"Barrie\"]},
		\"status\":{\"S\":\"chatting\"},
		\"time\":{\"N\":\"2000\"},
		\"user\":{\"S\":\"Fredy\"}
		}],
\"ConsumedCapacityUnits\":0.5,
\"ScannedCount\":4
}",
             {ok, #ddb_scan
              {count = 2,
               items = [[{<<"friends">>, [<<"Dave">>, <<"Ziggy">>, <<"Barrie">>]},
                         {<<"status">>, <<"chatting">>},
                         {<<"time">>, 2000},
                         {<<"user">>, <<"Casey">>}],
                        [{<<"friends">>, [<<"Dave">>, <<"Ziggy">>, <<"Barrie">>]},
                         {<<"status">>, <<"chatting">>},
                         {<<"time">>, 2000},
                         {<<"user">>, <<"Fredy">>}]],
               scanned_count = 4,
               consumed_capacity_units = 0.5}}}),
         ?_ddb_test(
            {"Scan example 3 response", "
{\"Count\":1,
	\"Items\":[
		{\"friends\":{\"SS\":[\"Jane\",\"James\",\"John\"]},
		\"status\":{\"S\":\"exercising\"},
		\"time\":{\"N\":\"2200\"},
		\"user\":{\"S\":\"Roger\"}}
	],
	\"LastEvaluatedKey\":{\"HashKeyElement\":{\"S\":\"Riley\"},\"RangeKeyElement\":{\"N\":\"250\"}},
\"ConsumedCapacityUnits\":0.5,
\"ScannedCount\":2
}",
             {ok, #ddb_scan
              {count = 1,
               items = [[{<<"friends">>, [<<"Jane">>, <<"James">>, <<"John">>]},
                         {<<"status">>, <<"exercising">>},
                         {<<"time">>, 2200},
                         {<<"user">>, <<"Roger">>}]],
               last_evaluated_key = {{s, <<"Riley">>}, {n, 250}},
               scanned_count = 2,
               consumed_capacity_units = 0.5}}})
        ],
    
    output_tests(?_f(erlcloud_ddb:scan(<<"name">>, [{out, record}])), Tests).

%% UpdateItem test based on the API examples:
%% http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_UpdateItem.html
update_item_input_tests(_) ->
    Tests =
        [?_ddb_test(
            {"UpdateItem example request",
             ?_f(erlcloud_ddb:update_item(<<"comp5">>, {"Julie", 1307654350},
                                          [{<<"status">>, <<"online">>, put}],
                                          [{return_values, all_new},
                                           {expected, {<<"status">>, "offline"}}])), "
{\"TableName\":\"comp5\",
    \"Key\":
        {\"HashKeyElement\":{\"S\":\"Julie\"},\"RangeKeyElement\":{\"N\":\"1307654350\"}},
    \"AttributeUpdates\":
        {\"status\":{\"Value\":{\"S\":\"online\"},
        \"Action\":\"PUT\"}},
    \"Expected\":{\"status\":{\"Value\":{\"S\":\"offline\"}}},
    \"ReturnValues\":\"ALL_NEW\"
}"
            }),
         ?_ddb_test(
            {"UpdateItem different update types",
             ?_f(erlcloud_ddb:update_item(<<"comp5">>, {"Julie", 1307654350},
                                          [{<<"number">>, 5, add},
                                           {<<"numberset">>, {ns, [3]}, add},
                                           {<<"todelete">>, delete},
                                           {<<"toremove">>, {ss, [<<"bye">>]}, delete},
                                           {<<"defaultput">>, <<"online">>}])), "
{\"TableName\":\"comp5\",
    \"Key\":
        {\"HashKeyElement\":{\"S\":\"Julie\"},\"RangeKeyElement\":{\"N\":\"1307654350\"}},
    \"AttributeUpdates\":
        {\"number\":{\"Value\":{\"N\":\"5\"}, \"Action\":\"ADD\"},
         \"numberset\":{\"Value\":{\"NS\":[\"3\"]}, \"Action\":\"ADD\"},
         \"todelete\":{\"Action\":\"DELETE\"},
         \"toremove\":{\"Value\":{\"SS\":[\"bye\"]}, \"Action\":\"DELETE\"},
         \"defaultput\":{\"Value\":{\"S\":\"online\"}}}
}"
            })

        ],

    Response = "
{\"Attributes\":
    {\"friends\":{\"SS\":[\"Lynda, Aaron\"]},
    \"status\":{\"S\":\"online\"},
    \"time\":{\"N\":\"1307654350\"},
    \"user\":{\"S\":\"Julie\"}},
\"ConsumedCapacityUnits\":1
}",
    input_tests(Response, Tests).

update_item_output_tests(_) ->
    Tests = 
        [?_ddb_test(
            {"UpdateItem example response", "
{\"Attributes\":
    {\"friends\":{\"SS\":[\"Lynda\", \"Aaron\"]},
    \"status\":{\"S\":\"online\"},
    \"time\":{\"N\":\"1307654350\"},
    \"user\":{\"S\":\"Julie\"}},
\"ConsumedCapacityUnits\":1
}",
             {ok, [{<<"friends">>, [<<"Lynda">>, <<"Aaron">>]},
                   {<<"status">>, <<"online">>},
                   {<<"time">>, 1307654350},
                   {<<"user">>, <<"Julie">>}]}})
        ],
    
    output_tests(?_f(erlcloud_ddb:update_item(<<"table">>, <<"key">>, [])), Tests).

%% UpdateTable test based on the API examples:
%% http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_UpdateTable.html
update_table_input_tests(_) ->
    Tests =
        [?_ddb_test(
            {"UpdateTable example request",
             ?_f(erlcloud_ddb:update_table(<<"comp1">>, 5, 15)), "
{\"TableName\":\"comp1\",
    \"ProvisionedThroughput\":{\"ReadCapacityUnits\":5,\"WriteCapacityUnits\":15}
}"
            })
        ],

    Response = "
{\"TableDescription\":
    {\"CreationDateTime\":1.321657838135E9,
    \"KeySchema\":
        {\"HashKeyElement\":{\"AttributeName\":\"user\",\"AttributeType\":\"S\"},
        \"RangeKeyElement\":{\"AttributeName\":\"time\",\"AttributeType\":\"N\"}},
    \"ProvisionedThroughput\":
        {\"LastDecreaseDateTime\":1.321661704489E9,
        \"LastIncreaseDateTime\":1.321663607695E9,
        \"ReadCapacityUnits\":5,
        \"WriteCapacityUnits\":10},
    \"TableName\":\"comp1\",
    \"TableStatus\":\"UPDATING\"}
}",
    input_tests(Response, Tests).

update_table_output_tests(_) ->
    Tests = 
        [?_ddb_test(
            {"UpdateTable example response", "
{\"TableDescription\":
    {\"CreationDateTime\":1.321657838135E9,
    \"KeySchema\":
        {\"HashKeyElement\":{\"AttributeName\":\"user\",\"AttributeType\":\"S\"},
        \"RangeKeyElement\":{\"AttributeName\":\"time\",\"AttributeType\":\"N\"}},
    \"ProvisionedThroughput\":
        {\"LastDecreaseDateTime\":1.321661704489E9,
        \"LastIncreaseDateTime\":1.321663607695E9,
        \"ReadCapacityUnits\":5,
        \"WriteCapacityUnits\":10},
    \"TableName\":\"comp1\",
    \"TableStatus\":\"UPDATING\"}
}",
             {ok, #ddb_table_description
              {creation_date_time = 1321657838.135,
               key_schema = {{<<"user">>, s}, {<<"time">>, n}},
               provisioned_throughput = #ddb_provisioned_throughput{
                                           read_capacity_units = 5,
                                           write_capacity_units = 10,
                                           last_decrease_date_time = 1321661704.489,
                                           last_increase_date_time = 1321663607.695},
               table_name = <<"comp1">>,
               table_status = <<"UPDATING">>}}})
        ],
    
    output_tests(?_f(erlcloud_ddb:update_table(<<"name">>, 5, 15)), Tests).

