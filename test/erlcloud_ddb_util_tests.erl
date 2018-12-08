%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_ddb_util_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").
-include("erlcloud_ddb.hrl").
-include("erlcloud_ddb2.hrl").

%% Unit tests for erlcloud_ddb_util.
%% These tests work by using meck to mock erlcloud_httpc.
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
                            
%%%===================================================================
%%% Test entry points
%%%===================================================================

operation_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun delete_all_tests/1,
      fun delete_hash_key_tests/1,
      fun get_all_tests/1,
      fun list_tables_all_tests/1,
      fun put_all_tests/1,
      fun q_all_tests/1,
      fun q_all_attributes/0,
      fun q_all_count/0,
      fun scan_all_tests/1,
      fun scan_all_attributes/0,
      fun scan_all_count/0,
      fun wait_for_table_active_tests/1,
      fun write_all_tests/1
     ]}.

start() ->
    meck:new(erlcloud_httpc),
    ok.

stop(_) ->
    meck:unload(erlcloud_httpc).

%%%===================================================================
%%% Multi-call test helpers
%%%===================================================================

-type description() :: string().
-type expected_body() :: string().
-type response_body() :: string().
-type http_call() :: {expected_body(), response_body()}.

%% returns the mock of the erlcloud_httpc function multi-call tests expect to be called.
-spec multi_call_expect([http_call(),...]) -> fun().
multi_call_expect([{Expected, Response} | TCalls]) ->
    fun(_Url, post, _Headers, Body, _Timeout, _Config) -> 
            erlcloud_ddb2_tests:validate_body(Body, Expected),
            case TCalls of
                [] ->
                    %% No more calls expected
                    meck:delete(erlcloud_httpc, request, 6);
                _ ->
                    %% Set up the expectation for the next call
                    meck:expect(erlcloud_httpc, request, multi_call_expect(TCalls))
            end,
            {ok, {{200, "OK"}, [], list_to_binary(Response)}} 
    end.
    

%% mutil_call_test converts a multi_call_test specifier into an eunit test generator
-type multi_call_test_spec() :: {pos_integer(), {description(), fun(), [http_call()], term()}}.
-spec multi_call_test(multi_call_test_spec()) -> tuple().
multi_call_test({Line, {Description, Fun, Calls, Result}}) ->
    {Description,
     {Line,
      fun() ->
              meck:expect(erlcloud_httpc, request, multi_call_expect(Calls)),
              erlcloud_ddb:configure(string:copies("A", 20), string:copies("a", 40)),
              Actual = Fun(),
              case Result =:= Actual of
                  true -> ok;
                  false ->
                      ?debugFmt("~nEXPECTED~n~p~nACTUAL~n~p~n", [Result, Actual])
              end,
              ?assertEqual(Result, Actual)
      end}}.
      
%% multi_call_tests converts a list of multi_call_test specifiers into an eunit test generator
-spec multi_call_tests([multi_call_test_spec()]) -> [term()].       
multi_call_tests(Tests) ->
    [multi_call_test(Test) || Test <- Tests].

%%%===================================================================
%%% Actual test specifiers
%%%===================================================================

delete_all_tests(_) ->
    Tests =
        [?_ddb_test(
            {"delete_all delete one item",
             ?_f(erlcloud_ddb_util:delete_all(<<"tn">>, [{<<"hkn">>,<<"hkv">>}])),
             [{"
{
    \"RequestItems\": {
        \"tn\": [{
            \"DeleteRequest\": {
                \"Key\": {
                    \"hkn\":{\"S\":\"hkv\"}
                }
            }
        }]
    }
}"
               , "
{
    \"UnprocessedItems\": {
    }
}"
               }],
             ok})
         ],
    multi_call_tests(Tests).

delete_hash_key_tests(_) ->
    Tests =
        [?_ddb_test(
            {"delete_hash_key simple",
             ?_f(erlcloud_ddb_util:delete_hash_key(<<"tn">>, {<<"hkn">>, <<"hkv">>}, <<"rkn">>, [])),
             [{"
{\"TableName\":\"tn\",
 \"KeyConditions\": {
  \"hkn\": {
   \"AttributeValueList\": [
    { \"S\": \"hkv\"} ],
   \"ComparisonOperator\": \"EQ\"
  }
 },
 \"AttributesToGet\":[\"rkn\"],
 \"Limit\":25,
 \"ConsistentRead\":true
}", "
{\"Count\":2,
 \"Items\":[
  {\"rkn\":{\"B\":\"AQ==\"}},
  {\"rkn\":{\"B\":\"Ag==\"}}]
}"
              }, {"
{\"RequestItems\":{
  \"tn\":[
   {\"DeleteRequest\":{\"Key\":{\"hkn\":{\"S\":\"hkv\"}, \"rkn\":{\"B\":\"AQ==\"}}}},
   {\"DeleteRequest\":{\"Key\":{\"hkn\":{\"S\":\"hkv\"}, \"rkn\":{\"B\":\"Ag==\"}}}}]
 }}", "
{\"Responses\":{
  \"tn\":{\"ConsumedCapacityUnits\":2.0}
 }}"
                 }],
             ok}),
         ?_ddb_test(
            {"delete_hash_key no items",
             ?_f(erlcloud_ddb_util:delete_hash_key(<<"tn">>, {<<"hkn">>, <<"hkv">>}, <<"rkn">>, [])),
             [{"
{\"TableName\":\"tn\",
 \"KeyConditions\": {
  \"hkn\": {
   \"AttributeValueList\": [
    { \"S\": \"hkv\"} ],
   \"ComparisonOperator\": \"EQ\"
  }
 },
 \"AttributesToGet\":[\"rkn\"],
 \"Limit\":25,
 \"ConsistentRead\":true
}", "
{\"Count\":0,
 \"Items\":[],
 \"ConsumedCapacityUnits\":1
}"
              }],
             ok}),
         ?_ddb_test(
            {"delete_hash_key incomplete query",
             ?_f(erlcloud_ddb_util:delete_hash_key(<<"tn">>, {<<"hkn">>, <<"hkv">>}, <<"rkn">>, [])),
             [{"
{\"TableName\":\"tn\",
 \"KeyConditions\": {
  \"hkn\": {
   \"AttributeValueList\": [
    { \"S\": \"hkv\"} ],
   \"ComparisonOperator\": \"EQ\"
  }
 },
 \"AttributesToGet\":[\"rkn\"],
 \"Limit\":25,
 \"ConsistentRead\":true
}", "
{\"Count\":2,
 \"Items\":[
  {\"rkn\":{\"N\":\"1\"}},
  {\"rkn\":{\"N\":\"2\"}}],
 \"LastEvaluatedKey\":{\"hkn\":{\"S\":\"hkv\"},\"rkn\":{\"N\":\"2\"}},
 \"ConsumedCapacityUnits\":1
}"
              }, {"
{\"RequestItems\":{
  \"tn\":[
   {\"DeleteRequest\":{\"Key\":{\"hkn\":{\"S\":\"hkv\"}, \"rkn\":{\"N\":\"1\"}}}},
   {\"DeleteRequest\":{\"Key\":{\"hkn\":{\"S\":\"hkv\"}, \"rkn\":{\"N\":\"2\"}}}}]
 }}", "
{\"Responses\":{
  \"tn\":{\"ConsumedCapacityUnits\":2.0}
 }}"
                 }, {"
{\"TableName\":\"tn\",
 \"KeyConditions\": {
  \"hkn\": {
   \"AttributeValueList\": [
    { \"S\": \"hkv\"} ],
   \"ComparisonOperator\": \"EQ\"
  }
 },
 \"AttributesToGet\":[\"rkn\"],
 \"Limit\":25,
 \"ConsistentRead\":true
}", "
{\"Count\":1,
 \"Items\":[
  {\"rkn\":{\"N\":\"3\"}}],
 \"ConsumedCapacityUnits\":1
}"
              }, {"
{\"RequestItems\":{
  \"tn\":[
   {\"DeleteRequest\":{\"Key\":{\"hkn\":{\"S\":\"hkv\"}, \"rkn\":{\"N\":\"3\"}}}}]
 }}", "
{\"Responses\":{
  \"tn\":{\"ConsumedCapacityUnits\":1.0}
 }}"
                 }],
             ok}),
         ?_ddb_test(
            {"delete_hash_key incomplete batch delete",
             ?_f(erlcloud_ddb_util:delete_hash_key(<<"tn">>, {<<"hkn">>, <<"hkv">>}, <<"rkn">>, [])),
             [{"
{\"TableName\":\"tn\",
 \"KeyConditions\": {
  \"hkn\": {
   \"AttributeValueList\": [
    { \"S\": \"hkv\"} ],
   \"ComparisonOperator\": \"EQ\"
  }
 },
 \"AttributesToGet\":[\"rkn\"],
 \"Limit\":25,
 \"ConsistentRead\":true
}", "
{\"Count\":2,
 \"Items\":[
  {\"rkn\":{\"N\":\"1\"}},
  {\"rkn\":{\"N\":\"2\"}}],
 \"ConsumedCapacityUnits\":1
}"
              }, {"
{\"RequestItems\":{
  \"tn\":[
   {\"DeleteRequest\":{\"Key\":{\"hkn\":{\"S\":\"hkv\"}, \"rkn\":{\"N\":\"1\"}}}},
   {\"DeleteRequest\":{\"Key\":{\"hkn\":{\"S\":\"hkv\"}, \"rkn\":{\"N\":\"2\"}}}}]
 }}", "
{\"Responses\":{
  \"tn\":{\"ConsumedCapacityUnits\":1.0}},
 \"UnprocessedItems\":{
  \"tn\":[{\"DeleteRequest\":{\"Key\":{\"hkn\":{\"S\":\"hkv\"}, \"rkn\":{\"N\":\"2\"}}}}]
 }}"
                 }, {"
{\"TableName\":\"tn\",
 \"KeyConditions\": {
  \"hkn\": {
   \"AttributeValueList\": [
    { \"S\": \"hkv\"} ],
   \"ComparisonOperator\": \"EQ\"
  }
 },
 \"AttributesToGet\":[\"rkn\"],
 \"Limit\":25,
 \"ConsistentRead\":true
}", "
{\"Count\":1,
 \"Items\":[
  {\"rkn\":{\"N\":\"2\"}}],
 \"ConsumedCapacityUnits\":1
}"
              }, {"
{\"RequestItems\":{
  \"tn\":[
   {\"DeleteRequest\":{\"Key\":{\"hkn\":{\"S\":\"hkv\"}, \"rkn\":{\"N\":\"2\"}}}}]
 }}", "
{\"Responses\":{
  \"tn\":{\"ConsumedCapacityUnits\":1.0}
 }}"
                 }],
             ok})],
    multi_call_tests(Tests).

%% Currently don't have tests for the parallel get (more than 100 items).
%% Hard to write because of non-deterministic order
get_all_tests(_) ->
    Tests =
        [?_ddb_test(
            {"get_all one item",
             ?_f(erlcloud_ddb_util:get_all(<<"tn">>, [{<<"hkn">>, <<"hkv">>}])),
             [{"
{
    \"RequestItems\": {
        \"tn\": {
            \"Keys\": [
                {
                    \"hkn\":{\"S\":\"hkv\"}
                }
            ]
        }
    }
}"
               , "
{
    \"Responses\": {
        \"tn\": [
            {
                \"hkn\":{
                    \"S\":\"hkv\"
                }
            }
        ]
    },
    \"UnprocessedKeys\": {
    }
}"
               }],
             {ok, [[{<<"hkn">>, <<"hkv">>}]]}}),
         ?_ddb_test(
            {"get_all typed_out = true",
             ?_f(erlcloud_ddb_util:get_all(<<"tn">>, [{<<"hkn">>, <<"hkv">>}], [], [{typed_out, true}])),
             [{"
{
    \"RequestItems\": {
        \"tn\": {
            \"Keys\": [
                {
                    \"hkn\":{\"S\":\"hkv\"}
                }
            ]
        }
    }
}"
               , "
{
    \"Responses\": {
        \"tn\": [
            {
                \"hkn\":{
                    \"S\":\"hkv\"
                }
            }
        ]
    },
    \"UnprocessedKeys\": {
    }
}"
               }],
             {ok, [[{<<"hkn">>, {s, <<"hkv">>}}]]}}),
         ?_ddb_test(
            {"get_all unprocessed",
             ?_f(erlcloud_ddb_util:get_all(<<"tn">>, [{<<"hkn">>, <<"hk1">>}, {<<"hkn">>, <<"hk2">>}])),
             [{"
{
    \"RequestItems\": {
        \"tn\": {
            \"Keys\": [
                {
                    \"hkn\":{\"S\":\"hk1\"}
                },
                {
                    \"hkn\":{\"S\":\"hk2\"}
                }
            ]
        }
    }
}"
               , "
{
    \"Responses\": {
        \"tn\": [
            {
                \"hkn\":{
                    \"S\":\"hk1\"
                }
            }
        ]
    },
    \"UnprocessedKeys\": {
        \"tn\": {
            \"Keys\": [
                {
                    \"hkn\":{\"S\":\"hk2\"}
                }
            ]
        }
    }
}"
               }, {"
{
    \"RequestItems\": {
        \"tn\": {
            \"Keys\": [
                {
                    \"hkn\":{\"S\":\"hk2\"}
                }
            ]
        }
    }
}"
               , "
{
    \"Responses\": {
        \"tn\": [
            {
                \"hkn\":{
                    \"S\":\"hk2\"
                }
            }
        ]
    }
}"
                  }],
             {ok, [[{<<"hkn">>, <<"hk2">>}],
                   [{<<"hkn">>, <<"hk1">>}]
                  ]}})
         ],
    multi_call_tests(Tests).

list_tables_all_tests(_) ->
    Tests =
    [?_ddb_test(
        {"list_tables_all return 3 tablenames",
         ?_f(erlcloud_ddb_util:list_tables_all()),
         [{"{}",
           "{\"TableNames\":[\"tab1\",\"tab2\",\"tab3\"]}"}],
         {ok, [<<"tab1">>,<<"tab2">>,<<"tab3">>]}}),

     ?_ddb_test(
         {"list_tables_all tow batches, keep order",
          ?_f(erlcloud_ddb_util:list_tables_all()),
          [{"{}",
            "{\"LastEvaluatedTableName\": \"tab3\", \"TableNames\":[\"tab1\",\"tab2\",\"tab3\"]}"},
           {"{\"ExclusiveStartTableName\":\"tab3\"}",
            "{\"TableNames\":[\"tab5\",\"tab4\"]}"}],
          {ok, [<<"tab1">>,<<"tab2">>,<<"tab3">>,<<"tab5">>,<<"tab4">>]}})
    ],
    multi_call_tests(Tests).

put_all_tests(_) ->
    Tests =
        [?_ddb_test(
            {"put_all put one item",
             ?_f(erlcloud_ddb_util:put_all(<<"tn">>, [[{<<"hkn">>,<<"hkv">>}, {<<"atn">>,<<"atv">>}]])),
             [{"
{
    \"RequestItems\": {
        \"tn\": [{
            \"PutRequest\": {
                \"Item\": {
                    \"hkn\":{\"S\":\"hkv\"},
                    \"atn\":{\"S\":\"atv\"}
                }
            }
        }]
    }
}"
               , "
{
    \"UnprocessedItems\": {
    }
}"
               }],
             ok})
         ],
    multi_call_tests(Tests).

q_all_tests(_) ->
    Tests =
        [?_ddb_test(
            {"q_all one item",
             ?_f(erlcloud_ddb_util:q_all(<<"tn">>, {<<"hkn">>, <<"hkv">>})),
             [{"
{
    \"TableName\": \"tn\",
    \"KeyConditions\": {
        \"hkn\": {
            \"AttributeValueList\": [
                {
                    \"S\": \"hkv\"
                }
            ],
            \"ComparisonOperator\": \"EQ\"
        }
    }
}"
               , "
{
    \"Count\": 1,
    \"Items\": [
        {
            \"hkn\": {
                \"S\": \"hkv\"
            },
            \"rkn\": {
                \"S\": \"rkv\"
            }
        }
    ]
}"
               }],
             {ok, [[{<<"hkn">>, <<"hkv">>}, {<<"rkn">>, <<"rkv">>}]]}}),
         ?_ddb_test(
            {"q_all typed_out = true",
             ?_f(erlcloud_ddb_util:q_all(<<"tn">>, {<<"hkn">>, <<"hkv">>}, [{typed_out, true}])),
             [{"
{
    \"TableName\": \"tn\",
    \"KeyConditions\": {
        \"hkn\": {
            \"AttributeValueList\": [
                {
                    \"S\": \"hkv\"
                }
            ],
            \"ComparisonOperator\": \"EQ\"
        }
    }
}"
               , "
{
    \"Count\": 1,
    \"Items\": [
        {
            \"hkn\": {
                \"S\": \"hkv\"
            },
            \"rkn\": {
                \"S\": \"rkv\"
            }
        }
    ]
}"
               }],
             {ok, [[{<<"hkn">>, {s, <<"hkv">>}}, {<<"rkn">>, {s, <<"rkv">>}}]]}}),
         ?_ddb_test(
            {"q_all two batches",
             ?_f(erlcloud_ddb_util:q_all(<<"tn">>, {<<"hkn">>, <<"hkv">>})),
             [{"
{
    \"TableName\": \"tn\",
    \"KeyConditions\": {
        \"hkn\": {
            \"AttributeValueList\": [
                {
                    \"S\": \"hkv\"
                }
            ],
            \"ComparisonOperator\": \"EQ\"
        }
    }
}"
               , "
{
    \"Count\": 2,
    \"Items\": [
        {
            \"hkn\": {
                \"S\": \"hkv\"
            },
            \"rkn\": {
                \"S\": \"rk1\"
            }
        },
        {
            \"hkn\": {
                \"S\": \"hkv\"
            },
            \"rkn\": {
                \"S\": \"rk2\"
            }
        }
    ],
    \"LastEvaluatedKey\": {
        \"hkn\": {
            \"S\": \"hkv\"
        },
        \"rkn\": {
            \"S\": \"rk2\"
        }
    }
}"}, 
              {"
{
    \"TableName\": \"tn\",
    \"KeyConditions\": {
        \"hkn\": {
            \"AttributeValueList\": [
                {
                    \"S\": \"hkv\"
                }
            ],
            \"ComparisonOperator\": \"EQ\"
        }
    },
    \"ExclusiveStartKey\": {
        \"hkn\": {
            \"S\": \"hkv\"
        },
        \"rkn\": {
            \"S\": \"rk2\"
        }
    }
}"
               , "
{
    \"Count\": 1,
    \"Items\": [
        {
            \"hkn\": {
                \"S\": \"hkv\"
            },
            \"rkn\": {
                \"S\": \"rk3\"
            }
        }
    ]
}"
              }],
             {ok, [[{<<"hkn">>, <<"hkv">>}, {<<"rkn">>, <<"rk1">>}],
                   [{<<"hkn">>, <<"hkv">>}, {<<"rkn">>, <<"rk2">>}],
                   [{<<"hkn">>, <<"hkv">>}, {<<"rkn">>, <<"rk3">>}]
                  ]}})
         ],
    multi_call_tests(Tests).

q_all_attributes() ->
    Item1 = <<"item_1">>,
    Item2 = <<"item_2">>,
    meck:new(EDDB = erlcloud_ddb2),
    meck:sequence(EDDB, q, 4, [
        {ok, #ddb2_q{last_evaluated_key = <<"key">>,
                     items              = [Item1]}},
        {ok, #ddb2_q{last_evaluated_key = undefined,
                     items              = [Item2]}}
    ]),
    ?assertEqual({ok, [Item1, Item2]},
                 erlcloud_ddb_util:q_all(<<"tbl">>, [])),
    meck:unload(EDDB).

q_all_count() ->
    meck:new(EDDB = erlcloud_ddb2),
    meck:sequence(EDDB, q, 4, [
        {ok, #ddb2_q{last_evaluated_key = <<"key">>,
                     items              = undefined,
                     count              = 2}},
        {ok, #ddb2_q{last_evaluated_key = undefined,
                     items              = undefined,
                     count              = 1}}
    ]),
    ?assertEqual({ok, 3},
                 erlcloud_ddb_util:q_all(<<"tbl">>, [])),
    meck:unload(EDDB).

scan_all_tests(_) ->
    Tests =
        [?_ddb_test(
            {"scan_all one item",
             ?_f(erlcloud_ddb_util:scan_all(<<"tn">>)),
             [{"
{
    \"TableName\": \"tn\"
}"
               , "
{
    \"Count\": 1,
    \"Items\": [
        {
            \"hkn\": {
                \"S\": \"hkv\"
            },
            \"rkn\": {
                \"S\": \"rkv\"
            }
        }
    ]
}"
               }],
             {ok, [[{<<"hkn">>, <<"hkv">>}, {<<"rkn">>, <<"rkv">>}]]}}),
         ?_ddb_test(
            {"scan_all typed_out = true",
             ?_f(erlcloud_ddb_util:scan_all(<<"tn">>, [{typed_out, true}])),
             [{"
{
    \"TableName\": \"tn\"
}"
               , "
{
    \"Count\": 1,
    \"Items\": [
        {
            \"hkn\": {
                \"S\": \"hkv\"
            },
            \"rkn\": {
                \"S\": \"rkv\"
            }
        }
    ]
}"
               }],
             {ok, [[{<<"hkn">>, {s, <<"hkv">>}}, {<<"rkn">>, {s, <<"rkv">>}}]]}}),
         ?_ddb_test(
            {"scan_all two batches",
             ?_f(erlcloud_ddb_util:scan_all(<<"tn">>)),
             [{"
{
    \"TableName\": \"tn\"
}"
               , "
{
    \"Count\": 2,
    \"Items\": [
        {
            \"hkn\": {
                \"S\": \"hkv\"
            },
            \"rkn\": {
                \"S\": \"rk1\"
            }
        },
        {
            \"hkn\": {
                \"S\": \"hkv\"
            },
            \"rkn\": {
                \"S\": \"rk2\"
            }
        }
    ],
    \"LastEvaluatedKey\": {
        \"hkn\": {
            \"S\": \"hkv\"
        },
        \"rkn\": {
            \"S\": \"rk2\"
        }
    }
}"},
              {"
{
    \"TableName\": \"tn\",
    \"ExclusiveStartKey\": {
        \"hkn\": {
            \"S\": \"hkv\"
        },
        \"rkn\": {
            \"S\": \"rk2\"
        }
    }
}"
               , "
{
    \"Count\": 1,
    \"Items\": [
        {
            \"hkn\": {
                \"S\": \"hkv\"
            },
            \"rkn\": {
                \"S\": \"rk3\"
            }
        }
    ]
}"
              }],
             {ok, [[{<<"hkn">>, <<"hkv">>}, {<<"rkn">>, <<"rk1">>}],
                   [{<<"hkn">>, <<"hkv">>}, {<<"rkn">>, <<"rk2">>}],
                   [{<<"hkn">>, <<"hkv">>}, {<<"rkn">>, <<"rk3">>}]
                  ]}})
         ],
    multi_call_tests(Tests).

scan_all_attributes() ->
    Item1 = <<"item_1">>,
    Item2 = <<"item_2">>,
    meck:new(EDDB = erlcloud_ddb2),
    meck:sequence(EDDB, scan, 3, [
        {ok, #ddb2_scan{last_evaluated_key = <<"key">>,
                        items              = [Item1]}},
        {ok, #ddb2_scan{last_evaluated_key = undefined,
                        items              = [Item2]}}
    ]),
    ?assertEqual({ok, [Item1, Item2]},
                 erlcloud_ddb_util:scan_all(<<"tbl">>, [])),
    meck:unload(EDDB).

scan_all_count() ->
    meck:new(EDDB = erlcloud_ddb2),
    meck:sequence(EDDB, scan, 3, [
        {ok, #ddb2_scan{last_evaluated_key = <<"key">>,
                        items              = undefined,
                        count              = 2}},
        {ok, #ddb2_scan{last_evaluated_key = undefined,
                        items              = undefined,
                        count              = 1}}
    ]),
    ?assertEqual({ok, 3},
                 erlcloud_ddb_util:scan_all(<<"tbl">>, [])),
    meck:unload(EDDB).

wait_for_table_active_tests(_) ->
    Tests =
    [?_ddb_test(
        {"wait_for_table_active table is active",
         ?_f(erlcloud_ddb_util:wait_for_table_active(<<"Thread">>)),
         [{"{\"TableName\":\"Thread\"}",
              "
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
        \"GlobalSecondaryIndexes\": [
            {
                \"IndexName\": \"SubjectIndex\",
                \"IndexSizeBytes\": 2048,
                \"IndexStatus\": \"CREATING\",
                \"ItemCount\": 47,
                \"KeySchema\": [
                    {
                        \"AttributeName\": \"Subject\",
                        \"KeyType\": \"HASH\"
                    },
                    {
                        \"AttributeName\": \"LastPostDateTime\",
                        \"KeyType\": \"RANGE\"
                    }
                ],
                \"Projection\": {
                    \"NonKeyAttributes\" : [
                        \"Author\"
                    ],
                    \"ProjectionType\": \"INCLUDE\"
                },
                \"ProvisionedThroughput\": {
                    \"LastDecreaseDateTime\": 0,
                    \"LastIncreaseDateTime\": 1,
                    \"NumberOfDecreasesToday\": 2,
                    \"ReadCapacityUnits\": 3,
                    \"WriteCapacityUnits\": 4
                }
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
}
         "}],
         ok}),

     ?_ddb_test(
         {"wait_for_table_active updating to active, RetryTimes = infinity",
          ?_f(erlcloud_ddb_util:wait_for_table_active(<<"Thread">>)),
          [{"{\"TableName\":\"Thread\"}",
            "
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
      \"GlobalSecondaryIndexes\": [
          {
              \"IndexName\": \"SubjectIndex\",
              \"IndexSizeBytes\": 2048,
              \"IndexStatus\": \"CREATING\",
              \"ItemCount\": 47,
              \"KeySchema\": [
                  {
                      \"AttributeName\": \"Subject\",
                      \"KeyType\": \"HASH\"
                  },
                  {
                      \"AttributeName\": \"LastPostDateTime\",
                      \"KeyType\": \"RANGE\"
                  }
              ],
              \"Projection\": {
                  \"NonKeyAttributes\" : [
                      \"Author\"
                  ],
                  \"ProjectionType\": \"INCLUDE\"
              },
              \"ProvisionedThroughput\": {
                  \"LastDecreaseDateTime\": 0,
                  \"LastIncreaseDateTime\": 1,
                  \"NumberOfDecreasesToday\": 2,
                  \"ReadCapacityUnits\": 3,
                  \"WriteCapacityUnits\": 4
              }
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
      \"TableStatus\": \"UPDATING\"
  }
}
       "},
           {"{\"TableName\":\"Thread\"}",
            "
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
      \"GlobalSecondaryIndexes\": [
          {
              \"IndexName\": \"SubjectIndex\",
              \"IndexSizeBytes\": 2048,
              \"IndexStatus\": \"CREATING\",
              \"ItemCount\": 47,
              \"KeySchema\": [
                  {
                      \"AttributeName\": \"Subject\",
                      \"KeyType\": \"HASH\"
                  },
                  {
                      \"AttributeName\": \"LastPostDateTime\",
                      \"KeyType\": \"RANGE\"
                  }
              ],
              \"Projection\": {
                  \"NonKeyAttributes\" : [
                      \"Author\"
                  ],
                  \"ProjectionType\": \"INCLUDE\"
              },
              \"ProvisionedThroughput\": {
                  \"LastDecreaseDateTime\": 0,
                  \"LastIncreaseDateTime\": 1,
                  \"NumberOfDecreasesToday\": 2,
                  \"ReadCapacityUnits\": 3,
                  \"WriteCapacityUnits\": 4
              }
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
}
       "}],
          ok}),

     ?_ddb_test(
         {"wait_for_table_active table is deleting",
          ?_f(erlcloud_ddb_util:wait_for_table_active(<<"Thread">>)),
          [{"{\"TableName\":\"Thread\"}",
            "
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
      \"GlobalSecondaryIndexes\": [
          {
              \"IndexName\": \"SubjectIndex\",
              \"IndexSizeBytes\": 2048,
              \"IndexStatus\": \"CREATING\",
              \"ItemCount\": 47,
              \"KeySchema\": [
                  {
                      \"AttributeName\": \"Subject\",
                      \"KeyType\": \"HASH\"
                  },
                  {
                      \"AttributeName\": \"LastPostDateTime\",
                      \"KeyType\": \"RANGE\"
                  }
              ],
              \"Projection\": {
                  \"NonKeyAttributes\" : [
                      \"Author\"
                  ],
                  \"ProjectionType\": \"INCLUDE\"
              },
              \"ProvisionedThroughput\": {
                  \"LastDecreaseDateTime\": 0,
                  \"LastIncreaseDateTime\": 1,
                  \"NumberOfDecreasesToday\": 2,
                  \"ReadCapacityUnits\": 3,
                  \"WriteCapacityUnits\": 4
              }
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
      \"TableStatus\": \"DELETING\"
  }
}
       "}],
          {error, deleting}}),

     ?_ddb_test(
         {"wait_for_table_active retry_threshold_exceeded",
          ?_f(erlcloud_ddb_util:wait_for_table_active(<<"Thread">>, 10, 2)),
          [{"{\"TableName\":\"Thread\"}",
            "
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
      \"GlobalSecondaryIndexes\": [
          {
              \"IndexName\": \"SubjectIndex\",
              \"IndexSizeBytes\": 2048,
              \"IndexStatus\": \"CREATING\",
              \"ItemCount\": 47,
              \"KeySchema\": [
                  {
                      \"AttributeName\": \"Subject\",
                      \"KeyType\": \"HASH\"
                  },
                  {
                      \"AttributeName\": \"LastPostDateTime\",
                      \"KeyType\": \"RANGE\"
                  }
              ],
              \"Projection\": {
                  \"NonKeyAttributes\" : [
                      \"Author\"
                  ],
                  \"ProjectionType\": \"INCLUDE\"
              },
              \"ProvisionedThroughput\": {
                  \"LastDecreaseDateTime\": 0,
                  \"LastIncreaseDateTime\": 1,
                  \"NumberOfDecreasesToday\": 2,
                  \"ReadCapacityUnits\": 3,
                  \"WriteCapacityUnits\": 4
              }
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
      \"TableStatus\": \"UPDATING\"
  }
}
       "},
           {"{\"TableName\":\"Thread\"}",
            "
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
      \"GlobalSecondaryIndexes\": [
          {
              \"IndexName\": \"SubjectIndex\",
              \"IndexSizeBytes\": 2048,
              \"IndexStatus\": \"CREATING\",
              \"ItemCount\": 47,
              \"KeySchema\": [
                  {
                      \"AttributeName\": \"Subject\",
                      \"KeyType\": \"HASH\"
                  },
                  {
                      \"AttributeName\": \"LastPostDateTime\",
                      \"KeyType\": \"RANGE\"
                  }
              ],
              \"Projection\": {
                  \"NonKeyAttributes\" : [
                      \"Author\"
                  ],
                  \"ProjectionType\": \"INCLUDE\"
              },
              \"ProvisionedThroughput\": {
                  \"LastDecreaseDateTime\": 0,
                  \"LastIncreaseDateTime\": 1,
                  \"NumberOfDecreasesToday\": 2,
                  \"ReadCapacityUnits\": 3,
                  \"WriteCapacityUnits\": 4
              }
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
      \"TableStatus\": \"UPDATING\"
  }
}
       "}],
          {error, retry_threshold_exceeded}})
    ],
    multi_call_tests(Tests).

%% Currently don't have tests for the parallel write (more than 25 items).
write_all_tests(_) ->
    Tests =
        [?_ddb_test(
            {"write_all put one item",
             ?_f(erlcloud_ddb_util:write_all(<<"tn">>, [{put, [{<<"hkn">>,<<"hkv">>}, {<<"atn">>,<<"atv">>}]}])),
             [{"
{
    \"RequestItems\": {
        \"tn\": [{
            \"PutRequest\": {
                \"Item\": {
                    \"hkn\":{\"S\":\"hkv\"},
                    \"atn\":{\"S\":\"atv\"}
                }
            }
        }]
    }
}"
               , "
{
    \"UnprocessedItems\": {
    }
}"
               }],
             ok}),
         ?_ddb_test(
            {"write_all delete one item",
             ?_f(erlcloud_ddb_util:write_all(<<"tn">>, [{delete, {<<"hkn">>,<<"hkv">>}}])),
             [{"
{
    \"RequestItems\": {
        \"tn\": [{
            \"DeleteRequest\": {
                \"Key\": {
                    \"hkn\":{\"S\":\"hkv\"}
                }
            }
        }]
    }
}"
               , "
{
    \"UnprocessedItems\": {
    }
}"
               }],
             ok}),
         ?_ddb_test(
            {"write_all unprocessed",
             ?_f(erlcloud_ddb_util:write_all(<<"tn">>, [{put, [{<<"hkn">>,<<"hk1">>}, {<<"atn">>,<<"at1">>}]},{delete, {<<"hkn">>,<<"hk2">>}}])),
             [{"
{
    \"RequestItems\": {
        \"tn\": [{
            \"PutRequest\": {
                \"Item\": {
                    \"hkn\":{\"S\":\"hk1\"},
                    \"atn\":{\"S\":\"at1\"}
                }
            }
        },{
            \"DeleteRequest\": {
                \"Key\": {
                    \"hkn\":{\"S\":\"hk2\"}
                }
            }
        }]
    }
}"
               , "
{
    \"UnprocessedItems\": {
        \"tn\": [{
            \"DeleteRequest\": {
                \"Key\": {
                    \"hkn\":{\"S\":\"hk2\"}
                }
            }
        }]
    }
}"
               }, {"
{
    \"RequestItems\": {
        \"tn\": [{
            \"DeleteRequest\": {
                \"Key\": {
                    \"hkn\":{\"S\":\"hk2\"}
                }
            }
        }]
    }
}"
               , "
{
    \"UnprocessedItems\": {
    }
}"
                  }],
             ok})
         ],
    multi_call_tests(Tests).


set_out_opt_test_() ->
    [{"set_out_opt defaults to record",
      ?_assertEqual([{out, record}],
                    erlcloud_ddb_util:set_out_opt([]))},
     {"set_out_opt typed_out=true sets out=typed_record",
      ?_assertEqual([{out, typed_record}],
                    erlcloud_ddb_util:set_out_opt([{typed_out, true}]))},
     {"set_out_opt typed_out=false sets out=record",
      ?_assertEqual([{out, record}],
                    erlcloud_ddb_util:set_out_opt([{typed_out, false}]))},
     {"set_out_opt preserves location of out opt",
      ?_assertEqual([{foo, bar}, {out, record}],
                    erlcloud_ddb_util:set_out_opt([{typed_out, false}, {foo, bar}, {out, record}]))},
     {"set_out_opt overrides out opt with valid value",
      ?_assertEqual([{out, typed_record}, {foo, bar}],
                    erlcloud_ddb_util:set_out_opt([{typed_out, true}, {out, json}, {foo, bar}]))},
     {"set_out_opt returns default on bogus typed_out opt",
      ?_assertEqual([{out, record}],
                    erlcloud_ddb_util:set_out_opt([{typed_out, bogus}]))}].
