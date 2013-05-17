%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_ddb_util_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").
-include("erlcloud_ddb.hrl").

%% Unit tests for erlcloud_ddb_util.
%% These tests work by using meck to mock httpc.
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
     [fun delete_hash_key_tests/1,
      fun get_all_tests/1,
      fun q_all_tests/1
     ]}.

start() ->
    meck:new(httpc, [unstick]),
    ok.

stop(_) ->
    meck:unload(httpc).

%%%===================================================================
%%% Multi-call test helpers
%%%===================================================================

-type description() :: string().
-type expected_body() :: string().
-type response_body() :: string().
-type http_call() :: {expected_body(), response_body()}.

%% returns the mock of the httpc function multi-call tests expect to be called.
-spec multi_call_expect([http_call(),...]) -> fun().
multi_call_expect([{Expected, Response} | TCalls]) ->
    fun(post, {_Url, _Headers, _ContentType, Body}, _HTTPOpts, _Opts) -> 
            erlcloud_ddb2_tests:validate_body(Body, Expected),
            case TCalls of
                [] ->
                    %% No more calls expected
                    meck:delete(httpc, request, 4);
                _ ->
                    %% Set up the expectation for the next call
                    meck:expect(httpc, request, multi_call_expect(TCalls))
            end,
            {ok, {{0, 200, 0}, 0, list_to_binary(Response)}} 
    end.
    

%% mutil_call_test converts a multi_call_test specifier into an eunit test generator
-type multi_call_test_spec() :: {pos_integer(), {description(), fun(), [http_call()], term()}}.
-spec multi_call_test(multi_call_test_spec()) -> tuple().
multi_call_test({Line, {Description, Fun, Calls, Result}}) ->
    {Description,
     {Line,
      fun() ->
              meck:expect(httpc, request, multi_call_expect(Calls)),
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
  {\"rkn\":{\"N\":\"1\"}},
  {\"rkn\":{\"N\":\"2\"}}]
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
