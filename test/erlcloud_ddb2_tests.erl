%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_ddb2_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").
-include("erlcloud_ddb.hrl").

%% Unit tests for erlcloud_ddb2.
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

%% operation_test_() ->
%%     {foreach,
%%      fun start/0,
%%      fun stop/1,
%%      [fun delete_hash_key_tests/1
%%      ]}.

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
            erlcloud_ddb_tests:validate_body(Body, Expected),
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
             ?_f(erlcloud_ddb2:delete_hash_key(<<"tn">>, <<"hk">>, <<"rkn">>, [])),
             [{"
{\"TableName\":\"tn\",
 \"HashKeyValue\":{\"S\":\"hk\"},
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
   {\"DeleteRequest\":{\"Key\":{\"HashKeyElement\":{\"S\":\"hk\"}, \"RangeKeyElement\":{\"N\":\"1\"}}}},
   {\"DeleteRequest\":{\"Key\":{\"HashKeyElement\":{\"S\":\"hk\"}, \"RangeKeyElement\":{\"N\":\"2\"}}}}]
 }}", "
{\"Responses\":{
  \"tn\":{\"ConsumedCapacityUnits\":2.0}
 }}"
                 }],
             ok}),
         ?_ddb_test(
            {"delete_hash_key no items",
             ?_f(erlcloud_ddb2:delete_hash_key(<<"tn">>, <<"hk">>, <<"rkn">>, [])),
             [{"
{\"TableName\":\"tn\",
 \"HashKeyValue\":{\"S\":\"hk\"},
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
             ?_f(erlcloud_ddb2:delete_hash_key(<<"tn">>, <<"hk">>, <<"rkn">>, [])),
             [{"
{\"TableName\":\"tn\",
 \"HashKeyValue\":{\"S\":\"hk\"},
 \"AttributesToGet\":[\"rkn\"],
 \"Limit\":25,
 \"ConsistentRead\":true
}", "
{\"Count\":2,
 \"Items\":[
  {\"rkn\":{\"N\":\"1\"}},
  {\"rkn\":{\"N\":\"2\"}}],
 \"LastEvaluatedKey\":{\"HashKeyElement\":{\"S\":\"hk\"},\"RangeKeyElement\":{\"N\":\"2\"}},
 \"ConsumedCapacityUnits\":1
}"
              }, {"
{\"RequestItems\":{
  \"tn\":[
   {\"DeleteRequest\":{\"Key\":{\"HashKeyElement\":{\"S\":\"hk\"}, \"RangeKeyElement\":{\"N\":\"1\"}}}},
   {\"DeleteRequest\":{\"Key\":{\"HashKeyElement\":{\"S\":\"hk\"}, \"RangeKeyElement\":{\"N\":\"2\"}}}}]
 }}", "
{\"Responses\":{
  \"tn\":{\"ConsumedCapacityUnits\":2.0}
 }}"
                 }, {"
{\"TableName\":\"tn\",
 \"HashKeyValue\":{\"S\":\"hk\"},
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
   {\"DeleteRequest\":{\"Key\":{\"HashKeyElement\":{\"S\":\"hk\"}, \"RangeKeyElement\":{\"N\":\"3\"}}}}]
 }}", "
{\"Responses\":{
  \"tn\":{\"ConsumedCapacityUnits\":1.0}
 }}"
                 }],
             ok}),
         ?_ddb_test(
            {"delete_hash_key incomplete batch delete",
             ?_f(erlcloud_ddb2:delete_hash_key(<<"tn">>, <<"hk">>, <<"rkn">>, [])),
             [{"
{\"TableName\":\"tn\",
 \"HashKeyValue\":{\"S\":\"hk\"},
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
   {\"DeleteRequest\":{\"Key\":{\"HashKeyElement\":{\"S\":\"hk\"}, \"RangeKeyElement\":{\"N\":\"1\"}}}},
   {\"DeleteRequest\":{\"Key\":{\"HashKeyElement\":{\"S\":\"hk\"}, \"RangeKeyElement\":{\"N\":\"2\"}}}}]
 }}", "
{\"Responses\":{
  \"tn\":{\"ConsumedCapacityUnits\":1.0}},
 \"UnprocessedItems\":{
  \"tn\":[{\"DeleteRequest\":{\"Key\":{\"HashKeyElement\":{\"S\":\"hk\"}, \"RangeKeyElement\":{\"N\":\"2\"}}}}]
 }}"
                 }, {"
{\"TableName\":\"tn\",
 \"HashKeyValue\":{\"S\":\"hk\"},
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
   {\"DeleteRequest\":{\"Key\":{\"HashKeyElement\":{\"S\":\"hk\"}, \"RangeKeyElement\":{\"N\":\"2\"}}}}]
 }}", "
{\"Responses\":{
  \"tn\":{\"ConsumedCapacityUnits\":1.0}
 }}"
                 }],
             ok})],
    multi_call_tests(Tests).
