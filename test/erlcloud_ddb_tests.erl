%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_ddb_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").

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
                            
%%%===================================================================
%%% Test entry points
%%%===================================================================

operation_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun delete_item_input_tests/1,
      fun delete_item_output_tests/1,
      fun get_item_input_tests/1,
      fun get_item_output_tests/1]}.

start() ->
    meck:new(httpc, [unstick]),
    ok.

stop(_) ->
    meck:unload(httpc).

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

%% returns the mock of the httpc function input tests expect to be called.
%% Validates the request body and responds with the provided response.
-spec input_expect(string(), expected_body()) -> fun().
input_expect(Response, Expected) ->
    fun(post, {_Url, _Headers, _ContentType, Body}, [], _Opts) -> 
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

%% returns the mock of the httpc function output tests expect to be called.
-spec output_expect(string()) -> fun().
output_expect(Response) ->
    fun(post, {_Url, _Headers, _ContentType, _Body}, [], _Opts) -> 
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
              erlcloud_ddb:configure(string:copies("A", 20), string:copies("a", 40)),
              Actual = Fun(),
              ?assertEqual(Result, Actual)
      end}}.
%% output_test(Fun, {Line, {Response, Result}}) ->
%%     output_test(Fun, {Line, {"", Response, Result}}).
      
%% output_tests converts a list of output_test specifiers into an eunit test generator
-spec output_tests(fun(), [output_test_spec()]) -> [term()].       
output_tests(Fun, Tests) ->
    [output_test(Fun, Test) || Test <- Tests].


%%%===================================================================
%%% Actual test specifiers
%%%===================================================================

%% DeleteItem test based on the API examples:
%% http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_DeleteItem.html
delete_item_input_tests(_) ->
    Tests =
        [?_ddb_test(
            {"DeleteItem example request",
             ?_f(erlcloud_ddb:delete_item(<<"comp-table">>, {"Mingus", 200},
                                          [{expected, {<<"status">>, "shopping"}},
                                           {return_values, all_old}])), "
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
                                       [{attributes_to_get, [<<"status">>, <<"friends">>]},
                                        {consistent_read, true}])),
             Example1Response}),
         ?_ddb_test(
            {"GetItem example request, with inferred key types",
             ?_f(erlcloud_ddb:get_item(<<"comptable">>, {"Julie", 1307654345}, 
                                       [{attributes_to_get, [<<"status">>, <<"friends">>]},
                                        {consistent_read, true}])),
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
	 \"ns\":{\"NS\":[\"12\",\"13\",\"14\"]},
	 \"bs\":{\"BS\":[\"BbY=\"]},
	 \"es\":{\"SS\":[]},
	 \"s\":{\"S\":\"Lynda\"},
	 \"n\":{\"N\":\"12\"},
	 \"b\":{\"B\":\"BbY=\"},
	 \"empty\":{\"S\":\"\"}
	},
\"ConsumedCapacityUnits\": 1
}",
             {ok, [{<<"ss">>, [<<"Lynda">>, <<"Aaron">>]},
                   {<<"ns">>, [12,13,14]},
                   {<<"bs">>, [<<5,182>>]},
                   {<<"es">>, []},
                   {<<"s">>, <<"Lynda">>},
                   {<<"n">>, 12},
                   {<<"b">>, <<5,182>>},
                   {<<"empty">>, <<>>}]}})
        ],
    
    output_tests(?_f(erlcloud_ddb:get_item(<<"table">>, <<"key">>)), Tests).
