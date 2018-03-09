-module(erlcloud_mes_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").

%% Unit tests for mes.
%% These tests work by using meck to mock erlcloud_httpc. There are two classes of test: input and output.
%%
%% Input tests verify that different function args produce the desired JSON request.
%% An input test list provides a list of funs and the JSON that is expected to result.
%%
%% Output tests verify that the http response produces the correct return from the fun.
%% An output test lists provides a list of response bodies and the expected return.
%% TODO: Fix all the links

%% The _mes_test macro provides line number annotation to a test, similar to _test, but doesn't wrap in a fun
-define(_mes_test(T), {?LINE, T}).
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
     [fun get_entitlement_input_tests/1,
      fun get_entitlement_output_tests/1
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
              erlcloud_mes:configure(string:copies("A", 20), string:copies("a", 40)),
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
              erlcloud_mes:configure(string:copies("A", 20), string:copies("a", 40)),
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

%% TODO: Fix API Doc Links
%% GetEntitlements test based on the API examples:
%% http://docs.aws.amazon.com/AWSJavaScriptSDK/latest/AWS/MarketplaceEntitlements.html
get_entitlement_input_tests(_) ->
    Tests =
        [?_mes_test(
            {"GetEntitlements example request",
             ?_f(erlcloud_mes:get_entitlement(
                    <<"string">>)),
"{
    \"ProductCode\":\"string\"
}"
            }),
        ?_mes_test(
            {"GetEntitlements filter by customer identifier example request",
             ?_f(erlcloud_mes:get_entitlement(
                    <<"string">>,
                    [{customer_identifier, [<<"string">>]}])),
"{
    \"ProductCode\":\"string\",
    \"Filter\":
        {
         \"CUSTOMER_IDENTIFIER\": [\"string\"]
        }
}"
            }),
        ?_mes_test(
            {"GetEntitlements filter by dimension example request",
             ?_f(erlcloud_mes:get_entitlement(
                    <<"string">>,
                    [{dimension, [<<"string">>]}])),
"{
    \"ProductCode\":\"string\",
    \"Filter\":
        {
         \"DIMENSION\": [\"string\"]
        }
}"
            }),
        ?_mes_test(
            {"GetEntitlements filter by dimension example request",
             ?_f(erlcloud_mes:get_entitlement(
                    <<"string">>,
                    [{customer_identifier, [<<"string">>]}, {dimension, [<<"string">>]}])),
"{
    \"ProductCode\":\"string\",
    \"Filter\":
        {
         \"CUSTOMER_IDENTIFIER\": [\"string\"],
         \"DIMENSION\": [\"string\"]
        }
}"
            }),
        ?_mes_test(
            {"GetEntitlements example request by next_token",
             ?_f(erlcloud_mes:get_entitlement(
                    <<"string">>,
                    [{customer_identifier, [<<"string">>]}, {dimension, [<<"string">>]}],
                    [{next_token, <<"token">>}])),
"{
    \"ProductCode\":\"string\",
    \"Filter\":
        {
         \"CUSTOMER_IDENTIFIER\": [\"string\"],
         \"DIMENSION\": [\"string\"]
        },
    \"NextToken\":\"token\"
}"
            }),
        ?_mes_test(
            {"GetEntitlements example request by next_token and max_results",
             ?_f(erlcloud_mes:get_entitlement(
                    <<"string">>,
                    [{customer_identifier, [<<"string">>]}, {dimension, [<<"string">>]}],
                    [{next_token, <<"token">>}, {max_results, 10}])),
"{
    \"ProductCode\":\"string\",
    \"Filter\":
        {
         \"CUSTOMER_IDENTIFIER\": [\"string\"],
         \"DIMENSION\": [\"string\"]
        },
    \"NextToken\":\"token\",
    \"MaxResults\":10
}"
            })
        ],

    Response = "
{
    \"Entitlements\": [
        {
            \"ProductCode\": \"string\",
            \"Dimension\": \"string\",
            \"CustomerIdentifier\": \"string\",
            \"Value\": {
                \"EntitlementValueType\": \"string\",
                \"IntegerValue\": 5
            },
            \"ExpirationDate\": 1485477404000
        }
    ]
}",
    input_tests(Response, Tests).

get_entitlement_output_tests(_) ->
    Tests =
        [?_mes_test(
            {"GetEntitlements example response","
{
    \"Entitlements\": [
        {
            \"ProductCode\": \"string\",
            \"Dimension\": \"string\",
            \"CustomerIdentifier\": \"string\",
            \"Value\": {
                \"EntitlementValueType\": \"string\",
                \"IntegerValue\": 5
            },
            \"ExpirationDate\": 1485477404000
        }
    ]
}",
             {ok, jsx:decode(<<"
{
    \"Entitlements\": [
        {
            \"ProductCode\": \"string\",
            \"Dimension\": \"string\",
            \"CustomerIdentifier\": \"string\",
            \"Value\": {
                \"EntitlementValueType\": \"string\",
                \"IntegerValue\": 5
            },
            \"ExpirationDate\": 1485477404000
        }
    ]
}">>)}}
        ),
        ?_mes_test(
            {"GetEntitlements example response with NextToken","
{
    \"Entitlements\": [
        {
            \"ProductCode\": \"string\",
            \"Dimension\": \"string\",
            \"CustomerIdentifier\": \"string\",
            \"Value\": {
                \"EntitlementValueType\": \"string\",
                \"IntegerValue\": 5
            },
            \"NextToken\": \"token\",
            \"ExpirationDate\": 1485477404000
        }
    ]
}",
             {ok, jsx:decode(<<"
{
    \"Entitlements\": [
        {
            \"ProductCode\": \"string\",
            \"Dimension\": \"string\",
            \"CustomerIdentifier\": \"string\",
            \"Value\": {
                \"EntitlementValueType\": \"string\",
                \"IntegerValue\": 5
            },
            \"NextToken\": \"token\",
            \"ExpirationDate\": 1485477404000
        }
    ]
}">>)}}
        )],

    output_tests(?_f(erlcloud_mes:get_entitlement(
           <<"string">>,
           [{customer_identifier, [<<"string">>]}, {dimension, [<<"string">>]}])), Tests) ++
    output_tests(?_f(erlcloud_mes:get_entitlement(
           <<"string">>,
           [{customer_identifier, [<<"string">>]}, {dimension, [<<"string">>]}],
           [{next_token, <<"token">>}])), Tests).
