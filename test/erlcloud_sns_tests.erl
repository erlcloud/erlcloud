%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_sns_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").
-include_lib("../include/erlcloud_aws.hrl").

%% Unit tests for iam.
%% These tests work by using meck to mock httpc. There are two classes of test: input and output.
%%
%% Input tests verify that different function args produce the desired JSON request.
%% An input test list provides a list of funs and the JSON that is expected to result.
%%
%% Output tests verify that the http response produces the correct return from the fun.
%% An output test lists provides a list of response bodies and the expected return.

%% The _ddb_test macro provides line number annotation to a test, similar to _test, but doesn't wrap in a fun
-define(_sns_test(T), {?LINE, T}).
%% The _f macro is a terse way to wrap code in a fun. Similar to _test but doesn't annotate with a line number
-define(_f(F), fun() -> F end).

%%%===================================================================
%%% Test entry points
%%%===================================================================

sns_api_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun create_topic_input_tests/1,
      fun create_topic_output_tests/1,
      fun delete_topic_input_tests/1,
      fun subscribe_input_tests/1,
      fun subscribe_output_tests/1,
      fun set_topic_attributes_input_tests/1,
      fun set_topic_attributes_output_tests/1
     ]}.

start() ->
    meck:new(httpc, [unstick]),
    ok.

stop(_) ->
    meck:unload(httpc).

%%%===================================================================
%%% Input test helpers
%%%===================================================================

%% common_params returns the list of parameters that are not validated by these tests.
%% They should be checked by lower level unit tests.
-spec common_params() -> [string()].
common_params() ->
    ["AWSAccessKeyId",
     "SignatureMethod",
     "SignatureVersion",
     "Timestamp",
     "Version",
     "Signature"].



%% validate_param checks that the query parameter is either a common param or expected
%% by the test case. If expected, returns expected with the param deleted to be used in
%% subsequent calls.
-type expected_param() :: {string(), string()}.
-spec validate_param(string(), [expected_param()]) -> [expected_param()].
validate_param(Param, Expected) ->
    case string:tokens(Param, "=") of
        [Key, Value] -> 
            ok;
        [Key] ->
            Value = "",
            ok
    end,
    case lists:member(Key, common_params()) of
        true ->
            Expected;
        false ->
            Expected1 = lists:delete({Key, Value}, Expected),
            case length(Expected) - 1 =:= length(Expected1) of
                true -> ok;
                false -> 
                    ?debugFmt("Parameter not expected: ~p", [{Key, Value}])
            end,
            ?assertEqual(length(Expected) - 1, length(Expected1)),
            Expected1
    end.

%% verifies that the parameters in the body match the expected parameters
-spec validate_params(binary(), [expected_param()]) -> ok.
validate_params(Body, Expected) ->
    ParamList = string:tokens(binary_to_list(Body), "&"),
    Remain = lists:foldl(fun validate_param/2, Expected, ParamList),
    io:format("Remain: ~p", [Remain]),
    ?assertEqual([], Remain).

%% returns the mock of the httpc function input tests expect to be called.
%% Validates the query body and responds with the provided response.
-spec input_expect(string(), [expected_param()]) -> fun().
input_expect(Response, Expected) ->
    fun(post, {_Url, [] = _Headers, _ContentType, Body}, _, []) -> 
            validate_params(Body, Expected),
            {ok, {{0, 200, 0}, 0, Response}} 
    end.

%% input_test converts an input_test specifier into an eunit test generator
-type input_test_spec() :: {pos_integer(), {fun(), [expected_param()]} | {string(), fun(), [expected_param()]}}.
-spec input_test(string(), input_test_spec()) -> tuple().
input_test(Response, {Line, {Description, Fun, Params}}) when
      is_list(Description) ->
    {Description, 
     {Line,
      fun() ->
              meck:expect(httpc, request, input_expect(Response, Params)),
              %% Configure to make sure there is a key. Would like to do this in start, but
              %% that isn't called in the same process
              erlcloud_ec2:configure(string:copies("A", 20), string:copies("a", 40)),
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
    fun(post, {_Url, [] = _Headers, _ContentType, _Body}, _, []) -> 
            {ok, {{0, 200, 0}, 0, Response}} 
    end.

%% output_test converts an output_test specifier into an eunit test generator
-type output_test_spec() :: {pos_integer(), {string(), term()} | {string(), string(), term()}}.
-spec output_test(fun(), output_test_spec()) -> tuple().
output_test(Fun, {Line, {Description, Response, Result}}) ->
    {Description,
     {Line,
      fun() ->
              meck:expect(httpc, request, output_expect(Response)),
              erlcloud_ec2:configure(string:copies("A", 20), string:copies("a", 40)),
              Actual = Fun(),
              io:format("Actual: ~p~n", [Actual]),
              io:format("Result: ~p~n", [Result]),
              ?assertEqual(Result, Actual)
      end}}.
      
%% output_tests converts a list of output_test specifiers into an eunit test generator
-spec output_tests(fun(), [output_test_spec()]) -> [term()].       
output_tests(Fun, Tests) ->
    [output_test(Fun, Test) || Test <- Tests].


%% CreateTopic test based on the API examples:
%% http://docs.aws.amazon.com/sns/latest/APIReference/API_CreateTopic.html
create_topic_input_tests(_) ->
    Tests = 
        [?_sns_test(
            {"Test creates a topic in a region.",
             ?_f(erlcloud_sns:create_topic("test_topic")),
             [
              {"Name", "test_topic"},
              {"Action", "CreateTopic"}
              ]})
        ],

   Response = "
        <CreateTopicResponse xmlns=\"http://sns.amazonaws.com/doc/2010-03-31/\">
            <CreateTopicResult>
                <TopicArn>arn:aws:sns:us-east-1:123456789012:test_topic</TopicArn>
            </CreateTopicResult>
            <ResponseMetadata>
               <RequestId>a8dec8b3-33a4-11df-8963-01868b7c937a</RequestId>
            </ResponseMetadata>
        </CreateTopicResponse>",

    input_tests(Response, Tests).
 
create_topic_output_tests(_) ->
    Tests = [?_sns_test(
                {"This is a create topic test",
                     "<CreateTopicResponse xmlns=\"http://sns.amazonaws.com/doc/2010-03-31/\">
                         <CreateTopicResult>
                           <TopicArn>arn:aws:sns:us-east-1:123456789012:My-Topic</TopicArn>
                         </CreateTopicResult>
                         <ResponseMetadata>
                           <RequestId>a8dec8b3-33a4-11df-8963-01868b7c937a</RequestId>
                         </ResponseMetadata>
                      </CreateTopicResponse>",
                {ok, "arn:aws:sns:us-east-1:123456789012:My-Topic"}})
            ],
    output_tests(?_f(erlcloud_sns:create_topic("My-Topic")), Tests).

%% DeleteTopic test based on the API examples:
%% http://docs.aws.amazon.com/sns/latest/APIReference/API_DeleteTopic.html
delete_topic_input_tests(_) ->
    Tests = 
        [?_sns_test(
            {"Test deletes a topic in a region.",
             ?_f(erlcloud_sns:delete_topic("My-Topic")),
             [
              {"TopicArn", "My-Topic"},
              {"Action", "DeleteTopic"}
              ]})
        ],

   Response = "
        <DeleteTopicResponse xmlns=\"http://sns.amazonaws.com/doc/2010-03-31/\">
            <ResponseMetadata>
                <RequestId>f3aa9ac9-3c3d-11df-8235-9dab105e9c32</RequestId>
            </ResponseMetadata>
        </DeleteTopicResponse>",

    input_tests(Response, Tests).
 
%% Subscribe test based on the API examples:
%% http://docs.aws.amazon.com/sns/latest/APIReference/API_Subscribe.html
subscribe_input_tests(_) ->
    Tests = 
        [?_sns_test(
            {"Test to prepares to subscribe an endpoint.",
             ?_f(erlcloud_sns:subscribe("arn:aws:sqs:us-west-2:123456789012:MyQueue", sqs, 
                                        "arn:aws:sns:us-west-2:123456789012:MyTopic")),
             [
              {"Action", "Subscribe"},
              {"Endpoint", "arn%3Aaws%3Asqs%3Aus-west-2%3A123456789012%3AMyQueue"},
              {"Protocol", "sqs"},
              {"TopicArn", "arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3AMyTopic"}
              ]})
        ],

   Response = "
                <SubscribeResponse xmlns=\"http://sns.amazonaws.com/doc/2010-03-31/\">
                  <SubscribeResult>
                    <SubscriptionArn>arn:aws:sns:us-west-2:123456789012:MyTopic:6b0e71bd-7e97-4d97-80ce-4a0994e55286</SubscriptionArn>
                  </SubscribeResult>
                  <ResponseMetadata>
                    <RequestId>c4407779-24a4-56fa-982c-3d927f93a775</RequestId>
                  </ResponseMetadata>
                </SubscribeResponse>",

    input_tests(Response, Tests).
 
subscribe_output_tests(_) ->
    Tests = [?_sns_test(
                {"This is a create topic test",
                     "<SubscribeResponse xmlns=\"http://sns.amazonaws.com/doc/2010-03-31/\">
                          <SubscribeResult>
                            <SubscriptionArn>arn:aws:sns:us-west-2:123456789012:MyTopic:6b0e71bd-7e97-4d97-80ce-4a0994e55286</SubscriptionArn>
                          </SubscribeResult>
                          <ResponseMetadata>
                            <RequestId>c4407779-24a4-56fa-982c-3d927f93a775</RequestId>
                          </ResponseMetadata>
                        </SubscribeResponse>",
                {ok, "arn:aws:sns:us-west-2:123456789012:MyTopic:6b0e71bd-7e97-4d97-80ce-4a0994e55286"}})
            ],
    output_tests(?_f(erlcloud_sns:subscribe("arn:aws:sqs:us-west-2:123456789012:MyQueue", sqs, 
                                        "arn:aws:sns:us-west-2:123456789012:MyTopic")), Tests).


%% Set topic attributes test based on the API examples:
%% http://docs.aws.amazon.com/sns/latest/APIReference/API_SetTopicAttributes.html
set_topic_attributes_input_tests(_) ->
    Tests = 
        [?_sns_test(
            {"Test sets topic's attribute.",
             ?_f(erlcloud_sns:set_topic_attributes("DisplayName", "MyTopicName", "arn:aws:sns:us-west-2:123456789012:MyTopic")),
             [
              {"Action", "SetTopicAttributes"},
              {"AttributeName", "DisplayName"},
              {"AttributeValue", "MyTopicName"},
              {"TopicArn", "arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3AMyTopic"}
              ]})
        ],

   Response = "
               <SetTopicAttributesResponse xmlns=\"http://sns.amazonaws.com/doc/2010-03-31/\">
                  <ResponseMetadata>
                    <RequestId>a8763b99-33a7-11df-a9b7-05d48da6f042</RequestId>
                  </ResponseMetadata>
               </SetTopicAttributesResponse> ",

    input_tests(Response, Tests).
 
set_topic_attributes_output_tests(_) ->
    Tests = [?_sns_test(
                {"This test sets topic's attribute.",
                     "<SetTopicAttributesResponse xmlns=\"http://sns.amazonaws.com/doc/2010-03-31/\">
                      <ResponseMetadata>
                        <RequestId>a8763b99-33a7-11df-a9b7-05d48da6f042</RequestId>
                      </ResponseMetadata>
                    </SetTopicAttributesResponse>",
                ok})
            ],
    output_tests(?_f(erlcloud_sns:set_topic_attributes("DisplayName", "MyTopicName", "arn:aws:sns:us-west-2:123456789012:MyTopic")), Tests).



