%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_sns_tests).
-include_lib("eunit/include/eunit.hrl").
% -include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

%% Unit tests for sns.
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
     [fun defaults_to_https/1,
      fun supports_explicit_http/1,
      fun supports_https/1,
      fun is_case_insensitive/1,
      fun doesnt_support_gopher/1,
      fun doesnt_accept_non_strings/1,
      fun create_topic_input_tests/1,
      fun create_topic_output_tests/1,
      fun delete_topic_input_tests/1,
      fun subscribe_input_tests/1,
      fun subscribe_output_tests/1,
      fun set_topic_attributes_input_tests/1,
      fun set_topic_attributes_output_tests/1,
      fun set_subscription_attributes_input_tests/1,
      fun set_subscription_attributes_output_tests/1,
      fun list_topics_input_tests/1,
      fun list_topics_output_tests/1,
      fun list_subscriptions_input_tests/1,
      fun list_subscriptions_output_tests/1,
      fun list_subscriptions_by_topic_input_tests/1,
      fun list_subscriptions_by_topic_output_tests/1
     ]}.

start() ->
    erlcloud_sns:configure(string:copies("A", 20), string:copies("a", 40)),

    meck:new(erlcloud_httpc),
    meck:expect(erlcloud_httpc, request,
                 fun(_,_,_,_,_,_) -> mock_httpc_response() end),
    erlcloud_sns:configure(string:copies("A", 20), string:copies("a", 40)).

stop(_) ->
    meck:unload(erlcloud_httpc).

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
    [Key, Value] = case string:tokens(Param, "=") of
                        [K, V] ->
                            [K, V];
                        [K] ->
                            [K, ""]
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
    fun(_Url, post, _Headers, Body, _Timeout, _Config) ->
            validate_params(Body, Expected),
            {ok, {{200, "OK"}, [], list_to_binary(Response)}}
    end.

%% input_test converts an input_test specifier into an eunit test generator
-type input_test_spec() :: {pos_integer(), {fun(), [expected_param()]} | {string(), fun(), [expected_param()]}}.
-spec input_test(string(), input_test_spec()) -> tuple().
input_test(Response, {Line, {Description, Fun, Params}}) when
      is_list(Description) ->
    {Description,
     {Line,
      fun() ->
              meck:expect(erlcloud_httpc, request, input_expect(Response, Params)),
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
              erlcloud_ec2:configure(string:copies("A", 20), string:copies("a", 40)),
              Actual = Fun(),
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
                "arn:aws:sns:us-east-1:123456789012:My-Topic"})
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
                "arn:aws:sns:us-west-2:123456789012:MyTopic:6b0e71bd-7e97-4d97-80ce-4a0994e55286"})
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


%% Set subscription attributes test based on the API examples:
%% https://docs.aws.amazon.com/sns/latest/api/API_SetSubscriptionAttributes.html
set_subscription_attributes_input_tests(_) ->
    Tests =
    [?_sns_test(
        {"Test sets subscriptions's attribute.",
         ?_f(erlcloud_sns:set_subscription_attributes("FilterPolicy", "{\"a\": [\"b\"]}", "arn:aws:sns:us-east-1:123456789012:My-Topic:80289ba6-0fd4-4079-afb4-ce8c8260f0ca")),
         [
             {"Action", "SetSubscriptionAttributes"},
             {"AttributeName", "FilterPolicy"},
             {"AttributeValue", "%7B%22a%22%3A%20%5B%22b%22%5D%7D"},    % Url encoded version of above filter policy
             {"SubscriptionArn", "arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Topic%3A80289ba6-0fd4-4079-afb4-ce8c8260f0ca"}
         ]})
    ],

    Response = "
               <SetSubscriptionAttributesResponse xmlns=\"http://sns.amazonaws.com/doc/2010-03-31/\">
                  <ResponseMetadata>
                    <RequestId>a8763b99-33a7-11df-a9b7-05d48da6f042</RequestId>
                  </ResponseMetadata>
               </SetSubscriptionAttributesResponse> ",

    input_tests(Response, Tests).

set_subscription_attributes_output_tests(_) ->
    Tests = [?_sns_test(
        {"This test sets subscription's attribute.",
         "<SetSubscriptionAttributesResponse xmlns=\"http://sns.amazonaws.com/doc/2010-03-31/\">
          <ResponseMetadata>
            <RequestId>a8763b99-33a7-11df-a9b7-05d48da6f042</RequestId>
          </ResponseMetadata>
        </SetSubscriptionAttributesResponse>",
         ok})
    ],
    output_tests(?_f(erlcloud_sns:set_subscription_attributes("FilterPolicy", "{\"a\": [\"b\"]}", "arn:aws:sns:us-east-1:123456789012:My-Topic:80289ba6-0fd4-4079-afb4-ce8c8260f0ca")), Tests).


%% List topics test based on the API example:
%% http://docs.aws.amazon.com/sns/latest/APIReference/API_ListTopics.html
list_topics_input_tests(_) ->
    Tests =
        [?_sns_test(
            {"Test lists topics.",
             ?_f(erlcloud_sns:list_topics()),
             [
              {"Action", "ListTopics"}
              ]}),
        ?_sns_test(
            {"Test lists topics with token.",
             ?_f(erlcloud_sns:list_topics("token")),
             [
              {"Action", "ListTopics"},
              {"NextToken", "token"}
              ]}),
        ?_sns_test(
            {"Test lists topics all.",
             ?_f(erlcloud_sns:list_topics_all()),
             [
              {"Action", "ListTopics"}
              ]})
        ],

   Response = "<ListTopicsResponse xmlns=\"http://sns.amazonaws.com/doc/2010-03-31/\">
                  <ListTopicsResult>
                    <Topics>
                      <member>
                        <TopicArn>arn:aws:sns:us-east-1:123456789012:My-Topic</TopicArn>
                      </member>
                    </Topics>
                  </ListTopicsResult>
                  <ResponseMetadata>
                    <RequestId>3f1478c7-33a9-11df-9540-99d0768312d3</RequestId>
                  </ResponseMetadata>
                </ListTopicsResponse>",

    input_tests(Response, Tests).

list_topics_output_tests(_) ->
    output_tests(?_f(erlcloud_sns:list_topics()),
      [?_sns_test(
                {"Test lists topics.",
                     "<ListTopicsResponse xmlns=\"http://sns.amazonaws.com/doc/2010-03-31/\">
                  <ListTopicsResult>
                    <Topics>
                      <member>
                        <TopicArn>arn:aws:sns:us-east-1:123456789012:My-Topic</TopicArn>
                      </member>
                      <member>
                        <TopicArn>arn:aws:sns:us-east-1:123456789012:My-Another-Topic</TopicArn>
                      </member>
                    </Topics>
                  </ListTopicsResult>
                  <ResponseMetadata>
                    <RequestId>3f1478c7-33a9-11df-9540-99d0768312d3</RequestId>
                  </ResponseMetadata>
                </ListTopicsResponse>",
                [{topics,
                  [
                    [{arn, "arn:aws:sns:us-east-1:123456789012:My-Topic"}],
                    [{arn, "arn:aws:sns:us-east-1:123456789012:My-Another-Topic"}]
                  ]},
                 {next_token, ""}
                ]}),
        ?_sns_test(
                {"Test lists topics with token.",
                     "<ListTopicsResponse xmlns=\"http://sns.amazonaws.com/doc/2010-03-31/\">
                  <ListTopicsResult>
                    <Topics>
                      <member>
                        <TopicArn>arn:aws:sns:us-east-1:123456789012:My-Topic</TopicArn>
                      </member>
                      <member>
                        <TopicArn>arn:aws:sns:us-east-1:123456789012:My-Another-Topic</TopicArn>
                      </member>
                    </Topics>
                    <NextToken>token</NextToken>
                  </ListTopicsResult>
                  <ResponseMetadata>
                    <RequestId>3f1478c7-33a9-11df-9540-99d0768312d3</RequestId>
                  </ResponseMetadata>
                </ListTopicsResponse>",
                [{topics,
                  [
                    [{arn, "arn:aws:sns:us-east-1:123456789012:My-Topic"}],
                    [{arn, "arn:aws:sns:us-east-1:123456789012:My-Another-Topic"}]
                  ]},
                 {next_token, "token"}
                ]})
            ]) ++
    output_tests(?_f(erlcloud_sns:list_topics_all()),
      [?_sns_test(
                {"Test lists topics all.",
                     "<ListTopicsResponse xmlns=\"http://sns.amazonaws.com/doc/2010-03-31/\">
                  <ListTopicsResult>
                    <Topics>
                      <member>
                        <TopicArn>arn:aws:sns:us-east-1:123456789012:My-Topic</TopicArn>
                      </member>
                      <member>
                        <TopicArn>arn:aws:sns:us-east-1:123456789012:My-Another-Topic</TopicArn>
                      </member>
                    </Topics>
                  </ListTopicsResult>
                  <ResponseMetadata>
                    <RequestId>3f1478c7-33a9-11df-9540-99d0768312d3</RequestId>
                  </ResponseMetadata>
                </ListTopicsResponse>",
                  [
                    [{arn, "arn:aws:sns:us-east-1:123456789012:My-Topic"}],
                    [{arn, "arn:aws:sns:us-east-1:123456789012:My-Another-Topic"}]
                  ]})
            ]).

%% List Subscriptions test based on the API example:
%% http://docs.aws.amazon.com/sns/latest/api/API_ListSubscriptions.html
list_subscriptions_input_tests(_) ->
  Tests =
    [?_sns_test(
      {"Test lists Subscriptions.",
        ?_f(erlcloud_sns:list_subscriptions()),
        [
          {"Action","ListSubscriptions"}
        ]}),
      ?_sns_test(
        {"Test lists Subscriptions token.",
          ?_f(erlcloud_sns:list_subscriptions("Token")),
          [
            {"Action","ListSubscriptions"},
            {"NextToken", "Token"}
          ]}),
      ?_sns_test(
        {"Test lists Subscriptions all.",
          ?_f(erlcloud_sns:list_subscriptions_all()),
          [
            {"Action","ListSubscriptions"}
          ]})
    ],

  Response = "<ListSubscriptionsResponse xmlns=\"http://sns.amazonaws.com/doc/2010-03-31/\">
               <ListSubscriptionsResult>
                <Subscriptions>
                 <member>
                  <TopicArn>arn:aws:sns:us-east-1:698519295917:My-Topic</TopicArn>
                  <Protocol>email</Protocol>
                  <SubscriptionArn>arn:aws:sns:us-east-1:123456789012:My-Topic:80289ba6-0fd4-4079-afb4-ce8c8260f0ca</SubscriptionArn>
                  <Owner>123456789012</Owner>
                  <Endpoint>example@amazon.com</Endpoint>
                 </member>
                </Subscriptions>
               </ListSubscriptionsResult>
               <ResponseMetadata>
               <RequestId>384ac68d-3775-11df-8963-01868b7c937a</RequestId>
               </ResponseMetadata>
              </ListSubscriptionsResponse>",

  input_tests(Response, Tests).

list_subscriptions_output_tests(_) ->
  output_tests(?_f(erlcloud_sns:list_subscriptions()),
    [?_sns_test(
      {"Test lists Subscriptions.",
        "<ListSubscriptionsResponse xmlns=\"http://sns.amazonaws.com/doc/2010-03-31/\">
          <ListSubscriptionsResult>
           <Subscriptions>
            <member>
             <TopicArn>arn:aws:sns:us-east-1:698519295917:My-Topic</TopicArn>
             <Protocol>email</Protocol>
             <SubscriptionArn>arn:aws:sns:us-east-1:123456789012:My-Topic:80289ba6-0fd4-4079-afb4-ce8c8260f0ca</SubscriptionArn>
             <Owner>123456789012</Owner>
             <Endpoint>example@amazon.com</Endpoint>
            </member>
           </Subscriptions>
          </ListSubscriptionsResult>
          <ResponseMetadata>
          <RequestId>384ac68d-3775-11df-8963-01868b7c937a</RequestId>
          </ResponseMetadata>
         </ListSubscriptionsResponse>",
        [{subscriptions,
          [
            [{topic_arn, "arn:aws:sns:us-east-1:698519295917:My-Topic"},
             {protocol, "email"},
             {arn, "arn:aws:sns:us-east-1:123456789012:My-Topic:80289ba6-0fd4-4079-afb4-ce8c8260f0ca"},
             {owner, "123456789012"},
             {endpoint, "example@amazon.com"}]
          ]},
          {next_token, ""}
        ]}),
      ?_sns_test(
        {"Test lists Subscriptions with token.",
          "<ListSubscriptionsResponse xmlns=\"http://sns.amazonaws.com/doc/2010-03-31/\">
            <ListSubscriptionsResult>
             <Subscriptions>
              <member>
               <TopicArn>arn:aws:sns:us-east-1:698519295917:My-Topic</TopicArn>
               <Protocol>email</Protocol>
               <SubscriptionArn>arn:aws:sns:us-east-1:123456789012:My-Topic:80289ba6-0fd4-4079-afb4-ce8c8260f0ca</SubscriptionArn>
               <Owner>123456789012</Owner>
               <Endpoint>example@amazon.com</Endpoint>
              </member>
             </Subscriptions>
             <NextToken>token</NextToken>
            </ListSubscriptionsResult>
            <ResponseMetadata>
            <RequestId>384ac68d-3775-11df-8963-01868b7c937a</RequestId>
            </ResponseMetadata>
           </ListSubscriptionsResponse>",
          [{subscriptions,
            [
              [{topic_arn, "arn:aws:sns:us-east-1:698519295917:My-Topic"},
                {protocol, "email"},
                {arn, "arn:aws:sns:us-east-1:123456789012:My-Topic:80289ba6-0fd4-4079-afb4-ce8c8260f0ca"},
                {owner, "123456789012"},
                {endpoint, "example@amazon.com"}]
            ]},
            {next_token, "token"}
          ]})
    ]) ++
  output_tests(?_f(erlcloud_sns:list_subscriptions_all()),
    [?_sns_test(
      {"Test lists topics all.",
        "<ListSubscriptionsResponse xmlns=\"http://sns.amazonaws.com/doc/2010-03-31/\">
          <ListSubscriptionsResult>
           <Subscriptions>
            <member>
             <TopicArn>arn:aws:sns:us-east-1:698519295917:My-Topic</TopicArn>
             <Protocol>email</Protocol>
             <SubscriptionArn>arn:aws:sns:us-east-1:123456789012:My-Topic:80289ba6-0fd4-4079-afb4-ce8c8260f0ca</SubscriptionArn>
             <Owner>123456789012</Owner>
             <Endpoint>example@amazon.com</Endpoint>
            </member>
           </Subscriptions>
          </ListSubscriptionsResult>
          <ResponseMetadata>
          <RequestId>384ac68d-3775-11df-8963-01868b7c937a</RequestId>
          </ResponseMetadata>
         </ListSubscriptionsResponse>",
        [[{topic_arn, "arn:aws:sns:us-east-1:698519295917:My-Topic"},
              {protocol, "email"},
              {arn, "arn:aws:sns:us-east-1:123456789012:My-Topic:80289ba6-0fd4-4079-afb4-ce8c8260f0ca"},
              {owner, "123456789012"},
              {endpoint, "example@amazon.com"}]
        ]})
    ]).


%% List Subscriptions By Topic  test based on the API example:
%% http://docs.aws.amazon.com/sns/latest/api/API_ListSubscriptionsByTopic.html
list_subscriptions_by_topic_input_tests(_) ->
  Tests =
    [?_sns_test(
      {"Test lists Subscriptions.",
        ?_f(erlcloud_sns:list_subscriptions_by_topic("Arn")),
        [
          {"Action","ListSubscriptionsByTopic"},
          {"TopicArn", "Arn"}
        ]}),
      ?_sns_test(
        {"Test lists Subscriptions toke.",
          ?_f(erlcloud_sns:list_subscriptions_by_topic("Arn", "Token")),
          [
            {"Action","ListSubscriptionsByTopic"},
            {"TopicArn", "Arn"},
            {"NextToken", "Token"}
          ]}),
      ?_sns_test(
        {"Test lists Subscriptions all.",
          ?_f(erlcloud_sns:list_subscriptions_by_topic_all("Arn")),
          [
            {"Action","ListSubscriptionsByTopic"},
            {"TopicArn", "Arn"}
          ]})
    ],

  Response = "<ListSubscriptionsByTopicResponse xmlns=\"http://sns.amazonaws.com/doc/2010-03-31/\">
                <ListSubscriptionsByTopicResult>
                  <Subscriptions>
                    <member>
                      <TopicArn>arn:aws:sns:us-east-1:123456789012:My-Topic</TopicArn>
                      <Protocol>email</Protocol>
                      <SubscriptionArn>arn:aws:sns:us-east-1:123456789012:My-Topic:80289ba6-0fd4-4079-afb4-ce8c8260f0ca</SubscriptionArn>
                      <Owner>123456789012</Owner>
                      <Endpoint>example@amazon.com</Endpoint>
                    </member>
                  </Subscriptions>
                </ListSubscriptionsByTopicResult>
                <ResponseMetadata>
                  <RequestId>b9275252-3774-11df-9540-99d0768312d3</RequestId>
                </ResponseMetadata>
              </ListSubscriptionsByTopicResponse>",

  input_tests(Response, Tests).

list_subscriptions_by_topic_output_tests(_) ->
  output_tests(?_f(erlcloud_sns:list_subscriptions_by_topic("arn:aws:sns:us-east-1:123456789012:My-Topic")),
    [?_sns_test(
      {"Test lists Subscriptions.",
        "<ListSubscriptionsByTopicResponse xmlns=\"http://sns.amazonaws.com/doc/2010-03-31/\">
          <ListSubscriptionsByTopicResult>
            <Subscriptions>
              <member>
                <TopicArn>arn:aws:sns:us-east-1:123456789012:My-Topic</TopicArn>
                <Protocol>email</Protocol>
                <SubscriptionArn>arn:aws:sns:us-east-1:123456789012:My-Topic:80289ba6-0fd4-4079-afb4-ce8c8260f0ca</SubscriptionArn>
                <Owner>123456789012</Owner>
                <Endpoint>example@amazon.com</Endpoint>
              </member>
            </Subscriptions>
          </ListSubscriptionsByTopicResult>
          <ResponseMetadata>
            <RequestId>b9275252-3774-11df-9540-99d0768312d3</RequestId>
          </ResponseMetadata>
        </ListSubscriptionsByTopicResponse>",
        [{subscriptions,
          [
            [{topic_arn, "arn:aws:sns:us-east-1:123456789012:My-Topic"},
             {protocol, "email"},
             {arn, "arn:aws:sns:us-east-1:123456789012:My-Topic:80289ba6-0fd4-4079-afb4-ce8c8260f0ca"},
             {owner, "123456789012"},
             {endpoint, "example@amazon.com"}]
          ]},
          {next_token, ""}
        ]}),
      ?_sns_test(
        {"Test lists Subscriptions with token.",
          "<ListSubscriptionsByTopicResponse xmlns=\"http://sns.amazonaws.com/doc/2010-03-31/\">
            <ListSubscriptionsByTopicResult>
              <Subscriptions>
                <member>
                  <TopicArn>arn:aws:sns:us-east-1:123456789012:My-Topic</TopicArn>
                  <Protocol>email</Protocol>
                  <SubscriptionArn>arn:aws:sns:us-east-1:123456789012:My-Topic:80289ba6-0fd4-4079-afb4-ce8c8260f0ca</SubscriptionArn>
                  <Owner>123456789012</Owner>
                  <Endpoint>example@amazon.com</Endpoint>
                </member>
              </Subscriptions>
              <NextToken>token</NextToken>
            </ListSubscriptionsByTopicResult>
            <ResponseMetadata>
              <RequestId>b9275252-3774-11df-9540-99d0768312d3</RequestId>
            </ResponseMetadata>
          </ListSubscriptionsByTopicResponse>",
          [{subscriptions,
            [
              [{topic_arn, "arn:aws:sns:us-east-1:123456789012:My-Topic"},
                {protocol, "email"},
                {arn, "arn:aws:sns:us-east-1:123456789012:My-Topic:80289ba6-0fd4-4079-afb4-ce8c8260f0ca"},
                {owner, "123456789012"},
                {endpoint, "example@amazon.com"}]
            ]},
            {next_token, "token"}
          ]})
    ]) ++
  output_tests(?_f(erlcloud_sns:list_subscriptions_by_topic_all("arn:aws:sns:us-east-1:123456789012:My-Topic")),
    [?_sns_test(
      {"Test lists topics all.",
        "<ListSubscriptionsByTopicResponse xmlns=\"http://sns.amazonaws.com/doc/2010-03-31/\">
             <ListSubscriptionsByTopicResult>
               <Subscriptions>
                 <member>
                   <TopicArn>arn:aws:sns:us-east-1:123456789012:My-Topic</TopicArn>
                   <Protocol>email</Protocol>
                   <SubscriptionArn>arn:aws:sns:us-east-1:123456789012:My-Topic:80289ba6-0fd4-4079-afb4-ce8c8260f0ca</SubscriptionArn>
                   <Owner>123456789012</Owner>
                   <Endpoint>example@amazon.com</Endpoint>
                 </member>
               </Subscriptions>
             </ListSubscriptionsByTopicResult>
             <ResponseMetadata>
               <RequestId>b9275252-3774-11df-9540-99d0768312d3</RequestId>
             </ResponseMetadata>
           </ListSubscriptionsByTopicResponse>",
        [[{topic_arn, "arn:aws:sns:us-east-1:123456789012:My-Topic"},
              {protocol, "email"},
              {arn, "arn:aws:sns:us-east-1:123456789012:My-Topic:80289ba6-0fd4-4079-afb4-ce8c8260f0ca"},
              {owner, "123456789012"},
              {endpoint, "example@amazon.com"}]
        ]})
    ]).



defaults_to_https(_) ->
    Config = erlcloud_aws:default_config(),
    erlcloud_sns:publish_to_topic("topicarn", "message", "subject", Config),
    ?_assertMatch({"https://sns.amazonaws.com/", _, _, _, _, Config}, request_params()).

supports_explicit_http(_) ->
    Config = (erlcloud_aws:default_config())#aws_config{sns_scheme="http://"},
    erlcloud_sns:publish_to_topic("topicarn", "message", "subject", Config),
    ?_assertMatch({"http://sns.amazonaws.com/", _, _, _, _, Config}, request_params()).

supports_https(_) ->
    Config = (erlcloud_aws:default_config())#aws_config{sns_scheme="https://"},
    erlcloud_sns:publish_to_topic("topicarn", "message", "subject", Config),
    ?_assertMatch({"https://sns.amazonaws.com/", _, _, _, _, Config}, request_params()).

is_case_insensitive(_) ->
    Config = (erlcloud_aws:default_config())#aws_config{sns_scheme="HTTPS://"},
    erlcloud_sns:publish_to_topic("topicarn", "message", "subject", Config),
    ?_assertMatch({"https://sns.amazonaws.com/", _, _, _, _, Config}, request_params()).

doesnt_support_gopher(_) ->
    Config = (erlcloud_aws:default_config())#aws_config{sns_scheme="gopher://"},
    ?_assertError({sns_error, {unsupported_scheme,"gopher://"}},
                  erlcloud_sns:publish_to_topic("topicarn", "message", "subject", Config)).

doesnt_accept_non_strings(_) ->
    Config = (erlcloud_aws:default_config())#aws_config{sns_scheme=https},
    ?_assertError({sns_error, badarg},
                  erlcloud_sns:publish_to_topic("topicarn", "message", "subject", Config)).


% ==================
% Internal functions
% ==================

get_values_from_history(Plist) ->
    [Call1] = [ Params || {_, {erlcloud_httpc, request, Params}, _} <- Plist ],
    list_to_tuple(Call1).

request_params() ->
    get_values_from_history(meck:history(erlcloud_httpc)).

mock_httpc_response() ->
    {ok, {{200, "ok"}, [], response_body()}}.

response_body() ->
    <<"<PublishResponse xmlns='http://sns.amazonaws.com/doc/2010-03-31/'>
  <PublishResult>
    <MessageId>94f20ce6-13c5-43a0-9a9e-ca52d816e90b</MessageId>
  </PublishResult>
  <ResponseMetadata>
    <RequestId>f187a3c1-376f-11df-8963-01868b7c937a</RequestId>
  </ResponseMetadata>
</PublishResponse>">>.
