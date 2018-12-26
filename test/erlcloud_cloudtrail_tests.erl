%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_cloudtrail_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

%% Unit tests for cloudtrail.
%% These tests work by using meck to mock erlcloud_httpc. 
%% There are two classes of test: input and output.
%%
%% Input tests verify that different function args produce the desired JSON request.
%% An input test list provides a list of funs and the JSON that is expected to result.
%%
%% Output tests verify that the http response produces the correct return from the fun.
%% An output test lists provides a list of response bodies and the expected return.

%% The _cloudtrail_test macro provides line number annotation to a test, similar to _test, but doesn't wrap in a fun
-define(_cloudtrail_test(T), {?LINE, T}).
%% The _f macro is a terse way to wrap code in a fun. Similar to _test but doesn't annotate with a line number
-define(_f(F), fun() -> F end).

%%%===================================================================
%%% Test entry points
%%%===================================================================

operation_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun create_trail_input_tests/1,
      fun create_trail_output_tests/1,
      fun delete_trail_input_tests/1,
      fun delete_trail_output_tests/1,
      fun describe_trails_input_tests/1,
      fun describe_trails_output_tests/1,
      fun get_event_selectors_input_tests/1,
      fun get_event_selectors_output_tests/1,
      fun get_trail_status_input_tests/1,
      fun get_trail_status_output_tests/1,
      fun start_logging_input_tests/1,
      fun start_logging_output_tests/1,
      fun stop_logging_input_tests/1,
      fun stop_logging_output_tests/1,
      fun update_trail_input_tests/1,
      fun update_trail_output_tests/1
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

%% returns the mock of the httpc function input tests expect to be called.
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
              AwsConfig = #aws_config{
                    access_key_id=string:copies("A", 20),
                    secret_access_key=string:copies("a", 40)
              },
              put(aws_config, AwsConfig),
              
              Fun()
      end}}.


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
            {ok, {{200, "OK"}, [], Response}}
    end.

%% output_test converts an output_test specifier into an eunit test generator
-type output_test_spec() :: {pos_integer(), {string(), term()} | {string(), string(), term()}}.
-spec output_test(fun(), output_test_spec()) -> tuple().
output_test(Fun, {Line, {Description, Response, Result}}) ->
    {Description,
     {Line,
      fun() ->
              meck:expect(erlcloud_httpc, request, output_expect(Response)),
              AwsConfig = #aws_config{
                    access_key_id=string:copies("A", 20),
                    secret_access_key=string:copies("a", 40),
                    cloudtrail_raw_result = false
              },
              put(aws_config, AwsConfig),

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



%% CreateTrail test based on the API examples:
%% http://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_CreateTrail.html
create_trail_input_tests(_) ->
    Tests = 
        [?_cloudtrail_test(
            {"CreateTrail example request",
             ?_f(erlcloud_cloudtrail:create_trail("test", "test_bucket", "test_prefix", "test_topic", true, erlcloud_aws:default_config())), "
                    {
                        \"Name\": \"test\",
                        \"S3BucketName\": \"test_bucket\",
                        \"S3KeyPrefix\": \"test_prefix\",
                        \"SnsTopicName\": \"test_topic\",
                        \"IncludeGlobalServiceEvents\": \"true\"
                    }"
            })
        ],

    Response = "{}",

    input_tests(Response, Tests).

create_trail_output_tests(_) ->
    Response = <<"{
                    \"Name\": \"test\",
                    \"S3BucketName\": \"test_bucket\",
                    \"S3KeyPrefix\": \"test_prefix\",
                    \"SnsTopicName\": \"test_topic\",
                    \"IncludeGlobalServiceEvents\": \"true\"
                }">>,
    Tests = 
        [?_cloudtrail_test(
            {"CreateTrail example response", Response,
                {ok, jsx:decode(Response)}})
        ],
    output_tests(?_f(erlcloud_cloudtrail:create_trail("test", "test_bucket", "test_prefix", "test_topic", true, erlcloud_aws:default_config())), Tests).


%% DeleteTrail test based on the API examples:
%% http://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_DeleteTrail.html
delete_trail_input_tests(_) ->
    Tests = 
        [?_cloudtrail_test(
            {"DeleteTrail example request",
             ?_f(erlcloud_cloudtrail:delete_trail("test", erlcloud_aws:default_config())), "
{

    \"Name\": \"test\"
}"
            })
        ],
    Response = "{}",

    input_tests(Response, Tests).

delete_trail_output_tests(_) ->
    Response = <<"{}">>,
    Tests = 
        [?_cloudtrail_test(
            {"DeleteTrail example response", Response,
                {ok, jsx:decode(Response)}})
        ],
    output_tests(?_f(erlcloud_cloudtrail:delete_trail("test", erlcloud_aws:default_config())), Tests).

%% StartLogging test based on the API examples:
%% http://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_StartLogging.html
start_logging_input_tests(_) ->
    Tests = 
        [?_cloudtrail_test(
            {"StartLogging example request",
             ?_f(erlcloud_cloudtrail:start_logging("test", erlcloud_aws:default_config())), "
{

    \"Name\": \"test\"
}"
            })
        ],
    Response = "{}",

    input_tests(Response, Tests).

start_logging_output_tests(_) ->
    Response = <<"{}">>,
    Tests = 
        [?_cloudtrail_test(
            {"StartLogging example response", Response,
                {ok, jsx:decode(Response)}})
        ],
    output_tests(?_f(erlcloud_cloudtrail:start_logging("test", erlcloud_aws:default_config())), Tests).

%% StopLogging test based on the API examples:
%% http://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_StopLogging.html
stop_logging_input_tests(_) ->
    Tests = 
        [?_cloudtrail_test(
            {"StopLogging example request",
             ?_f(erlcloud_cloudtrail:stop_logging("test", erlcloud_aws:default_config())), "
{

    \"Name\": \"test\"
}"
            })
        ],
    Response = "{}",

    input_tests(Response, Tests).

stop_logging_output_tests(_) ->
    Response = <<"{}">>,
    Tests = 
        [?_cloudtrail_test(
            {"StopLogging example response", Response,
                {ok, jsx:decode(Response)}})
        ],
    output_tests(?_f(erlcloud_cloudtrail:stop_logging("test", erlcloud_aws:default_config())), Tests).

%% DescribeTrails test based on the API examples:
%% http://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_DescribeTrails.html
describe_trails_input_tests(_) ->
    Tests = 
        [?_cloudtrail_test(
            {"DescribeTrails example request",
             ?_f(erlcloud_cloudtrail:describe_trails(["test"], erlcloud_aws:default_config())), "
{

    \"trailNameList\": [\"test\"]
}"
            })
        ],
    Response = "{}",

    input_tests(Response, Tests).

describe_trails_output_tests(_) ->
    Response = <<"{\"TrailNameList\": [
                    {
                     \"Name\": \"test\",
                     \"S3BucketName\": \"test_bucket\",
                     \"S3KeyPrefix\": \"test_prefix\",
                     \"SnsTopicName\": \"test_topic\",
                     \"IncludeGlobalServiceEvents\": \"true\"
                    } ]
                }">>,

    Tests = 
        [?_cloudtrail_test(
            {"DescribeTrails example response", Response,
                {ok, jsx:decode(Response)}})
        ],
    output_tests(?_f(erlcloud_cloudtrail:describe_trails(["test"], erlcloud_aws:default_config())), Tests).

%% GetEventSelectors test based on the API examples:
%% https://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_GetEventSelectors.html
get_event_selectors_input_tests(_) ->
    Tests =
        [?_cloudtrail_test(
            {"GetEventSelectors example request",
             ?_f(erlcloud_cloudtrail:get_event_selectors("test", erlcloud_aws:default_config())), "
{

    \"TrailName\": \"test\"
}"
            })
        ],
    Response = "{}",

    input_tests(Response, Tests).

get_event_selectors_output_tests(_) ->
    Response = <<"{\"EventSelectors\": [
                    {
                     \"DataResources\": [
                      {
                       \"Type\": \"AWS::S3::Object\",
                       \"Values\": [ \"arn:aws:s3:::test-bucket/\" ]
                      } ],
                     \"IncludeManagementEvents\": true,
                     \"ReadWriteType\": \"All\"
                    } ],
                   \"TrailARN\": \"awn:aws:cloudtrail:us-west-1:1234567890:trail/test-trail\"
                  }">>,

    Tests =
        [?_cloudtrail_test(
            {"GetEventSelectors example response", Response,
                {ok, jsx:decode(Response)}})
        ],
    output_tests(?_f(erlcloud_cloudtrail:get_event_selectors("test", erlcloud_aws:default_config())), Tests).

%% GetTrailStatus test based on the API examples:
%% http://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_GetTrailStatus.html
get_trail_status_input_tests(_) ->
    Tests = 
        [?_cloudtrail_test(
            {"GetTrailStatus example request",
             ?_f(erlcloud_cloudtrail:get_trail_status("test", erlcloud_aws:default_config())), "
{

    \"Name\": \"test\"
}"
            })
        ],
    Response = "{}",

    input_tests(Response, Tests).

get_trail_status_output_tests(_) ->
    Response = <<"{
                    \"IsLogging\": \"true\",
                    \"LatestDeliveryError\": \"\",
                    \"LatestDeliveryTime\": \"12341234\",
                    \"LatestNotificationError\": \"\",
                    \"LatestNotificationTime\": \"13234\",
                    \"StartLoggingTime\": \"12314\",
                    \"StopLoggingTime\": \"12345\"
                 }">>,

    Tests = 
        [?_cloudtrail_test(
            {"GetTrailStatus example response", Response,
                {ok, jsx:decode(Response)}})
        ],
    output_tests(?_f(erlcloud_cloudtrail:get_trail_status("test", erlcloud_aws:default_config())), Tests).

%% UpdateTrail test based on the API examples:
%% http://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_UpdateTrail.html
update_trail_input_tests(_) ->
    Tests = 
        [?_cloudtrail_test(
            {"UpdateTrail example request",
             ?_f(erlcloud_cloudtrail:update_trail("test", "test_bucket", "test_prefix", "test_topic", true, erlcloud_aws:default_config())), "
                    {
                        \"Name\": \"test\",
                        \"S3BucketName\": \"test_bucket\",
                        \"S3KeyPrefix\": \"test_prefix\",
                        \"SnsTopicName\": \"test_topic\",
                        \"IncludeGlobalServiceEvents\": \"true\"
                    }"
            })
        ],

    Response = "{}",

    input_tests(Response, Tests).

update_trail_output_tests(_) ->
    Response = <<"{
                    \"Name\": \"test\",
                    \"S3BucketName\": \"test_bucket\",
                    \"S3KeyPrefix\": \"test_prefix\",
                    \"SnsTopicName\": \"test_topic\",
                    \"IncludeGlobalServiceEvents\": \"true\"
                }">>,
    Tests = 
        [?_cloudtrail_test(
            {"UpdateTrail example response", Response,
                {ok, jsx:decode(Response)}})
        ],
    output_tests(?_f(erlcloud_cloudtrail:update_trail("test", "test_bucket", "test_prefix", "test_topic", true, erlcloud_aws:default_config())), Tests).


