-module(erlcloud_cloudwatch_logs_tests).


-include_lib("eunit/include/eunit.hrl").


%% Unit tests for cloudwatch.
%% These tests work by using meck to mock erlcloud_httpc. There are two classes
%% of test: input and output.
%%
%% Input tests verify that different function args produce the desired query
%% parameters.
%%
%% An input test list provides a list of funs and the parameters that are
%% expected to result.
%%
%% Output tests verify that the http response produces the correct return
%% from the fun.
%% An output test lists provides a list of response bodies and the
%% expected return.


%% The _cloudwatch_test macro provides line number annotation to a test,
%% similar to _test, but doesn't wrap in a fun
-define(_cloudwatch_test(T), {?LINE, T}).
%% The _f macro is a terse way to wrap code in a fun.
%% Similar to _test but doesn't annotate with a line number
-define(_f(F), fun() -> F end).


-define(ACCESS_KEY_ID, string:copies("A", 20)).
-define(SECRET_ACCESS_KEY, string:copies("a", 40)).


-define(API_VERSION, <<"2014-03-28">>).
-define(DEFAULT_LIMIT, 50).
-define(NON_DEFAULT_LIMIT, 100).
-define(LOG_GROUP_NAME_PREFIX, <<"/aws/apigateway/welcome">>).
-define(LOG_GROUP_NAME, <<"/aws/apigateway/welcome">>).
-define(LOG_STREAM_NAME_PREFIX, <<"welcome">>).
-define(LOG_STREAM_NAME, <<"welcome">>).
-define(PAGING_TOKEN, <<"arn:aws:logs:us-east-1:352773894028:log-group:/aws/apigateway/welcome:*">>).
-define(FILTER_NAME_PREFIX, <<"aws/apigateway/welcome">>).
-define(METRIC_NAME, <<"ct_test_metric">>).
-define(METRIC_NAMESPACE, <<"CISBenchmark">>).

-define(LOG_GROUP, [
    {<<"arn">>, <<"arn:aws:logs:us-east-1:352773894028:log-group:/aws/apigateway/welcome:*">>},
    {<<"creationTime">>, 1476283527335},
    {<<"logGroupName">>, <<"/aws/apigateway/welcome">>},
    {<<"metricFilterCount">>, 0},
    {<<"retentionInDays">>, 10},
    {<<"storedBytes">>, 85}
]).

-define(METRIC_FILTER, [
    {<<"creationTime">>, 1518024063379},
    {<<"filterName">>, <<"ct_test_filter">>},
    {<<"filterPattern">>, <<"{ ($.errorCode = \"*UnauthorizedOperation\") "
                            "|| ($.errorCode = \"AccessDenied*\") }">>},
    {<<"logGroupName">>, ?LOG_GROUP_NAME},
    {<<"metricTransformations">>, [
        {<<"defaultValue">>, <<"0">>},
        {<<"metricValue">>, <<"1">>},
        {<<"metricNamespace">>, ?METRIC_NAMESPACE},
        {<<"metricName">>, ?METRIC_NAME}
    ]}
]).

-define(LOG_STREAM, [
    {<<"arn">>, <<"arn:aws:logs:us-east-1:352773894028:log-group:/aws/apigateway/welcome:log-stream:welcome">>},
    {<<"creationTime">>, 1476283527335},
    {<<"firstEventTimestamp">>, 1476283527335},
    {<<"lastEventTimestamp">>, 1576283527335},
    {<<"lastIngestionTime">>, 1576283527335},
    {<<"logStreamName">>, <<"welcome">>},
    {<<"storedBytes">>, 1024},
    {<<"uploadSequenceToken">>, <<"0123456789">>}
]).

-define(LOG_SEQUENCE, <<"xxx">>).
-define(LOG_BATCH, [#{timestamp => 1526233086694, message => <<"test">>}]).
-define(LOG_BATCH_EXPECTED, [[{<<"message">>,<<"test">>}, {<<"timestamp">>,1526233086694}]]).

%%==============================================================================
%% Test generator functions
%%==============================================================================


erlcloud_cloudwatch_test_() ->
    {foreach, fun start/0, fun stop/1, [
        fun create_log_group_input_test/1,

        fun create_log_stream_input_test/1,

        fun delete_log_group_input_test/1,

        fun delete_log_stream_input_test/1,

        fun describe_log_groups_input_tests/1,
        fun describe_log_groups_output_tests/1,

        fun describe_metric_filters_input_tests/1,
        fun describe_metric_filters_output_tests/1,

        fun describe_log_streams_input_tests/1,
        fun describe_log_streams_output_tests/1,

        fun put_logs_events_input_tests/1
    ]}.


%%==============================================================================
%% Setup functions
%%==============================================================================


start() ->
    meck:new(erlcloud_httpc).


stop(_) ->
    meck:unload(erlcloud_httpc).


%%==============================================================================
%% Test functions
%%==============================================================================


create_log_group_input_test(_) ->
    input_tests(jsx:encode([]), [
        ?_cloudwatch_test(
            {"Tests creating log group",
             ?_f(erlcloud_cloudwatch_logs:create_log_group(?LOG_GROUP_NAME)),
             [{<<"Action">>, <<"CreateLogGroup">>},
              {<<"Version">>, ?API_VERSION},
              {<<"logGroupName">>, ?LOG_GROUP_NAME}]}
        ),
        ?_cloudwatch_test(
            {"Tests creating log group with custom AWS config provided",
             ?_f(erlcloud_cloudwatch_logs:create_log_group(
                 ?LOG_GROUP_NAME,
                 erlcloud_aws:default_config()
             )),
             [{<<"Action">>, <<"CreateLogGroup">>},
              {<<"Version">>, ?API_VERSION},
              {<<"logGroupName">>, ?LOG_GROUP_NAME}]}
        ),
        ?_cloudwatch_test(
            {"Tests creating log group with AWS Tags and KMS key",
             ?_f(erlcloud_cloudwatch_logs:create_log_group(
                 ?LOG_GROUP_NAME,
                 [{<<"tag_name">>, <<"tag_value">>}],
                 "alias/example",
                 erlcloud_aws:default_config()
             )),
             [{<<"Action">>, <<"CreateLogGroup">>},
              {<<"tags">>, [{<<"tag_name">>, <<"tag_value">>}]},
              {<<"kmsKeyId">>, <<"alias/example">>},
              {<<"Version">>, ?API_VERSION},
              {<<"logGroupName">>, ?LOG_GROUP_NAME}]}
        ),
        ?_cloudwatch_test(
            {"Tests creating log group without AWS Tags and with KMS key",
             ?_f(erlcloud_cloudwatch_logs:create_log_group(
                ?LOG_GROUP_NAME,
                undefined,
                "alias/example",
                erlcloud_aws:default_config()
             )),
             [{<<"Action">>, <<"CreateLogGroup">>},
              {<<"kmsKeyId">>, <<"alias/example">>},
              {<<"Version">>, ?API_VERSION},
              {<<"logGroupName">>, ?LOG_GROUP_NAME}]}
        )
    ]).


delete_log_group_input_test(_) ->
    input_tests(jsx:encode([]), [
        ?_cloudwatch_test(
            {"Tests creating log group",
             ?_f(erlcloud_cloudwatch_logs:delete_log_group(?LOG_GROUP_NAME)),
             [{<<"Action">>, <<"DeleteLogGroup">>},
              {<<"Version">>, ?API_VERSION},
              {<<"logGroupName">>, ?LOG_GROUP_NAME}]}
        ),
        ?_cloudwatch_test(
            {"Tests creating log group with custom AWS config provided",
             ?_f(erlcloud_cloudwatch_logs:delete_log_group(
                 ?LOG_GROUP_NAME,
                 erlcloud_aws:default_config()
             )),
             [{<<"Action">>, <<"DeleteLogGroup">>},
              {<<"Version">>, ?API_VERSION},
              {<<"logGroupName">>, ?LOG_GROUP_NAME}]}
        )
    ]).


create_log_stream_input_test(_) ->
    input_tests(jsx:encode([]), [
        ?_cloudwatch_test(
            {"Tests creating log stream",
             ?_f(erlcloud_cloudwatch_logs:create_log_stream(
                 ?LOG_GROUP_NAME,
                 ?LOG_STREAM_NAME
             )),
             [{<<"Action">>, <<"CreateLogStream">>},
              {<<"Version">>, ?API_VERSION},
              {<<"logGroupName">>, ?LOG_GROUP_NAME},
              {<<"logStreamName">>, ?LOG_STREAM_NAME}]}
        ),
        ?_cloudwatch_test(
            {"Tests creating log stream with custom AWS config provided",
             ?_f(erlcloud_cloudwatch_logs:create_log_stream(
                 ?LOG_GROUP_NAME,
                 ?LOG_STREAM_NAME,
                 erlcloud_aws:default_config()
             )),
             [{<<"Action">>, <<"CreateLogStream">>},
              {<<"Version">>, ?API_VERSION},
              {<<"logGroupName">>, ?LOG_GROUP_NAME},
              {<<"logStreamName">>, ?LOG_STREAM_NAME}]}
        )
    ]).


delete_log_stream_input_test(_) ->
    input_tests(jsx:encode([]), [
        ?_cloudwatch_test(
            {"Tests creating log stream",
             ?_f(erlcloud_cloudwatch_logs:delete_log_stream(
                ?LOG_GROUP_NAME,
                 ?LOG_STREAM_NAME
             )),
             [{<<"Action">>, <<"DeleteLogStream">>},
              {<<"Version">>, ?API_VERSION},
              {<<"logGroupName">>, ?LOG_GROUP_NAME},
              {<<"logStreamName">>, ?LOG_STREAM_NAME}]}
        ),
        ?_cloudwatch_test(
            {"Tests creating log stream with custom AWS config provided",
             ?_f(erlcloud_cloudwatch_logs:delete_log_stream(
                 ?LOG_GROUP_NAME,
                 ?LOG_STREAM_NAME,
                 erlcloud_aws:default_config()
             )),
             [{<<"Action">>, <<"DeleteLogStream">>},
              {<<"Version">>, ?API_VERSION},
              {<<"logGroupName">>, ?LOG_GROUP_NAME},
              {<<"logStreamName">>, ?LOG_STREAM_NAME}]}
        )
    ]).


describe_log_groups_input_tests(_) ->
    input_tests(jsx:encode([{<<"logGroups">>, []}]), [
        ?_cloudwatch_test(
            {"Tests describing log groups with no parameters",
             ?_f(erlcloud_cloudwatch_logs:describe_log_groups()),
             [{<<"Action">>, <<"DescribeLogGroups">>},
              {<<"Version">>, ?API_VERSION},
              {<<"limit">>, ?DEFAULT_LIMIT}]}
        ),
        ?_cloudwatch_test(
            {"Tests describing log groups with custom AWS config provided",
             ?_f(erlcloud_cloudwatch_logs:describe_log_groups(
                 erlcloud_aws:default_config()
             )),
             [{<<"Action">>, <<"DescribeLogGroups">>},
              {<<"Version">>, ?API_VERSION},
              {<<"limit">>, ?DEFAULT_LIMIT}]}
        ),
        ?_cloudwatch_test(
            {"Tests describing log groups with log group name prefix provided",
             ?_f(erlcloud_cloudwatch_logs:describe_log_groups(
                 ?LOG_GROUP_NAME_PREFIX
             )),
             [{<<"Action">>, <<"DescribeLogGroups">>},
              {<<"Version">>, ?API_VERSION},
              {<<"limit">>, ?DEFAULT_LIMIT},
              {<<"logGroupNamePrefix">>, ?LOG_GROUP_NAME_PREFIX}]}
        ),
        ?_cloudwatch_test(
            {"Tests describing log groups with custom AWS config and "
             "log group name prefix provided",
             ?_f(erlcloud_cloudwatch_logs:describe_log_groups(
                 ?LOG_GROUP_NAME_PREFIX,
                 erlcloud_aws:default_config()
             )),
             [{<<"Action">>, <<"DescribeLogGroups">>},
              {<<"Version">>, ?API_VERSION},
              {<<"limit">>, ?DEFAULT_LIMIT},
              {<<"logGroupNamePrefix">>, ?LOG_GROUP_NAME_PREFIX}]}
        ),
        ?_cloudwatch_test(
            {"Tests describing log groups with custom AWS config, "
             "log group name prefix and limit provided",
             ?_f(erlcloud_cloudwatch_logs:describe_log_groups(
                 ?LOG_GROUP_NAME_PREFIX,
                 ?NON_DEFAULT_LIMIT,
                 erlcloud_aws:default_config()
             )),
             [{<<"Action">>, <<"DescribeLogGroups">>},
              {<<"Version">>, ?API_VERSION},
              {<<"limit">>, ?NON_DEFAULT_LIMIT},
              {<<"logGroupNamePrefix">>, ?LOG_GROUP_NAME_PREFIX}]}
        ),
        ?_cloudwatch_test(
            {"Tests describing log groups with custom AWS config, log group "
             "name prefix, limit and pagination token provided",
             ?_f(erlcloud_cloudwatch_logs:describe_log_groups(
                 ?LOG_GROUP_NAME_PREFIX,
                 ?NON_DEFAULT_LIMIT,
                 ?PAGING_TOKEN,
                 erlcloud_aws:default_config()
             )),
             [{<<"Action">>, <<"DescribeLogGroups">>},
              {<<"Version">>, ?API_VERSION},
              {<<"limit">>, ?NON_DEFAULT_LIMIT},
              {<<"nextToken">>, ?PAGING_TOKEN},
              {<<"logGroupNamePrefix">>, ?LOG_GROUP_NAME_PREFIX}]}
        )
    ]).


describe_metric_filters_input_tests(_) ->
    input_tests(jsx:encode([{<<"metricFilters">>, []}]), [
        ?_cloudwatch_test(
            {"Tests describing metric filters with no parameters",
             ?_f(erlcloud_cloudwatch_logs:describe_metric_filters()),
             [{<<"Action">>, <<"DescribeMetricFilters">>},
              {<<"Version">>, ?API_VERSION},
              {<<"limit">>, ?DEFAULT_LIMIT}]}
        ),
        ?_cloudwatch_test(
            {"Tests describing metric filters with custom AWS config provided",
             ?_f(erlcloud_cloudwatch_logs:describe_metric_filters(
                 erlcloud_aws:default_config()
             )),
             [{<<"Action">>, <<"DescribeMetricFilters">>},
              {<<"Version">>, ?API_VERSION},
              {<<"limit">>, ?DEFAULT_LIMIT}]}
        ),
        ?_cloudwatch_test(
            {"Tests describing metric filters with log group name provided",
             ?_f(erlcloud_cloudwatch_logs:describe_metric_filters(
                 ?LOG_GROUP_NAME
             )),
             [{<<"Action">>, <<"DescribeMetricFilters">>},
              {<<"Version">>, ?API_VERSION},
              {<<"limit">>, ?DEFAULT_LIMIT},
              {<<"logGroupName">>, ?LOG_GROUP_NAME}]}
        ),
        ?_cloudwatch_test(
            {"Tests describing metric filters with custom AWS config and "
             "log group name provided",
             ?_f(erlcloud_cloudwatch_logs:describe_metric_filters(
                 ?LOG_GROUP_NAME,
                 erlcloud_aws:default_config()
             )),
             [{<<"Action">>, <<"DescribeMetricFilters">>},
              {<<"Version">>, ?API_VERSION},
              {<<"limit">>, ?DEFAULT_LIMIT},
              {<<"logGroupName">>, ?LOG_GROUP_NAME}]}
        ),
        ?_cloudwatch_test(
            {"Tests describing metric filters with custom AWS config, "
             "log group name and limit provided",
             ?_f(erlcloud_cloudwatch_logs:describe_metric_filters(
                 ?LOG_GROUP_NAME,
                 ?NON_DEFAULT_LIMIT,
                 erlcloud_aws:default_config()
             )),
             [{<<"Action">>, <<"DescribeMetricFilters">>},
              {<<"Version">>, ?API_VERSION},
              {<<"limit">>, ?NON_DEFAULT_LIMIT},
              {<<"logGroupName">>, ?LOG_GROUP_NAME}]}
        ),
        ?_cloudwatch_test(
            {"Tests describing metric filters with custom AWS config, "
             "log group name, limit and filter name prefix provided",
             ?_f(erlcloud_cloudwatch_logs:describe_metric_filters(
                 ?LOG_GROUP_NAME,
                 ?NON_DEFAULT_LIMIT,
                 ?FILTER_NAME_PREFIX,
                 erlcloud_aws:default_config()
             )),
             [{<<"Action">>, <<"DescribeMetricFilters">>},
              {<<"Version">>, ?API_VERSION},
              {<<"filterNamePrefix">>, ?FILTER_NAME_PREFIX},
              {<<"limit">>, ?NON_DEFAULT_LIMIT},
              {<<"logGroupName">>, ?LOG_GROUP_NAME}]}
        ),
        ?_cloudwatch_test(
            {"Tests describing metric filters with custom AWS config, "
             "log group name, limit, filter name prefix, metric name and "
             "metric namespace provided",
             ?_f(erlcloud_cloudwatch_logs:describe_metric_filters(
                 ?LOG_GROUP_NAME,
                 ?NON_DEFAULT_LIMIT,
                 ?FILTER_NAME_PREFIX,
                 ?METRIC_NAME,
                 ?METRIC_NAMESPACE,
                 erlcloud_aws:default_config()
             )),
             [{<<"Action">>, <<"DescribeMetricFilters">>},
              {<<"Version">>, ?API_VERSION},
              {<<"limit">>, ?NON_DEFAULT_LIMIT},
              {<<"filterNamePrefix">>, ?FILTER_NAME_PREFIX},
              {<<"metricName">>, ?METRIC_NAME},
              {<<"metricNamespace">>, ?METRIC_NAMESPACE},
              {<<"logGroupName">>, ?LOG_GROUP_NAME}]}
        ),
        ?_cloudwatch_test(
            {"Tests describing metric filters with custom AWS config, "
             "log group name, limit, filter name prefix, metric name, "
             "metric namespace and pagination token provided",
             ?_f(erlcloud_cloudwatch_logs:describe_metric_filters(
                 ?LOG_GROUP_NAME,
                 ?NON_DEFAULT_LIMIT,
                 ?FILTER_NAME_PREFIX,
                 ?METRIC_NAME,
                 ?METRIC_NAMESPACE,
                 ?PAGING_TOKEN,
                 erlcloud_aws:default_config()
             )),
             [{<<"Action">>, <<"DescribeMetricFilters">>},
              {<<"Version">>, ?API_VERSION},
              {<<"limit">>, ?NON_DEFAULT_LIMIT},
              {<<"filterNamePrefix">>, ?FILTER_NAME_PREFIX},
              {<<"metricName">>, ?METRIC_NAME},
              {<<"metricNamespace">>, ?METRIC_NAMESPACE},
              {<<"nextToken">>, ?PAGING_TOKEN},
              {<<"logGroupName">>, ?LOG_GROUP_NAME}]}
        )
    ]).

describe_log_groups_output_tests(_) ->
    output_tests(?_f(erlcloud_cloudwatch_logs:describe_log_groups()), [
        ?_cloudwatch_test(
            {"Tests describing all log groups",
             jsx:encode([{<<"logGroups">>, [?LOG_GROUP]}]),
             {ok, [?LOG_GROUP], undefined}}
        )
    ]).


describe_metric_filters_output_tests(_) ->
    output_tests(?_f(erlcloud_cloudwatch_logs:describe_metric_filters()), [
        ?_cloudwatch_test(
            {"Tests describing all metric filters",
             jsx:encode([{<<"metricFilters">>, [?METRIC_FILTER]}]),
             {ok, [?METRIC_FILTER], undefined}}
        )
    ]).

describe_log_streams_input_tests(_) ->
    input_tests(jsx:encode([{<<"logStreams">>, []}]), [
        ?_cloudwatch_test(
            {"Tests describing log streams with with log group name",
             ?_f(erlcloud_cloudwatch_logs:describe_log_streams(?LOG_GROUP_NAME)),
             [{<<"Action">>, <<"DescribeLogStreams">>},
              {<<"Version">>, ?API_VERSION},
              {<<"descending">>, false},
              {<<"limit">>, ?DEFAULT_LIMIT},
              {<<"logGroupName">>, ?LOG_GROUP_NAME},
              {<<"orderBy">>,<<"LogStreamName">>}]}
        ),
        ?_cloudwatch_test(
            {"Tests describing log streams with with log group name and stream name prefix",
             ?_f(erlcloud_cloudwatch_logs:describe_log_streams(
                 ?LOG_GROUP_NAME,
                 ?LOG_STREAM_NAME_PREFIX,
                 erlcloud_aws:default_config()
             )),
             [{<<"Action">>, <<"DescribeLogStreams">>},
              {<<"Version">>, ?API_VERSION},
              {<<"descending">>, false},
              {<<"limit">>, ?DEFAULT_LIMIT},
              {<<"logGroupName">>, ?LOG_GROUP_NAME},
              {<<"logStreamNamePrefix">>, ?LOG_STREAM_NAME_PREFIX},
              {<<"orderBy">>,<<"LogStreamName">>}]}
        ),
        ?_cloudwatch_test(
            {"Tests describing log streams with with log group name, stream name prefix"
             "and stream sorting",
             ?_f(erlcloud_cloudwatch_logs:describe_log_streams(
                 ?LOG_GROUP_NAME,
                 ?LOG_STREAM_NAME_PREFIX,
                 last_event_time,
                 true,
                 erlcloud_aws:default_config()
             )),
             [{<<"Action">>, <<"DescribeLogStreams">>},
              {<<"Version">>, ?API_VERSION},
              {<<"descending">>, true},
              {<<"limit">>, ?DEFAULT_LIMIT},
              {<<"logGroupName">>, ?LOG_GROUP_NAME},
              {<<"logStreamNamePrefix">>, ?LOG_STREAM_NAME_PREFIX},
              {<<"orderBy">>,<<"LastEventTime">>}]}
        ),
        ?_cloudwatch_test(
            {"Tests describing log streams with with log group name, stream name prefix,"
             "stream sorting and limits",
             ?_f(erlcloud_cloudwatch_logs:describe_log_streams(
                 ?LOG_GROUP_NAME,
                 ?LOG_STREAM_NAME_PREFIX,
                 last_event_time,
                 true,
                 ?NON_DEFAULT_LIMIT,
                 erlcloud_aws:default_config()
             )),
             [{<<"Action">>, <<"DescribeLogStreams">>},
              {<<"Version">>, ?API_VERSION},
              {<<"descending">>, true},
              {<<"limit">>, ?NON_DEFAULT_LIMIT},
              {<<"logGroupName">>, ?LOG_GROUP_NAME},
              {<<"logStreamNamePrefix">>, ?LOG_STREAM_NAME_PREFIX},
              {<<"orderBy">>,<<"LastEventTime">>}]}
        ),
        ?_cloudwatch_test(
            {"Tests describing log streams with with log group name, stream name prefix,"
             "stream sorting, limits and page token",
             ?_f(erlcloud_cloudwatch_logs:describe_log_streams(
                 ?LOG_GROUP_NAME,
                 ?LOG_STREAM_NAME_PREFIX,
                 last_event_time,
                 true,
                 ?NON_DEFAULT_LIMIT,
                 ?PAGING_TOKEN,
                 erlcloud_aws:default_config()
             )),
             [{<<"Action">>, <<"DescribeLogStreams">>},
              {<<"Version">>, ?API_VERSION},
              {<<"descending">>, true},
              {<<"limit">>, ?NON_DEFAULT_LIMIT},
              {<<"logGroupName">>, ?LOG_GROUP_NAME},
              {<<"logStreamNamePrefix">>, ?LOG_STREAM_NAME_PREFIX},
              {<<"nextToken">>, ?PAGING_TOKEN},
              {<<"orderBy">>,<<"LastEventTime">>}]}
        )
    ]).


describe_log_streams_output_tests(_) ->
    output_tests(?_f(erlcloud_cloudwatch_logs:describe_log_streams(?LOG_GROUP_NAME)), [
        ?_cloudwatch_test(
            {"Tests describing all log streams",
             jsx:encode([{<<"logStreams">>, [?LOG_STREAM]}]),
             {ok, [?LOG_STREAM], undefined}}
        )
    ]).



put_logs_events_input_tests(_) ->
    input_tests(jsx:encode([{<<"uploadSequenceToken">>, ?LOG_SEQUENCE}]), [
        ?_cloudwatch_test(
            {"Tests publishing of batch log events",
             ?_f(erlcloud_cloudwatch_logs:put_logs_events(
                ?LOG_GROUP_NAME,
                ?LOG_STREAM_NAME,
                ?LOG_SEQUENCE,
                ?LOG_BATCH,
                erlcloud_aws:default_config()
            )),
             [{<<"Action">>, <<"PutLogEvents">>},
              {<<"Version">>, ?API_VERSION},
              {<<"logEvents">>, ?LOG_BATCH_EXPECTED},
              {<<"logGroupName">>, ?LOG_GROUP_NAME},
              {<<"logStreamName">>, ?LOG_STREAM_NAME},
              {<<"sequenceToken">>, ?LOG_SEQUENCE}]}
        )
    ]).

%%==============================================================================
%% Internal functions
%%==============================================================================


input_tests(ResponseBody, Tests) ->
    [input_test(ResponseBody, Test) || Test <- Tests].


input_test(ResponseBody, {Line, {Description, Fun, ExpectedParams}}) ->
    {Description, {Line,
        fun() ->
            meck:expect(
                erlcloud_httpc,
                request,
                fun(_Url, post, _Headers, RequestBody, _Timeout, _Config) ->
                    ActualParams = jsx:decode(RequestBody),
                    ?assertEqual(sort_json(ExpectedParams), sort_json(ActualParams)),
                    {ok, {{200, "OK"}, [], ResponseBody}}
                end
            ),
            erlcloud_cloudwatch_logs:configure(?ACCESS_KEY_ID, ?SECRET_ACCESS_KEY),
            Fun()
        end
    }}.


output_tests(Fun, Tests) ->
    [output_test(Fun, Test) || Test <- Tests].


output_test(Fun, {Line, {Description, ResponseBody, Expected}}) ->
    {Description, {Line,
        fun() ->
            meck:expect(
                erlcloud_httpc,
                request,
                fun(_Url, post, _Headers, _Body, _Timeout, _Config) ->
                    {ok, {{200, "OK"}, [], ResponseBody}}
                end
            ),
            erlcloud_cloudwatch_logs:configure(?ACCESS_KEY_ID, ?SECRET_ACCESS_KEY),
            ?assertEqual(Expected, _Actual = Fun())
        end
    }}.


sort_json([{_, _} | _] = Json) ->
    Sorted = [{Key, sort_json(Value)} || {Key, Value} <- Json],
    lists:keysort(1, Sorted);
sort_json([_ | _] = Json) ->
    [sort_json(Item) || Item <- Json];
sort_json(Value) ->
    Value.
