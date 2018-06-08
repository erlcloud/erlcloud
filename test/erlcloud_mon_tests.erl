%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_mon_tests).


-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

-define(_mon_test(T), {?LINE, T}).

-define(_f(F), fun() -> F end).

%%==============================================================================
%% Test entry points
%%==============================================================================

describe_mon_test_() ->
    {foreach, fun start/0, fun stop/1, [
        fun describe_alarms_for_metric_input_tests/1,
        fun describe_alarms_for_metric_output_tests/1
    ]}.

start() ->
    meck:new(erlcloud_aws),
    ok.

stop(_) ->
    meck:unload(erlcloud_aws).

%%==============================================================================
%% Output Test helpers
%%==============================================================================

output_test(Fun, {Line, {Description, Response, Result}}) ->
    {Description, {Line, fun() ->
                        meck:expect(erlcloud_aws, aws_request_xml4, 8,
                               {ok, element(1, xmerl_scan:string(
                                    binary_to_list(Response)))}
                            ),
                        Actual = Fun(),
                        ?assertEqual(Result, Actual)
                    end}}.


%%==============================================================================
%% Input Test helpers
%%==============================================================================

validate_param(_Param = {Key, _Value}, Params) ->
    ?assertEqual(true, proplists:is_defined(Key, Params)).

validate_params(Params, Expected) ->
    [validate_param(X, Params)
        || X <- [{"Action", ""}, {"Version", ""} | Expected]

    ].
input_expect(Response, Params) ->

    fun(get, undefined, "monitoring.amazonaws.com", undefined, "/", QParams,
            "monitoring", _) ->
        validate_params(QParams, Params),
        Response
    end.

input_test(Response, {Line, {Description, Fun, Params}})
    when is_list(Description) ->

    InputFunction = input_expect(Response, Params),

    meck:expect(erlcloud_aws, aws_request_xml4, InputFunction),

    {Description, {Line, fun() ->
            Fun()
        end}}.


%%==============================================================================
%% Input Tests
%%==============================================================================

describe_alarms_for_metric_input_tests(_) ->
    Response = {ok, element(1, xmerl_scan:string(
        binary_to_list(<<"<DescribeAlarmsForMetricResponse>"
            "</DescribeAlarmsForMetricResponse>">>)
    ))},

    ExpectedParams = [
        {"Namespace", ""},
        {"MetricName", ""},
        {"Dimensions.member.1.Name", ""},
        {"Dimensions.member.1.Value", ""},
        {"ExtendedStatistic", ""},
        {"Period", ""},
        {"Statistic", ""},
        {"Unit", ""}
    ],

    input_test(Response, ?_mon_test(
        {"Test describe alarms for metric",
        ?_f(erlcloud_mon:describe_alarms_for_metric(
            "AWS/EC2", "NetworkIn", [{"InstanceType","m1.large"}], "p95",
            17, "Average", "Seconds",
            #aws_config{})), ExpectedParams})).


%%==============================================================================
%% Output Tests
%%==============================================================================

describe_alarms_for_metric_output_tests(_) ->

    Test = ?_mon_test({"Test describe alarms for metric",
            <<"<DescribeAlarmsForMetricResponse xmlns="
            "\"http://monitoring.amazonaws.com/doc/2010-08-01/\">
                    <DescribeAlarmsForMetricResult>
                        <MetricAlarms>
                        <member>
                            <MetricName>rgallego_unauthorized_metric</MetricName>
                            <AlarmConfigurationUpdatedTimestamp>2018-02-07T17:38:24.224Z</AlarmConfigurationUpdatedTimestamp>
                            <StateValue>ALARM</StateValue>
                            <Threshold>1.0</Threshold>
                            <StateReason>Threshold Crossed: 1 datapoint [2.0 (07/02/18 17:33:00)] was greater than or equal to the threshold (1.0).</StateReason>
                            <InsufficientDataActions/>
                            <AlarmActions>
                            <member>arn:aws:sns:us-east-1:352283894008:rgallego_cloudtrail_sns_topic</member>
                            </AlarmActions>
                            <StateUpdatedTimestamp>2018-02-07T17:38:24.952Z</StateUpdatedTimestamp>
                            <Period>300</Period>
                            <Statistic>Sum</Statistic>
                            <ComparisonOperator>GreaterThanOrEqualToThreshold</ComparisonOperator>
                            <AlarmName>rgallego_unauthorized_alarm</AlarmName>
                            <EvaluationPeriods>1</EvaluationPeriods>
                            <StateReasonData>{\"version\":\"1.0\",\"queryDate\":\"2018-02-07T17:38:24.953+0000\",\"startDate\":\"2018-02-07T17:33:00.000+0000\",\"statistic\":\"Sum\",\"period\":300,\"recentDatapoints\":[2.0],\"threshold\":1.0}</StateReasonData>
                            <ActionsEnabled>true</ActionsEnabled>
                            <Namespace>CISBenchmark</Namespace>
                            <OKActions/>
                            <AlarmArn>arn:aws:cloudwatch:us-east-1:352283894008:alarm:rgallego_unauthorized_alarm</AlarmArn>
                            <Dimensions/>
                        </member>
                        </MetricAlarms>
                    </DescribeAlarmsForMetricResult>
                    <ResponseMetadata>
                        <RequestId>0e8470e6-1032-11e8-b27b-7db55194b86f</RequestId>
                    </ResponseMetadata>
                </DescribeAlarmsForMetricResponse>">>,
            [[{metric_name,"rgallego_unauthorized_metric"},
                {namespace,"CISBenchmark"},
                {dimensions,[]},
                {actions_enabled,true},
                {alarm_actions,[{arn,"arn:aws:sns:us-east-1:352283894008:rgallego_cloudtrail_sns_topic"}]},
                {alarm_arn,"arn:aws:cloudwatch:us-east-1:352283894008:alarm:rgallego_unauthorized_alarm"},
                {alarm_configuration_updated_timestamp,{{2018,2,7},
                                                        {17,38,24}}},
                {alarm_description,[]},
                {alarm_name,"rgallego_unauthorized_alarm"},
                {comparison_operator,"GreaterThanOrEqualToThreshold"},
                {evaluate_low_sample_count_percentile,[]},
                {evaluation_periods,1},
                {extended_statistic,[]},
                {insufficient_data_actions,[]},
                {ok_actions,[]},
                {period,300},
                {state_reason,"Threshold Crossed: 1 datapoint [2.0 (07/02/18 17:33:00)] was greater than or equal to the threshold (1.0)."},
                {state_reason_data,"{\"version\":\"1.0\",\"queryDate\":\"2018-02-07T17:38:24.953+0000\",\"startDate\":\"2018-02-07T17:33:00.000+0000\",\"statistic\":\"Sum\",\"period\":300,\"recentDatapoints\":[2.0],\"threshold\":1.0}"},
                {state_updated_timestamp,{{2018,2,7},{17,38,24}}},
                {state_value,"ALARM"},
                {statistic,"Sum"},
                {threshold,1.0},
                {treat_missing_data,[]},
                {unit,[]}]]
        }),

    output_test(?_f(erlcloud_mon:describe_alarms_for_metric(
                "AWS/EC2", "NetworkIn", [{"InstanceType","m1.large"}], "p95",
                17, "Average", "Seconds", #aws_config{}
            )), Test).
