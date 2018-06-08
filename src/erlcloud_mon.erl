%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% File: erlcloud_mon.erl
%% Date: 14-Sep-2011
%%
%% @doc AWS CloudWatch erlang binding (the CLI SDK uses "mon_" prefix)
%%
%% @author Zvi Avraham <zvi-AT-nivertech-DOT-com>
%% @copyright 2011 Zvi Avraham
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(erlcloud_mon).

%%% Library initialization.
-export([configure/2, configure/3, new/2, new/3]).

-export([
         list_metrics/5, list_metrics/4,
         describe_alarms_for_metric/8,
         describe_alarms_for_metric/7,
         describe_alarms_for_metric/2,
         put_metric_data/3, put_metric_data/2,
         put_metric_data/6, put_metric_data/5,
         get_metric_statistics/4, get_metric_statistics/9, get_metric_statistics/8,
         configure_host/3,
         test/0,
         test2/0
        ]).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").
-include("erlcloud_mon.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-import(erlcloud_xml, [get_text/2, get_time/2, get_bool/2, get_integer/2,
                       get_float/2, get_text/1]).

-define(XMLNS_MON, "http://monitoring.amazonaws.com/doc/2010-08-01/").
-define(API_VERSION, "2010-08-01").

-define(FMT(Fmt,Args), lists:flatten(io_lib:format((Fmt),(Args)))).

%%------------------------------------------------------------------------------
%% @doc CloudWatch API - ListMetrics
%% [http://docs.amazonwebservices.com/AmazonCloudWatch/latest/APIReference/index.html?API_ListMetrics.html]
%%
%% USAGE:
%%
%% erlcloud_mon:list_metrics("AWS/EC2", "NetworkIn", [{"InstanceType","m1.large"}], "").
%% [[{metric_name,"NetworkIn"},
%%   {namespace,"AWS/EC2"},
%%   {dimensions,[[{name,"InstanceType"},{value,"m1.large"}]]}]]
%%
%% @end
%%------------------------------------------------------------------------------
-spec list_metrics(
        Namespace       ::string(),
        MetricName      ::string(),
        DimensionFilter ::[{string(),string()}],
        NextToken       ::string()
                          ) -> term() | no_return().

list_metrics(
  Namespace,
  MetricName,
  DimensionFilter,
  NextToken
 ) ->
    list_metrics(Namespace, MetricName, DimensionFilter, NextToken, default_config()).

-spec list_metrics(
        Namespace       ::string(),
        MetricName      ::string(),
        DimensionFilter ::[{string(),string()}],
        NextToken       ::string(),
        Config          ::aws_config()
                          ) -> term() | no_return().

list_metrics(
  Namespace,
  MetricName,
  DimensionFilter,
  NextToken,
  #aws_config{} = Config
 ) ->

    Params =
        [{"Namespace",  Namespace}  || Namespace/=""]
        ++
        [{"MetricName", MetricName} || MetricName/=""]
        ++
        [{"NextToken", NextToken}   || NextToken/=""]
        ++
        lists:flatten(
          [begin
               {Name, Value} = lists:nth(N, DimensionFilter),
               [{?FMT("Dimensions.member.~b.Name",  [N]), Name},
                {?FMT("Dimensions.member.~b.Value", [N]), Value}]
           end
           || N<-lists:seq(1, length(DimensionFilter))]
         ),

    Doc = mon_query(Config, "ListMetrics", Params),
    Members = xmerl_xpath:string("/ListMetricsResponse/ListMetricsResult/Metrics/member", Doc),
    [extract_member(Member) || Member <- Members].

extract_member(Node) ->
    [
     {metric_name,   get_text("MetricName", Node)},
     {namespace,     get_text("Namespace", Node)},
     {dimensions,
      [extract_dimension(Item) || Item <- xmerl_xpath:string("Dimensions/member", Node)]
     }
    ].

extract_dimension(Node) ->
    [
     {name,  get_text("Name",  Node)},
     {value, get_text("Value", Node)}
    ].

%%------------------------------------------------------------------------------
%% @doc CloudWatch API - DescribeAlarmsForMetric
%% [https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeAlarmsForMetric.html]
%%
%% USAGE:
%%
%% erlcloud_mon:describe_alarms_for_metric("AWS/EC2",
%%                                         "NetworkIn").
%% [[{metric_name,"NetworkIn"},
%%  {namespace,"AWS/EC2"},
%%  {dimensions,[]},
%%  {actions_enabled,true},
%%  {alarm_actions,[{arn,"arn:aws:sns:us-east-1:352283897777:rgallego_cloudtrail_sns_topic"}]},
%%  {alarm_arn,"arn:aws:cloudwatch:us-east-1:352283897777:alarm:rgallego_unauthorized_alarm"},
%%  {alarm_configuration_updated_timestamp,{{2018,2,7},
%%                                          {17,38,24}}},
%%  {alarm_description,[]},
%%  {alarm_name,"rgallego_unauthorized_alarm"},
%%  {comparison_operator,"GreaterThanOrEqualToThreshold"},
%%  {evaluate_low_sample_count_percentile,[]},
%%  {evaluation_periods,1},
%%  {extended_statistic,[]},
%%  {insufficient_data_actions,[]},
%%  {ok_actions,[]},
%%  {period,300},
%%  {state_reason,"Threshold Crossed: 1 datapoint [2.0 (07/02/18 17:33:00)] was greater than or equal to the threshold (1.0)."},
%%  {state_reason_data,"{\"version\":\"1.0\",\"queryDate\":\"2018-02-07T17:38:24.953+0000\",\"startDate\":\"2018-02-07T17:33:00.000+0000\",\"statistic\":\"Sum\",\"period\":300,\"recentDatapoints\":[2.0],\"threshold\":1.0}"},
%%  {state_updated_timestamp,{{2018,2,7},{17,38,24}}},
%%  {state_value,"ALARM"},
%%  {statistic,"Sum"},
%%  {threshold,1.0},
%%  {treat_missing_data,[]},
%%  {unit,[]}]]
%%
%% @end
%%------------------------------------------------------------------------------
-spec describe_alarms_for_metric(
        Namespace           ::string(),
        MetricName          ::string()
                          ) -> term().

describe_alarms_for_metric(
  Namespace,
  MetricName
 ) ->
    describe_alarms_for_metric(Namespace, MetricName, [],
                               "", undefined, "",
                               "", default_config()).

%%------------------------------------------------------------------------------
%% @doc CloudWatch API - DescribeAlarmsForMetric
%% [https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeAlarmsForMetric.html]
%%
%% USAGE:
%%
%% erlcloud_mon:describe_alarms_for_metric("AWS/EC2",
%%                                         "NetworkIn",
%%                                         [{"InstanceType","m1.large"}],
%%                                         "p95",
%%                                         17,
%%                                         "",
%%                                         "Seconds").
%% See describe_alarms_for_metric/2
%%
%% @end
%%------------------------------------------------------------------------------
-spec describe_alarms_for_metric(
        Namespace           ::string(),
        MetricName          ::string(),
        DimensionFilter     ::[{string(),string()}],
        ExtendedStatistic   ::string(),
        Period              ::pos_integer(),
        Statistic           ::statistic(),
        Unit                ::unit()
                          ) -> term().

describe_alarms_for_metric(
  Namespace,
  MetricName,
  DimensionFilter,
  ExtendedStatistic,
  Period,
  Statistic,
  Unit
 ) ->
    describe_alarms_for_metric(Namespace, MetricName, DimensionFilter,
                               ExtendedStatistic, Period, Statistic,
                               Unit, default_config()).

-spec describe_alarms_for_metric(
        Namespace           ::string(),
        MetricName          ::string(),
        DimensionFilter     ::[{string(),string()}],
        ExtendedStatistic   ::string(),
        Period              ::pos_integer() | undefined,
        Statistic           ::statistic(),
        Unit                ::unit(),
        Config              ::aws_config()
                          ) -> term().

describe_alarms_for_metric(
  Namespace,
  MetricName,
  DimensionFilter,
  ExtendedStatistic,
  Period,
  Statistic,
  Unit,
  #aws_config{} = Config
 ) ->

    Params =
        [{"Namespace",  Namespace},
         {"MetricName", MetricName}]
        ++
        [{"ExtendedStatistic", ExtendedStatistic}   || ExtendedStatistic/=""]
        ++
        [{"Period", Period}   || Period/=undefined]
        ++
        [{"Statistic", Statistic}   || Statistic/=""]
        ++
        [{"Unit", Unit}   || Unit/=""]
        ++
        lists:flatten(
          [begin
               {Name, Value} = lists:nth(N, DimensionFilter),
               [{?FMT("Dimensions.member.~b.Name",  [N]), Name},
                {?FMT("Dimensions.member.~b.Value", [N]), Value}]
           end
           || N<-lists:seq(1, length(DimensionFilter))]
         ),
    Doc = mon_query(Config, "DescribeAlarmsForMetric", Params),
    Members = xmerl_xpath:string("/DescribeAlarmsForMetricResponse/DescribeAlarmsForMetricResult/MetricAlarms/member", Doc),
    [extract_member_dafm(Member) || Member <- Members].

extract_member_dafm(Node) ->
    [
     {metric_name,   get_text("MetricName", Node)},
     {namespace,     get_text("Namespace", Node)},
     {dimensions,
      [extract_dimension_dafm(Item) || Item <- xmerl_xpath:string("Dimensions/member", Node)]
     },
     {actions_enabled, get_bool("ActionsEnabled", Node)},
     {alarm_actions,
      [extract_actions_dafm(Item) || Item <- xmerl_xpath:string("AlarmActions/member", Node)]},
     {alarm_arn, get_text("AlarmArn", Node)},
     {alarm_configuration_updated_timestamp, get_time("AlarmConfigurationUpdatedTimestamp", Node)},
     {alarm_description, get_text("AlarmDescription", Node)},
     {alarm_name, get_text("AlarmName", Node)},
     {comparison_operator, get_text("ComparisonOperator", Node)},
     {evaluate_low_sample_count_percentile, get_text("EvaluateLowSampleCountPercentile", Node)},
     {evaluation_periods, get_integer("EvaluationPeriods", Node)},
     {extended_statistic, get_text("ExtendedStatistic", Node)},
     {insufficient_data_actions,
      [extract_actions_dafm(Item) || Item <- xmerl_xpath:string("InsufficientDataActions/member", Node)]},
     {ok_actions,
      [extract_actions_dafm(Item) || Item <- xmerl_xpath:string("OKActions/member", Node)]},
     {period, get_integer("Period", Node)},
     {state_reason, get_text("StateReason", Node)},
     {state_reason_data, get_text("StateReasonData", Node)},
     {state_updated_timestamp, get_time("StateUpdatedTimestamp", Node)},
     {state_value, get_text("StateValue", Node)},
     {statistic, get_text("Statistic", Node)},
     {threshold, get_float("Threshold", Node)},
     {treat_missing_data, get_text("TreatMissingData", Node)},
     {unit, get_text("Unit", Node)}
    ].

extract_dimension_dafm(Node) ->
    [
     {name,  get_text("Name",  Node)},
     {value, get_text("Value", Node)}
    ].

extract_actions_dafm(Node) ->
    {arn,  get_text(Node)}.

%%------------------------------------------------------------------------------
%% @doc CloudWatch API - PutMetricData
%% [http://docs.amazonwebservices.com/AmazonCloudWatch/latest/APIReference/index.html?API_PutMetricData.html]
%% `
%% &MetricData.member.1.MetricName=buffers
%% &MetricData.member.1.Unit=Bytes
%% &MetricData.member.1.Value=231434333
%% &MetricData.member.1.Dimensions.member.1.Name=InstanceID
%% &MetricData.member.1.Dimensions.member.1.Value=i-aaba32d4
%% &MetricData.member.1.Dimensions.member.2.Name=InstanceType
%% &MetricData.member.1.Dimensions.member.2.Value=m1.small
%% &MetricData.member.2.MetricName=latency
%% &MetricData.member.2.Unit=Milliseconds
%% &MetricData.member.2.Value=23
%% &MetricData.member.2.Dimensions.member.1.Name=InstanceID
%% &MetricData.member.2.Dimensions.member.1.Value=i-aaba32d4
%% &MetricData.member.2.Dimensions.member.2.Name=InstanceType
%% &MetricData.member.2.Dimensions.member.2.Value=m1.small
%% '
%% @end
%%------------------------------------------------------------------------------
-spec put_metric_data(
        Namespace   ::string(),
        MetricData  ::[metric_datum()]
                      ) -> term() | no_return().

put_metric_data(Namespace, MetricData) ->
    put_metric_data(Namespace, MetricData, default_config()).

-spec put_metric_data(
        Namespace   ::string(),
        MetricData  ::[metric_datum()],
        Config      ::aws_config()
                      ) -> term() | no_return().

put_metric_data(Namespace, MetricData, #aws_config{} = Config) ->

    Params =
        [
         {"Namespace", Namespace} |
         lists:flatten(
           [ params_metric_data(N,MD) || {N,MD} <- lists:zip(lists:seq(1,length(MetricData)), MetricData) ]
          )
        ],
    mon_query(Config, "PutMetricData", Params).

%%------------------------------------------------------------------------------
-spec params_metric_data(NM::pos_integer(), MD::metric_datum()) -> [{string(),string()}].
params_metric_data(NM,MD) ->
    %% TODO - check case when both Value and statistics specified or both undefined
    Prefix = ?FMT("MetricData.member.~b", [NM]),
    lists:flatten(
      [
       [ {Prefix++".MetricName",   MD#metric_datum.metric_name} ],
       [ {Prefix++".Unit",         MD#metric_datum.unit}                       || MD#metric_datum.unit=/=undefined ],
       [ {Prefix++".Timestamp",    format_timestamp(MD#metric_datum.timestamp)}|| MD#metric_datum.timestamp=/=undefined ],
       [ {Prefix++".Value",        float_to_list(MD#metric_datum.value)}       || MD#metric_datum.value=/=undefined ],
       [ params_stat(Prefix, MD#metric_datum.statistic_values)                 || MD#metric_datum.statistic_values=/=undefined ],
       [ params_dimension(Prefix, ND, Dimension)
         || {ND,Dimension} <- lists:zip(lists:seq(1, length(MD#metric_datum.dimensions)), MD#metric_datum.dimensions)
       ]
      ]
     ).

%%------------------------------------------------------------------------------
%% @doc format datetime as Amazon timestamp
%% @end
%%------------------------------------------------------------------------------
format_timestamp({{Yr, Mo, Da}, {H, M, S}} = Timestamp)
  when is_integer(Yr), is_integer(Mo), is_integer(Da),
       is_integer(H),  is_integer(M),  is_integer(S)
       ->
    erlcloud_aws:format_timestamp(Timestamp);

format_timestamp(Timestamp) when is_list(Timestamp) ->
    Timestamp.

%%------------------------------------------------------------------------------
-spec params_dimension(Prefix::string(), ND::pos_integer(), Dimension::dimension()) -> [{string(),string()}].
params_dimension(Prefix, ND, Dimension) ->
    DimPrefix = ?FMT("~s.Dimensions.member.~b", [Prefix, ND]),
    [
     {DimPrefix++".Name",    Dimension#dimension.name},
     {DimPrefix++".Value",   Dimension#dimension.value}
    ].

%%------------------------------------------------------------------------------
%% @doc format statistic value records to URI params
%% @end
%%------------------------------------------------------------------------------
-spec params_stat(Prefix::string(), StatisticValues::statistic_set()) -> [{string(),string()}].
params_stat(Prefix, StatisticValues) ->
    [
     {Prefix++".StatisticValues.Maximum",    float_to_list(StatisticValues#statistic_set.maximum)},
     {Prefix++".StatisticValues.Minimum",    float_to_list(StatisticValues#statistic_set.minimum)},
     {Prefix++".StatisticValues.Sum",        float_to_list(StatisticValues#statistic_set.sum)},
     {Prefix++".StatisticValues.SampleCount",integer_to_list(StatisticValues#statistic_set.sample_count)}
    ].

%%------------------------------------------------------------------------------
%% @doc CloudWatch API - PutMetricData
%% [http://docs.amazonwebservices.com/AmazonCloudWatch/latest/APIReference/index.html?API_PutMetricData.html]
%% @end
%%------------------------------------------------------------------------------
-spec put_metric_data(
        Namespace   ::string(),
        MetricName  ::string(),
        Value       ::string(),
        Unit        ::unit(),
        Timestamp   ::datetime()|string()
                      ) -> term() | no_return().

put_metric_data(Namespace, MetricName, Value, Unit, Timestamp) ->
    put_metric_data(Namespace, MetricName, Value, Unit, Timestamp, default_config()).

-spec put_metric_data(
        Namespace   ::string(),
        MetricName  ::string(),
        Value       ::string(),
        Unit        ::unit(),
        Timestamp   ::datetime()|string(),
        Config      ::aws_config()
                      ) -> term() | no_return().

put_metric_data(Namespace, MetricName, Value, Unit, Timestamp, #aws_config{} = Config) ->
    Params =
        lists:flatten(
          [
           {"Namespace",                      Namespace},
           {"MetricData.member.1.MetricName", MetricName},
           {"MetricData.member.1.Value",      Value},
           [ {"MetricData.member.1.Unit",       Unit}                          || Unit=/=undefined, Unit=/="" ],
           [ {"MetricData.member.1.Timestamp",  format_timestamp(Timestamp)}   || Timestamp=/=undefined, Timestamp=/="" ]
          ]
         ),
    mon_simple_query(Config, "PutMetricData", Params).


%%------------------------------------------------------------------------------
%% @doc CloudWatch API - GetMetricStatistics - Easy average version
%% Gets average and max stats at 60 second intervals for 
%% the given metric on the given instance for the given interval
%% @end
%%------------------------------------------------------------------------------
-spec get_metric_statistics(
        MetricName  ::string(),
        StartTime   ::datetime() | string(),
        EndTime     ::datetime() | string(),
        InstanceId  ::string()
       ) -> term() | no_return().

get_metric_statistics(
  MetricName,
  StartTime,
  EndTime,
  InstanceId) ->
          get_metric_statistics(
              "AWS/EC2",
              MetricName,
              StartTime,
              EndTime,
              60,
              "",
              ["Average","Maximum"],
              [{"InstanceId", InstanceId}]).

%%------------------------------------------------------------------------------
%% @doc CloudWatch API - GetMetricStatistics
%% [http://docs.amazonwebservices.com/AmazonCloudWatch/latest/APIReference/index.html?API_GetMetricStatistics.html]
%%
%% USAGE:
%%
%%  erlcloud_mon:get_metric_statistics(
%%    "AWS/EC2",
%%    "CPUUtilization",
%%    {{2016, 06, 29}, {0, 0, 0}},
%%    "2016-06-29T00:30:00Z",
%%    60,
%%    "Percent",
%%    ["Average", "Maximum"],
%%    [{"InstanceType", "t2.micro"}]).
%%                                     
%% @end 
%%------------------------------------------------------------------------------
-spec get_metric_statistics(
        Namespace   ::string(),
        MetricName  ::string(),
        StartTime   ::datetime() | string(),
        EndTime     ::datetime() | string(),
        Period      ::pos_integer(),
        Unit        ::string(),
        Statistics  ::[string()],
        Dimensions  ::[{string(), string()}]
                      ) -> term() | no_return().

get_metric_statistics(
  Namespace,
  MetricName,
  StartTime,
  EndTime,
  Period,
  Unit,
  Statistics,
  Dimensions
 ) ->
    get_metric_statistics(
      Namespace,
      MetricName,
      StartTime,
      EndTime,
      Period,
      Unit,
      Statistics,
      Dimensions,
      default_config()
     ).

-spec get_metric_statistics(
        Namespace   ::string(),
        MetricName  ::string(),
        StartTime   ::datetime() | string(),
        EndTime     ::datetime() | string(),
        Period      ::pos_integer(),
        Unit        ::string(),
        Statistics  ::[string()],
        Dimensions  ::[{string(), string()}],
        Config      ::aws_config()
                      ) -> term() | no_return().

get_metric_statistics(
  Namespace,
  MetricName,
  StartTime,
  EndTime,
  Period,
  Unit,
  Statistics,
  Dimensions,
  #aws_config{} = Config
 ) ->
    Params =
          lists:flatten([
           {"Namespace",  Namespace},
           {"MetricName", MetricName},
           {"StartTime",  format_timestamp(StartTime)},
           {"EndTime",    format_timestamp(EndTime)},
           {"Period",     Period},
           [{"Unit",      Unit} || Unit=/=undefined, Unit=/=""]
          ])
          ++
          lists:flatten(
            [begin
                 Value = lists:nth(N, Statistics),
                 [{?FMT("Statistics.member.~b", [N]), Value}]
             end
             || N<-lists:seq(1, length(Statistics))]
           )
          ++
          lists:flatten(
            [begin
                 {Name, Value} = lists:nth(N, Dimensions),
                 [{?FMT("Dimensions.member.~b.Name", [N]), Name},
                  {?FMT("Dimensions.member.~b.Value", [N]), Value}]
             end
             || N<-lists:seq(1, length(Dimensions))]
           ),
    Doc = mon_query(Config, "GetMetricStatistics", Params),
    Members = xmerl_xpath:string("/GetMetricStatisticsResponse/GetMetricStatisticsResult/Datapoints/member", Doc),
    Label = get_text("Label", hd(xmerl_xpath:string("/GetMetricStatisticsResponse/GetMetricStatisticsResult", Doc))),
    [{"label", Label}, {"datapoints", [extract_metrics(Member, Statistics) || Member <- Members]}].

extract_metrics(Node, Statistics) ->
    [
     {timestamp, get_time("Timestamp", Node)},
     {unit,      get_text("Unit", Node)}
    ]
    ++
    [ {string:to_lower(Statistic), get_text(Statistic, Node)} || Statistic <- Statistics].

%%------------------------------------------------------------------------------
mon_simple_query(Config, Action, Params) ->
    mon_query(Config, Action, Params),
    ok.

mon_query(Config, Action, Params) ->
    mon_query(Config, Action, Params, ?API_VERSION).

mon_query(Config, Action, Params, ApiVersion) ->
    QParams = [{"Action", Action}, {"Version", ApiVersion}|Params],
    case erlcloud_aws:aws_request_xml4(get,
                                 Config#aws_config.mon_protocol,
                                 Config#aws_config.mon_host,
                                 Config#aws_config.mon_port,
                                 "/",
                                 QParams,
                                 "monitoring",
                                 Config)
    of
        {ok, Body} ->
            Body;
        {error, Reason} ->
            erlang:error({aws_error, Reason})
    end.

configure_host(Host, Port, Protocol) ->
    Config = default_config(),
    NewConfig = Config#aws_config{mon_host=Host,
                                  mon_port=Port,
                                  mon_protocol=Protocol},
    put(aws_config, NewConfig).

-spec new(string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey}.

-spec new(string(), string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                mon_host=Host}.

-spec configure(string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey)),
    ok.

-spec configure(string(), string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host)),
    ok.

default_config() -> erlcloud_aws:default_config().

%%------------------------------------------------------------------------------
%% tests
%% TODO : convert into e-unit tests
%%------------------------------------------------------------------------------
test() ->
    M1 = #metric_datum{
            metric_name = "zvi",
            dimensions  = [],
            statistic_values = undefined,
            timestamp        = undefined,
            unit = "Count",
            value = 10.0
           },
    M2 = #metric_datum{
            metric_name = "zvi",
            dimensions  = [],
            statistic_values = #statistic_set{minimum=18.0, maximum=21.4, sum=67.7, sample_count=15},
            timestamp        = undefined,
            unit = "Count",
            value = undefined
           },
    put_metric_data("my", [M1, M2]).
                                                %put_metric_data("my", [M2]).

test2() ->
    put_metric_data("my", "zvi", "13", "Count", "").
