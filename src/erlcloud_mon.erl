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

-export([
    list_metrics/4,
    put_metric_data/2,
    put_metric_data/5,
    get_metric_statistics/8,
    configure_host/3,
    test/0,
    test2/0
]).

-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include_lib("erlcloud/include/erlcloud_mon.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-import(erlcloud_xml, [get_text/2]).

-define(XMLNS_MON, "http://monitoring.amazonaws.com/doc/2010-08-01/").
-define(API_VERSION, "2010-08-01").

-define(FMT(Fmt,Args), lists:flatten(io_lib:format((Fmt),(Args)))).

%%------------------------------------------------------------------------------
%% @doc CloudWatch API - ListMetrics
%% @see [ http://docs.amazonwebservices.com/AmazonCloudWatch/latest/APIReference/index.html?API_ListMetrics.html ]
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
    ) -> term().

list_metrics(
        Namespace,
        MetricName,
        DimensionFilter,
        NextToken
    ) ->

    Config = default_config(),
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
%% @doc CloudWatch API - PutMetricData
%% @see [ http://docs.amazonwebservices.com/AmazonCloudWatch/latest/APIReference/index.html?API_PutMetricData.html ]
%%
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
%%
%% @end
%%------------------------------------------------------------------------------
-spec put_metric_data(
        Namespace   ::string(),
        MetricData  ::[metric_datum()]
    ) -> term().

put_metric_data(Namespace, MetricData) ->
    Config = default_config(),
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
        {Prefix++".StatisticValues.Minimum",    float_to_list(StatisticValues#statistic_set.maximum)},
        {Prefix++".StatisticValues.Sum",        float_to_list(StatisticValues#statistic_set.sum)},
        {Prefix++".StatisticValues.SampleCount",integer_to_list(StatisticValues#statistic_set.sample_count)}
    ].

%%------------------------------------------------------------------------------
%% @doc CloudWatch API - PutMetricData
%% @see [ http://docs.amazonwebservices.com/AmazonCloudWatch/latest/APIReference/index.html?API_PutMetricData.html ]
%% @end
%%------------------------------------------------------------------------------
-spec put_metric_data(
        Namespace   ::string(),
        MetricName  ::string(),
        Value       ::string(),
        Unit        ::unit(),
        Timestamp   ::datetime()|string()
    ) -> term().

put_metric_data(Namespace, MetricName, Value, Unit, Timestamp) ->
    Config = default_config(),
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
%% @doc CloudWatch API - GetMetricStatistics
%% @see [ http://docs.amazonwebservices.com/AmazonCloudWatch/latest/APIReference/index.html?API_GetMetricStatistics.html ]
%% @end
%%------------------------------------------------------------------------------
-spec get_metric_statistics(
        Namespace   ::string(),
        MetricName  ::string(),
        StartTime   ::string(),
        EndTime     ::string(),
        Period      ::pos_integer(),
        Unit        ::string(),
        Statistics  ::[string()],
        Dimensions  ::[string()]
    ) -> term().

get_metric_statistics(
        _Namespace,
        _MetricName,
        _StartTime,
        _EndTime,
        _Period,
        _Unit,
        _Statistics,
        _Dimensions
    ) ->
    todo.

%%------------------------------------------------------------------------------
mon_simple_query(Config, Action, Params) ->
    mon_query(Config, Action, Params),
    ok.

mon_query(Config, Action, Params) ->
    mon_query(Config, Action, Params, ?API_VERSION).

mon_query(Config, Action, Params, ApiVersion) ->
    QParams = [{"Action", Action}, {"Version", ApiVersion}|Params],
    erlcloud_aws:aws_request_xml(get,
                                 Config#aws_config.mon_prot,
                                 Config#aws_config.mon_host,
                                 Config#aws_config.mon_port,
                                 "/",
                                 QParams,
                                 Config#aws_config.access_key_id,
                                 Config#aws_config.secret_access_key).

default_config() -> erlcloud_aws:default_config().

configure_host(Host, Port, Protocol) ->
    Config = default_config(),
    NewConfig = Config#aws_config{mon_host=Host,
                                  mon_port=Port,
                                  mon_prot=Protocol},
    put(aws_config, NewConfig).



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

