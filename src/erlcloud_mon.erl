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
    get_metric_statistics/8
]).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").
-include("erlcloud_mon.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-import(erlcloud_xml, [get_text/1, get_text/2, get_text/3, get_bool/2, get_list/2, get_integer/2]).

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
            {"Namespace",                      Namespace}
            %,
            %{"MetricData.member.1.MetricName", MetricName},
            %{"MetricData.member.1.Value",      Value}
        ],
    mon_query(Config, "PutMetricData", Params).

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
        Namespace,
        MetricName,
        StartTime,
        EndTime,
        Period,
        Unit,
        Statistics,
        Dimensions
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
                                Config#aws_config.mon_host,
                                "/",
                                QParams,
                                Config#aws_config.access_key_id,
                                Config#aws_config.secret_access_key).

default_config() -> erlcloud_aws:default_config().

