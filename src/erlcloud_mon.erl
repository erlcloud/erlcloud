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
    list_metrics/0,
    put_metric_data/3
]).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").
-include("erlcloud_mon.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-import(erlcloud_xml, [get_text/1, get_text/2, get_text/3, get_bool/2, get_list/2, get_integer/2]).

-define(XMLNS_MON, "http://monitoring.amazonaws.com/doc/2010-08-01/").
-define(API_VERSION, "2010-08-01").

%%------------------------------------------------------------------------------
%% @doc CloudWatch API - ListMetrics
%% @end
%%------------------------------------------------------------------------------
-spec list_metrics() -> term().
list_metrics() ->
    Config = default_config(),
    Doc = mon_query(Config, "ListMetrics", []),
    Members = xmerl_xpath:string("/ListMetricsResponse/ListMetricsResult/Metrics/member", Doc),
    [extract_member(Member) || Member <- Members].
    %Doc.

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
%% @end
%%------------------------------------------------------------------------------
-spec put_metric_data(Namespace::string(), MetricName::string(), Value::string()) -> term().
put_metric_data(Namespace, MetricName, Value) ->
    Config = default_config(),
    Params = 
        [{"Namespace",                      Namespace}, 
         {"MetricData.member.1.MetricName", MetricName},
         {"MetricData.member.1.Value",      Value}],
    mon_query(Config, "PutMetricData", Params).

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

