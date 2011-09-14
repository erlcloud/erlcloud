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
-include_lib("xmerl/include/xmerl.hrl").

% TODO - define types

%-define(XMLNS_MON, "http://monitoring.us-east-1.amazonaws.com/doc/2006-03-01/").
-define(API_VERSION, "2010-08-01").

%%------------------------------------------------------------------------------
%% @doc CloudWatch API - ListMetrics
%% @end
%%------------------------------------------------------------------------------
-spec list_metrics() -> term().
list_metrics() ->
    Config = default_config(),
    mon_query(Config, "ListMetrics", []).

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

