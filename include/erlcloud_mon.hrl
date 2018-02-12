-ifndef(erlcloud_mon_hrl).
-define(erlcloud_mon_hrl, 0).

-include("erlcloud.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% File: erlcloud_mon.hrl
%% Date: 18-Sep-2011
%%
%% @doc AWS CloudWatch erlang binding (the CLI SDK uses "mon_" prefix)
%%
%% @author Zvi Avraham <zvi-AT-nivertech-DOT-com>
%% @copyright 2011 Zvi Avraham
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%------------------------------------------------------------------------------
%% @doc date time
%% @end
%%------------------------------------------------------------------------------
                                                %-type datetime() :: string().

%%------------------------------------------------------------------------------
%% @doc The unit of the metric.
%% Valid Values: Seconds | Microseconds | Milliseconds
%%  | Bytes | Kilobytes | Megabytes | Gigabytes | Terabytes | Bits | Kilobits | Megabits | Gigabits | Terabits
%%  | Percent | Count | Bytes/Second | Kilobytes/Second | Megabytes/Second | Gigabytes/Second | Terabytes/Second
%%  | Bits/Second | Kilobits/Second | Megabits/Second | Gigabits/Second | Terabits/Second | Count/Second | None
%% @end
%%------------------------------------------------------------------------------
-type unit()     :: string().

%%------------------------------------------------------------------------------
%% @doc The statistic for the metric, other than percentiles.
%% Valid Values: SampleCount | Average | Sum | Minimum | Maximum
%% @end
%%------------------------------------------------------------------------------
-type statistic() :: string().

%%------------------------------------------------------------------------------
%% @doc Dimension
%% The Dimension data type further expands on the identity of a metric using a Name, Value pair.
%% For examples that use one or more dimensions, see PutMetricData.
%% @see[ http://docs.amazonwebservices.com/AmazonCloudWatch/latest/APIReference/index.html?API_Dimension.html ]
%% @end
%%------------------------------------------------------------------------------
-record(dimension, {
          name    ::string(), %% The name of the dimension. Length constraints: Minimum value of 1. Maximum value of 255.
          value   ::string()  %% The value representing the dimension measurement. Length constraints: Minimum value of 1. Maximum value of 255.
         }).
-type dimension() :: #dimension{}.

%%------------------------------------------------------------------------------
%% @doc StatisticSet
%% @see[ http://docs.amazonwebservices.com/AmazonCloudWatch/latest/APIReference/index.html?API_StatisticSet.html ]
%% @end
%%------------------------------------------------------------------------------
-record(statistic_set, {
          sample_count    ::non_neg_integer(), %% The number of samples used for the statistic set.
          maximum         ::float(), %% The maximum value of the sample set.
          minimum         ::float(), %% The minimum value of the sample set.
          sum             ::float()  %% The sum of values for the sample set.
         }).
-type statistic_set() :: #statistic_set{}.

%%------------------------------------------------------------------------------
%% @doc MetricDatum
%% @see[ http://docs.amazonwebservices.com/AmazonCloudWatch/latest/APIReference/index.html?API_MetricDatum.html ]
%% @end
%%------------------------------------------------------------------------------
-record(metric_datum, {
          metric_name       ::string(),                    %% The name of the metric.
          %% Length constraints: Minimum value of 1. Maximum value of 255.
          dimensions      ::[dimension()],                 %% A list of dimensions associated with the metric.
          %% Length constraints: Minimum of 0 item(s) in the list. Maximum of 10 item(s) in the list.
          statistic_values::undefined|statistic_set(),     %% A set of statistical values describing the metric.
          timestamp       ::undefined|datetime()|string(), %% The time stamp used for the metric. If not specified, the default value is set to the time the metric data was received.
          unit            ::unit(),                        %% The unit of the metric.
          value           ::undefined|float()              %% The value for the metric.
         }).
-type metric_datum() :: #metric_datum{}.

-endif.