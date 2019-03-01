-module(erlcloud_cloudwatch_logs).

-include("erlcloud_aws.hrl").


-define(API_VERSION, "2014-03-28").
-define(API_PREFIX, "Logs_20140328").
-define(SERVICE_NAME, "logs").
-define(DEFAULT_LIMIT, 50).
-define(DEFAULT_HEADERS, [
    {"content-type", "application/x-amz-json-1.1"},
    {"accept", "application/json"}
]).


-type access_key_id() :: string().
-type secret_access_key() :: string().
-type cw_host() :: string().


-type paging_token() :: string() | binary() | undefined.
-type seq_token() :: string() | binary() | undefined.
-type log_group_name() :: string() | binary() | undefined.
-type log_group_name_prefix() :: string() | binary() | undefined.
-type log_stream_name() :: string() | binary() | undefined.
-type log_stream_prefix() :: string() | binary() | undefined.
-type limit() :: pos_integer() | undefined.
-type filter_name_prefix() :: string() | binary() | undefined.
-type metric_name() :: string() | binary() | undefined.
-type metric_namespace() :: string() | binary() | undefined.
-type log_stream_order() :: log_stream_name | last_event_time | undefined.
-type events() :: [#{message => binary(), timestamp => pos_integer()}].


-type success_result_paged(ObjectType) :: {ok, [ObjectType], paging_token()}.
-type error_result() :: {error, Reason :: term()}.
-type result_paged(ObjectType) :: success_result_paged(ObjectType) | error_result().


-type log_group() :: jsx:json_term().
-type log_stream() :: jsx:json_term().
-type metric_filters() :: jsx:json_term().

-type tag():: {binary(), binary()}.
-type tags_return() :: jsx:json_term().


%% Library initialization
-export([
    configure/2,
    configure/3,
    new/2,
    new/3
]).


%% CloudWatch API
-export([
    describe_log_groups/0,
    describe_log_groups/1,
    describe_log_groups/2,
    describe_log_groups/3,
    describe_log_groups/4,

    describe_log_streams/1,
    describe_log_streams/2,
    describe_log_streams/3,
    describe_log_streams/5,
    describe_log_streams/6,
    describe_log_streams/7,

    describe_metric_filters/0,
    describe_metric_filters/1,
    describe_metric_filters/2,
    describe_metric_filters/3,
    describe_metric_filters/4,
    describe_metric_filters/6,
    describe_metric_filters/7,

    put_logs_events/4,
    put_logs_events/5,

    list_tags_log_group/1,
    list_tags_log_group/2,

    tag_log_group/2,
    tag_log_group/3

    ]).

%%==============================================================================
%% Library initialization
%%==============================================================================


-spec configure(access_key_id(), secret_access_key()) -> ok.
configure(AccessKeyID, SecretAccessKey) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey)),
    ok.


-spec configure(access_key_id(), secret_access_key(), cw_host()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host)),
    ok.


-spec new(access_key_id(), secret_access_key()) -> aws_config().
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{
        access_key_id = AccessKeyID,
        secret_access_key = SecretAccessKey
    }.


-spec new(access_key_id(), secret_access_key(), cw_host()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{
        access_key_id = AccessKeyID,
        secret_access_key = SecretAccessKey,
        cloudwatch_logs_host = Host
    }.


%%==============================================================================
%% CloudWatch API
%%==============================================================================


%%------------------------------------------------------------------------------
%% @doc
%%
%% DescribeLogGroups action
%% http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeLogGroups.html
%%
%% @end
%%------------------------------------------------------------------------------
-spec describe_log_groups() -> result_paged(log_group()).
describe_log_groups() ->
    describe_log_groups(default_config()).


-spec describe_log_groups(
    aws_config() | log_group_name_prefix()
) -> result_paged(log_group()).
describe_log_groups(#aws_config{} = Config) ->
    describe_log_groups(undefined, Config);
describe_log_groups(LogGroupNamePrefix) ->
    describe_log_groups(LogGroupNamePrefix, default_config()).


-spec describe_log_groups(
    log_group_name_prefix(),
    aws_config()
) -> result_paged(log_group()).
describe_log_groups(LogGroupNamePrefix, Config) ->
    describe_log_groups(LogGroupNamePrefix, ?DEFAULT_LIMIT, Config).


-spec describe_log_groups(
    log_group_name_prefix(),
    limit(),
    aws_config()
) -> result_paged(log_group()).
describe_log_groups(LogGroupNamePrefix, Limit, Config) ->
    describe_log_groups(LogGroupNamePrefix, Limit, undefined, Config).


-spec describe_log_groups(
    log_group_name_prefix(),
    limit(),
    paging_token(),
    aws_config()
) -> result_paged(log_group()).
describe_log_groups(LogGroupNamePrefix, Limit, Token, Config) ->
    case 
        cw_request(Config, "DescribeLogGroups",
            req_log_groups(LogGroupNamePrefix, Limit, Token)
        )
    of
        {ok, Json} ->
            LogGroups = proplists:get_value(<<"logGroups">>, Json, []),
            NextToken = proplists:get_value(<<"nextToken">>, Json, undefined), 
            {ok, LogGroups, NextToken};
        {error, _} = Error ->
            Error
    end.

req_log_groups(LogGroupNamePrefix, Limit, Token) ->
    [
        {<<"limit">>, Limit},
        {<<"logGroupNamePrefix">>, LogGroupNamePrefix},
        {<<"nextToken">>, Token}
    ].

%%------------------------------------------------------------------------------
%% @doc
%%
%% DescribeLogStreams action
%% https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeLogStreams.html
%%
%% @end
%%------------------------------------------------------------------------------
-spec describe_log_streams(log_group_name()) -> result_paged(log_stream()).

describe_log_streams(LogGroupName) ->
    describe_log_streams(LogGroupName, default_config()).

-spec describe_log_streams(
    log_group_name(),
    aws_config()
) -> result_paged(log_stream()).
describe_log_streams(LogGroupName, Config) ->
    describe_log_streams(LogGroupName, undefined, Config).

-spec describe_log_streams(
    log_group_name(),
    log_stream_prefix(),
    aws_config()
) -> result_paged(log_stream()).
describe_log_streams(LogGroupName, LogStreamPrefix, Config) ->
    describe_log_streams(LogGroupName, LogStreamPrefix, log_stream_name, false, Config).

-spec describe_log_streams(
    log_group_name(),
    log_stream_prefix(),
    log_stream_order(),
    boolean(),
    aws_config()
) -> result_paged(log_stream()).
describe_log_streams(LogGroupName, LogStreamPrefix, OrderBy, Desc, Config) ->
    describe_log_streams(LogGroupName, LogStreamPrefix, OrderBy, Desc, ?DEFAULT_LIMIT, Config).

-spec describe_log_streams(
    log_group_name(),
    log_stream_prefix(),
    log_stream_order(),
    boolean(),
    limit(),
    aws_config()
) -> result_paged(log_stream()).
describe_log_streams(LogGroupName, LogStreamPrefix, OrderBy, Desc, Limit, Config) ->
    describe_log_streams(LogGroupName, LogStreamPrefix, OrderBy, Desc, Limit, undefined, Config).

-spec describe_log_streams(
    log_group_name(),
    log_stream_prefix(),
    log_stream_order(),
    boolean(),
    limit(),
    paging_token(),
    aws_config()
) -> result_paged(log_stream()).

describe_log_streams(LogGroupName, LogStreamPrefix, OrderBy, Desc, Limit, Token, Config) ->
    case
        cw_request(Config, "DescribeLogStreams",
            req_log_streams(LogGroupName, LogStreamPrefix, OrderBy, Desc, Limit, Token)
        )
    of
        {ok, Json} ->
            LogStream = proplists:get_value(<<"logStreams">>, Json, []),
            NextToken = proplists:get_value(<<"nextToken">>, Json, undefined), 
            {ok, LogStream, NextToken};
        {error, _} = Error ->
            Error
    end.

req_log_streams(LogGroupName, LogStreamPrefix, OrderBy, Desc, Limit, Token) ->
    [
        {<<"descending">>, Desc},
        {<<"limit">>, Limit},
        {<<"logGroupName">>, LogGroupName},
        {<<"logStreamNamePrefix">>, LogStreamPrefix},
        {<<"nextToken">>, Token},
        {<<"orderBy">>, log_stream_order_by(OrderBy)}
    ].

log_stream_order_by(undefined) -> <<"LogStreamName">>;
log_stream_order_by(log_stream_name) -> <<"LogStreamName">>;
log_stream_order_by(last_event_time) -> <<"LastEventTime">>.

%%------------------------------------------------------------------------------
%% @doc
%%
%% PutLogEvents action
%% https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutLogEvents.html
%%
%% ===Example===
%% 
%%   Put log events requires a Upload Sequence Token, it is available via DescribeLogStreams
%%
%% `
%%   application:ensure_all_started(erlcloud). 
%%   {ok, Config} = erlcloud_aws:auto_config().
%%   {ok, Streams, _} = erlcloud_cloudwatch_logs:describe_log_streams(GroupName, StreamName, Config).
%%   {_, Seq} = lists:keyfind(<<"uploadSequenceToken">>, 1, hd(Streams)). 
%%
%%   Batch = [#{timestamp => 1526233086694, message => <<"Example Message">>}].
%%   erlcloud_cloudwatch_logs:put_logs_events(GroupName, StreamName, Seq, Batch, Config).
%% `
%%
%% @end
%%------------------------------------------------------------------------------

-spec put_logs_events(
    log_group_name(),
    log_stream_name(),
    seq_token(),
    events()
) -> datum:either( seq_token() ).

put_logs_events(LogGroup, LogStream, SeqToken, Events) ->
    put_logs_events(LogGroup, LogStream, SeqToken, Events, default_config()).


-spec put_logs_events(
    log_group_name(),
    log_stream_name(),
    seq_token(),
    events(),
    aws_config()
) -> datum:either( seq_token() ).

put_logs_events(LogGroup, LogStream, SeqToken, Events, Config) ->
    case
        cw_request(Config, "PutLogEvents",
            req_logs_events(LogGroup, LogStream, SeqToken, Events)
        )
    of
        {ok, Json} ->
            Seq = proplists:get_value(<<"nextSequenceToken">>, Json, []),
            {ok, Seq};
        {error, _} = Error ->
            Error
    end.
  
req_logs_events(LogGroup, LogStream, SeqToken, Events) ->
    [
        {<<"logEvents">>, log_events(Events)},
        {<<"logGroupName">>, LogGroup},
        {<<"logStreamName">>, LogStream},
        {<<"sequenceToken">>, SeqToken}
    ].

log_events(Events) ->
    [maps:with([message, timestamp], X) ||
        #{message := _, timestamp := _} = X <- Events].

%%------------------------------------------------------------------------------
%% @doc
%%
%% DescribeMetricFilters action
%% https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeMetricFilters.html
%%
%% @end
%%------------------------------------------------------------------------------
-spec describe_metric_filters() -> result_paged(metric_filters()).
describe_metric_filters() ->
    describe_metric_filters(default_config()).


-spec describe_metric_filters(
    aws_config() | log_group_name()
) -> result_paged(metric_filters()).
describe_metric_filters(#aws_config{} = Config) ->
    describe_metric_filters(undefined, Config);
describe_metric_filters(LogGroupName) ->
    describe_metric_filters(LogGroupName, default_config()).


-spec describe_metric_filters(
    log_group_name(),
    aws_config()
) -> result_paged(metric_filters()).
describe_metric_filters(LogGroupName, Config) ->
    describe_metric_filters(LogGroupName, ?DEFAULT_LIMIT, Config).


-spec describe_metric_filters(
    log_group_name(),
    limit(),
    aws_config()
) -> result_paged(metric_filters()).
describe_metric_filters(LogGroupName, Limit, Config) ->
    describe_metric_filters(LogGroupName, Limit, undefined, Config).


-spec describe_metric_filters(
    log_group_name(),
    limit(),
    filter_name_prefix(),
    aws_config()
) -> result_paged(metric_filters()).
describe_metric_filters(LogGroupName, Limit, FilterNamePrefix, Config) ->
    describe_metric_filters(LogGroupName, Limit, FilterNamePrefix, undefined,
                            undefined, Config).


-spec describe_metric_filters(
    log_group_name(),
    limit(),
    filter_name_prefix(),
    metric_name(),
    metric_namespace(),
    aws_config()
) -> result_paged(metric_filters()).
describe_metric_filters(LogGroupName, Limit, FilterNamePrefix, MetricName,
                        MetricNamespace, Config) ->
    describe_metric_filters(LogGroupName, Limit, FilterNamePrefix, MetricName,
                            MetricNamespace, undefined, Config).


-spec describe_metric_filters(
    log_group_name(),
    limit(),
    filter_name_prefix(),
    metric_name(),
    metric_namespace(),
    paging_token(),
    aws_config()
) -> result_paged(metric_filters()).
describe_metric_filters(LogGroupName, Limit, FilterNamePrefix, MetricName,
                        MetricNamespace, PrevToken, Config) ->
    case cw_request(Config, "DescribeMetricFilters", [
        {<<"logGroupName">>, LogGroupName},
        {<<"limit">>, Limit},
        {<<"filterNamePrefix">>, FilterNamePrefix},
        {<<"metricName">>, MetricName},
        {<<"metricNamespace">>, MetricNamespace},
        {<<"nextToken">>, PrevToken}
    ]) of
        {ok, Data} ->
            MetricFilters = proplists:get_value(<<"metricFilters">>, Data, []),
            NextToken = proplists:get_value(<<"nextToken">>, Data, undefined),
            {ok, MetricFilters, NextToken};
        {error, Reason} ->
            {error, Reason}
    end.

%%------------------------------------------------------------------------------
%% @doc
%%
%% ListTagsLogGroup
%% https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_ListTagsLogGroup.html
%%
%% @end
%%------------------------------------------------------------------------------

-spec list_tags_log_group(
    log_group_name()
) -> tags_return().

list_tags_log_group(LogGroup) ->
    list_tags_log_group(LogGroup, default_config()).


-spec list_tags_log_group(
    log_group_name(),
    aws_config()
) -> tags_return().

list_tags_log_group(LogGroup, Config) ->
    case
        cw_request(Config, "ListTagsLogGroup", [{<<"logGroupName">>, LogGroup}])
    of
        {ok, Json} ->
            Tags = proplists:get_value(<<"tags">>, Json, []),
            {ok, Tags};
        {error, _} = Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%%
%% TagLogGroup action
%% https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_TagLogGroup.html
%%
%% @end
%%------------------------------------------------------------------------------

-spec tag_log_group(
    log_group_name(),
    list(tag())
) -> ok.

tag_log_group(LogGroup, Tags) when is_list(Tags) ->
    tag_log_group(LogGroup, Tags, default_config()).


-spec tag_log_group(
    log_group_name(),
    list(tag()),
    aws_config()
) -> ok.

tag_log_group(LogGroup, Tags, Config) when is_list(Tags) ->
    Params = [{<<"logGroupName">>, LogGroup},
              {<<"tags">>, Tags}
    ],
    cw_request(Config, "TagLogGroup", Params).

%%==============================================================================
%% Internal functions
%%==============================================================================


default_config() ->
    erlcloud_aws:default_config().


cw_request(Config, Action, Params) ->
    maybe_cw_request(erlcloud_aws:update_config(Config), Action, Params).

maybe_cw_request({ok, Config}, Action, Params) ->
    Request = make_request_body(Action, Params),
    maybe_json(
        erlcloud_aws:aws_request_form_raw(
            post,
            Config#aws_config.cloudwatch_logs_scheme,
            Config#aws_config.cloudwatch_logs_host,
            Config#aws_config.cloudwatch_logs_port,
            "/",
            Request,
            make_request_headers(Config, Action, Request),
            Config
        )
    );

maybe_cw_request({error, _} = Error, _Action, _Params) ->
    Error.

maybe_json({ok, <<>>}) ->
    {ok, []};
maybe_json({ok, Response}) ->
    {ok, jsx:decode(Response)};
maybe_json({error, _} = Error) ->
    Error.

make_request_headers(Config, Action, Body) ->
    lists:append(make_signed_headers(Config, Action, Body), ?DEFAULT_HEADERS).


make_signed_headers(Config, Action, Body) ->
    #aws_config{cloudwatch_logs_host = Host} = Config,
    Target = lists:append([?API_PREFIX, ".", Action]),
    Headers = [{"host", Host}, {"x-amz-target", Target}],
    Region = erlcloud_aws:aws_region_from_host(Host),
    erlcloud_aws:sign_v4_headers(Config, Headers, Body, Region, ?SERVICE_NAME).


make_request_body(Action, RequestParams) ->
    DefaultParams = [{<<"Action">>, Action}, {<<"Version">>, ?API_VERSION}],
    Params = lists:append(DefaultParams, RequestParams),
    jsx:encode(prepare_request_params(Params)).


prepare_request_params(Params) ->
    lists:filtermap(fun prepare_request_param/1, Params).


prepare_request_param({_Key, undefined}) ->
    false;
prepare_request_param({Key, [H | _] = Value})
 when is_integer(H)  ->
    {true, {Key, list_to_binary(Value)}};
prepare_request_param({Key, Value}) ->
    {true, {Key, Value}}.
