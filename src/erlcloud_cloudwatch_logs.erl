-module(erlcloud_cloudwatch_logs).

-compile({parse_transform, category}).
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
-type log_group_name() :: string() | binary() | undefined.
-type log_group_name_prefix() :: string() | binary() | undefined.
-type log_stream_prefix() :: string() | binary() | undefined.
-type limit() :: pos_integer() | undefined.
-type log_stream_order() :: log_stream_name | last_event_time | undefined.


-type success_result_paged(ObjectType) :: {ok, [ObjectType], paging_token()}.
-type error_result() :: {error, Reason :: term()}.
-type result_paged(ObjectType) :: success_result_paged(ObjectType) | error_result().


-type log_group() :: jsx:json_term().
-type log_stream() :: jsx:json_term().


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
    describe_log_streams/7
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
    [either ||
        req_log_groups(LogGroupNamePrefix, Limit, Token),
        Json <- cw_request(Config, "DescribeLogGroups", _),
        cats:unit(
            lens:get(lens:pair(<<"logGroups">>, []), Json),
            lens:get(lens:pair(<<"nextToken">>), Json)
        )
    ].

req_log_groups(LogGroupNamePrefix, Limit, Token) ->
    {ok, [
        {<<"limit">>, Limit},
        {<<"logGroupNamePrefix">>, LogGroupNamePrefix},
        {<<"nextToken">>, Token}
    ]}.

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
    [either ||
        req_log_streams(LogGroupName, LogStreamPrefix, OrderBy, Desc, Limit, Token),
        Json <- cw_request(Config, "DescribeLogStreams", _),
        cats:unit(
            lens:get(lens:pair(<<"logStreams">>, []), Json),
            lens:get(lens:pair(<<"nextToken">>), Json)
        )
    ].

req_log_streams(LogGroupName, LogStreamPrefix, OrderBy, Desc, Limit, Token) ->
    {ok, [
        {<<"descending">>, Desc},
        {<<"limit">>, Limit},
        {<<"logGroupName">>, LogGroupName},
        {<<"logStreamNamePrefix">>, LogStreamPrefix},
        {<<"nextToken">>, Token},
        {<<"orderBy">>, log_stream_order_by(OrderBy)}
    ]}.

log_stream_order_by(undefined) -> <<"LogStreamName">>;
log_stream_order_by(log_stream_name) -> <<"LogStreamName">>;
log_stream_order_by(last_event_time) -> <<"LastEventTime">>.


%%==============================================================================
%% Internal functions
%%==============================================================================


default_config() ->
    erlcloud_aws:default_config().


cw_request(Config, Action, Params) ->
    case erlcloud_aws:update_config(Config) of
        {ok, NewConfig} ->
            RequestBody = make_request_body(
                Action, Params
            ),
            RequestHeaders = make_request_headers(
                NewConfig, Action, RequestBody
            ),
            case erlcloud_aws:aws_request_form_raw(
                post,
                NewConfig#aws_config.cloudwatch_logs_scheme,
                NewConfig#aws_config.cloudwatch_logs_host,
                NewConfig#aws_config.cloudwatch_logs_port,
                "/",
                RequestBody,
                RequestHeaders,
                NewConfig
            ) of
                {ok, ResponseBody} ->
                    {ok, jsx:decode(ResponseBody)};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


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
prepare_request_param({Key, Value}) when is_list(Value) ->
    {true, {Key, list_to_binary(Value)}};
prepare_request_param({Key, Value}) ->
    {true, {Key, Value}}.
