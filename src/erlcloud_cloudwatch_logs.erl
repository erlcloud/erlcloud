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
-type log_group_name_prefix() :: string() | binary() | undefined.
-type limit() :: pos_integer() | undefined.


-type success_result_paged(ObjectType) :: {ok, [ObjectType], paging_token()}.
-type error_result() :: {error, Reason :: term()}.
-type result_paged(ObjectType) :: success_result_paged(ObjectType) | error_result().


-type log_group() :: jsx:json_term().

-type log_group_name() :: binary().
-type log_stream_name() :: binary().
-type sequence_token() :: binary().
-type log_event() :: #{message => binary(), timestamp => integer()}.

-type log_events() :: [log_event()].
-type log_put_result() :: #{nextSequenceToken => binary(),
                            rejectedLogEventsInfo => map()}.


%% Library initialization
-export([
    configure/2,
    configure/3,
    new/2,
    new/3,
    set_cloudwatch_host/2
]).


%% CloudWatch API
-export([
    describe_log_groups/0,
    describe_log_groups/1,
    describe_log_groups/2,
    describe_log_groups/3,
    describe_log_groups/4,
    create_log_stream/3,
    put_log_events/4,
    put_log_events/5
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

-spec set_cloudwatch_host(cw_host(), aws_config()) -> aws_config().
set_cloudwatch_host(Host, AwsConfig) ->
    AwsConfig#aws_config{
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
describe_log_groups(LogGroupNamePrefix, Limit, PrevToken, Config) ->
    case cw_request(Config, "DescribeLogGroups", [
        {<<"limit">>, Limit},
        {<<"logGroupNamePrefix">>, LogGroupNamePrefix},
        {<<"nextToken">>, PrevToken}
    ]) of
        {ok, Data} ->
            LogGroups = proplists:get_value(<<"logGroups">>, Data, []),
            NextToken = proplists:get_value(<<"nextToken">>, Data, undefined),
            {ok, LogGroups, NextToken};
        {error, Reason} ->
            {error, Reason}
    end.

-spec create_log_stream(
    log_group_name(),
    log_stream_name(),
    aws_config()
) -> ok|error_result().
create_log_stream(LogGroupName, LogStreamName, Config) ->
    cw_request(Config, "CreateLogStream", [
      {<<"logGroupName">>, LogGroupName},
      {<<"logStreamName">>, LogStreamName}
    ]).


-spec put_log_events(
    log_group_name(),
    log_stream_name(),
    log_events(),
    aws_config()
) -> ok|error_result().
put_log_events(LogGroupName, LogStreamName, 
               LogEvents, Config) when is_binary(LogGroupName),
                                       is_binary(LogStreamName), 
                                       is_list(LogEvents) ->
  Events = make_log_events(LogEvents),
  cw_prepared_request(Config, "PutLogEvents", [
    {<<"logGroupName">>, LogGroupName},
    {<<"logStreamName">>, LogStreamName},
    {<<"logEvents">>, Events}
  ],  _ReturnMap = true).

-spec put_log_events(
    log_group_name(),
    log_stream_name(),
    sequence_token(),
    log_events(),
    aws_config()
) -> {ok, log_put_result()}| error_result().
put_log_events(LogGroupName, LogStreamName, SequenceToken, 
               LogEvents, Config) when is_binary(LogGroupName),
                                       is_binary(LogStreamName), 
                                       is_list(LogEvents),
                                       is_binary(SequenceToken) ->
  Events = make_log_events(LogEvents),
  cw_prepared_request(Config, "PutLogEvents", [
    {<<"logGroupName">>, LogGroupName},
    {<<"logStreamName">>, LogStreamName},
    {<<"sequenceToken">>, SequenceToken},
    {<<"logEvents">>, Events}
  ],  _ReturnMap = true ).

make_log_events([]) ->
  [];
make_log_events([#{message := Message, 
                   timestamp := Timestamp}|R]) when is_binary(Message),
                                                    is_integer(Timestamp) ->
  [[{<<"message">>, Message}, {<<"timestamp">>, Timestamp}]|make_log_events(R)].

%%==============================================================================
%% Internal functions
%%==============================================================================


default_config() ->
    erlcloud_aws:default_config().

b(B) when is_binary(B) ->
  B;
b(L) when is_list(L) ->
  list_to_binary(L).

cw_request(Config, Action, Params) ->
    %% Filter out params without values
    %% Perform list to binary conversion
    PreparedParams = prepare_request_params(Params),
    cw_prepared_request(Config, b(Action), PreparedParams, _ReturnMap = false).
    
cw_prepared_request(Config, Action, BodyParams, ReturnMap) when is_list(BodyParams) ->    
    JSXReturnOptions = case ReturnMap of
        true -> [{labels, atom},return_maps];
        false -> []
    end,
    case erlcloud_aws:update_config(Config) of
        {ok, NewConfig} ->
            JSXBody = [{<<"Action">>, b(Action)}, 
                       {<<"Version">>, b(?API_VERSION)} | BodyParams],
            RequestBody = jsx:encode(JSXBody),
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
                {ok, <<>>} ->
                    ok;
                {ok, ResponseBody} ->
                    {ok, jsx:decode(ResponseBody, JSXReturnOptions)};
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

prepare_request_params(Params) ->
    lists:filtermap(fun prepare_request_param/1, Params).


prepare_request_param({_Key, undefined}) ->
    false;
prepare_request_param({Key, Value}) when is_list(Value) ->
    {true, {Key, list_to_binary(Value)}};
prepare_request_param({Key, Value}) ->
    {true, {Key, Value}}.
