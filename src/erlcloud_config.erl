-module(erlcloud_config).

-include("erlcloud_aws.hrl").
-include("erlcloud.hrl").

%% API
-export([new/2, new/3, new/4, new/5,
         configure/3, configure/4, configure/5, configure/6]).

-export([
    describe_compliance_by_config_rule/2,
    describe_compliance_by_resource/2,
    describe_config_rule_evaluation_status/2,
    describe_config_rules/2,
    describe_configuration_recorders/2,
    describe_configuration_recorder_status/2,
    describe_delivery_channels/2,
    describe_delivery_channel_status/2,
    get_compliance_details_by_config_rule/3,
    get_compliance_details_by_resource/4,
    get_compliance_summary_by_config_rule/1,
    get_compliance_summary_by_resource_type/2,
    get_discovered_resource_counts/2,
    get_resource_config_history/4,
    list_discovered_resources/3

]).

-define(LIMIT_MAX, 100).
-define(CONFIG_RULE_NAMES, <<"ConfigRuleNames">>).
-define(CONFIG_RULE_NAME, <<"ConfigRuleName">>).
-define(COMPLIANCE_TYPES, <<"ComplianceTypes">>).
-define(LIMIT, <<"Limit">>).
-define(RESOURCE_NAME, <<"ResourceName">>).
-define(RESOURCE_ID, <<"ResourceId">>).
-define(RESOURCE_IDS, <<"ResourceIds">>).
-define(RESOURCE_TYPE, <<"ResourceType">>).
-define(RESOURCE_TYPES, <<"ResourceTypes">>).
-define(CONFIGURATION_RECORDER_NAMES, <<"ConfigurationRecorderNames">>).
-define(DELIVERY_CHANNEL_NAMES, <<"DeliveryChannelNames">>).
-define(CHRONOLOGICAL_ORDER, <<"chronologicalOrder">>).
-define(EARLIER_TIME, <<"earlierTime">>).
-define(LATER_TIME, <<"laterTime">>).
-define(INCLUDE_DELETED_RESOURCES, <<"includeDeletedResources">>).


%%------------------------------------------------------------------------------
%% Library initialization.
%%------------------------------------------------------------------------------

-spec new(string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id     = AccessKeyID,
                secret_access_key = SecretAccessKey,
                retry             = fun erlcloud_retry:default_retry/1}.

-spec new(string(), string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id     = AccessKeyID,
                secret_access_key = SecretAccessKey,
                config_host       = Host,
                retry             = fun erlcloud_retry:default_retry/1}.

-spec new(string(), string(), string(), non_neg_integer()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host, Port) ->
    #aws_config{access_key_id     = AccessKeyID,
                secret_access_key = SecretAccessKey,
                config_host       = Host,
                config_port       = Port,
                retry             = fun erlcloud_retry:default_retry/1}.

-spec new(string(), string(), string(), non_neg_integer(), string()) ->
    aws_config().
new(AccessKeyID, SecretAccessKey, Host, Port, Scheme) ->
    #aws_config{access_key_id     = AccessKeyID,
                secret_access_key = SecretAccessKey,
                config_host       = Host,
                config_port       = Port,
                config_scheme     = Scheme,
                retry             = fun erlcloud_retry:default_retry/1}.

%%------------------------------------------------------------------------------
%% AWS Config API Functions
%%------------------------------------------------------------------------------

-spec configure(string(), string(), function()) -> ok.
configure(AccessKeyID, SecretAccessKey, New)
    when is_list(AccessKeyID), is_list(SecretAccessKey), is_function(New, 2) ->
    put(aws_config, New(AccessKeyID, SecretAccessKey)),
    ok.

-spec configure(string(), string(), string(), function()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host, New)
    when is_list(AccessKeyID), is_list(SecretAccessKey), is_list(Host), is_function(New, 3) ->
    put(aws_config, New(AccessKeyID, SecretAccessKey, Host)),
    ok.

-spec configure(string(), string(), string(), non_neg_integer(), function()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host, Port, New)
    when is_list(AccessKeyID), is_list(SecretAccessKey),
    is_list(Host), is_function(New, 4) ->
    put(aws_config, New(AccessKeyID, SecretAccessKey, Host, Port)),
    ok.

-spec configure(string(), string(), string(), non_neg_integer(), string(), function()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host, Port, Scheme, New)
    when is_list(AccessKeyID), is_list(SecretAccessKey), is_list(Host),
    is_list(Scheme), is_function(New, 5) ->
    put(aws_config, New(AccessKeyID, SecretAccessKey, Host, Port, Scheme)),
    ok.
%%------------------------------------------------------------------------------
%% @doc
%% DescribeComplianceByConfigRule
%%
%% https://docs.aws.amazon.com/config/latest/APIReference/API_DescribeComplianceByConfigRule.html
%%------------------------------------------------------------------------------
-spec describe_compliance_by_config_rule(Opts :: map(),
    Config :: aws_config()) ->
    result_paged(map()).
describe_compliance_by_config_rule(Opts, Config) when is_map(Opts) orelse Opts =:= #{} ->
    Request = #{?COMPLIANCE_TYPES => maps:get(?COMPLIANCE_TYPES, Opts, []),
        ?CONFIG_RULE_NAMES        => maps:get(?CONFIG_RULE_NAMES, Opts, []),
        ?NEXT_TOKEN_LABEL         => maps:get(?NEXT_TOKEN_LABEL, Opts, null)
    },
    request(Config, "DescribeComplianceByConfigRule", Request).


%%------------------------------------------------------------------------------
%% @doc
%% DescribeComplianceByResource
%%
%% https://docs.aws.amazon.com/config/latest/APIReference/API_DescribeComplianceByResource.html
%%------------------------------------------------------------------------------
-spec describe_compliance_by_resource(Opts :: map(),
    Config :: aws_config()) ->
    result_paged(map()).
describe_compliance_by_resource(Opts, Config) when is_map(Opts) orelse Opts =:= #{} ->
    Request = #{?COMPLIANCE_TYPES => maps:get(?COMPLIANCE_TYPES, Opts, []),
        ?LIMIT                    => maps:get(?LIMIT, Opts, ?LIMIT_MAX),
        ?NEXT_TOKEN_LABEL         => maps:get(?NEXT_TOKEN_LABEL, Opts, null),
        ?RESOURCE_ID              => maps:get(?RESOURCE_ID, Opts, null),
        ?RESOURCE_TYPE            => maps:get(?RESOURCE_TYPE, Opts, null)
    },
    request(Config, "DescribeComplianceByResource", Request).

%%------------------------------------------------------------------------------
%% @doc
%% DescribeConfigRuleEvaluationStatus
%%
%% https://docs.aws.amazon.com/config/latest/APIReference/API_DescribeConfigRuleEvaluationStatus.html
%%------------------------------------------------------------------------------
-spec describe_config_rule_evaluation_status(Opts :: map(),
    Config :: aws_config()) ->
    result_paged(map()).
describe_config_rule_evaluation_status(Opts, Config) when is_map(Opts) orelse Opts =:= #{} ->
    Request = #{?CONFIG_RULE_NAMES => maps:get(?CONFIG_RULE_NAMES, Opts, []),
        ?LIMIT                    => maps:get(?LIMIT, Opts, ?LIMIT_MAX),
        ?NEXT_TOKEN_LABEL         => maps:get(?NEXT_TOKEN_LABEL, Opts, null)
    },
    request(Config, "DescribeConfigRuleEvaluationStatus", Request).

%%------------------------------------------------------------------------------
%% @doc
%% DescribeConfigRules
%%
%% https://docs.aws.amazon.com/config/latest/APIReference/API_DescribeConfigRules.html
%%------------------------------------------------------------------------------
-spec describe_config_rules(Opts :: map(),
    Config :: aws_config()) ->
    result_paged(map()).
describe_config_rules(Opts, Config)
    when is_map(Opts) orelse Opts =:= #{} ->
    Request = #{?CONFIG_RULE_NAMES    => maps:get(?CONFIG_RULE_NAMES, Opts, []),
        ?NEXT_TOKEN_LABEL             => maps:get(?NEXT_TOKEN_LABEL, Opts, null)
    },
    request(Config, "DescribeConfigRules", Request).

%%------------------------------------------------------------------------------
%% @doc
%% DescribeConfigurationRecorders
%%
%% https://docs.aws.amazon.com/config/latest/APIReference/API_DescribeConfigurationRecorders.html
%%------------------------------------------------------------------------------
-spec describe_configuration_recorders(Opts :: map(),
    Config :: aws_config()) ->
    result_paged(map()).
describe_configuration_recorders(Opts, Config) when is_map(Opts) orelse Opts =:= #{} ->
    Request = #{?CONFIGURATION_RECORDER_NAMES => maps:get(?CONFIGURATION_RECORDER_NAMES, Opts, [])},
    request(Config, "DescribeConfigurationRecorders", Request).

%%------------------------------------------------------------------------------
%% @doc
%% DescribeConfigurationRecorderStatus
%%
%% https://docs.aws.amazon.com/config/latest/APIReference/API_DescribeConfigurationRecorderStatus.html
%%------------------------------------------------------------------------------
-spec describe_configuration_recorder_status(Opts :: map(),
    Config :: aws_config()) ->
    result_paged(map()).
describe_configuration_recorder_status(Opts, Config) when is_map(Opts) orelse Opts =:= #{} ->
    Request = #{?CONFIGURATION_RECORDER_NAMES => maps:get(?CONFIGURATION_RECORDER_NAMES, Opts, [])},
    request(Config, "DescribeConfigurationRecorderStatus", Request).

%%------------------------------------------------------------------------------
%% @doc
%% DescribeDeliveryChannels
%%
%% https://docs.aws.amazon.com/config/latest/APIReference/API_DescribeDeliveryChannels.html
%%------------------------------------------------------------------------------
-spec describe_delivery_channels(Opts :: map(),
    Config :: aws_config()) ->
    result_paged(map()).
describe_delivery_channels(Opts, Config) when is_map(Opts) orelse Opts =:= #{} ->
    Request = #{?DELIVERY_CHANNEL_NAMES => maps:get(?DELIVERY_CHANNEL_NAMES, Opts, [])},
    request(Config, "DescribeDeliveryChannels", Request).

%%------------------------------------------------------------------------------
%% @doc
%% DescribeDeliveryChannelStatus
%%
%% https://docs.aws.amazon.com/config/latest/APIReference/API_DescribeDeliveryChannelStatus.html
%%------------------------------------------------------------------------------
-spec describe_delivery_channel_status(Opts :: map(),
    Config :: aws_config()) ->
    result_paged(map()).
describe_delivery_channel_status(Opts, Config) when is_map(Opts) orelse Opts =:= #{} ->
    Request = #{?DELIVERY_CHANNEL_NAMES => maps:get(?DELIVERY_CHANNEL_NAMES, Opts, [])},
    request(Config, "DescribeDeliveryChannelStatus", Request).

%%------------------------------------------------------------------------------
%% @doc
%% GetComplianceDetailsByConfigRule
%%
%% https://docs.aws.amazon.com/config/latest/APIReference/API_GetComplianceDetailsByConfigRule.html
%%------------------------------------------------------------------------------
-spec get_compliance_details_by_config_rule(ConfigRuleName :: binary(),
    Opts :: map(),
    Config :: aws_config()) ->
    result_paged(map()).
get_compliance_details_by_config_rule(ConfigRuleName, Opts, Config) when is_map(Opts) orelse Opts =:= #{} ->
    Request = #{?COMPLIANCE_TYPES => maps:get(?COMPLIANCE_TYPES, Opts, []),
        ?CONFIG_RULE_NAME         => ConfigRuleName,
        ?LIMIT                    => maps:get(?LIMIT, Opts, ?LIMIT_MAX),
        ?NEXT_TOKEN_LABEL         => maps:get(?NEXT_TOKEN_LABEL, Opts, null)},
    request(Config, "GetComplianceDetailsByConfigRule", Request).

%%------------------------------------------------------------------------------
%% @doc
%% GetComplianceDetailsByResource
%%
%% https://docs.aws.amazon.com/config/latest/APIReference/API_GetComplianceDetailsByResource.html
%%------------------------------------------------------------------------------
-spec get_compliance_details_by_resource(ResourceId :: binary(),
    ResourceType :: binary(),
    Opts :: map(),
    Config :: aws_config()) ->
    result_paged(map()).
get_compliance_details_by_resource(ResourceId, ResourceType, Opts, Config) when is_map(Opts) orelse Opts =:= #{} ->
    Request = #{?COMPLIANCE_TYPES => maps:get(?COMPLIANCE_TYPES, Opts, []),
        ?NEXT_TOKEN_LABEL         => maps:get(?NEXT_TOKEN_LABEL, Opts, null),
        ?RESOURCE_ID              => ResourceId,
        ?RESOURCE_TYPE            => ResourceType},
    request(Config, "GetComplianceDetailsByResource", Request).

%%------------------------------------------------------------------------------
%% @doc
%% GetComplianceSummaryByConfigRule
%%
%% https://docs.aws.amazon.com/config/latest/APIReference/API_GetComplianceSummaryByConfigRule.html
%%------------------------------------------------------------------------------
-spec get_compliance_summary_by_config_rule(Config :: aws_config()) ->
    result_paged(map()).
get_compliance_summary_by_config_rule(Config) ->
    request(Config, "GetComplianceSummaryByConfigRule", #{}).

%%------------------------------------------------------------------------------
%% @doc
%% GetComplianceSummaryByResourceType
%%
%% https://docs.aws.amazon.com/config/latest/APIReference/API_GetComplianceSummaryByResourceType.html
%%------------------------------------------------------------------------------
-spec get_compliance_summary_by_resource_type(Opts :: map(),
    Config :: aws_config()) ->
    result_paged(map()).
get_compliance_summary_by_resource_type(Opts, Config) when is_map(Opts) orelse Opts =:= #{} ->
    Request = #{?RESOURCE_TYPES => maps:get(?RESOURCE_TYPES, Opts, [])},
    request(Config, "GetComplianceSummaryByResourceType", Request).

%%------------------------------------------------------------------------------
%% @doc
%% GetDiscoveredResourceCounts
%%
%% https://docs.aws.amazon.com/config/latest/APIReference/API_GetDiscoveredResourceCounts.html
%%------------------------------------------------------------------------------
-spec get_discovered_resource_counts(Opts :: map(),
    Config :: aws_config()) ->
    result_paged(map()).
get_discovered_resource_counts(Opts, Config) when is_map(Opts) orelse Opts =:= #{} ->
    Request = #{?LIMIT     => maps:get(?LIMIT, Opts, ?LIMIT_MAX),
        ?NEXT_TOKEN_LABEL  => maps:get(?NEXT_TOKEN_LABEL, Opts, null),
        ?RESOURCE_TYPES    => maps:get(?RESOURCE_TYPES, Opts, null)},
    request(Config, "GetDiscoveredResourceCounts", Request).


%%------------------------------------------------------------------------------
%% @doc
%% GetResourceConfigHistory
%%
%% https://docs.aws.amazon.com/config/latest/APIReference/API_GetResourceConfigHistory.html
%%------------------------------------------------------------------------------
-spec get_resource_config_history(ResourceId :: binary(),
    ResourceType :: binary(),
    Opts :: map(),
    Config :: aws_config()) ->
    result_paged(map()).
get_resource_config_history(ResourceId, ResourceType, Opts, Config) when is_map(Opts) orelse Opts =:= #{} ->
    Request = #{?CHRONOLOGICAL_ORDER => maps:get(?CHRONOLOGICAL_ORDER, Opts, null),
        ?EARLIER_TIME                => maps:get(?EARLIER_TIME, Opts, null),
        ?LATER_TIME                  => maps:get(?LATER_TIME, Opts, null),
        ?LIMIT                       => maps:get(?LIMIT, Opts, ?LIMIT_MAX),
        ?NEXT_TOKEN_LABEL            => maps:get(?NEXT_TOKEN_LABEL, Opts, null),
        ?RESOURCE_ID                 => ResourceId,
        ?RESOURCE_TYPE               => ResourceType},
    request(Config, "GetResourceConfigHistory", Request).

%%------------------------------------------------------------------------------
%% @doc
%% ListDiscoveredResources
%%
%% https://docs.aws.amazon.com/config/latest/APIReference/API_ListDiscoveredResources.html
%%------------------------------------------------------------------------------
-spec list_discovered_resources(ResourceType :: binary(),
    Opts :: map(),
    Config :: aws_config()) ->
    result_paged(map()).
list_discovered_resources(ResourceType, Opts, Config) when is_map(Opts) orelse Opts =:= #{} ->
    Request = #{?INCLUDE_DELETED_RESOURCES => maps:get(?INCLUDE_DELETED_RESOURCES, Opts, true),
        ?LIMIT                             => maps:get(?LIMIT, Opts, ?LIMIT_MAX),
        ?NEXT_TOKEN_LABEL                  => maps:get(?NEXT_TOKEN_LABEL, Opts, null),
        ?RESOURCE_IDS                      => maps:get(?RESOURCE_IDS, Opts, []),
        ?RESOURCE_NAME                     => maps:get(?RESOURCE_NAME, Opts, null),
        ?RESOURCE_TYPE                     => ResourceType},
    request(Config, "ListDiscoveredResources", Request).

%%%------------------------------------------------------------------------------
%%% @doc
%%% Internal Functions
%%%------------------------------------------------------------------------------

request(Config0, OperationName, Request) ->
    case erlcloud_aws:update_config(Config0) of
        {ok, Config} ->
            Body       = jsx:encode(Request),
            Operation  = "StarlingDoveService." ++ OperationName,
            Headers    = get_headers(Config, Operation, Body),
            AwsRequest = #aws_request{service         = config,
                uri             = get_url(Config),
                method          = post,
                request_headers = Headers,
                request_body    = Body},
            request(Config, AwsRequest);
        {error, Reason} ->
            {error, Reason}
    end.

request(Config, Request) ->
    Result = erlcloud_retry:request(Config, Request, fun handle_result/1),
    case erlcloud_aws:request_to_return(Result) of
        {ok, {_, <<>>}}     -> {ok, #{}};
        {ok, {_, RespBody}} -> {ok, jsx:decode(RespBody, [return_maps])};
        {error, _} = Error  -> Error
    end.

handle_result(#aws_request{response_type = ok} = Request) ->
    Request;
handle_result(#aws_request{response_type    = error,
    error_type      = aws,
    response_status = Status} = Request)
    when Status >= 500 ->
    Request#aws_request{should_retry = true};
handle_result(#aws_request{response_type = error,
    error_type    = aws} = Request) ->
    Request#aws_request{should_retry = false}.

get_headers(#aws_config{config_host = Host} = Config, Operation, Body) ->
    Headers = [{"host",         Host},
        {"x-amz-target", Operation},
        {"content-type", "application/x-amz-json-1.1"}],
    Region = erlcloud_aws:aws_region_from_host(Host),
    erlcloud_aws:sign_v4_headers(Config, Headers, Body, Region, "config").

get_url(#aws_config{config_scheme = Scheme,
    config_host   = Host,
    config_port   = Port}) ->
    Scheme ++ Host ++ ":" ++ integer_to_list(Port).

