-module(erlcloud_config).

-include("erlcloud_aws.hrl").
-include("erlcloud.hrl").

%% API
-export([configure/2, configure/3, configure/4, configure/5,
         new/2, new/3, new/4, new/5]).

-export([
    describe_compliance_by_config_rule/0,
    describe_compliance_by_config_rule/1,
    describe_compliance_by_config_rule/2,

    describe_compliance_by_resource/0,
    describe_compliance_by_resource/1,
    describe_compliance_by_resource/2,

    describe_config_rule_evaluation_status/0,
    describe_config_rule_evaluation_status/1,
    describe_config_rule_evaluation_status/2,

    describe_config_rules/0,
    describe_config_rules/1,
    describe_config_rules/2,

    describe_configuration_recorders/0,
    describe_configuration_recorders/1,
    describe_configuration_recorders/2,

    describe_configuration_recorder_status/0,
    describe_configuration_recorder_status/1,
    describe_configuration_recorder_status/2,

    describe_delivery_channels/0,
    describe_delivery_channels/1,
    describe_delivery_channels/2,

    describe_delivery_channel_status/0,
    describe_delivery_channel_status/1,
    describe_delivery_channel_status/2,

    get_compliance_details_by_config_rule/1,
    get_compliance_details_by_config_rule/2,
    get_compliance_details_by_config_rule/3,

    get_compliance_details_by_resource/2,
    get_compliance_details_by_resource/3,
    get_compliance_details_by_resource/4,

    get_compliance_summary_by_config_rule/0,
    get_compliance_summary_by_config_rule/1,

    get_compliance_summary_by_resource_type/0,
    get_compliance_summary_by_resource_type/1,
    get_compliance_summary_by_resource_type/2,

    get_discovered_resource_counts/0,
    get_discovered_resource_counts/1,
    get_discovered_resource_counts/2,

    get_resource_config_history/2,
    get_resource_config_history/3,
    get_resource_config_history/4,

    list_discovered_resources/1,
    list_discovered_resources/2,
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

-spec configure(string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey)),
    ok.

-spec configure(string(), string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host)),
    ok.

-spec configure(string(), string(), string(), non_neg_integer()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host, Port) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host, Port)),
    ok.

-spec configure(string(), string(), string(), non_neg_integer(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host, Port, Scheme) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host, Port, Scheme)),
    ok.

%%------------------------------------------------------------------------------
%% AWS Config API Functions
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% DescribeComplianceByConfigRule
%%
%% https://docs.aws.amazon.com/config/latest/APIReference/API_DescribeComplianceByConfigRule.html
%%------------------------------------------------------------------------------
-spec describe_compliance_by_config_rule() ->
    result_paged(map()).
describe_compliance_by_config_rule() ->
    describe_compliance_by_config_rule(#{}).

-spec describe_compliance_by_config_rule(Opts :: map()) ->
    result_paged(map()).
describe_compliance_by_config_rule(Opts) ->
    describe_compliance_by_config_rule(Opts, default_config()).

-spec describe_compliance_by_config_rule(Opts :: map(),
    Config :: aws_config()) ->
    result_paged(map()).
describe_compliance_by_config_rule(Opts, Config) when is_map(Opts) orelse Opts =:= #{} ->
    Request = #{?COMPLIANCE_TYPES => maps:get(?COMPLIANCE_TYPES, Opts, []),
        ?CONFIG_RULE_NAMES        => maps:get(?CONFIG_RULE_NAMES, Opts, []),
        ?NEXT_TOKEN_LABEL         => maps:get(?NEXT_TOKEN_LABEL, Opts, <<"">>)
    },
    request(Config, "DescribeComplianceByConfigRule", Request).


%%------------------------------------------------------------------------------
%% @doc
%% DescribeComplianceByResource
%%
%% https://docs.aws.amazon.com/config/latest/APIReference/API_DescribeComplianceByResource.html
%%------------------------------------------------------------------------------
-spec describe_compliance_by_resource() ->
    result_paged(map()).
describe_compliance_by_resource() ->
    describe_compliance_by_resource(#{}).

-spec describe_compliance_by_resource(Opts :: map()) ->
    result_paged(map()).
describe_compliance_by_resource(Opts) ->
    describe_compliance_by_resource(Opts, default_config()).

-spec describe_compliance_by_resource(Opts :: map(),
    Config :: aws_config()) ->
    result_paged(map()).
describe_compliance_by_resource(Opts, Config) when is_map(Opts) orelse Opts =:= #{} ->
    Request = #{?COMPLIANCE_TYPES => maps:get(?COMPLIANCE_TYPES, Opts, []),
        ?LIMIT                    => maps:get(?LIMIT, Opts, ?LIMIT_MAX),
        ?NEXT_TOKEN_LABEL         => maps:get(?NEXT_TOKEN_LABEL, Opts, <<"">>),
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
-spec describe_config_rule_evaluation_status() ->
    result_paged(map()).
describe_config_rule_evaluation_status() ->
    describe_config_rule_evaluation_status(#{}).

-spec describe_config_rule_evaluation_status(Opts :: map()) ->
    result_paged(map()).
describe_config_rule_evaluation_status(Opts) ->
    describe_config_rule_evaluation_status(Opts, default_config()).

-spec describe_config_rule_evaluation_status(Opts :: map(),
    Config :: aws_config()) ->
    result_paged(map()).
describe_config_rule_evaluation_status(Opts, Config) when is_map(Opts) orelse Opts =:= #{} ->
    Request = #{?CONFIG_RULE_NAMES => maps:get(?CONFIG_RULE_NAMES, Opts, []),
        ?LIMIT                    => maps:get(?LIMIT, Opts, ?LIMIT_MAX),
        ?NEXT_TOKEN_LABEL         => maps:get(?NEXT_TOKEN_LABEL, Opts, <<"">>)
    },
    request(Config, "DescribeConfigRuleEvaluationStatus", Request).

%%------------------------------------------------------------------------------
%% @doc
%% DescribeConfigRules
%%
%% https://docs.aws.amazon.com/config/latest/APIReference/API_DescribeConfigRules.html
%%------------------------------------------------------------------------------
-spec describe_config_rules() ->
    result_paged(map()).
describe_config_rules() ->
    describe_config_rules(#{}).

-spec describe_config_rules(Opts :: map()) ->
    result_paged(map()).
describe_config_rules(Opts) ->
    describe_config_rules(Opts, default_config()).

-spec describe_config_rules(Opts :: map(),
    Config :: aws_config()) ->
    result_paged(map()).
describe_config_rules(Opts, Config)
    when is_map(Opts) orelse Opts =:= #{} ->
    Request = #{?CONFIG_RULE_NAMES    => maps:get(?CONFIG_RULE_NAMES, Opts, []),
        ?NEXT_TOKEN_LABEL             => maps:get(?NEXT_TOKEN_LABEL, Opts, <<"">>)
    },
    request(Config, "DescribeConfigRules", Request).

%%------------------------------------------------------------------------------
%% @doc
%% DescribeConfigurationRecorders
%%
%% https://docs.aws.amazon.com/config/latest/APIReference/API_DescribeConfigurationRecorders.html
%%------------------------------------------------------------------------------
-spec describe_configuration_recorders() ->
    result_paged(map()).
describe_configuration_recorders() ->
    describe_configuration_recorders(#{}).

-spec describe_configuration_recorders(Opts :: map()) ->
    result_paged(map()).
describe_configuration_recorders(Opts) ->
    describe_configuration_recorders(Opts, default_config()).

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
-spec describe_configuration_recorder_status() ->
    result_paged(map()).
describe_configuration_recorder_status() ->
    describe_configuration_recorder_status(#{}).

-spec describe_configuration_recorder_status(Opts :: map()) ->
    result_paged(map()).
describe_configuration_recorder_status(Opts) ->
    describe_configuration_recorder_status(Opts, default_config()).

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
-spec describe_delivery_channels() ->
    result_paged(map()).
describe_delivery_channels() ->
    describe_delivery_channels(#{}).

-spec describe_delivery_channels(Opts :: map()) ->
    result_paged(map()).
describe_delivery_channels(Opts) ->
    describe_delivery_channels(Opts, default_config()).

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
-spec describe_delivery_channel_status() ->
    result_paged(map()).
describe_delivery_channel_status() ->
    describe_delivery_channel_status(#{}).

-spec describe_delivery_channel_status(Opts :: map()) ->
    result_paged(map()).
describe_delivery_channel_status(Opts) ->
    describe_delivery_channel_status(Opts, default_config()).

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
-spec get_compliance_details_by_config_rule(ConfigRuleName :: binary()) ->
    result_paged(map()).
get_compliance_details_by_config_rule(ConfigRuleName) ->
    get_compliance_details_by_config_rule(ConfigRuleName, #{}).

-spec get_compliance_details_by_config_rule(ConfigRuleName :: binary(), Opts :: map()) ->
    result_paged(map()).
get_compliance_details_by_config_rule(ConfigRuleName, Opts) ->
    get_compliance_details_by_config_rule(ConfigRuleName, Opts, default_config()).

-spec get_compliance_details_by_config_rule(ConfigRuleName :: binary(),
    Opts :: map(),
    Config :: aws_config()) ->
    result_paged(map()).
get_compliance_details_by_config_rule(ConfigRuleName, Opts, Config) when is_map(Opts) orelse Opts =:= #{} ->
    Request = #{?COMPLIANCE_TYPES => maps:get(?COMPLIANCE_TYPES, Opts, []),
        ?CONFIG_RULE_NAME         => ConfigRuleName,
        ?LIMIT                    => maps:get(?LIMIT, Opts, ?LIMIT_MAX),
        ?NEXT_TOKEN_LABEL         => maps:get(?NEXT_TOKEN_LABEL, Opts, <<"">>)},
    request(Config, "GetComplianceDetailsByConfigRule", Request).

%%------------------------------------------------------------------------------
%% @doc
%% GetComplianceDetailsByResource
%%
%% https://docs.aws.amazon.com/config/latest/APIReference/API_GetComplianceDetailsByResource.html
%%------------------------------------------------------------------------------
-spec get_compliance_details_by_resource(ResourceId :: binary(),
    ResourceType :: binary()) ->
    result_paged(map()).
get_compliance_details_by_resource(ResourceId, ResourceType) ->
    get_compliance_details_by_resource(ResourceId, ResourceType, #{}).

-spec get_compliance_details_by_resource(ResourceId :: binary(),
    ResourceType :: binary(),
    Opts :: map()) ->
    result_paged(map()).
get_compliance_details_by_resource(ResourceId, ResourceType, Opts) ->
    get_compliance_details_by_resource(ResourceId, ResourceType, Opts, default_config()).

-spec get_compliance_details_by_resource(ResourceId :: binary(),
    ResourceType :: binary(),
    Opts :: map(),
    Config :: aws_config()) ->
    result_paged(map()).
get_compliance_details_by_resource(ResourceId, ResourceType, Opts, Config) when is_map(Opts) orelse Opts =:= #{} ->
    Request = #{?COMPLIANCE_TYPES => maps:get(?COMPLIANCE_TYPES, Opts, []),
        ?NEXT_TOKEN_LABEL         => maps:get(?NEXT_TOKEN_LABEL, Opts, <<"">>),
        ?RESOURCE_ID              => ResourceId,
        ?RESOURCE_TYPE            => ResourceType},
    request(Config, "GetComplianceDetailsByResource", Request).

%%------------------------------------------------------------------------------
%% @doc
%% GetComplianceSummaryByConfigRule
%%
%% https://docs.aws.amazon.com/config/latest/APIReference/API_GetComplianceSummaryByConfigRule.html
%%------------------------------------------------------------------------------
-spec get_compliance_summary_by_config_rule() ->
    result_paged(map()).
get_compliance_summary_by_config_rule() ->
    get_compliance_summary_by_config_rule(default_config()).

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
-spec get_compliance_summary_by_resource_type() ->
    result_paged(map()).
get_compliance_summary_by_resource_type() ->
    get_compliance_summary_by_resource_type(#{}).

-spec get_compliance_summary_by_resource_type(Opts :: map()) ->
    result_paged(map()).
get_compliance_summary_by_resource_type(Opts) ->
    get_compliance_summary_by_resource_type(Opts, default_config()).

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
-spec get_discovered_resource_counts() ->
    result_paged(map()).
get_discovered_resource_counts() ->
    get_discovered_resource_counts(#{}).

-spec get_discovered_resource_counts(Opts :: map()) ->
    result_paged(map()).
get_discovered_resource_counts(Opts) ->
    get_discovered_resource_counts(Opts, default_config()).

-spec get_discovered_resource_counts(Opts :: map(),
    Config :: aws_config()) ->
    result_paged(map()).
get_discovered_resource_counts(Opts, Config) when is_map(Opts) orelse Opts =:= #{} ->
    Request = #{?LIMIT     => maps:get(?LIMIT, Opts, ?LIMIT_MAX),
        ?NEXT_TOKEN_LABEL  => maps:get(?NEXT_TOKEN_LABEL, Opts, <<"">>),
        ?RESOURCE_TYPES    => maps:get(?RESOURCE_TYPES, Opts, null)},
    request(Config, "GetDiscoveredResourceCounts", Request).


%%------------------------------------------------------------------------------
%% @doc
%% GetResourceConfigHistory
%%
%% https://docs.aws.amazon.com/config/latest/APIReference/API_GetResourceConfigHistory.html
%%------------------------------------------------------------------------------
-spec get_resource_config_history(ResourceId :: binary(),
    ResourceType :: binary()) ->
    result_paged(map()).
get_resource_config_history(ResourceId, ResourceType) ->
    get_resource_config_history(ResourceId, ResourceType, #{}).

-spec get_resource_config_history(ResourceId :: binary(),
    ResourceType :: binary(),
    Opts :: map()) ->
    result_paged(map()).
get_resource_config_history(ResourceId, ResourceType, Opts) ->
    get_resource_config_history(ResourceId, ResourceType, Opts, default_config()).

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
        ?NEXT_TOKEN_LABEL            => maps:get(?NEXT_TOKEN_LABEL, Opts, <<"">>),
        ?RESOURCE_ID                 => ResourceId,
        ?RESOURCE_TYPE               => ResourceType},
    request(Config, "GetResourceConfigHistory", Request).

%%------------------------------------------------------------------------------
%% @doc
%% ListDiscoveredResources
%%
%% https://docs.aws.amazon.com/config/latest/APIReference/API_ListDiscoveredResources.html
%%------------------------------------------------------------------------------
-spec list_discovered_resources(ResourceType :: binary()) ->
    result_paged(map()).
list_discovered_resources(ResourceType) ->
    list_discovered_resources(ResourceType, #{}).

-spec list_discovered_resources(ResourceType :: binary(),
    Opts :: map()) ->
    result_paged(map()).
list_discovered_resources(ResourceType, Opts) ->
    list_discovered_resources(ResourceType, Opts, default_config()).

-spec list_discovered_resources(ResourceType :: binary(),
    Opts :: map(),
    Config :: aws_config()) ->
    result_paged(map()).
list_discovered_resources(ResourceType, Opts, Config) when is_map(Opts) orelse Opts =:= #{} ->
    Request = #{?INCLUDE_DELETED_RESOURCES => maps:get(?INCLUDE_DELETED_RESOURCES, Opts, true),
        ?LIMIT                             => maps:get(?LIMIT, Opts, ?LIMIT_MAX),
        ?NEXT_TOKEN_LABEL                  => maps:get(?NEXT_TOKEN_LABEL, Opts, <<"">>),
        ?RESOURCE_IDS                      => maps:get(?RESOURCE_IDS, Opts, []),
        ?RESOURCE_NAME                     => maps:get(?RESOURCE_NAME, Opts, null),
        ?RESOURCE_TYPE                     => ResourceType},
    request(Config, "ListDiscoveredResources", Request).

%%%------------------------------------------------------------------------------
%%% @doc
%%% Internal Functions
%%%------------------------------------------------------------------------------

default_config() -> erlcloud_aws:default_config().

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

