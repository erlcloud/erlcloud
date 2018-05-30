-module (erlcloud_cloudformation).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

-define(API_VERSION, "2010-05-15").

-type access_key_id() :: string().
-type secret_access_key() :: string().

-type params() :: proplists:proplist().
-type cloudformation_list() :: [proplists:proplist()].


-type error_reason() :: metadata_not_available | container_credentials_unavailable | erlcloud_aws:httpc_result_error().

%% Library initialization
-export([
    configure/2,
    new/2
]).


%% Cloud Formation API Functions
-export ([
          list_stacks_all/1,
          list_stacks_all/2,
          list_stacks/2,
          list_stacks/1,
          list_stack_resources_all/2,
          list_stack_resources_all/3,
          list_stack_resources/2,
          list_stack_resources/1,
          describe_stack_resources/2,
          describe_stack_resources/3,
          describe_stack_resource/3,
          describe_stack_resource/4,
          describe_stacks_all/1,
          describe_stacks_all/2,
          describe_stacks/2,
          describe_stacks/1,
          get_stack_policy/2,
          get_stack_policy/3,
          get_template/2,
          get_template/1,
          get_template_summary/2,
          get_template_summary/3,
          describe_account_limits_all/0,
          describe_account_limits_all/1,
          describe_account_limits/2,
          describe_account_limits/1,
          describe_stack_events_all/1,
          describe_stack_events_all/2,
          describe_stack_events/2,
          describe_stack_events/1]).


%%==============================================================================
%% Library initialization
%%==============================================================================

-spec configure(access_key_id(), secret_access_key()) -> ok.
configure(AccessKeyID, SecretAccessKey) ->
    erlcloud_config:configure(AccessKeyID, SecretAccessKey, fun new/2).


-spec new(access_key_id(), secret_access_key()) -> #aws_config{}.
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{
        access_key_id = AccessKeyID,
        secret_access_key = SecretAccessKey
    }.

%%==============================================================================
%% Cloud Formation API Functions
%%==============================================================================
-spec list_stacks_all(params()) -> {ok, cloudformation_list()} | {error, error_reason()}.
list_stacks_all(Params) ->
    list_stacks_all(Params, default_config()).

-spec list_stacks_all(params(), aws_config()) -> {ok, cloudformation_list()} | {error, error_reason()}.
list_stacks_all(Params, Config = #aws_config{}) ->
    list_all(fun list_stacks/2, Params, Config, []).

-spec list_stacks(params()) -> {ok, cloudformation_list(), NextToken :: undefined | string()}
| {error, error_reason()}.
list_stacks(Params) ->
    list_stacks(Params, default_config()).

-spec list_stacks(params(), aws_config()) -> {ok, cloudformation_list(), NextToken :: undefined | string()}
    | {error, error_reason()}.
list_stacks(Params, Config = #aws_config{}) ->

     ExtraParams= lists:map(fun(T) ->
            case T of
                {stack_status_filter, N, Filter} ->
                    {"StackStatusFilter.member." ++ integer_to_list(N), Filter};

                _ -> convert_param(T)
            end
        end, Params),

    case cloudformation_request(Config, "ListStacks", ExtraParams) of
        {ok, XmlNode} ->
            NextToken = erlcloud_xml:get_text(
                    "/ListStacksResponse/ListStacksResult/NextToken",
                    XmlNode,
                    undefined),

            StackSummaries = extract_stack_summaries(xmerl_xpath:string(
                        "/ListStacksResponse/ListStacksResult",
                        XmlNode)),
            {ok, StackSummaries, NextToken};
        {error, Error} ->
            {error, Error}
    end.

-spec list_stack_resources_all(params(), string()) ->
    {ok, cloudformation_list()} | {error, error_reason()}.
list_stack_resources_all(Params, StackName) ->
    list_stack_resources_all(Params, StackName, default_config()).

-spec list_stack_resources_all(params(), string(), aws_config()) ->
    {ok, cloudformation_list()} | {error, error_reason()}.
list_stack_resources_all(Params, StackName, Config = #aws_config{}) ->

    FullParams = [{"StackName", StackName}
        | lists:map(
            fun(T) ->
                convert_param(T)
            end, Params)],

    list_all(fun list_stack_resources/2, FullParams, Config, []).

-spec list_stack_resources(params()) ->
    {ok, cloudformation_list(), NextToken :: undefined | string()} | {error, error_reason()}.
list_stack_resources(Params) ->
    list_stack_resources(Params, default_config()).

-spec list_stack_resources(params(), aws_config()) ->
    {ok, cloudformation_list(), NextToken :: undefined | string()} | {error, error_reason()}.
list_stack_resources(Params, Config = #aws_config{}) ->

    case cloudformation_request(Config, "ListStackResources", Params) of
        {ok, XmlNode} ->
            NextToken = erlcloud_xml:get_text(
                "/ListStackResourcesResponse/"
                "ListStackResourcesResult/NextToken",
                XmlNode,
                undefined),

            StackResources = extract_list_stack_resources(xmerl_xpath:string(
                "/ListStackResourcesResponse/ListStackResourcesResult",
                XmlNode)),

            {ok, StackResources, NextToken};
        {error, Error} ->
            {error, Error}
    end.

-spec describe_stack_resources(params(), string()) ->
    cloudformation_list() | {error, error_reason()}.
describe_stack_resources(Params, StackName) ->
    describe_stack_resources(Params, StackName, default_config()).

-spec describe_stack_resources(params(), string(), aws_config()) ->
    cloudformation_list() | {error, error_reason()}.
describe_stack_resources(Params, StackName, Config = #aws_config{}) ->

    FullParams = [{"StackName", StackName}
        | lists:map(
            fun(T) ->
                convert_param(T)
            end, Params)],

    case cloudformation_request(Config, "DescribeStackResources", FullParams) of
        {ok, XmlNode} ->
            extract_stack_resources_members(xmerl_xpath:string(
                "/DescribeStackResourcesResponse",
                XmlNode));

        {error, Error} ->
            {error, Error}
    end.

-spec describe_stack_resource(params(), string(), string()) ->
    cloudformation_list() | {error, error_reason()}.
describe_stack_resource(Params, StackName, LogicalResourceId) ->
    describe_stack_resource(Params, StackName, LogicalResourceId,
        default_config()).

-spec describe_stack_resource(params(), string(), string(), aws_config()) ->
    cloudformation_list() | {error, error_reason()}.
describe_stack_resource(Params, StackName, LogicalResourceId,
    Config = #aws_config{}) ->

    FullParams = [{"StackName", StackName},
        {"LogicalResourceId", LogicalResourceId}
            | lists:map(
                fun(T) ->
                    convert_param(T)
                end,Params)],

    case cloudformation_request(Config, "DescribeStackResource", FullParams) of
        {ok, XmlNode} ->
            extract_stack_details(xmerl_xpath:string(
                "/DescribeStackResourceResponse",
                XmlNode));

        {error, Error} ->
            {error, Error}
    end.

-spec describe_stacks_all(params()) ->
    {ok, cloudformation_list()} | {error, error_reason()}.
describe_stacks_all(Params) ->
    describe_stacks_all(Params, default_config()).

-spec describe_stacks_all(params(), aws_config()) ->
    {ok, cloudformation_list()} | {error, error_reason()}.
describe_stacks_all(Params, Config = #aws_config{}) ->

    RequestParams = lists:map(fun(T) -> convert_param(T) end, Params),

    list_all(fun describe_stacks/2, RequestParams, Config, []).

-spec describe_stacks(params()) ->
    {ok, cloudformation_list(), NextToken :: undefined | string()} | {error, error_reason()}.
describe_stacks(Params) ->
    describe_stacks(Params, default_config()).

-spec describe_stacks(params(), aws_config()) ->
    {ok, cloudformation_list(), NextToken :: undefined | string()} | {error, error_reason()}.
describe_stacks(Params, Config = #aws_config{}) ->

    case cloudformation_request(Config, "DescribeStacks", Params) of
        {ok, XmlNode} ->
            NextToken = erlcloud_xml:get_text(
                "/DescribeStacksResponse/DescribeStacksResult/NextToken",
                XmlNode,
                undefined),

            DescribedStacks = extract_described_stacks_result(
                xmerl_xpath:string(
                    "/DescribeStacksResponse",
                    XmlNode)
            ),

            {ok, DescribedStacks, NextToken};
        {error, Error} ->
            {error, Error}
    end.

-spec get_stack_policy(params(), string()) ->
    cloudformation_list() | {error, error_reason()}.
get_stack_policy(Params, StackName) ->
    get_stack_policy(Params, StackName, default_config()).

-spec get_stack_policy(params(), string(), aws_config()) ->
    cloudformation_list() | {error, error_reason()}.
get_stack_policy(Params, StackName, Config = #aws_config{}) ->
    FullParams = [{"StackName", StackName}
        | lists:map(
            fun(T) ->
                convert_param(T)
            end, Params)],

    case cloudformation_request(Config, "GetStackPolicy", FullParams) of
        {ok, XmlNode} ->
            extract_stack_policy_body(xmerl_xpath:string(
                "/GetStackPolicyResponse",
                XmlNode));

        {error, Error} ->
            {error, Error}
    end.

-spec describe_stack_events_all(params()) ->
    {ok, cloudformation_list()} | {error, error_reason()}.
describe_stack_events_all(Params) ->
    describe_stack_events_all(Params, default_config()).

-spec describe_stack_events_all(params(), aws_config()) ->
    {ok, cloudformation_list()} | {error, error_reason()}.
describe_stack_events_all(Params, Config = #aws_config{}) ->

    RequestParams = lists:map(
        fun(T) ->
            convert_param(T)
        end, Params),

    list_all(fun describe_stack_events/2, RequestParams, Config, []).

-spec describe_stack_events(params()) ->
    {ok, cloudformation_list(), NextToken :: undefined | string()} | {error, error_reason()}.
describe_stack_events(Params) ->
    describe_stack_events(Params, default_config()).

-spec describe_stack_events(params(), aws_config()) ->
    {ok, cloudformation_list(), NextToken :: undefined | string()} | {error, error_reason()}.
describe_stack_events(Params, Config = #aws_config{}) ->

    case cloudformation_request(Config, "DescribeStackEvents", Params) of
        {ok, XmlNode} ->
            NextToken = erlcloud_xml:get_text(
                "/DescribeStackEventsResponse/DescribeStackEventsResult"
                "/NextToken",
                XmlNode,
                undefined),

            StackEvents = extract_described_stack_events_result(
                xmerl_xpath:string("/DescribeStackEventsResponse", XmlNode)),
            {ok, StackEvents, NextToken};
        {error, Error} ->
            {error, Error}
    end.

-spec get_template(string()) ->
    cloudformation_list() | {error, error_reason()}.
get_template(StackName) ->
    get_template(StackName, default_config()).

-spec get_template(string(), aws_config()) ->
    cloudformation_list() | {error, error_reason()}.
get_template(StackName, Config = #aws_config{}) ->

    case cloudformation_request(Config, "GetTemplate",
            [{"StackName", StackName}]) of
        {ok, XmlNode} ->
            extract_template_response(XmlNode);
        {error, Error} ->
            {error, Error}
    end.

-spec get_template_summary(params(), string()) ->
    cloudformation_list() | {error, error_reason()}.
get_template_summary(Params, StackName) ->
    get_template_summary(Params, StackName, default_config()).

-spec get_template_summary(params(), string(), aws_config()) ->
    cloudformation_list() | {error, error_reason()}.
get_template_summary(Params, StackName, Config = #aws_config{}) ->

    FullParams = [{"StackName", StackName}
        | lists:map(
            fun(T) ->
                convert_param(T)
            end, Params)],

    case cloudformation_request(Config, "GetTemplateSummary", FullParams) of
        {ok, XmlNodes} ->
            extract_template_summary_response(XmlNodes);
        {error, Error} ->
            {error, Error}
    end.

-spec describe_account_limits_all() ->
    {ok, cloudformation_list()} | {error, error_reason()}.
describe_account_limits_all() ->
    describe_account_limits_all(default_config()).

-spec describe_account_limits_all(aws_config()) ->
    {ok, cloudformation_list()} | {error, error_reason()}.
describe_account_limits_all(Config = #aws_config{}) ->
    list_all(fun describe_account_limits/2, [], Config, []).

-spec describe_account_limits(params()) ->
    {ok, cloudformation_list(), NextToken :: undefined | string()} | {error, error_reason()}.
describe_account_limits(Params) ->
    describe_account_limits(Params, default_config()).

-spec describe_account_limits(params(), aws_config()) ->
    {ok, cloudformation_list(), NextToken :: undefined | string()} | {error, error_reason()}.
describe_account_limits(Params, Config = #aws_config{}) ->

    RequestParams = lists:map(fun(T) -> convert_param(T) end, Params),

    case cloudformation_request(Config, "DescribeAccountLimits",
            RequestParams) of
        {ok, XmlNodes} ->
            NextToken = erlcloud_xml:get_text(
                "/DescribeAccountLimitsResponse/DescribeAccountLimitsResult/"
                "NextToken",
                XmlNodes,
                undefined),

            AccountLimits = extract_accout_limits_response(xmerl_xpath:string(
                "/DescribeAccountLimitsResponse",
                XmlNodes)),

            {ok, AccountLimits, NextToken};
        {error, Error} ->
            {error, Error}
    end.

%%==============================================================================
%% Internal functions
%%==============================================================================

convert_param({Key, Value}) ->
    case Key of
        stack_name -> {"StackName", Value};
        stack_id -> {"StackName", Value};
        logical_resource_id -> {"LogicalResourceId", Value};
        next_token -> {"NextToken", Value}
    end.

default_config() ->
    erlcloud_aws:default_config().

cloudformation_request(Config = #aws_config{}, Action, ExtraParams) ->

    QParams = [
        {"Action", Action},
        {"Version", ?API_VERSION}
        | ExtraParams],

    erlcloud_aws:aws_request_xml4(post, Config#aws_config.cloudformation_host,
        "/", QParams, "cloudformation", Config).

list_all(Fun, Params, Config, Acc) ->
    case Fun(Params, Config) of
        {ok, Data, NextToken} ->
            case NextToken of
                undefined ->
                    lists:foldl(fun erlang:'++'/2, [], [Data | Acc]);
                _ ->
                    list_all(Fun, [{next_token, NextToken} | Params],
                        Config, [Data | Acc])
            end;
        {error, Reason} ->
            {error, Reason}
    end.

extract_stack_summaries(XmlNodes) ->
    lists:map(fun(T) -> erlcloud_xml:decode([{summaries, "StackSummaries",
        {map, fun extract_stacks/1}}], T) end, XmlNodes).

extract_stacks(XmlNode) ->
    erlcloud_xml:decode([
            {member, "member", {map, fun extract_stack/1}}
        ], XmlNode).

extract_stack(XmlNode) ->
    erlcloud_xml:decode([
        {stack_id, "StackId", optional_text},
        {stack_status, "StackStatus", optional_text},
        {stack_name, "StackName", optional_text},
        {creation_time, "CreationTime", optional_text},
        {template_description, "TemplateDescription", optional_text},
        {resource_types, "ResourceTypes", list}], XmlNode).

extract_list_stack_resources(XmlNodes) ->
    lists:map(fun(T) -> erlcloud_xml:decode([
        {summaries, "StackResourceSummaries",
            {map, fun extract_list_resources/1}}
        ], T) end, XmlNodes).

extract_list_resources(XmlNode) ->
    erlcloud_xml:decode([
        {member, "member", {map, fun extract_list_resource/1}}
        ], XmlNode).

extract_list_resource(XmlNode) ->
    erlcloud_xml:decode([
        {resource_status, "ResourceStatus", optional_text},
        {logical_resource_id, "LogicalResourceId", optional_text},
        {last_updated_timestamp, "LastUpdatedTimestamp", optional_text},
        {physical_resource_id, "PhysicalResourceId", optional_text},
        {resource_type, "ResourceType", optional_text}
        ], XmlNode).

extract_stack_resources_members(XmlNodes) ->
    lists:map(fun(T) -> erlcloud_xml:decode([
        {resources, "DescribeStackResourcesResult/StackResources",
            {map, fun extract_member_resources/1}},

        {response_meta, "ResponseMetadata",
            {map, fun extract_template_meta_body/1}}

        ], T) end, XmlNodes).

extract_member_resources(XmlNode) ->
    erlcloud_xml:decode([{member, "member",
        {map, fun extract_resource/1}}], XmlNode).

extract_resource(XmlNode) ->
    erlcloud_xml:decode([
        {stack_id, "StackId", optional_text},
        {stack_name, "StackName", optional_text},
        {logical_resource_id, "LogicalResourceId", optional_text},
        {physical_resource_id, "PhysicalResourceId", optional_text},
        {resource_type, "ResourceType", optional_text},
        {timestamp, "Timestamp", optional_text},
        {resource_status, "ResourceStatus", optional_text}
        ], XmlNode).

extract_stack_details(XmlNodes) ->
    lists:map(fun(T) -> erlcloud_xml:decode(
        [{resources, "DescribeStackResourceResult/StackResourceDetail",
            {map, fun extract_resource/1}},
         {response_meta, "ResponseMetadata",
            {map, fun extract_template_meta_body/1}}
            ], T) end, XmlNodes).

extract_described_stacks_result(XmlNodes) ->
    lists:map(fun(T) -> erlcloud_xml:decode([
        {stacks, "DescribeStacksResult/Stacks",
            {map, fun extract_described_stacks/1}},
        {response_meta, "ResponseMetadata",
            {map, fun extract_template_meta_body/1}}
    ], T) end, XmlNodes).

extract_described_stacks(XmlNode) ->
    erlcloud_xml:decode([{member, "member",
        {map, fun extract_described_stack/1}}], XmlNode).

extract_described_stack(XmlNode) ->
    erlcloud_xml:decode([
            {stack_name, "StackName", optional_text},
            {stack_id, "StackId", optional_text},
            {creation_time, "CreationTime", optional_text},
            {stack_status, "StackStatus", optional_text},
            {disable_rollback, "DisableRollback", optional_text},
            {parameters, "Parameters", {map, fun extract_parameters/1}},
            {outputs, "Outputs", {map, fun extract_resource_outputs/1}}
        ], XmlNode).

extract_parameters(XmlNode) ->
    erlcloud_xml:decode([
            {member, "member", {map, fun extract_parameter/1}}
         ], XmlNode).

extract_parameter(XmlNode) ->
    erlcloud_xml:decode([
            {parameter_key, "ParameterKey", optional_text},
            {parameter_value, "ParameterValue", optional_text}
        ], XmlNode).

extract_resource_outputs(XmlNode) ->
    erlcloud_xml:decode([
            {member, "member", {map, fun extract_resource_output/1}}
        ], XmlNode).

extract_resource_output(XmlNode) ->
    erlcloud_xml:decode([
            {description, "Description", optional_text},
            {output_key, "OutputKey", optional_text},
            {output_value, "OutputValue", optional_text}
        ], XmlNode).

extract_stack_policy_body(XmlNodes) ->
    lists:map(fun(T) -> erlcloud_xml:decode([
                {stack_policy_body, "GetStackPolicyResult/StackPolicyBody/",
                    optional_text},

                {response_meta, "ResponseMetadata",
                    {map, fun extract_template_meta_body/1}}

            ], T) end, XmlNodes).

extract_described_stack_events_result(XmlNodes) ->
    lists:map(fun(T) -> erlcloud_xml:decode([
                {stack_events, "DescribeStackEventsResult/StackEvents",
                    {map, fun extract_stack_event_member/1}},

                {response_meta, "ResponseMetadata",
                    {map, fun extract_template_meta_body/1}}

            ], T) end, XmlNodes).

extract_stack_event_member(XmlNode) ->
    erlcloud_xml:decode([
            {member, "member", {map, fun extract_stack_event/1}}
    ], XmlNode).

extract_stack_event(XmlNode) ->
    erlcloud_xml:decode([
            {event_id, "EventId", optional_text},
            {stack_id, "StackId", optional_text},
            {stack_name, "StackName", optional_text},
            {logical_resource_id, "LogicalResourceId", optional_text},
            {physical_resource_id, "PhysicalResourceId", optional_text},
            {resource_type, "ResourceType", optional_text},
            {timestamp, "TimeStamp", optional_text},
            {resource_status, "ResourceStatus", optional_text},
            {resource_properties, "ResourceProperties", optional_text}
    ], XmlNode).

extract_template_response(XmlNodes) ->
    erlcloud_xml:decode([
                {template_result, "GetTemplateResult",
                    {map, fun extract_template_result/1}},
                {response_meta, "ResponseMetadata",
                    {map, fun extract_template_meta_body/1}}
            ], XmlNodes).

extract_template_meta_body(XmlNode) ->
    erlcloud_xml:decode([{request_id, "RequestId", optional_text}], XmlNode).

extract_template_result(XmlNode) ->
    erlcloud_xml:decode([{template_body, "TemplateBody", optional_text}],
        XmlNode).

extract_template_summary_response(XmlNodes) ->
    erlcloud_xml:decode([
                {template_summary_result, "GetTemplateSummaryResult",
                    {map, fun extract_template_summary_result/1}},
                {response_meta, "ResponseMetadata",
                    {map, fun extract_template_meta_body/1}}
        ], XmlNodes).

extract_template_summary_result(XmlNode) ->
    erlcloud_xml:decode([
                {description, "Description", optional_text},
                {parameters, "Parameters",
                    {map, fun extract_template_parameters/1}},
                {metadata, "Metadata", optional_text},
                {version, "Version", optional_text}
        ], XmlNode).

extract_template_parameters(XmlNode) ->
    erlcloud_xml:decode([
                {member, "member",
                    {map, fun extract_template_parameter/1}}
        ], XmlNode).

extract_template_parameter(XmlNode) ->
    erlcloud_xml:decode([
                {no_echo, "NoEcho", optional_text},
                {parameter_key, "ParameterKey", optional_text},
                {description, "Description", optional_text},
                {parameter_type, "ParameterType", optional_text}
        ], XmlNode).

extract_accout_limits_response(XmlNodes) ->
    lists:map(fun(T) -> erlcloud_xml:decode([
            {describe_account_limits_result, "DescribeAccountLimitsResult",
                {map, fun extract_account_limits_result/1}},
            {response_meta, "ResponseMetadata",
                {map, fun extract_template_meta_body/1}}
        ], T) end, XmlNodes).

extract_account_limits_result(XmlNode) ->
    erlcloud_xml:decode([
            {account_limits, "AccountLimits",
                {map, fun extract_account_limits/1}}
        ], XmlNode).

extract_account_limits(XmlNode) ->
    erlcloud_xml:decode([
            {member, "member",
                {map, fun extract_account_limit/1}}
        ], XmlNode).

extract_account_limit(XmlNode) ->
    erlcloud_xml:decode([
            {name, "Name", optional_text},
            {value, "Value", optional_text}
        ], XmlNode).




