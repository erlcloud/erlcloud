%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_cloudformation_tests).


-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

-define(_cloudformation_test(T), {?LINE, T}).

-define(_f(F), fun() -> F end).

%%==============================================================================
%% Test entry points
%%==============================================================================

describe_cloudformation_test_() ->
    {foreach, fun start/0, fun stop/1, [
        fun list_stacks_all_output_tests/1,
        fun list_stack_resources_output_tests/1,
        fun get_template_summary_output_tests/1,
        fun get_template_output_tests/1,
        fun get_stack_policy_output_tests/1,
        fun describe_stacks_output_tests/1,
        fun describe_stack_resources_output_tests/1,
        fun describe_stack_resource_output_tests/1,
        fun describe_stack_events_output_tests/1,
        fun describe_account_limits_output_tests/1,
        fun list_stacks_all_input_tests/1,
        fun list_stack_resources_input_tests/1,
        fun get_template_summary_input_tests/1,
        fun get_template_input_tests/1,
        fun get_stack_policy_input_tests/1,
        fun describe_stacks_input_tests/1,
        fun describe_stack_resources_input_tests/1,
        fun describe_stack_resource_input_tests/1,
        fun describe_stack_events_input_tests/1,
        fun describe_account_limits_input_tests/1
    ]}.

start() ->
    meck:new(erlcloud_aws),
    ok.

stop(_) ->
    meck:unload(erlcloud_aws).

%%==============================================================================
%% Output Test helpers
%%==============================================================================

output_test(Fun, {Line, {Description, Response, Result}}) ->
    {Description, {Line, fun() ->
                        meck:expect(erlcloud_aws, aws_request_xml4, 6,
                               {ok, element(1, xmerl_scan:string(
                                    binary_to_list(Response)))}
                            ),
                        Actual = Fun(),
                        ?assertEqual(Result, Actual)
                    end}}.


%%==============================================================================
%% Input Test helpers
%%==============================================================================

validate_param(_Param = {Key, _Value}, Params) ->
    true = proplists:is_defined(Key, Params).

validate_params(Params, Expected) ->
    [validate_param(X, Params)
        || X <- [{"Action", ""}, {"Version", ""} | Expected]

    ].
input_expect(Response, Params) ->

    fun(post, "cloudformation.us-east-1.amazonaws.com", "/", QParams,
            "cloudformation", _) ->
        validate_params(QParams, Params),
        Response
    end.

input_test(Response, {Line, {Description, Fun, Params}})
    when is_list(Description) ->

    InputFunction = input_expect(Response, Params),

    meck:expect(erlcloud_aws, aws_request_xml4, InputFunction),

    {Description, {Line, fun() ->
            Fun()
        end}}.


%%==============================================================================
%% Input Tests
%%==============================================================================

list_stacks_all_input_tests(_) ->
    Response = {ok, element(1, xmerl_scan:string(
            binary_to_list(<<"<ListStacksResponse>null</ListStacksResponse>">>)
        ))},

    ExpectedParams = [],

    input_test(Response, ?_cloudformation_test({"Test list all stacks input",
        ?_f(erlcloud_cloudformation:list_stacks_all([], #aws_config{})),
        ExpectedParams})).

list_stack_resources_input_tests(_) ->
    Response = {ok, element(1, xmerl_scan:string(
        binary_to_list(<<"<ListStackResourcesResponse>null"
            "</ListStackResourcesResponse>">>)
    ))},

    ExpectedParams = [{"StackName", ""}],

    input_test(Response, ?_cloudformation_test({"Test list all stack resources",
        ?_f(erlcloud_cloudformation:list_stack_resources_all([],
            "Stack Name", #aws_config{})), ExpectedParams})).

get_template_summary_input_tests(_) ->
    Response = {ok, element(1, xmerl_scan:string(
        binary_to_list(<<"<GetTemplateSummaryResponse>null"
            "</GetTemplateSummaryResponse>">>)
    ))},

    ExpectedParams = [{"StackName", ""}],

    input_test(Response, ?_cloudformation_test({"Test get template summary",
        ?_f(erlcloud_cloudformation:get_template_summary([],
        "Stack Name", #aws_config{})), ExpectedParams})).

get_template_input_tests(_) ->
    Response = {ok, element(1, xmerl_scan:string(
        binary_to_list(<<"<GetTemplateResponse>null</GetTemplateResponse>">>)
    ))},

    ExpectedParams = [{"StackName", ""}],

    input_test(Response, ?_cloudformation_test({"Test get template",
        ?_f(erlcloud_cloudformation:get_template("Stack Name", #aws_config{})),
        ExpectedParams})).

get_stack_policy_input_tests(_) ->
    Response = {ok, element(1, xmerl_scan:string(
        binary_to_list(<<"<GetStackPolicyResult>null</GetStackPolicyResult>">>)
    ))},

    ExpectedParams = [{"StackName", ""}],

    input_test(Response, ?_cloudformation_test({"Test get stack policy",
        ?_f(erlcloud_cloudformation:get_stack_policy([],
            "Stack Name", #aws_config{})), ExpectedParams})).

describe_stacks_input_tests(_) ->
    Response = {ok, element(1, xmerl_scan:string(
        binary_to_list(<<"<DescribeStacksResponse>null"
            "</DescribeStacksResponse>">>)
    ))},

    ExpectedParams = [],

    input_test(Response, ?_cloudformation_test({"Test describe stacks all",
        ?_f(erlcloud_cloudformation:describe_stacks_all(
            ExpectedParams,#aws_config{})), ExpectedParams})).

describe_stack_resources_input_tests(_) ->
    Response = {ok, element(1, xmerl_scan:string(
        binary_to_list(<<"<DescribeStackResourcesResponse>null"
            "</DescribeStackResourcesResponse>">>)
    ))},

    ExpectedParams = [{"StackName", ""}],

    input_test(Response, ?_cloudformation_test(
        {"Test describe stack resources all",
        ?_f(erlcloud_cloudformation:describe_stack_resources([],
            "Stack Name", #aws_config{})), ExpectedParams})).

describe_stack_resource_input_tests(_) ->
    Response = {ok, element(1, xmerl_scan:string(
        binary_to_list(<<"<DescribeStackResourceResponse>null"
            "</DescribeStackResourceResponse>">>)
    ))},

    ExpectedParams = [{"StackName", ""}, {"LogicalResourceId", ""}],

    input_test(Response, ?_cloudformation_test(
        {"Test describe stack resource",
        ?_f(erlcloud_cloudformation:describe_stack_resource(
            [], "Stack Name", "Logical Resource Id",
            #aws_config{})), ExpectedParams})).

describe_stack_events_input_tests(_) ->
    Response = {ok, element(1, xmerl_scan:string(
        binary_to_list(<<"<DescribeStackEventsResponse>null"
            "</DescribeStackEventsResponse>">>)
    ))},

    ExpectedParams = [{"StackName", ""}],

    input_test(Response, ?_cloudformation_test(
        {"Test describe all stack events",
        ?_f(erlcloud_cloudformation:describe_stack_events_all(
            [{stack_name, ""}], #aws_config{})), ExpectedParams})).

describe_account_limits_input_tests(_) ->
    Response = {ok, element(1, xmerl_scan:string(
        binary_to_list(<<"<DescribeAccountLimitsResponse>null"
            "</DescribeAccountLimitsResponse>">>)
    ))},

    ExpectedParams = [],

    input_test(Response, ?_cloudformation_test(
            {"test describe all account limits",
                ?_f(erlcloud_cloudformation:describe_account_limits_all(
                    #aws_config{})),
            ExpectedParams})).


%%==============================================================================
%% Output Tests
%%==============================================================================


list_stacks_all_output_tests(_) ->
    Test = ?_cloudformation_test({"Test listing all stacks",
                <<"<ListStacksResponse xmlns="
                "\"http://cloudformation.amazonaws.com/doc/2010-05-15/\">
                <NextToken>
                null
                </NextToken>
                <ListStacksResult>
                    <StackSummaries>
                        <member>
                            <StackId>Stack ID</StackId>
                            <StackStatus>Status</StackStatus>
                            <StackName>Stack Name</StackName>
                            <CreationTime>2015-05-23T15:47:44Z</CreationTime>
                            <TemplateDescription>Description"
                            "</TemplateDescription>
                            <ResourceTypes><member>Resource</member>"
                            "</ResourceTypes>
                        </member>
                    </StackSummaries>
                </ListStacksResult>
            </ListStacksResponse>">>,
            [
                [{summaries, [[{member,
                     [[{stack_id, "Stack ID"},
                      {stack_status, "Status"},
                      {stack_name, "Stack Name"},
                      {creation_time, "2015-05-23T15:47:44Z"},
                      {template_description, "Description"},
                      {resource_types, ["Resource"]}
                    ]]}
                ]]}]
            ]
        }),

    output_test(?_f(erlcloud_cloudformation:list_stacks_all(
                [], #aws_config{})), Test).


list_stack_resources_output_tests(_) ->

    Test = ?_cloudformation_test({"Test listing all stack resources",
            <<"<ListStackResourcesResponse xmlns="
            "\"http://cloudformation.amazonaws.com/doc/2010-05-15/\">
                <ListStackResourcesResult>
                    <StackResourceSummaries>
                        <member>
                            <ResourceStatus>Some Status</ResourceStatus>
                            <LogicalResourceId>DBSecurityGroup"
                            "</LogicalResourceId>
                            <LastUpdatedTimestamp>2015-06-21T20:25:57Z"
                            "</LastUpdatedTimestamp>
                            <PhysicalResourceId>Some Resource ID"
                            "</PhysicalResourceId>
                            <ResourceType>Some Resource Type</ResourceType>
                        </member>
                    </StackResourceSummaries>
                </ListStackResourcesResult>
                <ResponseMetadata>
                    <RequestId>Some Request ID</RequestId>
                </ResponseMetadata>
            </ListStackResourcesResponse>">>,

            [
                [{summaries, [[{member,
                    [[{resource_status, "Some Status"},
                      {logical_resource_id, "DBSecurityGroup"},
                      {last_updated_timestamp, "2015-06-21T20:25:57Z"},
                      {physical_resource_id, "Some Resource ID"},
                      {resource_type, "Some Resource Type"}]]
                }]]}]
            ]
        }),

    output_test(?_f(erlcloud_cloudformation:list_stack_resources_all([],
                "Stack Name", #aws_config{})), Test).

get_template_summary_output_tests(_) ->

    Test = ?_cloudformation_test({"Test get template summary",
            <<"<GetTemplateSummaryResponse xmlns="
                "\"http://cloudformation.amazonaws.com/doc/2010-05-15/\">
              <GetTemplateSummaryResult>
                <Description>description</Description>
                <Parameters>
                  <member>
                    <NoEcho>false</NoEcho>
                    <ParameterKey>KeyName</ParameterKey>
                    <Description>description</Description>
                    <ParameterType>Type</ParameterType>
                  </member>
                </Parameters>
                <Metadata>metadata</Metadata>
                <Version>version</Version>
              </GetTemplateSummaryResult>
              <ResponseMetadata>
                <RequestId>request ID</RequestId>
              </ResponseMetadata>
            </GetTemplateSummaryResponse>">>,

            [
                {template_summary_result, [[{description, "description"},
                {parameters, [[
                    {member, [[{no_echo, "false"},
                               {parameter_key, "KeyName"},
                               {description, "description"},
                               {parameter_type, "Type"}]]}
                ]]},
                {metadata, "metadata"},
                {version, "version"}]]},
                {response_meta, [[{request_id, "request ID"}]]}
            ]
        }),

    output_test(?_f(erlcloud_cloudformation:get_template_summary([],
                "Stack Name", #aws_config{}
            )), Test).

get_template_output_tests(_) ->

    Test = ?_cloudformation_test({"Test get template",

            <<"<GetTemplateResponse xmlns="
            "\"http://cloudformation.amazonaws.com/doc/2010-05-15/\">
                <GetTemplateResult>
                  <TemplateBody>template body</TemplateBody>
                </GetTemplateResult>
                <ResponseMetadata>
                  <RequestId>request ID</RequestId>
                </ResponseMetadata>
            </GetTemplateResponse>">>,
            [
                {template_result, [[{template_body, "template body"}]]},
                {response_meta, [[{request_id, "request ID"}]]}
            ]
        }),

    output_test(?_f(erlcloud_cloudformation:get_template(
                "Some Stack Name", #aws_config{}
            )), Test).

get_stack_policy_output_tests(_) ->

    Test = ?_cloudformation_test({"Test get stack policy",
            <<"<GetStackPolicyResponse xmlns="
            "\"http://cloudformation.amazonaws.com/doc/2010-05-15/\">
              <GetStackPolicyResult>
                  <StackPolicyBody>Policy</StackPolicyBody>
              </GetStackPolicyResult>
              <ResponseMetadata>
                <RequestId>request ID</RequestId>
              </ResponseMetadata>
            </GetStackPolicyResponse>">>,
            [
                [{stack_policy_body ,"Policy"},
                 {response_meta, [[{request_id, "request ID"}]]}]
            ]
        }),

    output_test(?_f(erlcloud_cloudformation:get_stack_policy([],
                "Stack Name", #aws_config{}
            )), Test).

describe_stacks_output_tests(_) ->

    Test = ?_cloudformation_test({"Test describe all stacks",
            <<"<DescribeStacksResponse xmlns="
            "\"http://cloudformation.amazonaws.com/doc/2010-05-15/\">
              <DescribeStacksResult>
                <Stacks>
                  <member>
                    <StackName>My Stack</StackName>
                    <StackId>Stack Id</StackId>
                    <CreationTime>Not Today</CreationTime>
                    <StackStatus>status</StackStatus>
                    <DisableRollback>false</DisableRollback>
                    <Parameters>
                      <member>
                        <ParameterKey>Some Key</ParameterKey>
                        <ParameterValue>Parameter</ParameterValue>
                      </member>
                    </Parameters>
                    <Outputs>
                      <member>
                        <Description>Some Description</Description>
                        <OutputKey>Some Key</OutputKey>
                        <OutputValue>Output</OutputValue>
                      </member>
                    </Outputs>
                  </member>
                </Stacks>
              </DescribeStacksResult>
              <ResponseMetadata>
                <RequestId>request ID</RequestId>
              </ResponseMetadata>
            </DescribeStacksResponse>">>,

            [[{stacks, [[
                {member, [[{stack_name, "My Stack"},
                           {stack_id, "Stack Id"},
                           {creation_time, "Not Today"},
                           {stack_status, "status"},
                           {disable_rollback, "false"},
                           {parameters, [[
                                {member, [[
                                    {parameter_key, "Some Key"},
                                    {parameter_value, "Parameter"}
                                ]]}
                            ]]},
                           {outputs, [[
                                {member, [[
                                    {description, "Some Description"},
                                    {output_key, "Some Key"},
                                    {output_value, "Output"}
                                ]]}
                            ]]}
                ]]}
            ]]},{response_meta, [[{request_id, "request ID"}]]}]]
        }),

    output_test(?_f(erlcloud_cloudformation:describe_stacks_all(
                [{stack_name, "Some Stack Name"}], #aws_config{}
            )), Test).

describe_stack_resources_output_tests(_) ->

    Test = ?_cloudformation_test({"Test describe stack resources",
            <<"<DescribeStackResourcesResponse xmlns="
            "\"http://cloudformation.amazonaws.com/doc/2010-05-15/\">
              <DescribeStackResourcesResult>
                <StackResources>
                  <member>
                    <StackId>Stack ID</StackId>
                    <StackName>My Stack</StackName>
                    <LogicalResourceId>MyDBInstance</LogicalResourceId>
                    <PhysicalResourceId>Resource ID</PhysicalResourceId>
                    <ResourceType>Resource Type</ResourceType>
                    <Timestamp>Timestamp</Timestamp>
                    <ResourceStatus>Status</ResourceStatus>
                  </member>
                </StackResources>
              </DescribeStackResourcesResult>
              <ResponseMetadata>
                <RequestId>Request ID</RequestId>
              </ResponseMetadata>
            </DescribeStackResourcesResponse>">>,

            [[{resources, [[{member, [[
                {stack_id, "Stack ID"},
                {stack_name, "My Stack"},
                {logical_resource_id, "MyDBInstance"},
                {physical_resource_id, "Resource ID"},
                {resource_type, "Resource Type"},
                {timestamp, "Timestamp"},
                {resource_status, "Status"}]]}]]},
            {response_meta, [[{request_id, "Request ID"}]]}]
            ]
        }),

    output_test(?_f(erlcloud_cloudformation:describe_stack_resources([],
                "Stack Name", #aws_config{}
            )), Test).

describe_stack_resource_output_tests(_) ->

    Test = ?_cloudformation_test({"Test describe stack resource",
            <<"<DescribeStackResourceResponse xmlns="
            "\"http://cloudformation.amazonaws.com/doc/2010-05-15/\">
             <DescribeStackResourceResult>
               <StackResourceDetail>
                 <StackId>Stack ID</StackId>
                 <StackName>My Stack</StackName>
                 <LogicalResourceId>MyDBInstance</LogicalResourceId>
                 <PhysicalResourceId>Resource ID</PhysicalResourceId>
                 <ResourceType>Resource Type</ResourceType>
                 <LastUpdatedTimestamp>Tiemstamp</LastUpdatedTimestamp>
                 <ResourceStatus>Status</ResourceStatus>
               </StackResourceDetail>
             </DescribeStackResourceResult>
             <ResponseMetadata>
               <RequestId>Request ID</RequestId>
             </ResponseMetadata>
            </DescribeStackResourceResponse>">>,
            [[{resources,
                [[{stack_id, "Stack ID"},
                  {stack_name, "My Stack"},
                  {logical_resource_id, "MyDBInstance"},
                  {physical_resource_id, "Resource ID"},
                  {resource_type, "Resource Type"},
                  {resource_status, "Status"}]]
            },
            {response_meta, [[{request_id, "Request ID"}]]}]]
        }),

    output_test(?_f(erlcloud_cloudformation:describe_stack_resource([],
                "Stack Name", "Logical Id", #aws_config{}
            )), Test).

describe_stack_events_output_tests(_) ->

    Test = ?_cloudformation_test({"Test describe alls tack events",
            <<"<DescribeStackEventsResponse xmlns="
            "\"http://cloudformation.amazonaws.com/doc/2010-05-15/\">
              <DescribeStackEventsResult>
                <StackEvents>
                  <member>
                    <EventId>Event ID</EventId>
                    <StackId>Stack ID</StackId>
                    <StackName>My Stack</StackName>
                    <LogicalResourceId>Resource ID</LogicalResourceId>
                    <PhysicalResourceId>Resource ID</PhysicalResourceId>
                    <ResourceType>Resource Type</ResourceType>
                    <Timestamp>Timestamp</Timestamp>
                    <ResourceStatus>Resource Status</ResourceStatus>
                    <ResourceStatusReason>Resource Status Reason"
                    "</ResourceStatusReason>
                  </member>
                </StackEvents>
              </DescribeStackEventsResult>
              <ResponseMetadata>
                <RequestId>Request ID</RequestId>
              </ResponseMetadata>
            </DescribeStackEventsResponse>">>,

            [[{stack_events, [[{
                member, [[
                {event_id, "Event ID"},
                {stack_id, "Stack ID"},
                {stack_name, "My Stack"},
                {logical_resource_id, "Resource ID"},
                {physical_resource_id, "Resource ID"},
                {resource_type, "Resource Type"},
                {resource_status, "Resource Status"}]]}]]},
            {response_meta, [[{request_id, "Request ID"}]]}]]
        }),

    output_test(?_f(erlcloud_cloudformation:describe_stack_events_all(
                [{stack_name, "Some Stack Name"}], #aws_config{}
            )), Test).

describe_account_limits_output_tests(_) ->

    Test = ?_cloudformation_test({"Test describe all account limits",
            <<"<DescribeAccountLimitsResponse xmlns="
            "\"http://cloudformation.amazonaws.com/doc/2010-05-15/\">
              <DescribeAccountLimitsResult>
                <AccountLimits>
                  <member>
                    <Name>Name</Name>
                    <Value>Value</Value>
                  </member>
                </AccountLimits>
              </DescribeAccountLimitsResult>
              <ResponseMetadata>
                <RequestId>Request ID</RequestId>
              </ResponseMetadata>
            </DescribeAccountLimitsResponse>">>,

            [[{describe_account_limits_result,[[{
                    account_limits, [[{member, [[
                                    {name, "Name"},
                                    {value, "Value"}
                                ]]}]]
                }]]},
            {response_meta, [[{request_id, "Request ID"}]]}]]
        }),

    output_test(?_f(erlcloud_cloudformation:describe_account_limits_all(
                #aws_config{}
            )), Test).
