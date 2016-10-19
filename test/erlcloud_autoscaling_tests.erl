%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_autoscaling_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

%% Unit tests for autoscaling api.
%% These tests work by using meck to mock httpc. There are two classes of test: input and output.
%%
%% Input tests verify that different function args produce the desired JSON request.
%% An input test list provides a list of funs and the JSON that is expected to result.
%%
%% Output tests verify that the http response produces the correct return from the fun.
%% An output test lists provides a list of response bodies and the expected return.

%% The _autoscaling_test macro provides line number annotation to a test, similar to _test, but doesn't wrap in a fun
-define(_autoscaling_test(T), {?LINE, T}).
%% The _f macro is a terse way to wrap code in a fun. Similar to _test but doesn't annotate with a line number
-define(_f(F), fun() -> F end).

%%%===================================================================
%%% Test entry points
%%%===================================================================

autoscaling_api_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun describe_autoscaling_groups_all_input_tests/1,
      fun describe_autoscaling_groups_all_output_tests/1,
      fun describe_autoscaling_groups_input_tests/1,
      fun describe_autoscaling_groups_output_tests/1,
      fun describe_launch_configurations_all_input_tests/1,
      fun describe_launch_configurations_all_output_tests/1,
      fun describe_launch_configurations_input_tests/1,
      fun describe_launch_configurations_output_tests/1
     ]}.

start() ->
    meck:new(erlcloud_httpc),
    ok.

stop(_) ->
    meck:unload(erlcloud_httpc).

%%%===================================================================
%%% Input test helpers
%%%===================================================================

%% common_params returns the list of parameters that are not validated by these tests.
%% They should be checked by lower level unit tests.
-spec common_params() -> [string()].
common_params() ->
    ["AWSAccessKeyId",
     "SignatureMethod",
     "SignatureVersion",
     "Timestamp",
     "Version",
     "Signature"].

%% validate_param checks that the query parameter is either a common param or expected
%% by the test case. If expected, returns expected with the param deleted to be used in
%% subsequent calls.
-type expected_param() :: {string(), string()}.
-spec validate_param(string(), [expected_param()]) -> [expected_param()].
validate_param(Param, Expected) ->
    [Key, Value] = case string:tokens(Param, "=") of
        [_K, _V] = P ->
            P;
        [K] ->
            [K,  ""]
    end,
    case lists:member(Key, common_params()) of
        true ->
            Expected;
        false ->
            Expected1 = lists:delete({Key, Value}, Expected),
            case length(Expected) - 1 =:= length(Expected1) of
                true -> ok;
                false -> 
                    ?debugFmt("Parameter not expected: ~p", [{Key, Value}])
            end,
            ?assertEqual(length(Expected) - 1, length(Expected1)),
            Expected1
    end.

%% verifies that the parameters in the body match the expected parameters
-spec validate_params(binary(), [expected_param()]) -> ok.
validate_params(Body, Expected) ->
    ParamList = string:tokens(binary_to_list(Body), "&"),
    Remain = lists:foldl(fun validate_param/2, Expected, ParamList),
    io:format("Remain: ~p", [Remain]),
    ?assertEqual([], Remain).

%% returns the mock of the httpc function input tests expect to be called.
%% Validates the query body and responds with the provided response.
-spec input_expect(string(), [expected_param()]) -> fun().
input_expect(Response, Expected) ->
    fun(_Url, post, _Headers, Body, _Timeout, _Config) -> 
            validate_params(Body, Expected),
            {ok, {{200, "OK"}, [], list_to_binary(Response)}} 
    end.

%% input_test converts an input_test specifier into an eunit test generator
-type input_test_spec() :: {pos_integer(), {fun(), [expected_param()]} | {string(), fun(), [expected_param()]}}.
-spec input_test(string(), input_test_spec()) -> tuple().
input_test(Response, {Line, {Description, Fun, Params}}) when
      is_list(Description) ->
    {Description, 
     {Line,
      fun() ->
              meck:expect(erlcloud_httpc, request, input_expect(Response, Params)),
              %% Configure to make sure there is a key. Would like to do this in start, but
              %% that isn't called in the same process
              erlcloud_ec2:configure(string:copies("A", 20), string:copies("a", 40)),
              Fun()
      end}}.
%% input_test(Response, {Line, {Fun, Params}}) ->
%%     input_test(Response, {Line, {"", Fun, Params}}).

%% input_tests converts a list of input_test specifiers into an eunit test generator
-spec input_tests(string(), [input_test_spec()]) -> [tuple()].
input_tests(Response, Tests) ->
    [input_test(Response, Test) || Test <- Tests].

%%%===================================================================
%%% Output test helpers
%%%===================================================================

%% returns the mock of the httpc function output tests expect to be called.
-spec output_expect(string()) -> fun().
output_expect(Response) ->
    fun(_Url, post, _Headers, _Body, _Timeout, _Config) -> 
            {ok, {{200, "OK"}, [], list_to_binary(Response)}} 
    end.

-spec output_expect_seq([string()]) -> meck:ret_spec().
output_expect_seq(Responses) ->
    meck:seq([{ok, {{200, "OK"}, [], list_to_binary(Response)}} || Response <- Responses]).


%% output_test converts an output_test specifier into an eunit test generator
-type output_test_spec() :: {pos_integer(), {string(), term()} | {string(), string(), term()}}.
-spec output_test(fun(), output_test_spec(), fun()) -> tuple().
output_test(Fun, {Line, {Description, Response, Result}}, OutputFun) ->
    {Description,
     {Line,
      fun() ->
              meck:expect(erlcloud_httpc, request, 6, OutputFun(Response)),
              erlcloud_ec2:configure(string:copies("A", 20), string:copies("a", 40)),
              Actual = Fun(),
              io:format("Actual: ~p~n", [Actual]),
              io:format("Result: ~p~n", [Result]),
              ?assertEqual(Result, Actual)
      end}}.
      
%% output_tests converts a list of output_test specifiers into an eunit test generator
-spec output_tests(fun(), [output_test_spec()]) -> [term()].       
output_tests(Fun, Tests) ->
    [output_test(Fun, Test, fun output_expect/1) || Test <- Tests].

%% output_tests converts a list of output_test specifiers into an eunit test generator
-spec output_tests_seq(fun(), [output_test_spec()]) -> [term()].       
output_tests_seq(Fun, Tests) ->
    [output_test(Fun, Test, fun output_expect_seq/1) || Test <- Tests].

% DescribeAutoScalingGroups test based on the API examples:
% http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeAutoScalingGroups.html
describe_autoscaling_groups_all_input_tests(_) ->
    Tests = 
        [?_autoscaling_test(
            {"Test retrieves a list of autoscaling groups in a region.",
            ?_f(erlcloud_autoscaling:describe_autoscaling_groups_all()),
            [{"Action", "DescribeAutoScalingGroups"}]}),
        ?_autoscaling_test(
            {"Test retrieves a specific autoscaling group in a region.",
            ?_f(erlcloud_autoscaling:describe_autoscaling_groups_all(["my-test-asg-lbs"])),
            [{"Action", "DescribeAutoScalingGroups"},
             {"AutoScalingGroupNames.member.1", "my-test-asg-lbs"}]})
        ],
    Response = "
<DescribeAutoScalingGroupsResponse xmlns=\"http://autoscaling.amazonaws.com/doc/2011-01-01/\">
    <ResponseMetadata>
        <requestId>0f02a07d-b677-11e2-9eb0-dd50EXAMPLE</requestId>
    </ResponseMetadata>
    <DescribeAutoScalingGroupsResult>
        <AutoScalingGroups/>
    </DescribeAutoScalingGroupsResult>
</DescribeAutoScalingGroupsResponse>",
    input_tests(Response, Tests).

describe_autoscaling_groups_all_output_tests(_) ->
    Tests = 
        [?_autoscaling_test(
            {"This example describes all autoscaling groups in a region.", ["
<DescribeAutoScalingGroupsResponse xmlns=\"http://autoscaling.amazonaws.com/doc/2011-01-01/\">
<DescribeAutoScalingGroupsResult>
    <NextToken>foobar1</NextToken>
    <AutoScalingGroups>
      <member>
        <Tags/>
        <SuspendedProcesses/>
        <AutoScalingGroupName>my-test-asg-lbs</AutoScalingGroupName>
        <HealthCheckType>ELB</HealthCheckType>
        <CreatedTime>2013-05-06T17:47:15.107Z</CreatedTime>
        <EnabledMetrics/>
        <LaunchConfigurationName>my-test-lc1</LaunchConfigurationName>
        <Instances/>
        <DesiredCapacity>2</DesiredCapacity>
        <AvailabilityZones>
          <member>us-east-1b</member>
          <member>us-east-1a</member>
        </AvailabilityZones>
        <LoadBalancerNames>
          <member>my-test-asg-loadbalancer</member>
        </LoadBalancerNames>
        <MinSize>2</MinSize>
        <VPCZoneIdentifier/>
        <HealthCheckGracePeriod>120</HealthCheckGracePeriod>
        <DefaultCooldown>300</DefaultCooldown>
        <AutoScalingGroupARN>arn:aws:autoscaling:us-east-1:803981987763:autoScalingGroup:ca861182-c8f9-4ca7-b1eb-cd35505f5ebb:autoScalingGroupName/my-test-asg-lbs</AutoScalingGroupARN>
        <TerminationPolicies>
          <member>Default</member>
        </TerminationPolicies>
        <MaxSize>10</MaxSize>
      </member>
    </AutoScalingGroups>
  </DescribeAutoScalingGroupsResult>
  <ResponseMetadata>
    <RequestId>0f02a07d-b677-11e2-9eb0-dd50EXAMPLE</RequestId>
  </ResponseMetadata>
</DescribeAutoScalingGroupsResponse>",  "
<DescribeAutoScalingGroupsResponse xmlns=\"http://autoscaling.amazonaws.com/doc/2011-01-01/\">
<DescribeAutoScalingGroupsResult>
    <NextToken>foobar2</NextToken>
    <AutoScalingGroups>
      <member>
        <Tags/>
        <SuspendedProcesses/>
        <AutoScalingGroupName>my-test-asg-lbs</AutoScalingGroupName>
        <HealthCheckType>ELB</HealthCheckType>
        <CreatedTime>2013-05-06T17:47:15.107Z</CreatedTime>
        <EnabledMetrics/>
        <LaunchConfigurationName>my-test-lc2</LaunchConfigurationName>
        <Instances/>
        <DesiredCapacity>2</DesiredCapacity>
        <AvailabilityZones>
          <member>us-east-1b</member>
          <member>us-east-1a</member>
        </AvailabilityZones>
        <LoadBalancerNames>
          <member>my-test-asg-loadbalancer</member>
        </LoadBalancerNames>
        <MinSize>2</MinSize>
        <VPCZoneIdentifier/>
        <HealthCheckGracePeriod>120</HealthCheckGracePeriod>
        <DefaultCooldown>300</DefaultCooldown>
        <AutoScalingGroupARN>arn:aws:autoscaling:us-east-1:803981987763:autoScalingGroup:ca861182-c8f9-4ca7-b1eb-cd35505f5ebb:autoScalingGroupName/my-test-asg-lbs</AutoScalingGroupARN>
        <TerminationPolicies>
          <member>Default</member>
        </TerminationPolicies>
        <MaxSize>10</MaxSize>
      </member>
    </AutoScalingGroups>
  </DescribeAutoScalingGroupsResult>
  <ResponseMetadata>
    <RequestId>0f02a07d-b677-11e2-9eb0-dd50EXAMPLE</RequestId>
  </ResponseMetadata>
</DescribeAutoScalingGroupsResponse>",  "
<DescribeAutoScalingGroupsResponse xmlns=\"http://autoscaling.amazonaws.com/doc/2011-01-01/\">
<DescribeAutoScalingGroupsResult>
    <NextToken>foobar3</NextToken>
    <AutoScalingGroups>
      <member>
        <Tags/>
        <SuspendedProcesses/>
        <AutoScalingGroupName>my-test-asg-lbs</AutoScalingGroupName>
        <HealthCheckType>ELB</HealthCheckType>
        <CreatedTime>2013-05-06T17:47:15.107Z</CreatedTime>
        <EnabledMetrics/>
        <LaunchConfigurationName>my-test-lc3</LaunchConfigurationName>
        <Instances/>
        <DesiredCapacity>2</DesiredCapacity>
        <AvailabilityZones>
          <member>us-east-1b</member>
          <member>us-east-1a</member>
        </AvailabilityZones>
        <LoadBalancerNames>
          <member>my-test-asg-loadbalancer</member>
        </LoadBalancerNames>
        <MinSize>2</MinSize>
        <VPCZoneIdentifier/>
        <HealthCheckGracePeriod>120</HealthCheckGracePeriod>
        <DefaultCooldown>300</DefaultCooldown>
        <AutoScalingGroupARN>arn:aws:autoscaling:us-east-1:803981987763:autoScalingGroup:ca861182-c8f9-4ca7-b1eb-cd35505f5ebb:autoScalingGroupName/my-test-asg-lbs</AutoScalingGroupARN>
        <TerminationPolicies>
          <member>Default</member>
        </TerminationPolicies>
        <MaxSize>10</MaxSize>
      </member>
    </AutoScalingGroups>
  </DescribeAutoScalingGroupsResult>
  <ResponseMetadata>
    <RequestId>0f02a07d-b677-11e2-9eb0-dd50EXAMPLE</RequestId>
  </ResponseMetadata>
</DescribeAutoScalingGroupsResponse>", "
<DescribeAutoScalingGroupsResponse xmlns=\"http://autoscaling.amazonaws.com/doc/2011-01-01/\">
<DescribeAutoScalingGroupsResult>
    <AutoScalingGroups>
      <member>
        <Tags/>
        <SuspendedProcesses/>
        <AutoScalingGroupName>my-test-asg-lbs</AutoScalingGroupName>
        <HealthCheckType>ELB</HealthCheckType>
        <CreatedTime>2013-05-06T17:47:15.107Z</CreatedTime>
        <EnabledMetrics/>
        <LaunchConfigurationName>my-test-lc4</LaunchConfigurationName>
        <Instances/>
        <DesiredCapacity>2</DesiredCapacity>
        <AvailabilityZones>
          <member>us-east-1b</member>
          <member>us-east-1a</member>
        </AvailabilityZones>
        <LoadBalancerNames>
          <member>my-test-asg-loadbalancer</member>
        </LoadBalancerNames>
        <MinSize>2</MinSize>
        <VPCZoneIdentifier/>
        <HealthCheckGracePeriod>120</HealthCheckGracePeriod>
        <DefaultCooldown>300</DefaultCooldown>
        <AutoScalingGroupARN>arn:aws:autoscaling:us-east-1:803981987763:autoScalingGroup:ca861182-c8f9-4ca7-b1eb-cd35505f5ebb:autoScalingGroupName/my-test-asg-lbs</AutoScalingGroupARN>
        <TerminationPolicies>
          <member>Default</member>
        </TerminationPolicies>
        <MaxSize>10</MaxSize>
      </member>
    </AutoScalingGroups>
  </DescribeAutoScalingGroupsResult>
  <ResponseMetadata>
    <RequestId>0f02a07d-b677-11e2-9eb0-dd50EXAMPLE</RequestId>
  </ResponseMetadata>
</DescribeAutoScalingGroupsResponse>"],
    {ok, [[
            {autoscaling_group_name, "my-test-asg-lbs"},
            {autoscaling_group_arn, "arn:aws:autoscaling:us-east-1:803981987763:autoScalingGroup:ca861182-c8f9-4ca7-b1eb-cd35505f5ebb:autoScalingGroupName/my-test-asg-lbs"},
            {launch_configuration_name, "my-test-lc1"},
            {min_size, 2},
            {max_size, 10},
            {create_time,{{2013,5,6},{17,47,15}}},
            {health_check_type, "ELB"},
            {desired_capacity,2},
            {placement_group,[]},
            {status,[]},
            {subnets, []},
            {availability_zones,["us-east-1b","us-east-1a"]},
            {load_balancers,["my-test-asg-loadbalancer"]},
            {instances,[]},
            {tag_set,[]}
         ],[
            {autoscaling_group_name, "my-test-asg-lbs"},
            {autoscaling_group_arn, "arn:aws:autoscaling:us-east-1:803981987763:autoScalingGroup:ca861182-c8f9-4ca7-b1eb-cd35505f5ebb:autoScalingGroupName/my-test-asg-lbs"},
            {launch_configuration_name, "my-test-lc2"},
            {min_size, 2},
            {max_size, 10},
            {create_time,{{2013,5,6},{17,47,15}}},
            {health_check_type, "ELB"},
            {desired_capacity,2},
            {placement_group,[]},
            {status,[]},
            {subnets, []},
            {availability_zones,["us-east-1b","us-east-1a"]},
            {load_balancers,["my-test-asg-loadbalancer"]},
            {instances,[]},
            {tag_set,[]}
         ],[
            {autoscaling_group_name, "my-test-asg-lbs"},
            {autoscaling_group_arn, "arn:aws:autoscaling:us-east-1:803981987763:autoScalingGroup:ca861182-c8f9-4ca7-b1eb-cd35505f5ebb:autoScalingGroupName/my-test-asg-lbs"},
            {launch_configuration_name, "my-test-lc3"},
            {min_size, 2},
            {max_size, 10},
            {create_time,{{2013,5,6},{17,47,15}}},
            {health_check_type, "ELB"},
            {desired_capacity,2},
            {placement_group,[]},
            {status,[]},
            {subnets, []},
            {availability_zones,["us-east-1b","us-east-1a"]},
            {load_balancers,["my-test-asg-loadbalancer"]},
            {instances,[]},
            {tag_set,[]}
         ],[
            {autoscaling_group_name, "my-test-asg-lbs"},
            {autoscaling_group_arn, "arn:aws:autoscaling:us-east-1:803981987763:autoScalingGroup:ca861182-c8f9-4ca7-b1eb-cd35505f5ebb:autoScalingGroupName/my-test-asg-lbs"},
            {launch_configuration_name, "my-test-lc4"},
            {min_size, 2},
            {max_size, 10},
            {create_time,{{2013,5,6},{17,47,15}}},
            {health_check_type, "ELB"},
            {desired_capacity,2},
            {placement_group,[]},
            {status,[]},
            {subnets, []},
            {availability_zones,["us-east-1b","us-east-1a"]},
            {load_balancers,["my-test-asg-loadbalancer"]},
            {instances,[]},
            {tag_set,[]}
         ]]}})],
    %% Remaining AWS API examples return subsets of the same data
    output_tests_seq(?_f(erlcloud_autoscaling:describe_autoscaling_groups_all()), Tests).

% DescribeAutoScalingGroups pagination version test based on the API examples:
% http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeAutoScalingGroups.html
describe_autoscaling_groups_input_tests(_) ->
    Tests = 
        [?_autoscaling_test(
            {"Test retrieves a first bunch of autoscaling groups in a region.",
            ?_f(erlcloud_autoscaling:describe_autoscaling_groups()),
            [{"Action", "DescribeAutoScalingGroups"}]}),
        ?_autoscaling_test(
            {"Test retrieves a specific autoscaling group in a region.",
            ?_f(erlcloud_autoscaling:describe_autoscaling_groups(["my-test-asg-lbs"])),
            [{"Action", "DescribeAutoScalingGroups"},
             {"AutoScalingGroupNames.member.1", "my-test-asg-lbs"}]})
        ],
    Response = "
<DescribeAutoScalingGroupsResponse xmlns=\"http://autoscaling.amazonaws.com/doc/2011-01-01/\">
    <ResponseMetadata>
        <requestId>0f02a07d-b677-11e2-9eb0-dd50EXAMPLE</requestId>
    </ResponseMetadata>
    <DescribeAutoScalingGroupsResult>
        <AutoScalingGroups/>
    </DescribeAutoScalingGroupsResult>
</DescribeAutoScalingGroupsResponse>",
    input_tests(Response, Tests).

describe_autoscaling_groups_output_tests(_) ->
    Tests = 
        [?_autoscaling_test(
            {"This example describes first bunch autoscaling groups in a region.", "
<DescribeAutoScalingGroupsResponse xmlns=\"http://autoscaling.amazonaws.com/doc/2011-01-01/\">
<DescribeAutoScalingGroupsResult>
    <AutoScalingGroups>
      <member>
        <Tags/>
        <SuspendedProcesses/>
        <AutoScalingGroupName>my-test-asg-lbs</AutoScalingGroupName>
        <HealthCheckType>ELB</HealthCheckType>
        <CreatedTime>2013-05-06T17:47:15.107Z</CreatedTime>
        <EnabledMetrics/>
        <LaunchConfigurationName>my-test-lc</LaunchConfigurationName>
        <Instances/>
        <DesiredCapacity>2</DesiredCapacity>
        <AvailabilityZones>
          <member>us-east-1b</member>
          <member>us-east-1a</member>
        </AvailabilityZones>
        <LoadBalancerNames>
          <member>my-test-asg-loadbalancer</member>
        </LoadBalancerNames>
        <MinSize>2</MinSize>
        <VPCZoneIdentifier/>
        <HealthCheckGracePeriod>120</HealthCheckGracePeriod>
        <DefaultCooldown>300</DefaultCooldown>
        <AutoScalingGroupARN>arn:aws:autoscaling:us-east-1:803981987763:autoScalingGroup:ca861182-c8f9-4ca7-b1eb-cd35505f5ebb:autoScalingGroupName/my-test-asg-lbs</AutoScalingGroupARN>
        <TerminationPolicies>
          <member>Default</member>
        </TerminationPolicies>
        <MaxSize>10</MaxSize>
      </member>
    </AutoScalingGroups>
  </DescribeAutoScalingGroupsResult>
  <ResponseMetadata>
    <RequestId>0f02a07d-b677-11e2-9eb0-dd50EXAMPLE</RequestId>
  </ResponseMetadata>
</DescribeAutoScalingGroupsResponse>",  
    {ok, [[
            {autoscaling_group_name, "my-test-asg-lbs"},
            {autoscaling_group_arn, "arn:aws:autoscaling:us-east-1:803981987763:autoScalingGroup:ca861182-c8f9-4ca7-b1eb-cd35505f5ebb:autoScalingGroupName/my-test-asg-lbs"},
            {launch_configuration_name, "my-test-lc"},
            {min_size, 2},
            {max_size, 10},
            {create_time,{{2013,5,6},{17,47,15}}},
            {health_check_type, "ELB"},
            {desired_capacity,2},
            {placement_group,[]},
            {status,[]},
            {subnets, []},
            {availability_zones,["us-east-1b","us-east-1a"]},
            {load_balancers,["my-test-asg-loadbalancer"]},
            {instances,[]},
            {tag_set,[]}
         ]]}})],
    %% Remaining AWS API examples return subsets of the same data
    output_tests(?_f(erlcloud_autoscaling:describe_autoscaling_groups()), Tests).

% DescribeLaunchConfigurations test based on the API examples:
% http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeLaunchConfigurations.html
describe_launch_configurations_all_input_tests(_) ->
    Tests = 
        [?_autoscaling_test(
            {"Test retrieves a list of launch configurations in a region.",
            ?_f(erlcloud_autoscaling:describe_launch_configurations_all()),
            [{"Action", "DescribeLaunchConfigurations"}]}),
        ?_autoscaling_test(
            {"Test retrieves a specific launch configurations in a region.",
            ?_f(erlcloud_autoscaling:describe_launch_configurations_all(["my-test-lc"])),
            [{"Action", "DescribeLaunchConfigurations"},
             {"LaunchConfigurationNames.member.1", "my-test-lc"}]})
        ],
    Response = "
        <DescribeLaunchConfigurationsResponse xmlns=\"http://autoscaling.amazonaws.com/doc/2011-01-01/\">
          <DescribeLaunchConfigurationsResult>
            <LaunchConfigurations/>
          </DescribeLaunchConfigurationsResult>
          <ResponseMetadata>
            <RequestId>d05a22f8-b690-11e2-bf8e-2113fEXAMPLE</RequestId>
          </ResponseMetadata>
        </DescribeLaunchConfigurationsResponse>",
    input_tests(Response, Tests).

describe_launch_configurations_all_output_tests(_) ->
    Tests = 
        [?_autoscaling_test(
            {"This example describes all launch configurations in a region.", ["
                <DescribeLaunchConfigurationsResponse xmlns=\"http://autoscaling.amazonaws.com/doc/2011-01-01/\">
                  <DescribeLaunchConfigurationsResult>
                    <NextToken>foobar1</NextToken>
                    <LaunchConfigurations>
                      <member>
                        <AssociatePublicIpAddress>true</AssociatePublicIpAddress>
                        <SecurityGroups/>
                        <PlacementTenancy>dedicated</PlacementTenancy>
                        <CreatedTime>2013-01-21T23:04:42.200Z</CreatedTime>
                        <KernelId/>
                        <LaunchConfigurationName>my-test-lc1</LaunchConfigurationName>
                        <UserData/>
                        <InstanceType>m1.small</InstanceType>
                        <LaunchConfigurationARN>arn:aws:autoscaling:us-east-1:803981987763:launchConfiguration:9dbbbf87-6141-428a-a409-0752edbe6cad:launchConfigurationName/my-test-lc</LaunchConfigurationARN>
                        <BlockDeviceMappings/>
                        <ImageId>ami-514ac838</ImageId>
                        <KeyName/>
                        <RamdiskId/>
                        <InstanceMonitoring>
                          <Enabled>true</Enabled>
                        </InstanceMonitoring>
                        <EbsOptimized>false</EbsOptimized>
                      </member>
                    </LaunchConfigurations>
                  </DescribeLaunchConfigurationsResult>
                  <ResponseMetadata>
                    <RequestId>d05a22f8-b690-11e2-bf8e-2113fEXAMPLE</RequestId>
                  </ResponseMetadata>
                </DescribeLaunchConfigurationsResponse>","
                <DescribeLaunchConfigurationsResponse xmlns=\"http://autoscaling.amazonaws.com/doc/2011-01-01/\">
                  <DescribeLaunchConfigurationsResult>
                    <NextToken>foobar2</NextToken>
                    <LaunchConfigurations>
                      <member>
                        <AssociatePublicIpAddress>true</AssociatePublicIpAddress>
                        <SecurityGroups/>
                        <PlacementTenancy>dedicated</PlacementTenancy>
                        <CreatedTime>2013-01-21T23:04:42.200Z</CreatedTime>
                        <KernelId/>
                        <LaunchConfigurationName>my-test-lc2</LaunchConfigurationName>
                        <UserData/>
                        <InstanceType>m1.small</InstanceType>
                        <LaunchConfigurationARN>arn:aws:autoscaling:us-east-1:803981987763:launchConfiguration:9dbbbf87-6141-428a-a409-0752edbe6cad:launchConfigurationName/my-test-lc</LaunchConfigurationARN>
                        <BlockDeviceMappings/>
                        <ImageId>ami-514ac838</ImageId>
                        <KeyName/>
                        <RamdiskId/>
                        <InstanceMonitoring>
                          <Enabled>true</Enabled>
                        </InstanceMonitoring>
                        <EbsOptimized>false</EbsOptimized>
                      </member>
                    </LaunchConfigurations>
                  </DescribeLaunchConfigurationsResult>
                  <ResponseMetadata>
                    <RequestId>d05a22f8-b690-11e2-bf8e-2113fEXAMPLE</RequestId>
                  </ResponseMetadata>
                </DescribeLaunchConfigurationsResponse>","
                <DescribeLaunchConfigurationsResponse xmlns=\"http://autoscaling.amazonaws.com/doc/2011-01-01/\">
                  <DescribeLaunchConfigurationsResult>
                    <NextToken>foobar3</NextToken>
                    <LaunchConfigurations>
                      <member>
                        <AssociatePublicIpAddress>true</AssociatePublicIpAddress>
                        <SecurityGroups/>
                        <PlacementTenancy>dedicated</PlacementTenancy>
                        <CreatedTime>2013-01-21T23:04:42.200Z</CreatedTime>
                        <KernelId/>
                        <LaunchConfigurationName>my-test-lc3</LaunchConfigurationName>
                        <UserData/>
                        <InstanceType>m1.small</InstanceType>
                        <LaunchConfigurationARN>arn:aws:autoscaling:us-east-1:803981987763:launchConfiguration:9dbbbf87-6141-428a-a409-0752edbe6cad:launchConfigurationName/my-test-lc</LaunchConfigurationARN>
                        <BlockDeviceMappings/>
                        <ImageId>ami-514ac838</ImageId>
                        <KeyName/>
                        <RamdiskId/>
                        <InstanceMonitoring>
                          <Enabled>true</Enabled>
                        </InstanceMonitoring>
                        <EbsOptimized>false</EbsOptimized>
                      </member>
                    </LaunchConfigurations>
                  </DescribeLaunchConfigurationsResult>
                  <ResponseMetadata>
                    <RequestId>d05a22f8-b690-11e2-bf8e-2113fEXAMPLE</RequestId>
                  </ResponseMetadata>
                </DescribeLaunchConfigurationsResponse>","
                <DescribeLaunchConfigurationsResponse xmlns=\"http://autoscaling.amazonaws.com/doc/2011-01-01/\">
                  <DescribeLaunchConfigurationsResult>
                    <LaunchConfigurations>
                      <member>
                        <AssociatePublicIpAddress>true</AssociatePublicIpAddress>
                        <SecurityGroups/>
                        <PlacementTenancy>dedicated</PlacementTenancy>
                        <CreatedTime>2013-01-21T23:04:42.200Z</CreatedTime>
                        <KernelId/>
                        <LaunchConfigurationName>my-test-lc4</LaunchConfigurationName>
                        <UserData/>
                        <InstanceType>m1.small</InstanceType>
                        <LaunchConfigurationARN>arn:aws:autoscaling:us-east-1:803981987763:launchConfiguration:9dbbbf87-6141-428a-a409-0752edbe6cad:launchConfigurationName/my-test-lc</LaunchConfigurationARN>
                        <BlockDeviceMappings/>
                        <ImageId>ami-514ac838</ImageId>
                        <KeyName/>
                        <RamdiskId/>
                        <InstanceMonitoring>
                          <Enabled>true</Enabled>
                        </InstanceMonitoring>
                        <EbsOptimized>false</EbsOptimized>
                      </member>
                    </LaunchConfigurations>
                  </DescribeLaunchConfigurationsResult>
                  <ResponseMetadata>
                    <RequestId>d05a22f8-b690-11e2-bf8e-2113fEXAMPLE</RequestId>
                  </ResponseMetadata>
                </DescribeLaunchConfigurationsResponse>"],
            {ok, [[
                    {launch_configuration_name, "my-test-lc1"},
                    {launch_configuration_arn, "arn:aws:autoscaling:us-east-1:803981987763:launchConfiguration:9dbbbf87-6141-428a-a409-0752edbe6cad:launchConfigurationName/my-test-lc"},
                    {image_id, "ami-514ac838"},
                    {kernel_id, []},
                    {ramdisk_id, []},
                    {instance_type, "m1.small"},
                    {iam_instance_profile, []},
                    {associate_public_ip_address, true},
                    {placement_tenancy, "dedicated"},
                    {create_time, {{2013,1,21},{23,4,42}}},
                    {key_name, []},
                    {user_data, []},
                    {security_groups, []}

                  ],[
                    {launch_configuration_name, "my-test-lc2"},
                    {launch_configuration_arn, "arn:aws:autoscaling:us-east-1:803981987763:launchConfiguration:9dbbbf87-6141-428a-a409-0752edbe6cad:launchConfigurationName/my-test-lc"},
                    {image_id, "ami-514ac838"},
                    {kernel_id, []},
                    {ramdisk_id, []},
                    {instance_type, "m1.small"},
                    {iam_instance_profile, []},
                    {associate_public_ip_address, true},
                    {placement_tenancy, "dedicated"},
                    {create_time, {{2013,1,21},{23,4,42}}},
                    {key_name, []},
                    {user_data, []},
                    {security_groups, []}

                  ],[
                    {launch_configuration_name, "my-test-lc3"},
                    {launch_configuration_arn, "arn:aws:autoscaling:us-east-1:803981987763:launchConfiguration:9dbbbf87-6141-428a-a409-0752edbe6cad:launchConfigurationName/my-test-lc"},
                    {image_id, "ami-514ac838"},
                    {kernel_id, []},
                    {ramdisk_id, []},
                    {instance_type, "m1.small"},
                    {iam_instance_profile, []},
                    {associate_public_ip_address, true},
                    {placement_tenancy, "dedicated"},
                    {create_time, {{2013,1,21},{23,4,42}}},
                    {key_name, []},
                    {user_data, []},
                    {security_groups, []}
            
                  ],[
                    {launch_configuration_name, "my-test-lc4"},
                    {launch_configuration_arn, "arn:aws:autoscaling:us-east-1:803981987763:launchConfiguration:9dbbbf87-6141-428a-a409-0752edbe6cad:launchConfigurationName/my-test-lc"},
                    {image_id, "ami-514ac838"},
                    {kernel_id, []},
                    {ramdisk_id, []},
                    {instance_type, "m1.small"},
                    {iam_instance_profile, []},
                    {associate_public_ip_address, true},
                    {placement_tenancy, "dedicated"},
                    {create_time, {{2013,1,21},{23,4,42}}},
                    {key_name, []},
                    {user_data, []},
                    {security_groups, []}

         ]]}})],
    %% Remaining AWS API examples return subsets of the same data
    output_tests_seq(?_f(erlcloud_autoscaling:describe_launch_configurations_all()), Tests).

% DescribeLaunchConfigurations pagination version test based on the API examples:
% http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeLaunchConfigurations.html
describe_launch_configurations_input_tests(_) ->
    Tests = 
        [?_autoscaling_test(
            {"Test retrieves first bunch of launch configurations in a region.",
            ?_f(erlcloud_autoscaling:describe_launch_configurations()),
            [{"Action", "DescribeLaunchConfigurations"}]}),
        ?_autoscaling_test(
            {"Test retrieves a specific launch configurations in a region.",
            ?_f(erlcloud_autoscaling:describe_launch_configurations(["my-test-lc"])),
            [{"Action", "DescribeLaunchConfigurations"},
             {"LaunchConfigurationNames.member.1", "my-test-lc"}]})
        ],
    Response = "
        <DescribeLaunchConfigurationsResponse xmlns=\"http://autoscaling.amazonaws.com/doc/2011-01-01/\">
          <DescribeLaunchConfigurationsResult>
            <LaunchConfigurations/>
          </DescribeLaunchConfigurationsResult>
          <ResponseMetadata>
            <RequestId>d05a22f8-b690-11e2-bf8e-2113fEXAMPLE</RequestId>
          </ResponseMetadata>
        </DescribeLaunchConfigurationsResponse>",
    input_tests(Response, Tests).

describe_launch_configurations_output_tests(_) ->
    Tests = 
        [?_autoscaling_test(
            {"This example describes first bunch of autoscaling groups in a region.", "
                <DescribeLaunchConfigurationsResponse xmlns=\"http://autoscaling.amazonaws.com/doc/2011-01-01/\">
                  <DescribeLaunchConfigurationsResult>
                    <LaunchConfigurations>
                      <member>
                        <AssociatePublicIpAddress>true</AssociatePublicIpAddress>
                        <SecurityGroups/>
                        <PlacementTenancy>dedicated</PlacementTenancy>
                        <CreatedTime>2013-01-21T23:04:42.200Z</CreatedTime>
                        <KernelId/>
                        <LaunchConfigurationName>my-test-lc</LaunchConfigurationName>
                        <UserData/>
                        <InstanceType>m1.small</InstanceType>
                        <LaunchConfigurationARN>arn:aws:autoscaling:us-east-1:803981987763:launchConfiguration:9dbbbf87-6141-428a-a409-0752edbe6cad:launchConfigurationName/my-test-lc</LaunchConfigurationARN>
                        <BlockDeviceMappings/>
                        <ImageId>ami-514ac838</ImageId>
                        <KeyName/>
                        <RamdiskId/>
                        <InstanceMonitoring>
                          <Enabled>true</Enabled>
                        </InstanceMonitoring>
                        <EbsOptimized>false</EbsOptimized>
                      </member>
                    </LaunchConfigurations>
                  </DescribeLaunchConfigurationsResult>
                  <ResponseMetadata>
                    <RequestId>d05a22f8-b690-11e2-bf8e-2113fEXAMPLE</RequestId>
                  </ResponseMetadata>
                </DescribeLaunchConfigurationsResponse>",
            {ok, [[
                    {launch_configuration_name, "my-test-lc"},
                    {launch_configuration_arn, "arn:aws:autoscaling:us-east-1:803981987763:launchConfiguration:9dbbbf87-6141-428a-a409-0752edbe6cad:launchConfigurationName/my-test-lc"},
                    {image_id, "ami-514ac838"},
                    {kernel_id, []},
                    {ramdisk_id, []},
                    {instance_type, "m1.small"},
                    {iam_instance_profile, []},
                    {associate_public_ip_address, true},
                    {placement_tenancy, "dedicated"},
                    {create_time, {{2013,1,21},{23,4,42}}},
                    {key_name, []},
                    {user_data, []},
                    {security_groups, []}

         ]]}})],
    %% Remaining AWS API examples return subsets of the same data
    output_tests(?_f(erlcloud_autoscaling:describe_launch_configurations()), Tests).
