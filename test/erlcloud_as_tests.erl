-module(erlcloud_as_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud_as.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

%% Some simple unit tests for autoscaling functions.
%% These mostly just test correct parsing based on the AWS sample documents.
%% I thought that this would be at least somewhat useful for whenever I or
%% someone else wants to expand the various records to capture more of the
%% information provided by the AWS autoscaling APIs.
%% 
%% I've taken a bit of a shortcut here in mocking only the function
%% erlcloud_aws:aws_request_xml2 as I figured the other tests cover
%% the underlying HTTP well enough.

autoscaling_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun description_tests/1,
      fun terminate_tests/1]}.

start() ->
    meck:new(erlcloud_aws, [non_strict]),
    mocked_aws_xml().

stop(_) ->
    meck:unload(erlcloud_aws).

extract_result({ok, Res}) ->
    Res.

description_tests(_) ->
    [
     fun() ->
             ?assertEqual(extract_result(erlcloud_as:describe_groups()), 
                          expected_groups()) end,
     fun() ->
             ?assertEqual(extract_result(erlcloud_as:describe_instances()),
                          expected_instances()) end,
    fun() ->
            ?assertEqual(extract_result(erlcloud_as:describe_launch_configs()),
                         expected_launch_configs()) end].

terminate_tests(_) ->
     [fun() ->
             Res = extract_result(erlcloud_as:terminate_instance("i-bdae7a84", true)),
             ?assertEqual(Res, expected_activity()) end].
    
mocked_aws_xml() ->
    meck:expect(erlcloud_aws, default_config, [{[], #aws_config{}}]),
    meck:expect(erlcloud_aws, aws_request_xml4, [
                                                 mocked_groups(), 
                                                 mocked_instances(),
                                                 mocked_launch_configs(),
                                                 mocked_activity()]).

parsed_mock_response(Text) ->
    {ok, element(1, xmerl_scan:string(Text))}.

mocked_groups() ->
    {[post, '_', "/", [
                       {"Action", "DescribeAutoScalingGroups"}, 
                       {"Version", '_'}, 
                       {"MaxRecords", '_'}], 
      "autoscaling", '_'], parsed_mock_response("
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
        <AutoScalingGroupARN>arn:aws:autoscaling:us-east-1:803981987763:autoScalingGroup:ca861182-c8f9-4ca7-b1eb-cd35505f5ebb
        :autoScalingGroupName/my-test-asg-lbs</AutoScalingGroupARN>
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
</DescribeAutoScalingGroupsResponse>")}.

expected_groups() ->
    [#aws_autoscaling_group{
        group_name = "my-test-asg-lbs",
        availability_zones = ["us-east-1b", "us-east-1a"],
        load_balancer_names = ["my-test-asg-loadbalancer"],
        instances = [],
        tags = [],
        desired_capacity = 2,
        min_size = 2,
        max_size = 10}].

mocked_instances() ->
    {[post, '_', "/", [
                       {"Action", "DescribeAutoScalingInstances"}, 
                       {"Version", '_'}, 
                       {"MaxRecords", '_'}], 
      "autoscaling", '_'], {ok, element(1, xmerl_scan:string("
<DescribeAutoScalingInstancesResponse xmlns=\"http://autoscaling.amazonaws.com/doc/2011-01-01/\">
  <DescribeAutoScalingInstancesResult>
    <AutoScalingInstances>
      <member>
        <HealthStatus>Healthy</HealthStatus>
        <AutoScalingGroupName>my-test-asg</AutoScalingGroupName>
        <AvailabilityZone>us-east-1e</AvailabilityZone>
        <InstanceId>i-78e0d40b</InstanceId>
        <LaunchConfigurationName>my-test-lc</LaunchConfigurationName>
        <LifecycleState>InService</LifecycleState>
      </member>
    </AutoScalingInstances>
  </DescribeAutoScalingInstancesResult>
  <ResponseMetadata>
    <RequestId>df992dc3-b72f-11e2-81e1-750aa6EXAMPLE</RequestId>
  </ResponseMetadata>
</DescribeAutoScalingInstancesResponse>"))}}.

expected_instances() ->
    [#aws_autoscaling_instance{
        instance_id = "i-78e0d40b",
        launch_config_name = "my-test-lc",
        group_name = "my-test-asg",
        availability_zone = "us-east-1e",
        health_status = "Healthy",
        lifecycle_state = "InService"}].

mocked_launch_configs() ->
    {[post, '_', "/", [
                       {"Action", "DescribeLaunchConfigurations"}, 
                       {"Version", '_'}, 
                       {"MaxRecords", '_'}], 
      "autoscaling", '_'], parsed_mock_response("
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
        <LaunchConfigurationARN>arn:aws:autoscaling:us-east-1:803981987763:launchConfiguration:
        9dbbbf87-6141-428a-a409-0752edbe6cad:launchConfigurationName/my-test-lc</LaunchConfigurationARN>
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
</DescribeLaunchConfigurationsResponse>")}.

expected_launch_configs() ->
    [#aws_launch_config{
        name = "my-test-lc",
        image_id = "ami-514ac838",
        instance_type = "m1.small",
        tenancy = "dedicated"}].

mocked_activity() ->
    {[post, '_', "/", [
                       {"Action", "TerminateInstanceInAutoScalingGroup"}, 
                       {"Version", "2011-01-01"}, 
                       {"InstanceId", "i-bdae7a84"}, 
                       {"ShouldDecrementDesiredCapacity", "true"}],
      "autoscaling", '_'], parsed_mock_response("
<TerminateInstanceInAutoScalingGroupResponse xmlns=\"http://autoscaling.amazonaws.com/doc/2011-01-01/\">
  <TerminateInstanceInAutoScalingGroupResult>
    <Activity>
      <ActivityId>108433a0-2a4e-451f-94be-0c3931a86614</ActivityId>
      <StatusCode>InProgress</StatusCode>
      <StartTime>2014-07-29T22:40:09.571Z</StartTime>
      <Progress>0</Progress>
      <Cause>At 2014-07-29T22:40:09Z instance i-bdae7a84 was taken out of service in response to a user request, shrinking the capacity from 1 to 0.</Cause>
      <Details>{&quot;Availability Zone&quot;:&quot;us-east-1c&quot;}</Details>
      <Description>Terminating EC2 instance: i-bdae7a84</Description>
    </Activity>
  </TerminateInstanceInAutoScalingGroupResult>
  <ResponseMetadata>
    <RequestId>4b9ae945-1771-15f4-cf8f-692b7d0854fb</RequestId>
  </ResponseMetadata>
</TerminateInstanceInAutoScalingGroupResponse>")}.

expected_activity() ->
    #aws_autoscaling_activity{
        id = "108433a0-2a4e-451f-94be-0c3931a86614",
        group_name = [],
        cause = "At 2014-07-29T22:40:09Z instance i-bdae7a84 was taken out of service in response to a user request, shrinking the capacity from 1 to 0.",
        description = "Terminating EC2 instance: i-bdae7a84",
        details = "{\"Availability Zone\":\"us-east-1c\"}",
        status_code = "InProgress",
        status_msg = [],
        start_time = {{2014, 07, 29}, {22, 40, 09}},
        end_time = undefined,
        progress = 0}.
