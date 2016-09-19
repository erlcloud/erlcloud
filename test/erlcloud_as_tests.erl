-module(erlcloud_as_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud_as.hrl").
-include("erlcloud_aws.hrl").

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
      fun terminate_tests/1,
      fun create_tests/1,
      fun detach_instances_tests/1,
      fun lifecycle_hooks_tests/1]}.

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
                          expected_launch_configs()) end,
     fun() ->
             ?assertEqual(extract_result(erlcloud_as:describe_scaling_activities("my-test-asg", 20, #aws_config{})),
                          expected_scaling_activity()) end
    ].

terminate_tests(_) ->
     [fun() ->
             Res = extract_result(erlcloud_as:terminate_instance("i-bdae7a84", true)),
             ?assertEqual(Res, expected_activity()) end].

create_tests(_) ->
    [fun() ->
             LC = #aws_launch_config{
                     name = "my-test-lc",
                     image_id = "ami-0078da69",
                     instance_type = "m1.small",
                     public_ip_address = true,
                     monitoring = false
                  },
             R = erlcloud_as:create_launch_config(LC, #aws_config{}),
             ?assertEqual(ok,R)
     end,
     fun() ->
             ASG = #aws_autoscaling_group{
                      group_name = "my-test-asg",
                      launch_configuration_name = "my-test-lc",
                      max_size = 10,
                      min_size = 1,
                      tags = [#aws_autoscaling_tag{'key' = "FOO",
                                                   'value' = "BAR",
                                                   'propogate_at_launch' = true}
                             ]
                   },
             R = erlcloud_as:create_auto_scaling_group(ASG, #aws_config{}),
             ?assertEqual(ok, R)
     end
    ].
                          
                          
    
detach_instances_tests(_) ->
     [fun() ->
             Res = extract_result(erlcloud_as:detach_instances(["i-bdae7a84"], "my-test-asg-lbs", false)),
             ?assertEqual(Res, expected_detach_activities()) end].
    
lifecycle_hooks_tests(_) ->
     [fun() ->
             Res = extract_result(erlcloud_as:describe_lifecycle_hooks("my-test-asg-lbs", ["TestHook"])),
             ?assertEqual(Res, expected_describe_lifecycle_hooks()) end,
      fun() ->
             Res = extract_result(erlcloud_as:complete_lifecycle_action("my-test-asg-lbs", "CONTINUE", "TestHook", {instance_id, "i-bdae7a84"})),
             ?assertEqual(Res, expected_complete_lifecycle_action()) end].
    
mocked_aws_xml() ->
    meck:expect(erlcloud_aws, default_config, [{[], #aws_config{}}]),
    meck:expect(erlcloud_aws, aws_request_xml4, [
                                                 mocked_groups(), 
                                                 mocked_instances(),
                                                 mocked_launch_configs(),
                                                 mocked_activity(),
                                                 mocked_scaling_activity(),
                                                 mocked_create_launch_config(),
                                                 mocked_create_asg(),
                                                 mocked_detach_instances(),
                                                 mocked_describe_lifecycle_hooks(),
                                                 mocked_complete_lifecycle_action()]).

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
        tags = [],
        desired_capacity = 2,
        min_size = 2,
        max_size = 10,
        launch_configuration_name = "my-test-lc",
        vpc_zone_id = [""],
        instances = [],
        status = []
        
}].

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
        tenancy = "dedicated",
        user_data = "",
        security_groups = [],
        public_ip_address = true,
        monitoring = true
       }].

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

mocked_scaling_activity() ->
    {[post, '_', "/", [
                       {"Action", "DescribeScalingActivities"}, 
                       {"Version", "2011-01-01"},
                       {"AutoScalingGroupName", "my-test-asg"},
                       {"MaxRecords", "20"}
                      ],
      "autoscaling", '_'], parsed_mock_response("
<DescribeScalingActivitiesResponse xmlns=\"http://ec2.amazonaws.com/doc/2011-01-01/\">
<DescribeScalingActivitiesResult>
<Activities>
   <member>
     <StatusCode>Failed</StatusCode>
     <Progress>0</Progress>
     <ActivityId>063308ae-aa22-4a9b-94f4-9faeEXAMPLE</ActivityId>
     <StartTime>2012-04-12T17:32:07.882Z</StartTime>
     <AutoScalingGroupName>my-test-asg</AutoScalingGroupName>
     <Cause>At 2012-04-12T17:31:30Z a user request created an AutoScalingGroup changing the desired capacity from 0 to 1.  At 2012-04-12T17:32:07Z an instance was started in response to a difference between desired and actual capacity, increasing the capacity from 0 to 1.</Cause>
     <Details>{}</Details>
     <Description>Launching a new EC2 instance.  Status Reason: The image id 'ami-4edb0327' does not exist. Launching EC2 instance failed.</Description>
     <EndTime>2012-04-12T17:32:08Z</EndTime>
     <StatusMessage>The image id 'ami-4edb0327' does not exist. Launching EC2 instance failed.</StatusMessage>
   </member>
</Activities>
  </DescribeScalingActivitiesResult>
  <ResponseMetadata>
   <RequestId>7a641adc-84c5-11e1-a8a5-217ebEXAMPLE</RequestId>
  </ResponseMetadata>
</DescribeScalingActivitiesResponse>")}.

mocked_create_launch_config() ->
    {[post, '_', "/", [
                       {"Action", "CreateLaunchConfiguration"}, 
                       {"Version", '_'},
                       {"LaunchConfigurationName", "my-test-lc"},
                       {"ImageId", "ami-0078da69"},
                       {"InstanceType", "m1.small"},
                       {"AssociatePublicIpAddress", "true"},
                       {"InstanceMonitoring.Enabled","false"}
                      ],
      "autoscaling", '_'], parsed_mock_response("
<CreateLaunchConfigurationResponse xmlns=\"http://autoscaling.amazonaws.com/doc/2011-01-01/\">
<ResponseMetadata>
   <RequestId>7c6e177f-f082-11e1-ac58-3714bEXAMPLE</RequestId>
</ResponseMetadata>
</CreateLaunchConfigurationResponse>")}.

mocked_create_asg() ->
    {[post, '_', "/", [
                       {"Action", "CreateAutoScalingGroup"}, 
                       {"Version", '_'},
                       {"AutoScalingGroupName", "my-test-asg"},
                       {"LaunchConfigurationName", "my-test-lc"},
                       {"MaxSize", "10"},
                       {"MinSize", "1"},
                       {"Tags.member.1.Key", "FOO"},
                       {"Tags.member.1.Value", "BAR"},
                       {"Tags.member.1.PropageteAtLaunch", "true"}
                      ],
      "autoscaling", '_'], parsed_mock_response("
<CreateAutoScalingGroupResponse xmlns=\"http://autoscaling.amazonaws.com/doc/2011-01-01/\">
<ResponseMetadata>
<RequestId>8d798a29-f083-11e1-bdfb-cb223EXAMPLE</RequestId>
</ResponseMetadata>
</CreateAutoScalingGroupResponse>")}.



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

expected_scaling_activity() ->
    [#aws_autoscaling_activity{
        id = "063308ae-aa22-4a9b-94f4-9faeEXAMPLE",
        group_name = "my-test-asg",
        cause = "At 2012-04-12T17:31:30Z a user request created an AutoScalingGroup changing the desired capacity from 0 to 1.  At 2012-04-12T17:32:07Z an instance was started in response to a difference between desired and actual capacity, increasing the capacity from 0 to 1.",
        description = "Launching a new EC2 instance.  Status Reason: The image id 'ami-4edb0327' does not exist. Launching EC2 instance failed.",
        details = "{}",
        status_code = "Failed",
        status_msg = "The image id 'ami-4edb0327' does not exist. Launching EC2 instance failed.",
        start_time = {{2012,04,12}, {17,32,07}},
        end_time = {{2012,04,12}, {17,32,08}},
        progress = 0
      }].
mocked_detach_instances() ->
    {[post, '_', "/", [
                       {"Action", "DetachInstances"},
                       {"Version", "2011-01-01"},
                       {"ShouldDecrementDesiredCapacity", "false"},
                       {"AutoScalingGroupName", "my-test-asg-lbs"},
                       {"InstanceIds.member.1", "i-bdae7a84"}],
      "autoscaling", '_'], parsed_mock_response("
<DetachInstancesResponse xmlns=\"http://autoscaling.amazonaws.com/doc/2011-01-01/\">
  <DetachInstancesResult>
      <Activities>
            <member>
                    <ActivityId>b8bd9d0c-194c-4b9a-a3e7-de6b960b4ea5</ActivityId>
                            <AutoScalingGroupName>my-test-asg-lbs</AutoScalingGroupName>
                                    <Description>Detaching EC2 instance: i-bdae7a84</Description>
                                            <Progress>50</Progress>
                                                    <Cause>At 2016-02-03T16:10:03Z instance i-bdae7a84 was detached in response to a user request, shrinking the capacity from 2 to 1.</Cause>
        <StartTime>2016-02-03T16:10:03.901Z</StartTime>
        <Details>{&quot;Availability Zone&quot;:&quot;eu-west-1a&quot;}</Details>
        <StatusCode>InProgress</StatusCode>
      </member>
    </Activities>
  </DetachInstancesResult>
  <ResponseMetadata>
    <RequestId>959cc9dc-ca90-11e5-b1ab-619935a1f6db</RequestId>
  </ResponseMetadata>
</DetachInstancesResponse>")}.

expected_detach_activities() ->
    [#aws_autoscaling_activity{
        id = "b8bd9d0c-194c-4b9a-a3e7-de6b960b4ea5",
        group_name = "my-test-asg-lbs",
        cause = "At 2016-02-03T16:10:03Z instance i-bdae7a84 was detached in response to a user request, shrinking the capacity from 2 to 1.",
        description = "Detaching EC2 instance: i-bdae7a84",
        details = "{\"Availability Zone\":\"eu-west-1a\"}",
        status_code = "InProgress",status_msg = [],
        start_time = {{2016,2,3},{16,10,3}},
        end_time = undefined,progress = 50}].


mocked_describe_lifecycle_hooks() ->
    {[post, '_', "/", [
                       {"Action", "DescribeLifecycleHooks"},
                       {"Version", "2011-01-01"},
                       {"AutoScalingGroupName", "my-test-asg-lbs"},
                       {"LifecycleHookNames.member.1", "TestHook"}],
      "autoscaling", '_'], parsed_mock_response("
<DescribeLifecycleHooksResponse xmlns=\"http://autoscaling.amazonaws.com/doc/2011-01-01/\">
  <DescribeLifecycleHooksResult>
    <LifecycleHooks>
      <member>
        <AutoScalingGroupName>my-test-asg-lbs</AutoScalingGroupName>
        <LifecycleTransition>autoscaling:EC2_INSTANCE_LAUNCHING</LifecycleTransition>
        <GlobalTimeout>172800</GlobalTimeout>
        <LifecycleHookName>TestHook</LifecycleHookName>
        <HeartbeatTimeout>3600</HeartbeatTimeout>
        <DefaultResult>ABANDON</DefaultResult>
      </member>
    </LifecycleHooks>
  </DescribeLifecycleHooksResult>
  <ResponseMetadata>
    <RequestId>ff028e98-1dc1-11e6-89f3-81231a887c98</RequestId>
  </ResponseMetadata>
</DescribeLifecycleHooksResponse>")}.

expected_describe_lifecycle_hooks() ->
    [#aws_autoscaling_lifecycle_hook{
        group_name = "my-test-asg-lbs",
        lifecycle_hook_name = "TestHook",
        global_timeout = 172800,
        heartbeat_timeout = 3600,
        default_result = "ABANDON",
        lifecycle_transition = "autoscaling:EC2_INSTANCE_LAUNCHING"}].

mocked_complete_lifecycle_action() ->
    {[post, '_', "/", [
                       {"Action", "CompleteLifecycleAction"},
                       {"Version", "2011-01-01"},
                       {"AutoScalingGroupName", "my-test-asg-lbs"},
                       {"LifecycleActionResult", "CONTINUE"},
                       {"LifecycleHookName", "TestHook"},
                       {"InstanceId", "i-bdae7a84"}],
      "autoscaling", '_'], parsed_mock_response("
<CompleteLifecycleActionResponse xmlns=\"http://autoscaling.amazonaws.com/doc/2011-01-01/\">
  <CompleteLifecycleActionResult/>
  <ResponseMetadata>
    <RequestId>ff028e98-1dc1-11e6-89f3-81231a887c98</RequestId>
  </ResponseMetadata>
</CompleteLifecycleActionResponse>")}.

expected_complete_lifecycle_action() ->
    "ff028e98-1dc1-11e6-89f3-81231a887c98".