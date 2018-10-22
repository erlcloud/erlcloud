-module(erlcloud_as).

-include("erlcloud_aws.hrl").
-include("erlcloud_as.hrl").

-include("erlcloud_xmerl.hrl").

%% AWS Autoscaling functions
-export([describe_groups/0, describe_groups/1, describe_groups/2, describe_groups/4,
         set_desired_capacity/2, set_desired_capacity/3, set_desired_capacity/4,

         describe_launch_configs/0, describe_launch_configs/1, describe_launch_configs/2, 
         describe_launch_configs/4,

         describe_instances/0, describe_instances/1, describe_instances/2, 
         describe_instances/4,
         terminate_instance/1, terminate_instance/2, terminate_instance/3,

         create_launch_config/2,
         create_auto_scaling_group/2,
         update_auto_scaling_group/2,
         delete_launch_configuration/2,
         delete_auto_scaling_group/3,
         describe_scaling_activities/3,

         suspend_processes/1, suspend_processes/2, suspend_processes/3,
         resume_processes/1, resume_processes/2, resume_processes/3,
         detach_instances/2, detach_instances/3, detach_instances/4,

         describe_lifecycle_hooks/1, describe_lifecycle_hooks/2, describe_lifecycle_hooks/3,
         complete_lifecycle_action/4, complete_lifecycle_action/5
]).

-define(API_VERSION, "2011-01-01").
-define(DEFAULT_MAX_RECORDS, 20).

% xpath for group descriptions used in describe_groups functions:
-define(DESCRIBE_GROUPS_PATH, 
        "/DescribeAutoScalingGroupsResponse/DescribeAutoScalingGroupsResult/AutoScalingGroups/member").
-define(DESCRIBE_GROUPS_NEXT_TOKEN, 
        "/DescribeAutoScalingGroupsResponse/DescribeAutoScalingGroupsResult/NextToken").
% xpath for the request ID returned from a SetDesiredCapacity operation:
-define(SET_SCALE_REQUEST_ID_PATH, "/SetDesiredCapacityResponse/ResponseMetadata/RequestId").

%% xpath for launch config functions:
-define(DESCRIBE_LAUNCH_CONFIG_PATH, 
        "/DescribeLaunchConfigurationsResponse/DescribeLaunchConfigurationsResult/LaunchConfigurations/member").
-define(LAUNCH_CONFIG_NEXT_TOKEN,
        "/DescribeLaunchConfigurationsResponse/DescribeLaunchConfigurationsResult/NextToken").

%% xpath for autoscaling instance description:
-define(DESCRIBE_INSTANCES,
        "/DescribeAutoScalingInstancesResponse/DescribeAutoScalingInstancesResult/AutoScalingInstances/member").
-define(DESCRIBE_INSTANCES_NEXT_TOKEN,
        "/DescribeAutoScalingInstancesResponse/DescribeAutoScalingInstancesResult/NextToken").

%% xpath for terminate instance:
-define(TERMINATE_INSTANCE_ACTIVITY, 
        "/TerminateInstanceInAutoScalingGroupResponse/TerminateInstanceInAutoScalingGroupResult/Activity").

%% xpath for describe scaling activity:
-define(DESCRIBE_SCALING_ACTIVITIES,
        "/DescribeScalingActivitiesResponse/DescribeScalingActivitiesResult/Activities/member").

%% xpath for suspend and resume ScalingProcesses:
-define(SUSPEND_PROCESSES_ACTIVITY, 
        "/SuspendProcessesResponse/ResponseMetadata/RequestId").
-define(RESUME_PROCESSES_ACTIVITY, 
        "/ResumeProcessesResponse/ResponseMetadata/RequestId").

%% xpath for detach instances:
-define(DETACH_INSTANCES_ACTIVITY, 
        "/DetachInstancesResponse/DetachInstancesResult/Activities/member").

-define(DESCRIBE_LIFECYCLE_HOOKS_PATH, 
        "/DescribeLifecycleHooksResponse/DescribeLifecycleHooksResult/LifecycleHooks/member").

-define(COMPLETE_LIFECYCLE_ACTION_ACTIVITY, 
        "/CompleteLifecycleActionResponse/ResponseMetadata/RequestId").


%% --------------------------------------------------------------------
%% @doc Calls describe_groups([], default_configuration())
%% @end
%% --------------------------------------------------------------------
describe_groups() ->
    describe_groups([], erlcloud_aws:default_config()).

%% --------------------------------------------------------------------
%% @doc describe_groups with a specific configuration OR with a 
%% specific list of members.
%% @end
%% --------------------------------------------------------------------
describe_groups(Config) when is_record(Config, aws_config) ->
    describe_groups([], Config);
describe_groups(GroupNames) ->
    describe_groups(GroupNames, erlcloud_aws:default_config()).

%% --------------------------------------------------------------------
%% @doc Get descriptions of the given autoscaling groups.
%%      The account calling this function needs permission for the
%%      autoscaling:DescribeAutoScalingGroups action.
%% 
%% Returns {{paged, NextPageId}, Results} if there are more than
%% the current maximum count of results, {ok, Results} if everything
%% fits and {error, Reason} if there was a problem.
%% @end
%% --------------------------------------------------------------------
-spec describe_groups(list(string()), aws_config()) -> 
                             {ok, term()} | {{paged, string()}, term()} | {error, term()}.
describe_groups(GN, Config) ->
    describe_groups(GN, ?DEFAULT_MAX_RECORDS, none, Config).

%% --------------------------------------------------------------------
%% @doc Get descriptions of the given autoscaling groups with a given
%%      maximum number of results and optional paging offset.
%% @end
%% --------------------------------------------------------------------
-spec describe_groups(list(string()), integer(), string() | none, aws_config()) -> 
                             {ok, term()} | {{paged, string()}, term()} | {error, term()}.
describe_groups(GN, MaxRecords, none, Config) ->
    describe_groups(GN, [{"MaxRecords", MaxRecords}], Config);
describe_groups(GN, MaxRecords, NextToken, Config) ->
    describe_groups(GN, [{"NextToken", NextToken}, {"MaxRecords", MaxRecords}], Config).

-spec describe_groups(list(string()), list({string(), term()}), aws_config()) -> 
                             {ok, term()} | {{paged, string()}, term()} | {error, term()}.
describe_groups(GN, Params, Config) ->
    P = member_params("AutoScalingGroupNames.member.", GN) ++ Params,
    case as_query(Config, "DescribeAutoScalingGroups", P, ?API_VERSION) of
        {ok, Doc} ->
            Groups = xmerl_xpath:string(?DESCRIBE_GROUPS_PATH, Doc),            
            {erlcloud_util:next_token(?DESCRIBE_GROUPS_NEXT_TOKEN, Doc), [extract_group(G) || G <- Groups]};
        {error, Reason} ->
            {error, Reason}
    end.
    
-spec extract_instance(xmerl_xpath_doc_entity() | xmerl_xpath_node_entity()) -> aws_autoscaling_instance().
extract_instance(I) ->
    extract_instance(I, erlcloud_xml:get_text("AutoScalingGroupName", I)).

-spec extract_instance(xmerl_xpath_doc_entity() | xmerl_xpath_node_entity(), string()) -> aws_autoscaling_instance().
extract_instance(I, GroupName) ->
    #aws_autoscaling_instance{
       instance_id = erlcloud_xml:get_text("InstanceId", I),
       launch_config_name = erlcloud_xml:get_text("LaunchConfigurationName", I),
       group_name = GroupName,
       availability_zone = erlcloud_xml:get_text("AvailabilityZone", I),
       health_status = erlcloud_xml:get_text("HealthStatus", I),
       lifecycle_state = erlcloud_xml:get_text("LifecycleState", I)
      }.

-spec extract_group(term()) -> aws_autoscaling_group().
extract_group(G) ->
    #aws_autoscaling_group{
       group_name = erlcloud_xml:get_text("AutoScalingGroupName", G),
       tags = extract_tags_from_group(G),
       availability_zones = 
           [erlcloud_xml:get_text(A) || A <- xmerl_xpath:string("AvailabilityZones/member", G)],
       load_balancer_names = 
           [erlcloud_xml:get_text(L) || L <- xmerl_xpath:string("LoadBalancerNames/member", G)],
       instances =
           [extract_instance(I, erlcloud_xml:get_text("AutoScalingGroupName", G)) ||
                I <- xmerl_xpath:string("Instances/member", G)],
       desired_capacity = erlcloud_xml:get_integer("DesiredCapacity", G),
       min_size = erlcloud_xml:get_integer("MinSize", G),
       max_size = erlcloud_xml:get_integer("MaxSize", G),
       launch_configuration_name = get_text("LaunchConfigurationName", G),
       vpc_zone_id = [ erlcloud_xml:get_text(Zid) || Zid <- xmerl_xpath:string("VPCZoneIdentifier", G)],
       status = get_text("Status", G)
    }.
extract_tags_from_group(G) ->
    [{erlcloud_xml:get_text("Key", T), erlcloud_xml:get_text("Value", T)} || 
        T <- xmerl_xpath:string("Tags/member", G)].

%% --------------------------------------------------------------------
%% @doc set_desired_capacity(GroupName, Capacity, false, default_config())
%% @end
%% --------------------------------------------------------------------
-spec set_desired_capacity(string(), integer()) -> {ok, string()} | {error, term()}.
set_desired_capacity(GroupName, Capacity) ->
    set_desired_capacity(GroupName, Capacity, false, erlcloud_aws:default_config()).

%% --------------------------------------------------------------------
%% @doc set_desired_capacity(GroupName, Capacity, false, Config)
%% @end
%% --------------------------------------------------------------------
-spec set_desired_capacity(string(), integer(), aws_config()) -> {ok, string()} | {error, term()}.
set_desired_capacity(GroupName, Capacity, Config) ->
    set_desired_capacity(GroupName, Capacity, false, Config).

%% --------------------------------------------------------------------
%% @doc Change the desired capacity of the given autoscaling group,
%% optionally ignoring cooldown periods (set false to basically force
%% change).
%% Requires permission for the autoscaling:SetDesiredCapacity action.
%% @end
%% --------------------------------------------------------------------
-spec set_desired_capacity(string(), integer(), boolean(), aws_config()) -> {ok, string()} | {error, term()}.
set_desired_capacity(GroupName, Capacity, HonorCooldown, Config) ->
    Params = [{"AutoScalingGroupName", GroupName}, 
              {"DesiredCapacity", Capacity}, 
              {"HonorCooldown", HonorCooldown}],
    case as_query(Config, "SetDesiredCapacity", Params, ?API_VERSION) of
        {ok, Doc} ->
            [RequestId] = xmerl_xpath:string(?SET_SCALE_REQUEST_ID_PATH, Doc),
            {ok, erlcloud_xml:get_text(RequestId)};
        {error, Reason} ->
            {error, Reason}
    end.

describe_launch_configs() ->
    describe_launch_configs([], erlcloud_aws:default_config()).

describe_launch_configs(Config) when is_record(Config, aws_config) ->
    describe_launch_configs([], Config);
describe_launch_configs(GroupNames) ->
    describe_launch_configs(GroupNames, erlcloud_aws:default_config()).

describe_launch_configs(LN, Config) ->
    describe_launch_configs(LN, ?DEFAULT_MAX_RECORDS, none, Config).

%% --------------------------------------------------------------------
%% @doc Get descriptions of the given launch configurations with a given
%%      maximum number of results and optional paging offset.
%% Pass an empty list of names to get all.
%% @end
%% --------------------------------------------------------------------
describe_launch_configs(LN, MaxRecords, none, Config) ->
    describe_launch_configs(LN, [{"MaxRecords", MaxRecords}], Config);
describe_launch_configs(LN, MaxRecords, NextToken, Config) ->
    describe_launch_configs(LN, [{"MaxRecords", MaxRecords}, {"NextToken", NextToken}], Config).

-spec describe_launch_configs(list(string()), list({string(), term()}), aws_config()) -> 
                                    {ok, list(aws_launch_config())} | 
                                    {{paged, string()}, list(aws_launch_config)} | 
                                    {error, term()}.                                     
describe_launch_configs(LN, Params, Config) ->
    P = member_params("LaunchConfigurationNames.member.", LN) ++ Params,
    case as_query(Config, "DescribeLaunchConfigurations", P, ?API_VERSION) of
        {ok, Doc} ->
            Status = erlcloud_util:next_token(?LAUNCH_CONFIG_NEXT_TOKEN, Doc),
            Configs = [extract_config(C) || C <- xmerl_xpath:string(?DESCRIBE_LAUNCH_CONFIG_PATH, Doc)],
            {Status, Configs};
        {error, Reason} ->
            {error, Reason}
    end.


%% --------------------------------------------------------------------
%% @doc Get scaling activities descriptions of the given autoscaling group with a given
%%      maximum number of results.
%% @end
%% --------------------------------------------------------------------
-spec describe_scaling_activities(string(), non_neg_integer(), aws_config()) ->
                                    {ok, list(aws_autoscaling_activity())} |
                                    {error, term()}.
describe_scaling_activities(GroupName, Count, Config) ->
    Params = [ {"AutoScalingGroupName", GroupName},
               {"MaxRecords", integer_to_list(Count)}
             ],
    describe_scaling_activities(Params, Config).

describe_scaling_activities(Params, Config) ->
    case as_query(Config, "DescribeScalingActivities", Params, ?API_VERSION) of
        {ok, Doc} ->
            Status = erlcloud_util:next_token(?LAUNCH_CONFIG_NEXT_TOKEN, Doc),
            Activities = [ extract_as_activity(A) || A <- xmerl_xpath:string(?DESCRIBE_SCALING_ACTIVITIES, Doc) ],
            {Status, Activities};
        {error, Reason} ->
            {error, Reason}
    end.


%% --------------------------------------------------------------------
%% @doc Delete launch configuration.
%% @end
%% --------------------------------------------------------------------
-spec delete_launch_configuration(string(), aws_config()) ->
                                    ok | {error, term()}.

delete_launch_configuration(Name, Config) ->
    Params = [ {"LaunchConfigurationName", Name} ],
    case as_query(Config, "DeleteLaunchConfiguration", Params, ?API_VERSION) of
        {ok, _Doc} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.


%% --------------------------------------------------------------------
%% @doc Delete AutoScaling group.
%%         ForceDelete Specifies that the group will be deleted along with all instances associated with the group,
%%         without waiting for all instances to be terminated. This parameter also deletes any lifecycle actions associated with the group.
%% @end
%% --------------------------------------------------------------------
-spec delete_auto_scaling_group(string(), boolean(), aws_config()) ->
                                   ok | {error, term()}.    
delete_auto_scaling_group(Name, ForceDelete, Config) ->
    Params = [ {"AutoScalingGroupName", Name},
               {"ForceDelete", atom_to_list(ForceDelete)}
             ],
    case as_query(Config, "DeleteAutoScalingGroup", Params, ?API_VERSION) of
        {ok, _Doc} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.


%% --------------------------------------------------------------------
%% @doc Create launch configuration.
%% @end
%% --------------------------------------------------------------------
-spec create_launch_config(aws_launch_config(), aws_config()) -> ok | {error, term()};
                          (list(), aws_config()) -> ok | {error, term()}.
create_launch_config(#aws_launch_config{
                         name = LCName,
                         image_id = ImageId,
                         instance_type = Type,
                         user_data = UserData,
                         public_ip_address = PublicIP,
                         monitoring = Monitoring,
                         security_groups = SGroups,
                         key_name = KeyPair
                     },
                     Config) ->
    Params = 
        lists:concat([
          [
           {"LaunchConfigurationName", LCName},
           {"ImageId", ImageId},
           {"InstanceType", Type}
          ],
          when_defined(UserData, [{"UserData", UserData}], []),
          when_defined(PublicIP, [{"AssociatePublicIpAddress", atom_to_list(PublicIP)}], []),
          when_defined(Monitoring, [{"InstanceMonitoring.Enabled", atom_to_list(Monitoring)}], []),
          member_params("SecurityGroups.member.", SGroups),
          when_defined(KeyPair, [{"KeyName", KeyPair}], [])
    ]),
    io:format("~p ~n", [Params]),
    create_launch_config(Params, Config);

create_launch_config(Params, Config) ->
    P = Params,
    case as_query(Config, "CreateLaunchConfiguration", P, ?API_VERSION) of
        {ok, _Doc} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.


%% --------------------------------------------------------------------
%% @doc Create AutoScaling group.
%% @end
%% --------------------------------------------------------------------
-spec create_auto_scaling_group(aws_autoscaling_group(), aws_config()) -> ok | {error, term()};
                               (list(), aws_config()) -> ok | {error, term()}.
create_auto_scaling_group(#aws_autoscaling_group{
                              group_name = GName,
                              launch_configuration_name = LaunchName,
                              max_size = MaxSize,
                              min_size = MinSize,
                              vpc_zone_id = VpcZoneIds,
                              availability_zones = AZones,
                              tags = Tags
                          },
                          Config) ->
    ProcessedTags = lists:flatten([tag_to_member_param(T, Idx) || {T, Idx} <- lists:zip(Tags, lists:seq(1, length(Tags)))]),
    Params = lists:concat([
                 [
                  {"AutoScalingGroupName", GName},
                  {"LaunchConfigurationName", LaunchName},
                  {"MaxSize", integer_to_list(MaxSize)},
                  {"MinSize", integer_to_list(MinSize)}
                 ],
                 case VpcZoneIds of
                     undefined -> [];
                     _         ->[{"VPCZoneIdentifier", string:join(VpcZoneIds, ",")}]
                 end,
                 case AZones of
                     undefined -> [];
                     _         -> member_params("AvailabilityZones.member.", AZones)
                 end,
                 ProcessedTags
             ]),
    create_auto_scaling_group(Params, Config);

create_auto_scaling_group(Params, Config) ->
    P = Params,
    case as_query(Config, "CreateAutoScalingGroup", P, ?API_VERSION) of
        {ok, _Doc} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.


%% --------------------------------------------------------------------
%% @doc Update AutoScaling group.
%% @end
%% --------------------------------------------------------------------
-spec update_auto_scaling_group(aws_autoscaling_group() | list({string(), term()}), aws_config()) ->
                                       ok | {error, term()}.
update_auto_scaling_group(#aws_autoscaling_group{
                              group_name = GName,
                              launch_configuration_name = LaunchName,
                              desired_capacity = DesiredCapacity,
                              max_size = MaxSize,
                              min_size = MinSize,
                              vpc_zone_id = VpcZoneIds,
                              availability_zones = AZones
                          },
                          Config) ->
    Params = lists:concat([
                 [
                  {"AutoScalingGroupName", GName}
                 ],
                 case LaunchName of
                     undefined -> [];
                     _         -> [{"LaunchConfigurationName", LaunchName}]
                 end,
                 case DesiredCapacity of
                     undefined -> [];
                     _         -> [{"DesiredCapacity", integer_to_list(DesiredCapacity)}]
                 end,
                 case MaxSize of
                     undefined -> [];
                     _         -> [{"MaxSize", integer_to_list(MaxSize)}]
                 end,
                 case MinSize of
                     undefined -> [];
                     _         -> [{"MinSize", integer_to_list(MinSize)}]
                 end,
                 case VpcZoneIds of
                     undefined -> [];
                     _         ->[{"VPCZoneIdentifier", string:join(VpcZoneIds, ",")}]
                 end,
                 case AZones of
                     undefined -> [];
                     _         -> member_params("AvailabilityZones.member.", AZones)
                 end
             ]),
    update_auto_scaling_group(Params, Config);

update_auto_scaling_group(Params, Config) ->
    P = Params,
    case as_query(Config, "UpdateAutoScalingGroup", P, ?API_VERSION) of
        {ok, _Doc} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.


%% --------------------------------------------------------------------
%% @doc describe_instances([], default max results, no paging offset, default config).
%% @end
%% --------------------------------------------------------------------
describe_instances() ->
    describe_instances([], erlcloud_aws:default_config()).

describe_instances(I, Config) ->
    describe_instances(I, ?DEFAULT_MAX_RECORDS, none, Config).

describe_instances(Config) when is_record(Config, aws_config) ->
    describe_instances([], Config);

describe_instances(I) ->
    describe_instances(I, erlcloud_aws:default_config()).

%% --------------------------------------------------------------------
%% @doc Get more information on the given list of instances in your
%% autoscaling groups or all if an empty list is passed in I.
%% @end
%% --------------------------------------------------------------------
describe_instances(I, MaxRecords, none, Config) ->
    describe_instances(I, [{"MaxRecords", MaxRecords}], Config);
describe_instances(I, MaxRecords, NextToken, Config) ->
    describe_instances(I, [{"MaxRecords", MaxRecords}, {"NextToken", NextToken}], Config).

-spec describe_instances(list(string()), list({string(), term()}), aws_config()) -> 
                                {ok, list(aws_autoscaling_instance())} | 
                                {{paged, string()}, list(aws_autoscaling_instance())} | 
                                {error, term()}.                       
describe_instances(I, Params, Config) ->
    P = member_params("InstanceIds.member.", I) ++ Params,
    case as_query(Config, "DescribeAutoScalingInstances", P, ?API_VERSION) of
        {ok, Doc} ->
            Status = erlcloud_util:next_token(?DESCRIBE_INSTANCES_NEXT_TOKEN, Doc),
            Instances = [extract_instance(ID) || ID <- xmerl_xpath:string(?DESCRIBE_INSTANCES, Doc)],
            {Status, Instances};
        {error, Reason} ->
            {error, Reason}
    end.

%% --------------------------------------------------------------------
%% @doc Terminate the given instance using the default configuration
%% without decrementing the desired capacity of the group.
%% @end
%% --------------------------------------------------------------------
-spec terminate_instance(string()) -> {ok, aws_autoscaling_activity()} | {error, term()}.
terminate_instance(InstanceId) ->
    terminate_instance(InstanceId, erlcloud_aws:default_config()).

%% --------------------------------------------------------------------
%% @doc Terminate the given instance.  The 2nd parameter can be:
%% 'false' to not decrement the desired capacity of the group
%% 'true' to decrement the desired capacity of the group
%% Config a supplied AWS configuration.
%% @end
%% --------------------------------------------------------------------
-spec terminate_instance(string(), boolean() | aws_config()) -> {ok, aws_autoscaling_activity()} | {error, term()}.
terminate_instance(InstanceId, false) ->
    terminate_instance(InstanceId, false, erlcloud_aws:default_config());
terminate_instance(InstanceId, true) ->
    terminate_instance(InstanceId, true, erlcloud_aws:default_config());
terminate_instance(InstanceId, Config) ->
    terminate_instance(InstanceId, false, Config).

-spec terminate_instance(string(), boolean(), aws_config()) -> {ok, aws_autoscaling_activity()} | {error, term()}.
terminate_instance(InstanceId, false, Config) ->
    priv_terminate_instance([{"InstanceId", InstanceId}, {"ShouldDecrementDesiredCapacity", "false"}], Config);
terminate_instance(InstanceId, true, Config) ->
    priv_terminate_instance([{"InstanceId", InstanceId}, {"ShouldDecrementDesiredCapacity", "true"}], Config).

priv_terminate_instance(Params, Config) ->
    case as_query(Config, "TerminateInstanceInAutoScalingGroup", Params, ?API_VERSION) of
        {ok, Doc} ->
            [Activity] = [extract_as_activity(A) || A <- xmerl_xpath:string(?TERMINATE_INSTANCE_ACTIVITY, Doc)],
            {ok, Activity};
        {error, Reason} ->
            {error, Reason}
    end.

%% --------------------------------------------------------------------
%% @doc Suspends Auto Scaling processes for the specified
%% Auto Scaling group. To suspend specific processes, use the
%% ScalingProcesses parameter. To suspend all processes, omit the
%% ScalingProcesses parameter.
%% @end
%% --------------------------------------------------------------------
-spec suspend_processes(string()) -> {ok, string()} | {error, term()}.
suspend_processes(GroupName) ->
    suspend_processes(GroupName, [], erlcloud_aws:default_config()).

%% --------------------------------------------------------------------
%% @doc Suspend the processes. The 2nd parameter can be used to specify
%% a list of ScalingProcesses or [] to suspend all
%% Config a supplied AWS configuration.
%% @end
%% --------------------------------------------------------------------
suspend_processes(GroupName, Config) when is_record(Config, aws_config) ->
    suspend_processes(GroupName, [], Config);
suspend_processes(GroupName, ScalingProcesses) when is_list(ScalingProcesses) ->
    suspend_processes(GroupName, ScalingProcesses, erlcloud_aws:default_config()).

-spec suspend_processes(string(), list(string()), aws_config()) -> {ok, string()} | {error, term()}.
suspend_processes(GroupName, ScalingProcesses, Config) ->
    Params = [{"AutoScalingGroupName", GroupName} | member_params("ScalingProcesses.member.", ScalingProcesses)],
    case as_query(Config, "SuspendProcesses", Params, ?API_VERSION) of
        {ok, Doc} ->
            [RequestId] = xmerl_xpath:string(?SUSPEND_PROCESSES_ACTIVITY, Doc),
            {ok, erlcloud_xml:get_text(RequestId)};
        {error, Reason} ->
            {error, Reason}
    end.

%% --------------------------------------------------------------------
%% @doc Resumes Auto Scaling processes for the specified
%% Auto Scaling group. To resume specific processes, use the
%% ScalingProcesses parameter. To resume all processes, omit the
%% ScalingProcesses parameter.
%% @end
%% --------------------------------------------------------------------
-spec resume_processes(string()) -> {ok, string()} | {error, term()}.
resume_processes(GroupName) ->
    resume_processes(GroupName, [], erlcloud_aws:default_config()).

%% --------------------------------------------------------------------
%% @doc Resume the processes. The 2nd parameter can be used to specify
%% a list of ScalingProcesses or [] to resume all
%% Config a supplied AWS configuration.
%% @end
%% --------------------------------------------------------------------
resume_processes(GroupName, Config) when is_record(Config, aws_config) ->
    resume_processes(GroupName, [], Config);
resume_processes(GroupName, ScalingProcesses) when is_list(ScalingProcesses) ->
    resume_processes(GroupName, ScalingProcesses, erlcloud_aws:default_config()).

-spec resume_processes(string(), list(string()), aws_config()) -> {ok, string()} | {error, term()}.
resume_processes(GroupName, ScalingProcesses, Config) ->
    Params = [{"AutoScalingGroupName", GroupName} | member_params("ScalingProcesses.member.", ScalingProcesses)],
    case as_query(Config, "ResumeProcesses", Params, ?API_VERSION) of
        {ok, Doc} ->
            [RequestId] = xmerl_xpath:string(?RESUME_PROCESSES_ACTIVITY, Doc),
            {ok, erlcloud_xml:get_text(RequestId)};
        {error, Reason} ->
            {error, Reason}
    end.


%% --------------------------------------------------------------------
%% @doc Detach the given instances from the group using the default configuration
%% without decrementing the desired capacity of the group.
%% @end
%% --------------------------------------------------------------------
-spec detach_instances(list(string()),string()) -> {ok, list(aws_autoscaling_activity())} | {error, term()}.
detach_instances(InstanceIds, GroupName) ->
    detach_instances(InstanceIds, GroupName, erlcloud_aws:default_config()).

%% --------------------------------------------------------------------
%% @doc Detach the given instances from the group.  The 3rd parameter can be:
%% 'false' to not decrement the desired capacity of the group
%% 'true' to decrement the desired capacity of the group
%% Config a supplied AWS configuration.
%% @end
%% --------------------------------------------------------------------
-spec detach_instances(list(string()),string(), boolean() | aws_config()) -> {ok, list(aws_autoscaling_activity())} | {error, term()}.
detach_instances(InstanceIds, GroupName, ShouldDecrementDesiredCapacity) when is_boolean(ShouldDecrementDesiredCapacity) ->
    detach_instances(InstanceIds, GroupName, ShouldDecrementDesiredCapacity, erlcloud_aws:default_config());
detach_instances(InstanceIds, GroupName, Config) ->
    detach_instances(InstanceIds, GroupName, false, Config).

-spec detach_instances(list(string()),string(), boolean(), aws_config()) -> {ok, list(aws_autoscaling_activity())} | {error, term()}.
detach_instances(InstanceIds, GroupName, ShouldDecrementDesiredCapacity, Config) ->
    P = case ShouldDecrementDesiredCapacity of
            true ->
                [{"ShouldDecrementDesiredCapacity", "true"}, {"AutoScalingGroupName", GroupName} | member_params("InstanceIds.member.", InstanceIds)];
            false ->
                [{"ShouldDecrementDesiredCapacity", "false"}, {"AutoScalingGroupName", GroupName} | member_params("InstanceIds.member.", InstanceIds)]
    end,
    case as_query(Config, "DetachInstances", P, ?API_VERSION) of
        {ok, Doc} ->
            Activities = [extract_as_activity(A) || A <- xmerl_xpath:string(?DETACH_INSTANCES_ACTIVITY, Doc)],
            {ok, Activities};
        {error, Reason} ->
            {error, Reason}
    end.


describe_lifecycle_hooks(GroupName) ->
    describe_lifecycle_hooks(GroupName, [], erlcloud_aws:default_config()).

describe_lifecycle_hooks(GroupName, LifecycleHookNames) when is_list(LifecycleHookNames) ->
    describe_lifecycle_hooks(GroupName, LifecycleHookNames, erlcloud_aws:default_config());
describe_lifecycle_hooks(GroupName, Config) ->
    describe_lifecycle_hooks(GroupName, [], Config).

-spec describe_lifecycle_hooks(string(), list(string()), aws_config()) -> {ok, list(aws_autoscaling_lifecycle_hook())} | {error, term()}.
describe_lifecycle_hooks(GroupName, LifecycleHookNames, Config) ->
    Params = [{"AutoScalingGroupName", GroupName} | member_params("LifecycleHookNames.member.", LifecycleHookNames)],
    case as_query(Config, "DescribeLifecycleHooks", Params, ?API_VERSION) of
        {ok, Doc} ->
            LifecycleHooks = xmerl_xpath:string(?DESCRIBE_LIFECYCLE_HOOKS_PATH, Doc),            
            Hooks = [extract_lifecycle_hook(H) || H <- LifecycleHooks],
            {ok, Hooks};
        {error, Reason} ->
            {error, Reason}
    end.

extract_lifecycle_hook(H) -> 
    #aws_autoscaling_lifecycle_hook{
          group_name = erlcloud_xml:get_text("AutoScalingGroupName", H),
          lifecycle_hook_name = erlcloud_xml:get_text("LifecycleHookName", H),
          global_timeout = erlcloud_xml:get_integer("GlobalTimeout", H),
          heartbeat_timeout = erlcloud_xml:get_integer("HeartbeatTimeout", H),
          default_result = erlcloud_xml:get_text("DefaultResult", H),
          lifecycle_transition = erlcloud_xml:get_text("LifecycleTransition", H)}.


-spec complete_lifecycle_action(string(), string(), string(), {instance_id | token, string()}) -> {ok, string()} | {error, term()}.
complete_lifecycle_action(GroupName, LifecycleActionResult, LifecycleHookName, InstanceIdOrLifecycleActionToken) ->
    complete_lifecycle_action(GroupName, LifecycleActionResult, LifecycleHookName, InstanceIdOrLifecycleActionToken, erlcloud_aws:default_config()).

-spec complete_lifecycle_action(string(), string(), string(), {instance_id | token, string()}, aws_config()) -> {ok, string()} | {error, term()}.
complete_lifecycle_action(GroupName, LifecycleActionResult, LifecycleHookName, InstanceIdOrLifecycleActionToken, Config) ->
    InstanceIdOrLifecycleActionTokenParam = case InstanceIdOrLifecycleActionToken of
        {instance_id,InstanceId} ->
            {"InstanceId", InstanceId};
        {token,LifecycleActionToken} ->
            {"LifecycleActionToken", LifecycleActionToken}
    end,
    Params = [{"AutoScalingGroupName", GroupName},
              {"LifecycleActionResult", LifecycleActionResult},
              {"LifecycleHookName", LifecycleHookName},
              InstanceIdOrLifecycleActionTokenParam],
    case as_query(Config, "CompleteLifecycleAction", Params, ?API_VERSION) of
        {ok, Doc} ->
            RequestId = erlcloud_xml:get_text(?COMPLETE_LIFECYCLE_ACTION_ACTIVITY, Doc),            
            {ok, RequestId};
        {error, Reason} ->
            {error, Reason}
    end.

%% given a list of member identifiers, return a list of 
%% {key with prefix, member identifier} for use in autoscaling calls.
%% Example pair that could be returned in a list is 
%% {"LaunchConfigurationNames.member.1", "my-launch-config}.
-spec member_params(string(), list(string())) -> list({string(), term()}).
member_params(Prefix, MemberIdentifiers) ->
    MemberKeys = [Prefix ++ integer_to_list(I) || I <- lists:seq(1, length(MemberIdentifiers))],
    [{K, V} || {K, V} <- lists:zip(MemberKeys, MemberIdentifiers)].
    

extract_config(C) ->
    #aws_launch_config{
       name = erlcloud_xml:get_text("LaunchConfigurationName", C),
       image_id = erlcloud_xml:get_text("ImageId", C),
       tenancy = erlcloud_xml:get_text("PlacementTenancy", C),
       instance_type = erlcloud_xml:get_text("InstanceType", C),
       user_data = erlcloud_xml:get_text("UserData", C),
       monitoring = erlcloud_xml:get_bool("InstanceMonitoring/Enabled", C),
       public_ip_address = erlcloud_xml:get_bool("AssociatePublicIpAddress", C),
       security_groups = [ erlcloud_xml:get_text(G) || G <- xmerl_xpath:string("SecurityGroups/member", C)]
      }.

extract_as_activity(A) ->
    #aws_autoscaling_activity{
       id = get_text("ActivityId", A),
       group_name = get_text("AutoScalingGroupName", A),
       cause = get_text("Cause", A),
       description = get_text("Description", A),
       details = get_text("Details", A),
       status_code = get_text("StatusCode", A),
       status_msg = get_text("StatusMessage", A),
       start_time = erlcloud_xml:get_time("StartTime", A),
       end_time = erlcloud_xml:get_time("EndTime", A),
       progress = erlcloud_xml:get_integer("Progress", A)
      }.

get_text(Label, Doc) ->
    erlcloud_xml:get_text(Label, Doc).

%% @TODO:  spec is too general with terms I think
-spec as_query(aws_config(), string(), list({string(), term()}), string()) -> {ok, #xmlElement{}} | {error, term()}.
as_query(Config, Action, Params, ApiVersion) ->
    QParams = [{"Action", Action}, {"Version", ApiVersion}|Params],
    erlcloud_aws:aws_request_xml4(post, Config#aws_config.as_host,
                                  "/", QParams, "autoscaling", Config).


when_defined(Value, Return, DefaultReturn) ->
    case Value of 
        undefined ->
            DefaultReturn;
        _ ->
            Return
    end.

-spec tag_to_member_param(aws_autoscaling_tag(), integer()) -> list().
tag_to_member_param(#aws_autoscaling_tag{
                        key = Key,
                        propogate_at_launch = AtLaunch,
                        resource_id = ResourceId,
                        resource_type = ResourceType,
                        value = Value
                    }, TagIndex) ->
    Prefix = "Tags.member." ++ integer_to_list(TagIndex) ++ ".",
    [
      {Prefix ++ "Key", Key},
      {Prefix ++ "Value", Value},
      when_defined(AtLaunch, {Prefix ++ "PropageteAtLaunch", atom_to_list(AtLaunch)}, []),
      when_defined(ResourceId, {Prefix ++ "ResourceId", ResourceId}, []),
      when_defined(ResourceType, {Prefix ++ "ResourceType", ResourceType}, [])
    ].
