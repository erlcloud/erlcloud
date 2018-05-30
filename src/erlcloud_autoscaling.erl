%% 
%% Amazon Auto Scaling Web Service API Client.
%% Documentation - http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_Operations.html
%%
-module(erlcloud_autoscaling).
-author('pavel@alertlogic.com').

-export([configure/2, configure/3, new/2, new/3]).

-export([describe_autoscaling_groups/0, describe_autoscaling_groups/1, describe_autoscaling_groups/2,
         describe_launch_configurations/0, describe_launch_configurations/1, describe_launch_configurations/2,
         describe_autoscaling_groups_all/0, describe_autoscaling_groups_all/1, describe_autoscaling_groups_all/2,
         describe_launch_configurations_all/0, describe_launch_configurations_all/1, describe_launch_configurations_all/2
        ]).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-import(erlcloud_xml, [get_text/1, get_text/2, get_bool/2, get_integer/2]).

-define(API_VERSION, "2011-01-01").
-define(SERVICE_NAME, "autoscaling").

-spec new(string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey}.

-spec new(string(), string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                autoscaling_host=Host}.

-spec configure(string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey) ->
    erlcloud_config:configure(AccessKeyID, SecretAccessKey, fun new/2).

-spec configure(string(), string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host) ->
    erlcloud_config:configure(AccessKeyID, SecretAccessKey, Host, fun new/3).

-spec describe_autoscaling_groups() -> {string(), proplist()} | {error, term()}.
describe_autoscaling_groups() -> describe_autoscaling_groups([]).

-spec describe_autoscaling_groups([string()] | aws_config()) -> {string(), proplist()} | {error, term()}.
describe_autoscaling_groups(Config) when is_record(Config, aws_config) -> 
    describe_autoscaling_groups([], Config);
describe_autoscaling_groups(GroupNames) ->
    describe_autoscaling_groups(GroupNames, default_config()).

-spec describe_autoscaling_groups([string()], aws_config()) -> {string(), proplist()} | {error, term()}.
describe_autoscaling_groups(GroupNames, Config) 
  when is_list(GroupNames), is_record(Config, aws_config) -> 
    %case autoscaling_query(Config, "DescribeAutoScalingGroups", []) of % erlcloud_aws:param_list(GroupNames, "AutoScalingGroupNames.member")) of
    case autoscaling_query(Config, "DescribeAutoScalingGroups", erlcloud_aws:param_list(GroupNames, "AutoScalingGroupNames.member")) of
        {ok, Doc} ->
            Groups = xmerl_xpath:string("/DescribeAutoScalingGroupsResponse/DescribeAutoScalingGroupsResult/AutoScalingGroups/member", Doc),
            {erlcloud_util:next_token("/DescribeAutoScalingGroupsResponse/DescribeAutoScalingGroupsResult/NextToken", Doc), [extract_autoscaling_group(Group) || Group <- Groups]};
        {error, Reason} ->
            {error, Reason}
    end.

-spec describe_autoscaling_groups_all() -> {ok, proplist()} | {error, term()}.
describe_autoscaling_groups_all() -> describe_autoscaling_groups_all([]).

-spec describe_autoscaling_groups_all([string()] | aws_config()) -> {ok, proplist()} | {error, term()}.
describe_autoscaling_groups_all(Config) when is_record(Config, aws_config) -> 
    describe_autoscaling_groups_all([], Config);
describe_autoscaling_groups_all(GroupNames) ->
    describe_autoscaling_groups_all(GroupNames, default_config()).

-spec describe_autoscaling_groups_all([string()], aws_config()) -> {ok, proplist()} | {error, term()}.
describe_autoscaling_groups_all(GroupNames, Config)
  when is_list(GroupNames), is_record(Config, aws_config) -> 
    %case autoscaling_query(Config, "DescribeAutoScalingGroups", []) of % erlcloud_aws:param_list(GroupNames, "AutoScalingGroupNames.member")) of
    case erlcloud_util:query_all_token(fun autoscaling_query/3, Config, "DescribeAutoScalingGroups", erlcloud_aws:param_list(GroupNames, "AutoScalingGroupNames.member")) of
        {ok, Docs} ->
            Groups = lists:append([xmerl_xpath:string("/DescribeAutoScalingGroupsResponse/DescribeAutoScalingGroupsResult/AutoScalingGroups/member", Doc) || Doc <- Docs]),
            {ok, [extract_autoscaling_group(Group) || Group <- Groups]};
        {error, Reason} ->
            {error, Reason}
    end.

-spec describe_launch_configurations() -> {string(), proplist()} | {error, term()}.
describe_launch_configurations() -> describe_launch_configurations([]).

-spec describe_launch_configurations([string()] | aws_config()) -> {string(), proplist()} | {error, term()}.
describe_launch_configurations(Config) when is_record(Config, aws_config) -> 
    describe_launch_configurations([], Config);
describe_launch_configurations(ConfiguratoinNames) ->
    describe_launch_configurations(ConfiguratoinNames, default_config()).

-spec describe_launch_configurations([string()], aws_config()) -> {string(), proplist()} | {error, term()}.
describe_launch_configurations(ConfiguratoinNames, Config) 
  when is_list(ConfiguratoinNames), is_record(Config, aws_config) -> 
    case autoscaling_query(Config, "DescribeLaunchConfigurations", erlcloud_aws:param_list(ConfiguratoinNames, "LaunchConfigurationNames.member")) of
        {ok, Doc} ->
            Configurations = xmerl_xpath:string("/DescribeLaunchConfigurationsResponse/DescribeLaunchConfigurationsResult/LaunchConfigurations/member", Doc),
            {erlcloud_util:next_token("/DescribeLaunchConfigurationsResponse/DescribeLaunchConfigurationsResult/NextToken", Doc), [extract_launch_configuration(Configuration) || Configuration <- Configurations]};
        {error, Reason} ->
            {error, Reason}  
    end.

-spec describe_launch_configurations_all() -> {ok, proplist()} | {error, term()}.
describe_launch_configurations_all() -> describe_launch_configurations_all([]).

-spec describe_launch_configurations_all([string()] | aws_config()) -> {ok, proplist()} | {error, term()}.
describe_launch_configurations_all(Config) when is_record(Config, aws_config) -> 
    describe_launch_configurations_all([], Config);
describe_launch_configurations_all(ConfiguratoinNames) ->
    describe_launch_configurations_all(ConfiguratoinNames, default_config()).

-spec describe_launch_configurations_all([string()], aws_config()) -> {ok, proplist()} | {error, term()}.
describe_launch_configurations_all(ConfiguratoinNames, Config)
  when is_list(ConfiguratoinNames), is_record(Config, aws_config) -> 
    case erlcloud_util:query_all_token(fun autoscaling_query/3, Config, "DescribeLaunchConfigurations", erlcloud_aws:param_list(ConfiguratoinNames, "LaunchConfigurationNames.member")) of
        {ok, Docs} ->
            Configurations = lists:append([xmerl_xpath:string("/DescribeLaunchConfigurationsResponse/DescribeLaunchConfigurationsResult/LaunchConfigurations/member", Doc) || Doc <- Docs]),
            {ok, [extract_launch_configuration(Configuration) || Configuration <- Configurations]};
        {error, Reason} ->
            {error, Reason}  
    end.

%
% @private
%
extract_autoscaling_group(Item) ->
    [
        {autoscaling_group_name, get_text("AutoScalingGroupName", Item)},
        {autoscaling_group_arn, get_text("AutoScalingGroupARN", Item)},
        {launch_configuration_name, get_text("LaunchConfigurationName", Item)},
        {min_size, get_integer("MinSize", Item)},
        {max_size, get_integer("MaxSize", Item)},
        {create_time, erlcloud_xml:get_time("CreatedTime", Item)},
        {health_check_type, get_text("HealthCheckType", Item)},
        {desired_capacity, get_integer("DesiredCapacity", Item)},
        {placement_group, get_text("PlacementGroup", Item)},
        {status, get_text("Status", Item)},
        {subnets, string:tokens(get_text("VPCZoneIdentifier", Item), ",")},
        {availability_zones, [get_text(Z) || Z <- xmerl_xpath:string("AvailabilityZones/member", Item)]},
        {load_balancers, [get_text(L) || L <- xmerl_xpath:string("LoadBalancerNames/member", Item)]},
        {instances, [extract_instance(I) || I <- xmerl_xpath:string("Instances/member", Item)]},
        {tag_set, [extract_tag_item(Tag) || Tag<- xmerl_xpath:string("Tags/member", Item)]}
    ].

extract_instance(Node) ->
    [
        {instance_id, get_text("InstanceId", Node)},
        {launch_configuration_name, get_text("LaunchConfigurationName", Node)},
        {lifecycle_state, get_text("LifecycleState", Node)},
        {health_status, get_text("HealthStatus", Node)},
        {availability_zone, get_text("AvailabilityZone", Node)}
    ].

extract_tag_item(Node) ->
    [
     {key, get_text("Key", Node)},
     {value, get_text("Value", Node)},
     {propogate_at_launch, get_text("PropagateAtLaunch", Node)},
     {resource_id, get_text("ResourceId", Node)},
     {resource_type, get_text("ResourceType", Node)}
    ].

extract_launch_configuration(Item) ->
    [
        {launch_configuration_name, get_text("LaunchConfigurationName", Item)},
        {launch_configuration_arn, get_text("LaunchConfigurationARN", Item)},
        {image_id, get_text("ImageId", Item)},
        {kernel_id, get_text("KernelId", Item)},
        {ramdisk_id, get_text("RamdiskId", Item)},
        {instance_type, get_text("InstanceType", Item)},
        {iam_instance_profile, get_text("IamInstanceProfile", Item)},
        {associate_public_ip_address, get_bool("AssociatePublicIpAddress", Item)},
        {placement_tenancy, get_text("PlacementTenancy", Item)},
        {create_time, erlcloud_xml:get_time("CreatedTime", Item)},
        {key_name, get_text("KeyName", Item)},
        {user_data, get_text("UserData", Item)},
        {security_groups, [get_text(L) || L <- xmerl_xpath:string("SecurityGroups/member", Item)]}
    ].

autoscaling_query(Config, Action, Params) ->
    autoscaling_query(Config, Action, Params, ?API_VERSION).

autoscaling_query(Config, Action, Params, ApiVersion) ->
    QParams = [{"Action", Action}, {"Version", ApiVersion}|Params],
    erlcloud_aws:aws_request_xml4(post,
                                  Config#aws_config.autoscaling_host,
                                  "/", QParams, ?SERVICE_NAME, Config).


default_config() -> erlcloud_aws:default_config().
