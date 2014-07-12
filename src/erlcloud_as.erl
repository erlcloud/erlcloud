-module(erlcloud_as).

-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include_lib("erlcloud/include/erlcloud_as.hrl").

%% AWS Autoscaling functions
-export([describe_groups/0, describe_groups/1, describe_groups/2, describe_groups/4,
         set_desired_capacity/2, set_desired_capacity/3, set_desired_capacity/4,

         describe_launch_configs/0, describe_launch_configs/1, describe_launch_configs/2, 
         describe_launch_configs/4]).

-define(API_VERSION, "2011-01-01").
-define(DEFAULT_MAX_RECORDS, 20).

% xpath for group descriptions used in describe_groups functions:
-define(DESCRIBE_GROUPS_PATH, 
        "/DescribeAutoScalingGroupsResponse/DescribeAutoScalingGroupsResult/AutoScalingGroups/member").
-define(DESCRIBE_GROUPS_NEXT_TOKEN, 
        "/DescribeAutoScalingGroupsResponse/DescribeAutoScalingGroupsResult/NextToken").
% xpath for the request ID returned from a SetDesiredCapacity operation:
-define(SET_SCALE_REQUEST_ID_PATH, "/SetDesiredCapacityResponse/ResponseMetadata/RequestId").
-define(DESCRIBE_LAUNCH_CONFIG_PATH, 
        "/DescribeLaunchConfigurationsResponse/DescribeLaunchConfigurationsResult/LaunchConfigurations/member").
-define(LAUNCH_CONFIG_NEXT_TOKEN,
        "/DescribeLaunchConfigurationsResponse/DescribeLaunchConfigurationsResult/NextToken").

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
describe_groups(GN, MaxRecords, NextToken, Config) ->
    describe_groups(GN, [{"NextToken", NextToken}, {"MaxRecords", MaxRecords}], Config).

-spec describe_groups(list(string()), list({string(), term()}), aws_config()) -> 
                             {ok, term()} | {{paged, string()}, term()} | {error, term()}.
describe_groups(GN, Params, Config) ->
    MemberKeys = ["AutoScalingGroupNames.member." ++ integer_to_list(I) || I <- lists:seq(1, length(GN))],
    MemberParams = [{K, V} || {K, V} <- lists:zip(MemberKeys, GN)],
    case as_query(Config, "DescribeAutoScalingGroups", MemberParams ++ Params, ?API_VERSION) of
        {ok, Doc} ->
            Groups = xmerl_xpath:string(?DESCRIBE_GROUPS_PATH, Doc),            
            {next_token(?DESCRIBE_GROUPS_NEXT_TOKEN, Doc), [extract_group(G) || G <- Groups]};
        {error, Reason} ->
            {error, Reason}
    end.
    

%% retrieve NextToken from the XML at Path location.  Path is expected to lead to a 
%% single occurrence and if it does not exist as such, this just returns ok.
-spec next_token(string(), term()) -> ok | {paged, string()}.
next_token(Path, XML) ->
    case xmerl_xpath:string(Path, XML) of
        [Next] ->
            {paged, erlcloud_xml:get_text(Next)};
        _ ->
            ok
    end.

extract_group(G) ->
    #aws_autoscaling_group{
       group_name = erlcloud_xml:get_text("AutoScalingGroupName", G),
       tags = extract_tags_from_group(G),
       availability_zones = 
           [erlcloud_xml:get_text(A) || A <- xmerl_xpath:string("AvailabilityZones/member", G)],
       load_balancer_names = 
           [erlcloud_xml:get_text(L) || L <- xmerl_xpath:string("LoadBalancerNames/member", G)],
       desired_capacity = erlcloud_xml:get_integer("DesiredCapacity", G),
       min_size = erlcloud_xml:get_integer("MinSize", G),
       max_size = erlcloud_xml:get_integer("MaxSize", G)}.
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
            erlcloud_xml:get_text(RequestId);
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
describe_launch_configs(LN, MaxRecords, NextToken, Config) ->
    describe_launch_configs(LN, [{"MaxRecords", MaxRecords}, {"NextToken", NextToken}], Config).

-spec describe_launch_configs(list(string()), list({string(), term()}), aws_config()) -> 
                                    {ok, list(aws_launch_config())} | 
                                    {{paged, string()}, list(aws_launch_config)} | 
                                    {error, term()}.                                     
describe_launch_configs(LN, Params, Config) ->
    MemberKeys = ["LaunchConfigurationNames.member." ++ integer_to_list(I) || I <- lists:seq(1, length(LN))],
    MemberParams = [{K, V} || {K, V} <- lists:zip(MemberKeys, LN)],
    case as_query(Config, "DescribeLaunchConfigurations", MemberParams ++ Params, ?API_VERSION) of
        {ok, Doc} ->
            Status = next_token(?LAUNCH_CONFIG_NEXT_TOKEN, Doc),
            Configs = [extract_config(C) || C <- xmerl_xpath:string(?DESCRIBE_LAUNCH_CONFIG_PATH, Doc)],
            {Status, Configs};
        {error, Reason} ->
            {error, Reason}
    end.

extract_config(C) ->
    #aws_launch_config{
       name = erlcloud_xml:get_text("LaunchConfigurationName", C),
       image_id = erlcloud_xml:get_text("ImageId", C),
       tenancy = erlcloud_xml:get_text("PlacementTenancy", C),
       instance_type = erlcloud_xml:get_text("InstanceType", C)
      }.

%% Based on erlcoud_ec2:ec2_query2()
%% @TODO:  spec is too general with terms I think
-spec as_query(aws_config(), string(), list({string(), string()}), string()) -> {ok, term()} | {error, term}.
as_query(Config, Action, Params, ApiVersion) ->
    QParams = [{"Action", Action}, {"Version", ApiVersion}|Params],
    erlcloud_aws:aws_request_xml2(post, Config#aws_config.as_host, 
                                  "/", QParams, Config).
