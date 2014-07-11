-module(erlcloud_as).

-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include_lib("erlcloud/include/erlcloud_as.hrl").

%% AWS Autoscaling functions
-export([describe_groups/0, describe_groups/1, describe_groups/2,
        set_desired_capacity/2, set_desired_capacity/3, set_desired_capacity/4]).

-define(API_VERSION, "2011-01-01").

% xpath for group descriptions used in describe_groups functions:
-define(DESCRIBE_GROUPS_PATH, 
        "/DescribeAutoScalingGroupsResponse/DescribeAutoScalingGroupsResult/AutoScalingGroups/member").
% xpath for the request ID returned from a SetDesiredCapacity operation:
-define(SET_SCALE_REQUEST_ID_PATH, "/SetDesiredCapacityResponse/ResponseMetadata/RequestId").

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
%% @end
%% --------------------------------------------------------------------
-spec describe_groups(list(string()), aws_config()) -> {ok, term()} | {error, term()}.
describe_groups(GN, Config) ->
    MemberKeys = ["AutoScalingGroupNames.member." ++ integer_to_list(I) || I <- lists:seq(1, length(GN))],
    Params = [{K, V} || {K, V} <- lists:zip(MemberKeys, GN)],
    case as_query(Config, "DescribeAutoScalingGroups", Params, ?API_VERSION) of
        {ok, Doc} ->
            Groups = xmerl_xpath:string(?DESCRIBE_GROUPS_PATH, Doc),
            [extract_group(G) || G <- Groups];
        {error, Reason} ->
            {error, Reason}
    end.

extract_group(G) ->
    #aws_autoscaling_group{
       group_name = erlcloud_xml:get_text("AutoScalingGroupName", G),
       tags = extract_tags_from_group(G),
       availability_zones = [erlcloud_xml:get_text(A) || A <- xmerl_xpath:string("AvailabilityZones/member", G)],
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

%% Based on erlcoud_ec2:ec2_query2()
%% @TODO:  spec is too general with terms I think
-spec as_query(aws_config(), string(), list({string(), string()}), string()) -> {ok, term()} | {error, term}.
as_query(Config, Action, Params, ApiVersion) ->
    QParams = [{"Action", Action}, {"Version", ApiVersion}|Params],
    erlcloud_aws:aws_request_xml2(post, Config#aws_config.as_host, 
                                  "/", QParams, Config).
