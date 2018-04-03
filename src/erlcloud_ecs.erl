%% @author Pavel Trakhtman <pavvel@alertlogic.com>
%% @doc
%% An Erlang interface to AWS ECS.
%%
%% [http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_Operations.html]
%%
%% erlcloud_ecs implements the entire 20141113 API except actions use only by EC2 Container Service Agent:
%% [http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DiscoverPollEndpoint.html]
%% [http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_SubmitContainerStateChange.html]
%% [http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_SubmitTaskStateChange.html]
%%
%% Method names match ECS operations converted to lower_case_with_underscores.
%%
%% Required parameters are passed as function arguments. In addition
%% all methods take an options proplist argument which can be used to
%% pass optional parameters. See function documentation for examples.
%% 
%% Output is in the form of `{ok, Value}' or `{error, Reason}'. The
%% format of `Value' is controlled by the `out' option, which defaults
%% to `json'. The possible values are:
%%
%% * `json' - The output from ECS as processed by `jsx:decode'
%% but with no further manipulation. This would rarely be useful,
%% unless the ECS API is updated to include data that is not yet
%% parsed correctly.
%%
%% * `record' - A record containing all the information from the
%% ECS response except field types.
%%
%% ECS errors are return in the form `{error, {ErrorCode, Message}}' 
%% where `ErrorCode' and 'Message' are both binary
%% strings. List of error codes:
%% [http://docs.aws.amazon.com/AmazonECS/latest/developerguide/troubleshooting.html#api_failures_messages].
%%
%% See the unit tests for additional usage examples beyond what are
%% provided for each function.
%%
%% @end

-module(erlcloud_ecs).
-author('pavel@alertlogic.com').

-include("erlcloud_aws.hrl").
-include("erlcloud_ecs.hrl").

%%% Library initialization.
-export([configure/2, configure/3, configure/4, new/2, new/3, new/4]).

-define(API_VERSION, "20141113").

-export([create_cluster/0, create_cluster/1, create_cluster/2,
         create_service/3, create_service/4, create_service/5,
         delete_cluster/1, delete_cluster/2, delete_cluster/3,
         delete_service/1, delete_service/2, delete_service/3,
         deregister_container_instance/1, deregister_container_instance/2, deregister_container_instance/3,
         deregister_task_definition/1, deregister_task_definition/2, deregister_task_definition/3,
         describe_clusters/0, describe_clusters/1, describe_clusters/2,
         describe_container_instances/1, describe_container_instances/2, describe_container_instances/3,
         describe_services/1, describe_services/2, describe_services/3,
         describe_task_definition/1, describe_task_definition/2, describe_task_definition/3,
         describe_tasks/1, describe_tasks/2, describe_tasks/3,
         list_clusters/0, list_clusters/1, list_clusters/2,
         list_container_instances/0, list_container_instances/1, list_container_instances/2,
         list_services/0, list_services/1, list_services/2,
         list_task_definition_families/0, list_task_definition_families/1, list_task_definition_families/2,
         list_task_definitions/0, list_task_definitions/1, list_task_definitions/2,
         list_tasks/0, list_tasks/1, list_tasks/2,
         register_task_definition/2, register_task_definition/3, register_task_definition/4,
         run_task/1, run_task/2, run_task/3,
         start_task/2, start_task/3, start_task/4,
         stop_task/1, stop_task/2, stop_task/3,
         update_container_agent/1, update_container_agent/2, update_container_agent/3,
         update_service/1, update_service/2, update_service/3
]).

-export_type([
    arn/0,
    attr_name/0,
    cluster_opt/0,
    client_token_opt/0,
    container_definition_opt/0,
    container_definition_opts/0,
    container_environment_opt/0,
    container_extra_hosts_opt/0,
    container_log_configuration_opt/0,
    container_mount_points_opt/0,
    container_name_opt/0,
    container_port_mappings_opt/0,
    container_port_opt/0,
    container_ulimits_opt/0,
    container_volumes_from_opt/0,
    create_cluster_opt/0,
    create_cluster_opts/0,
    create_service_opt/0,
    create_service_opts/0,
    delete_service_opt/0,
    delete_service_opts/0,
    deployment_configuration/0,
    deployment_configuration_opt/0,
    deployment_configuration_opts/0,
    deregister_container_instance_opt/0,
    deregister_container_instance_opts/0,
    describe_clusters_opt/0,
    describe_clusters_opts/0,
    describe_container_instances_opt/0,
    describe_container_instances_opts/0,
    describe_services_opt/0,
    describe_services_opts/0,
    describe_tasks_opt/0,
    describe_tasks_opts/0,
    ecs_container_override_opt/0,
    ecs_container_override_opts/0,
    ecs_host_entry_opt/0,
    ecs_host_entry_opts/0,
    ecs_log_configuration_opt/0,
    ecs_log_configuration_opts/0,
    ecs_mount_point_opt/0,
    ecs_mount_point_opts/0,
    ecs_port_mapping_opt/0,
    ecs_port_mapping_opts/0,
    ecs_opts/0,
    ecs_task_override_opt/0,
    ecs_task_override_opts/0,
    ecs_ulimit_opt/0,
    ecs_ulimit_opts/0,
    ecs_volume_from_opt/0,
    ecs_volume_from_opts/0,
    key_value_pair_opt/0,
    key_value_pair_opts/0,
    list_clusters_opt/0,
    list_clusters_opts/0,
    list_container_instances_opt/0,
    list_container_instances_opts/0,
    list_services_opt/0,
    list_services_opts/0,
    list_task_definition_families_opt/0,
    list_task_definition_families_opts/0,
    list_task_definitions_opt/0,
    list_task_definitions_opts/0,
    list_tasks_opt/0,
    list_tasks_opts/0,
    load_balancer_name_opt/0,
    load_balancers_opt/0,
    load_balancers_opts/0,
    maybe_list/1,
    maximum_percent_opt/0,
    minimum_healthy_percent_opt/0, 
    register_task_definition_opt/0,
    register_task_definition_opts/0,
    role_opt/0,
    run_task_opt/0,
    run_task_opts/0,
    start_task_opt/0,
    start_task_opts/0,
    stop_task_opt/0,
    stop_task_opts/0,
    target_group_arn_opt/0, 
    task_overrides_opt/0,
    update_container_agent_opts/0,
    update_service_opt/0,
    update_service_opts/0,
    volume_host_opt/0,
    volume_name_opt/0,
    volumes/0,
    volumes_opt/0,
    volumes_opts/0
]).


%%%------------------------------------------------------------------------------
%%% Library initialization.
%%%------------------------------------------------------------------------------

-spec new(string(), string()) -> aws_config().

new(AccessKeyID, SecretAccessKey) ->
    #aws_config{
       access_key_id=AccessKeyID,
       secret_access_key=SecretAccessKey
      }.

-spec new(string(), string(), string()) -> aws_config().

new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{
       access_key_id=AccessKeyID,
       secret_access_key=SecretAccessKey,
       ecs_host=Host
      }.

-spec new(string(), string(), string(), non_neg_integer()) -> aws_config().

new(AccessKeyID, SecretAccessKey, Host, Port) ->
    #aws_config{
       access_key_id=AccessKeyID,
       secret_access_key=SecretAccessKey,
       ecs_host=Host,
       ecs_port=Port
      }.

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

default_config() ->
    erlcloud_aws:default_config().


%%%------------------------------------------------------------------------------
%%% Shared types
%%%------------------------------------------------------------------------------
-type maybe_list(T) :: T | [T].

-type string_param() :: binary() | string().
-type attr_name() :: binary() | string().
-type arn() :: binary().

-type json_pair() :: {binary() | atom(), jsx:json_term()}.
-type json_return() :: {ok, jsx:json_term()} | {error, term()}.
-type ecs_return(Record) :: {ok, jsx:json_term() | Record } | {error, term()}.
-type decode_fun() :: fun((jsx:json_term(), decode_opts()) -> tuple()).

%%%------------------------------------------------------------------------------
%%% Shared Options
%%%------------------------------------------------------------------------------
-type out_type() :: json | record.
-type out_opt() :: {out, out_type()}.
-type property() :: proplists:property().

-type aws_opts() :: [json_pair()].
-type ecs_opts() :: [out_opt()].
-type opts() :: {aws_opts(), ecs_opts()}.

-spec verify_ecs_opt(atom(), term()) -> ok.
verify_ecs_opt(out, Value) ->
    case lists:member(Value, [json, record]) of
        true ->
            ok;
        false ->
            error({erlcloud_ecs, {invalid_opt, {out, Value}}})
    end;
verify_ecs_opt(Name, Value) ->
    error({erlcloud_ecs, {invalid_opt, {Name, Value}}}).

-type opt_table_entry() :: {atom(), binary(), fun((_) -> jsx:json_term())}.
-type opt_table() :: [opt_table_entry()].
-spec opt_folder(opt_table(), property(), opts()) -> opts().
opt_folder(_, {_, undefined}, Opts) ->
    %% ignore options set to undefined
    Opts;
opt_folder(Table, {Name, Value}, {AwsOpts, EcsOpts}) ->
    case lists:keyfind(Name, 1, Table) of
        {Name, Key, ValueFun} ->
            {[{Key, ValueFun(Value)} | AwsOpts], EcsOpts};
        false ->
            verify_ecs_opt(Name, Value),
            {AwsOpts, [{Name, Value} | EcsOpts]}
    end.

-spec opts(opt_table(), proplist()) -> opts().
opts(Table, Opts) when is_list(Opts) ->
    %% remove duplicate options
    Opts1 = lists:ukeysort(1, proplists:unfold(Opts)),
    lists:foldl(fun(Opt, A) -> opt_folder(Table, Opt, A) end, {[], []}, Opts1);
opts(_, _) ->
    error({erlcloud_ecs, opts_not_list}).


-type cluster_opt() :: {cluster, string_param()}.
-spec cluster_opt() -> opt_table_entry().
cluster_opt() ->
    {cluster, <<"cluster">>, fun to_binary/1}.

-type client_token_opt() :: {client_token, string_param()}.
-spec client_token_opt() -> opt_table_entry().
client_token_opt() ->
    {client_token, <<"clientToken">>, fun to_binary/1}.

-type maximum_percent_opt() :: {maximum_percent, pos_integer()}.
-spec maximum_percent_opt() -> opt_table_entry().
maximum_percent_opt() ->
        {maximum_percent, <<"maximumPercent">>, fun id/1}.

-type minimum_healthy_percent_opt() :: {minimum_healthy_percent, pos_integer()}.
-spec minimum_healthy_percent_opt() -> opt_table_entry().
minimum_healthy_percent_opt() ->
        {minimum_healthy_percent, <<"minimumHealthyPercent">>, fun id/1}.

-type deployment_configuration_opt() :: maximum_percent_opt() |
                                        minimum_healthy_percent_opt().
-type deployment_configuration_opts() :: [deployment_configuration_opt()].

-type deployment_configuration() :: {deployment_configuration, deployment_configuration_opts()}.
-spec deployment_configuration_opt() -> opt_table_entry().
deployment_configuration_opt() ->
    {deployment_configuration, <<"deploymentConfiguration">>, fun encode_deployment_configuration/1}.

-type role_opt() :: {role, string_param()}.
-spec role_opt() -> opt_table_entry().
role_opt() ->
    {role, <<"role">>, fun to_binary/1}.

-type container_name_opt() :: {container_name, string_param()}.
-spec container_name_opt() -> opt_table_entry().
container_name_opt() ->
    {container_name, <<"containerName">>, fun to_binary/1}.

-type container_port_opt() :: {container_port, pos_integer()}.
-spec container_port_opt() -> opt_table_entry().
container_port_opt() ->
    {container_port, <<"containerPort">>, fun id/1}.

-type load_balancer_name_opt() :: {load_balancer_name, string_param()}.
-spec load_balancer_name_opt() -> opt_table_entry().
load_balancer_name_opt() ->
    {load_balancer_name, <<"loadBalancerName">>, fun to_binary/1}.

-type target_group_arn_opt() :: {target_group_arn, string_param()}.
-spec target_group_arn_opt() -> opt_table_entry().
target_group_arn_opt() ->
    {target_group_arn, <<"targetGroupArn">>, fun to_binary/1}.


-type load_balancers_opt() :: container_name_opt() |
                              container_port_opt() |
                              load_balancer_name_opt() |
                              target_group_arn_opt().
-type load_balancers_opts() :: [load_balancers_opt()].

-spec load_balancers_opt() -> opt_table_entry().
load_balancers_opt() ->
    {load_balancers, <<"loadBalancers">>, fun encode_load_balancers/1}.

-type volume_name_opt() :: {name, string_param()}.
-spec volume_name_opt() -> opt_table_entry().
volume_name_opt() ->
    {name, <<"name">>, fun to_binary/1}.

-type volume_host_opt() :: {host, [{source_path, string_param()}]}.
-spec volume_host_opt() -> opt_table_entry().
volume_host_opt() ->
    {host, <<"host">>, fun encode_volume_host/1}.

-type volumes_opt() :: volume_name_opt() |
                       volume_host_opt().
-type volumes_opts() :: [volumes_opt()].

-type volumes() :: {volumes, maybe_list(volumes_opts())}.
-spec volumes_opt() -> opt_table_entry().
volumes_opt() ->
    {volumes, <<"volumes">>, fun encode_volumes/1}.

-type key_value_pair_opt() :: {name, string_param()} |
                              {value, string_param()}.
-type key_value_pair_opts() :: maybe_list([key_value_pair_opt()]).
-spec key_value_opt() -> opt_table().
key_value_opt() ->
    [ 
        {name, <<"name">>, fun to_binary/1},
        {value, <<"value">>, fun to_binary/1}
    ].

-type container_environment_opt() :: {environment, key_value_pair_opts()}.
-spec container_environment_opt() -> opt_table_entry().
container_environment_opt() ->
    {environment, <<"environment">>, fun encode_key_value_pair_list/1}.

-type ecs_host_entry_opt() :: {host_name, string_param()} |
                              {ip_address, string_param()}.
-type ecs_host_entry_opts() :: maybe_list([ecs_host_entry_opt()]).
-spec ecs_host_entry_opt() -> opt_table().
ecs_host_entry_opt() ->
    [ 
        {host_name, <<"hostname">>, fun to_binary/1},
        {ip_address, <<"ipAddress">>, fun to_binary/1}
    ].

-type container_extra_hosts_opt() :: {extra_hosts, ecs_host_entry_opts()}.
-spec container_extra_hosts_opt() -> opt_table_entry().
container_extra_hosts_opt() ->
    {extra_hosts, <<"extraHosts">>, fun encode_ecs_host_list/1}.

-type ecs_log_configuration_opt() :: {log_driver, string_param()} |
                                     {options, [{string_param(), string_param()}]}.
-type ecs_log_configuration_opts() :: [ecs_log_configuration_opt()].
-spec ecs_log_configuration_opts() -> opt_table().
ecs_log_configuration_opts() ->
    [
        {log_driver, <<"logDriver">>, fun to_binary/1},
        {options, <<"options">>, fun encode_pair/1}
    ].

-type container_log_configuration_opt() :: {log_configuration, ecs_log_configuration_opts()}.
-spec container_log_configuration_opt() -> opt_table_entry().
container_log_configuration_opt() ->
    {log_configuration, <<"logConfiguration">>, fun encode_log_configuration/1}.

-type ecs_mount_point_opt() :: {container_path, string_param()} |
                               {read_only, boolean()} |
                               {source_volume, string_param()}.
-type ecs_mount_point_opts() :: maybe_list([ecs_mount_point_opt()]).
-spec ecs_mount_point_opt() -> opt_table().
ecs_mount_point_opt() ->
    [
        {container_path, <<"containerPath">>, fun to_binary/1},
        {read_only, <<"readOnly">>, fun id/1},
        {source_volume, <<"sourceVolume">>, fun to_binary/1}
    ].

-type container_mount_points_opt() :: {mount_points, ecs_mount_point_opts()}.
-spec container_mount_points_opt() -> opt_table_entry().
container_mount_points_opt() ->
    {mount_points, <<"mountPoints">>, fun encode_ecs_mount_point_list/1}.

-type ecs_port_mapping_opt() :: {container_port, pos_integer()} |
                                {host_port, pos_integer()} |
                                {protocol, ecs_protocol()}.
-type ecs_port_mapping_opts() :: maybe_list([ecs_port_mapping_opt()]).
-spec ecs_port_mapping_opt() -> opt_table().
ecs_port_mapping_opt() ->
    [
        {container_port, <<"containerPort">>, fun id/1},
        {host_port, <<"hostPort">>, fun id/1},
        {protocol, <<"protocol">>, fun to_binary/1}
    ].

-type container_port_mappings_opt() :: {port_mappings, ecs_port_mapping_opts()}.
-spec container_port_mappings_opt() -> opt_table_entry().
container_port_mappings_opt() ->
    {port_mappings, <<"portMappings">>, fun encode_ecs_port_mapping_list/1}.

-type ecs_ulimit_opt() :: {hard_limit, pos_integer()} |
                           {soft_limit, pos_integer()} |
                           {name, string_param()}.
-type ecs_ulimit_opts() :: maybe_list([ecs_ulimit_opt()]).
-spec ecs_ulimit_opt() -> opt_table().
ecs_ulimit_opt() ->
    [
        {hard_limit, <<"hardLimit">>, fun id/1},
        {soft_limit, <<"softLimit">>, fun id/1},
        {name, <<"name">>, fun to_binary/1}
    ].

-type container_ulimits_opt() :: {ulimits, ecs_ulimit_opts()}.
-spec container_ulimits_opt() -> opt_table_entry().
container_ulimits_opt() ->
    {ulimits, <<"ulimits">>, fun encode_ecs_ulimit_list/1}.

-type ecs_volume_from_opt() :: {read_only, boolean()} |
                               {source_container, string_param()}.
-type ecs_volume_from_opts() :: maybe_list([ecs_volume_from_opt()]).
-spec ecs_volume_from_opt() -> opt_table().
ecs_volume_from_opt() ->
    [
        {read_only, <<"readOnly">>, fun id/1},
        {source_container, <<"sourceContainer">>, fun to_binary/1}
    ].

-type container_volumes_from_opt() :: {volumes_from, ecs_volume_from_opts()}.
-spec container_volumes_from_opt() -> opt_table_entry().
container_volumes_from_opt() ->
    {volumes_from, <<"volumesFrom">>, fun encode_ecs_volume_from_list/1}.

-type container_definition_opt() :: {command, [string_param()]} |
                                    {cpu, pos_integer()} |
                                    {disable_networking, boolean()} |
                                    {dns_search_domains, [string_param()]} |
                                    {dns_servers, [string_param()]} |
                                    {docker_labels, [{string_param(), string_param()}]} |
                                    {docker_security_options, [string() | boolean()]} |
                                    {entry_point, [string_param()]} |
                                    container_environment_opt() |
                                    {essential, boolean()} |
                                    container_extra_hosts_opt() |
                                    {host_name, string_param()} |
                                    {image, string_param()} |
                                    {links, [string_param()]} |
                                    container_log_configuration_opt() |
                                    {memory, pos_integer()} |
                                    {memory_reservation, pos_integer()} |
                                    container_mount_points_opt() |
                                    {name, string_param()} |
                                    container_port_mappings_opt() |
                                    {privileged, boolean()} |
                                    {readonly_root_filesystem, boolean()} |
                                    {user, string_param()} |
                                    container_ulimits_opt() |
                                    container_volumes_from_opt() |
                                    {working_directory, string_param()}.
-type container_definition_opts() :: [container_definition_opt()].

-type ecs_container_override_opt() :: {command, [string_param()]} |
                                      {environment, key_value_pair_opts()} |
                                      {name, string_param()}.
-type ecs_container_override_opts() :: [maybe_list(ecs_container_override_opt())].
-spec ecs_container_override_opt() -> opt_table().
ecs_container_override_opt() ->
    [
        {command, <<"command">>, fun to_binary/1},
        {name, <<"name">>, fun to_binary/1},
        {environment, <<"environment">>, fun encode_key_value_pair_list/1}
    ].

container_override_opt() ->
    {container_overrides, <<"containerOverrides">>, fun encode_ecs_container_overrides_list/1}.

-type ecs_task_override_opt() :: {container_overrides, ecs_container_override_opts()} |
                                 {task_role_arn, string_param()}.
-type ecs_task_override_opts() :: [ecs_task_override_opt()].
-spec ecs_task_override_opt() -> opt_table().
ecs_task_override_opt() ->
    [
        container_override_opt(),
        {task_role_arn, <<"taskRoleArn">>, fun to_binary/1}
    ].

-type task_overrides_opt() :: {overrides, ecs_container_override_opts()}.
-spec task_overrides_opt() -> opt_table_entry().
task_overrides_opt() ->
    {overrides, <<"overrides">>, fun encode_ecs_task_overrides/1}.
 
%%%------------------------------------------------------------------------------
%%% Shared Encoders 
%%%------------------------------------------------------------------------------
-spec encode_deployment_configuration(deployment_configuration_opts()) -> [json_pair()].
encode_deployment_configuration(Config) ->
    DeploymentConfigrationOpts = [
        maximum_percent_opt(),
        minimum_healthy_percent_opt()
    ],

    {AwsOpts, _EcsOpts} = opts(DeploymentConfigrationOpts, Config),
    AwsOpts.

-spec encode_load_balancers(load_balancers_opts()) -> [aws_opts()].
encode_load_balancers(Balancers) ->
    LoadBalancerOpts = [
        container_name_opt(),
        container_port_opt(),
        load_balancer_name_opt(),
        target_group_arn_opt()
    ],
    lists:map(
        fun(Balancer) ->
            {AwsOpts, _EcsOpts} = opts(LoadBalancerOpts, Balancer), AwsOpts
        end, 
    Balancers).

-spec encode_volume_host([{source_path, string_param()}]) -> [json_pair()].
encode_volume_host([{source_path, Path}]) ->
    [{<<"sourcePath">>, to_binary(Path)}];
encode_volume_host([]) ->
    [].

-spec encode_volumes(volumes_opts()) -> [aws_opts()].
encode_volumes(Volumes) ->
    VolumesOpts = [
        volume_name_opt(),
        volume_host_opt()
    ],
    lists:map(
        fun(Volume) ->
            {AwsOpts, _EcsOpts} = opts(VolumesOpts, Volume), AwsOpts
        end, 
    Volumes).
        
-spec encode_ecs_volume_from_list(ecs_volume_from_opts()) -> [aws_opts()].
encode_ecs_volume_from_list(VolumesFrom) ->
    encode_maybe_list(fun ecs_volume_from_opt/0, VolumesFrom).

-spec encode_ecs_ulimit_list(ecs_ulimit_opts()) -> [aws_opts()].
encode_ecs_ulimit_list(Ulimits) ->
    encode_maybe_list(fun ecs_ulimit_opt/0, Ulimits).

-spec encode_ecs_port_mapping_list(ecs_port_mapping_opts()) -> [aws_opts()].
encode_ecs_port_mapping_list(PortMappings) ->
    encode_maybe_list(fun ecs_port_mapping_opt/0, PortMappings).

-spec encode_ecs_mount_point_list(ecs_mount_point_opts()) -> [aws_opts()].
encode_ecs_mount_point_list(MountPoints) ->
    encode_maybe_list(fun ecs_mount_point_opt/0, MountPoints).

-spec encode_pair([{string_param(), string_param()}]) -> [json_pair()].
encode_pair(Opts) ->
    [{to_binary(Name), to_binary(Value)} || {Name, Value} <- Opts].

-spec encode_log_configuration(ecs_log_configuration_opts()) -> [json_pair()].
encode_log_configuration(Config) ->
    {AwsOpts, _EcsOpts} = opts(ecs_log_configuration_opts(), Config),
    AwsOpts.

-spec encode_ecs_host_list(ecs_host_entry_opts()) -> [aws_opts()].
encode_ecs_host_list(Hosts) ->
    encode_maybe_list(fun ecs_host_entry_opt/0, Hosts).

-spec encode_key_value_pair_list(key_value_pair_opts()) -> [aws_opts()].
encode_key_value_pair_list(Pairs) ->
    encode_maybe_list(fun key_value_opt/0, Pairs).
    
-spec container_definition_opts() -> opt_table().
container_definition_opts() ->
    [
        {command, <<"command">>, fun to_binary/1},
        {cpu, <<"cpu">>, fun id/1},
        {disable_networking, <<"disableNetworking">>, fun id/1},
        {dns_search_domains, <<"dnsSearchDomains">>,  fun to_binary/1},
        {dns_servers, <<"dnsServers">>, fun to_binary/1},
        {docker_labels, <<"dockerLabels">>, fun encode_pair/1},
        {docker_security_options, <<"dockerSecurityOptions">>, fun to_binary/1},
        {entry_point, <<"entryPoint">>, fun to_binary/1},
        container_environment_opt(),
        {essential, <<"essential">>, fun id/1},
        container_extra_hosts_opt(),
        {host_name, <<"hostname">>, fun to_binary/1},
        {image, <<"image">>, fun to_binary/1},
        {links, <<"links">>, fun to_binary/1},
        container_log_configuration_opt(),
        {memory, <<"memory">>, fun id/1},
        {memory_reservation, <<"memoryReservation">>, fun id/1},
        container_mount_points_opt(),
        {name, <<"name">>, fun to_binary/1},
        container_port_mappings_opt(),
        {privileged, <<"privileged">>, fun id/1},
        {readonly_root_filesystem, <<"readonlyRootFilesystem">>, fun id/1},
        {user, <<"user">>, fun to_binary/1},
        container_ulimits_opt(),
        container_volumes_from_opt(),
        {working_directory, <<"workingDirectory">>, fun to_binary/1}
    ].

-spec encode_container_definitions(Defs :: container_definition_opts()) -> [aws_opts()].
encode_container_definitions(Defs) ->
    encode_maybe_list(fun container_definition_opts/0, Defs).
    
-spec encode_ecs_task_overrides(Overrides :: ecs_task_override_opts()) -> jsx:json_term().
encode_ecs_task_overrides(Overrides) ->
    {AwsOpts, _EcsOpts} = opts(ecs_task_override_opt(), Overrides),
    AwsOpts.

-spec encode_ecs_container_overrides_list(ContainerOverrides :: ecs_container_override_opts()) -> [aws_opts()].
encode_ecs_container_overrides_list(ContainerOverrides) ->
    encode_maybe_list(fun ecs_container_override_opt/0, ContainerOverrides).

-spec encode_maybe_list(fun(() -> opt_table()), maybe_list(proplist())) -> [aws_opts()].
encode_maybe_list(Fun, List) when is_list(List), is_list(hd(List)) ->
    lists:map(
        fun(Item) ->
            {AwsOpts, _EcsOpts} = opts(Fun(), Item), AwsOpts
        end,
    List);

encode_maybe_list(Fun, List) when is_list(List) ->
    {AwsOpts, _EcsOpts} = opts(Fun(), List),
    [AwsOpts].

%%%------------------------------------------------------------------------------
%%% Shared Decoders
%%%------------------------------------------------------------------------------

-type decode_opt() :: {typed, boolean()}.
-type decode_opts() :: [decode_opt()].
-type record_desc() :: {tuple(), field_table()}.

-spec id(X) -> X.
id(X) -> X.

-spec id(X, decode_opts()) -> X.
id(X, _) -> X.

-type field_table() :: [{binary(), pos_integer(), 
                         fun((jsx:json_term(), decode_opts()) -> term())}].

-spec decode_folder(field_table(), json_pair(), decode_opts(), tuple()) -> tuple().
decode_folder(Table, {Key, Value}, Opts, A) ->
    case lists:keyfind(Key, 1, Table) of
        {Key, Index, ValueFun} ->
            setelement(Index, A, ValueFun(Value, Opts));
        false ->
            A
    end.


-spec decode_record(record_desc(), jsx:json_term(), decode_opts()) -> tuple().
decode_record({Record, _}, [{}], _) ->
    %% jsx returns [{}] for empty objects
    Record;
decode_record({Record, Table}, Json, Opts) ->
    lists:foldl(fun(Pair, A) -> decode_folder(Table, Pair, Opts, A) end, Record, Json).

-spec decode_single_record(record_desc(), binary(), jsx:json_term(), decode_opts()) -> tuple().
decode_single_record(Rec, Name, Json, Opts)->
    Desc = {#ecs_single_return{},
        [{Name, #ecs_single_return.object,
            fun(V, O) -> decode_record(Rec, V, O) end}
        ]},
    R = decode_record(Desc, Json, Opts),
    element(#ecs_single_return.object, R).


%%%------------------------------------------------------------------------------
%%% Output
%%%------------------------------------------------------------------------------
-spec out(json_return(), decode_fun(), ecs_opts())
         -> {ok, jsx:json_term() | tuple()} |
            {simple, term()} |
            {error, term()}.
out({error, Reason}, _, _) ->
    {error, Reason};
out({ok, Json}, Decode, Opts) ->
    case proplists:get_value(out, Opts, record) of
        json ->
            {ok, Json};
        record ->
            {ok, Decode(Json, [])}
    end.

%%%------------------------------------------------------------------------------
%%% Shared Records
%%%------------------------------------------------------------------------------
-spec attribute_record() -> record_desc().
attribute_record() ->
    {#ecs_attribute{},
        [
            {<<"name">>, #ecs_attribute.name, fun id/2},
            {<<"value">>, #ecs_attribute.value, fun id/2}
        ]
    }.

-spec resource_record() -> record_desc().
resource_record() ->
    {#ecs_resource{},
        [
            {<<"doubleValue">>, #ecs_resource.double_value, fun id/2},
            {<<"integerValue">>, #ecs_resource.integer_value, fun id/2},
            {<<"longValue">>, #ecs_resource.long_value, fun id/2},
            {<<"name">>, #ecs_resource.name, fun id/2},
            {<<"type">>, #ecs_resource.type, fun id/2},
            {<<"stringSetValue">>, #ecs_resource.string_set_value, fun id/2}
        ]
    }.


-spec deployment_configuration_record() -> record_desc().
deployment_configuration_record() ->
    {#ecs_deployment_configuration{},
        [
            {<<"maximumPercent">>, #ecs_deployment_configuration.maximum_percent, fun id/2},
            {<<"minimumHealthyPercent">>, #ecs_deployment_configuration.minimum_healthy_percent, fun id/2}
        ]
    }.

-spec deployment_record() -> record_desc().
deployment_record() ->
    {#ecs_deployment{},
        [
            {<<"createdAt">>, #ecs_deployment.created_at, fun id/2},
            {<<"desiredCount">>, #ecs_deployment.desired_count, fun id/2},
            {<<"id">>, #ecs_deployment.id, fun id/2},
            {<<"pendingCount">>, #ecs_deployment.pending_count, fun id/2},
            {<<"runningCount">>, #ecs_deployment.running_count, fun id/2},
            {<<"status">>, #ecs_deployment.status, fun id/2},
            {<<"taskDefinition">>, #ecs_deployment.task_definition, fun id/2},
            {<<"updatedAt">>, #ecs_deployment.updated_at, fun id/2}
        ]
    }.

-spec event_record() -> record_desc().
event_record() ->
    {#ecs_event{},
        [
            {<<"createdAt">>, #ecs_event.created_at, fun id/2},
            {<<"id">>, #ecs_event.id, fun id/2},
            {<<"message">>, #ecs_event.message, fun id/2}
        ]
    }.

-spec load_balancer_record() -> record_desc().
load_balancer_record() ->
    {#ecs_load_balancer{},
        [
            {<<"containerName">>, #ecs_load_balancer.container_name, fun id/2},
            {<<"containerPort">>, #ecs_load_balancer.container_port, fun id/2},
            {<<"loadBalancerName">>, #ecs_load_balancer.load_balancer_name, fun id/2},
            {<<"targetGroupArn">>, #ecs_load_balancer.target_group_arn, fun id/2}
        ]
    }.

-spec cluster_record() -> record_desc().
cluster_record() ->
    {#ecs_cluster{},
        [
            {<<"activeServicesCount">>, #ecs_cluster.active_services_count, fun id/2},
            {<<"clusterArn">>, #ecs_cluster.cluster_arn, fun id/2},
            {<<"clusterName">>, #ecs_cluster.cluster_name, fun id/2},
            {<<"pendingTasksCount">>, #ecs_cluster.pending_tasks_count, fun id/2},
            {<<"registeredContainerInstancesCount">>, #ecs_cluster.registered_container_instances_count, fun id/2},
            {<<"runningTasksCount">>, #ecs_cluster.running_tasks_count, fun id/2},
            {<<"status">>, #ecs_cluster.status, fun id/2}
        ]
    }.

-spec service_record() -> record_desc().
service_record() ->
    {#ecs_service{},
        [
            {<<"clusterArn">>, #ecs_service.cluster_arn, fun id/2},
            {<<"createdAt">>, #ecs_service.created_at, fun id/2},
            {<<"deploymentConfiguration">>, #ecs_service.deployment_configuration, fun decode_deployment_configuration/2},
            {<<"deployments">>, #ecs_service.deployments, fun decode_deployment_list/2},
            {<<"desiredCount">>, #ecs_service.desired_count, fun id/2},
            {<<"events">>, #ecs_service.events, fun decode_events_list/2},
            {<<"loadBalancers">>, #ecs_service.load_balancers, fun decode_load_balancers_list/2},
            {<<"pendingCount">>, #ecs_service.pending_count, fun id/2},
            {<<"roleArn">>, #ecs_service.role_arn, fun id/2},
            {<<"runningCount">>, #ecs_service.running_count, fun id/2},
            {<<"serviceArn">>, #ecs_service.service_arn, fun id/2},
            {<<"serviceName">>, #ecs_service.service_name, fun id/2},
            {<<"status">>, #ecs_service.status, fun id/2},
            {<<"taskDefinition">>, #ecs_service.task_definition, fun id/2}
        ]
    }.

-spec version_info_record() -> record_desc().
version_info_record() ->
    {#ecs_version_info{},
        [
            {<<"agentHash">>, #ecs_version_info.agent_hash, fun id/2},
            {<<"agentVersion">>, #ecs_version_info.agent_version, fun id/2},
            {<<"dockerVersion">>, #ecs_version_info.docker_version, fun id/2}
        ]
    }.

-spec container_instance_record() -> record_desc().
container_instance_record() ->
    {#ecs_container_instance{},
        [
            {<<"agentConnected">>, #ecs_container_instance.agent_connected, fun id/2},
            {<<"agentUpdateStatus">>, #ecs_container_instance.agent_update_status, fun id/2},
            {<<"attributes">>, #ecs_container_instance.attributes, fun decode_attributes_list/2},
            {<<"containerInstanceArn">>, #ecs_container_instance.container_instance_arn, fun id/2},
            {<<"ec2InstanceId">>, #ecs_container_instance.ec2_instance_id, fun id/2},
            {<<"pendingTasksCount">>, #ecs_container_instance.pending_tasks_count, fun id/2},
            {<<"registeredResources">>, #ecs_container_instance.registered_resources, fun decode_resources_list/2},
            {<<"remainingResources">>, #ecs_container_instance.remaining_resources, fun decode_resources_list/2},
            {<<"runningTasksCount">>, #ecs_container_instance.running_tasks_count, fun id/2},
            {<<"status">>, #ecs_container_instance.status, fun id/2},
            {<<"versionInfo">>, #ecs_container_instance.version_info, fun decode_version_info/2}
        ]
    }.

-spec failure_record() -> record_desc().
failure_record() ->
    {#ecs_failure{},
        [
            {<<"arn">>, #ecs_failure.arn, fun id/2},
            {<<"reason">>, #ecs_failure.reason, fun id/2}
        ]
    }.

-spec volume_host_record() -> record_desc().
volume_host_record() ->
    {#ecs_volume_host{},
        [
            {<<"sourcePath">>, #ecs_volume_host.source_path, fun id/2}
        ]
    }.

-spec volume_record() -> record_desc().
volume_record() ->
    {#ecs_volume{},
        [
            {<<"host">>, #ecs_volume.host, fun decode_volume_host/2},
            {<<"name">>, #ecs_volume.name, fun id/2}
        ]
    }.

-spec host_record() -> record_desc().
host_record() ->
    {#ecs_host{},
        [
            {<<"hostname">>, #ecs_host.host_name, fun id/2},
            {<<"ipAddress">>, #ecs_host.ip_address, fun id/2}
        ]
    }.

-spec log_configuration_record() -> record_desc().
log_configuration_record() ->
    {#ecs_log_configuration{},
        [
            {<<"logDriver">>, #ecs_log_configuration.log_driver, fun id/2},
            {<<"options">>, #ecs_log_configuration.options, fun id/2}
        ]
    }.

-spec mount_point_record() -> record_desc().
mount_point_record() ->
    {#ecs_mount_point{},
        [
            {<<"containerPath">>, #ecs_mount_point.container_path, fun id/2},
            {<<"readOnly">>, #ecs_mount_point.read_only, fun id/2},
            {<<"sourceVolume">>, #ecs_mount_point.source_volume, fun id/2}
        ]
    }.

-spec port_mapping_record() -> record_desc().
port_mapping_record() ->
    {#ecs_port_mapping{},
        [
            {<<"containerPort">>, #ecs_port_mapping.container_port, fun id/2},
            {<<"hostPort">>, #ecs_port_mapping.host_port, fun id/2},
            {<<"protocol">>, #ecs_port_mapping.protocol, fun decode_atom/2}
        ]
    }.

-spec ulimit_record() -> record_desc().
ulimit_record() ->
    {#ecs_ulimit{},
        [
            {<<"hardLimit">>, #ecs_ulimit.hard_limit, fun id/2},
            {<<"softLimit">>, #ecs_ulimit.soft_limit, fun id/2},
            {<<"name">>, #ecs_ulimit.name, fun decode_atom/2}
        ]
    }.

-spec volume_from_record() -> record_desc().
volume_from_record() ->
    {#ecs_volume_from{},
        [
            {<<"readOnly">>, #ecs_volume_from.read_only, fun id/2},
            {<<"sourceContainer">>, #ecs_volume_from.source_container, fun id/2}
        ]
    }.

-spec container_definition_record() -> record_desc().
container_definition_record() ->
    {#ecs_container_definition{},
        [
            {<<"command">>, #ecs_container_definition.command, fun id/2},
            {<<"cpu">>, #ecs_container_definition.cpu, fun id/2},
            {<<"disableNetworking">>, #ecs_container_definition.disable_networking, fun id/2},
            {<<"dnsSearchDomains">>, #ecs_container_definition.dns_search_domains, fun id/2},
            {<<"dockerLabels">>, #ecs_container_definition.docker_labels, fun id/2},
            {<<"dockerSecurityOptions">>, #ecs_container_definition.docker_security_options, fun id/2},
            {<<"entryPoint">>, #ecs_container_definition.entry_point, fun id/2},
            {<<"environment">>, #ecs_container_definition.environment, fun decode_attributes_list/2},
            {<<"essential">>, #ecs_container_definition.essential, fun id/2},
            {<<"extraHosts">>, #ecs_container_definition.extra_hosts, fun decode_hosts_list/2},
            {<<"hostname">>, #ecs_container_definition.host_name, fun id/2},
            {<<"image">>, #ecs_container_definition.image, fun id/2},
            {<<"links">>, #ecs_container_definition.links, fun id/2},
            {<<"logConfiguration">>, #ecs_container_definition.log_configuration, fun decode_log_configuration/2},
            {<<"memory">>, #ecs_container_definition.memory, fun id/2},
            {<<"memoryReservation">>, #ecs_container_definition.memory_reservation, fun id/2},
            {<<"mountPoints">>, #ecs_container_definition.mount_points, fun decode_mount_points_list/2},
            {<<"name">>, #ecs_container_definition.name, fun id/2},
            {<<"portMappings">>, #ecs_container_definition.port_mappings, fun decode_port_mappings_list/2},
            {<<"privileged">>, #ecs_container_definition.privileged, fun id/2},
            {<<"readonlyRootFilesystem">>, #ecs_container_definition.readonly_root_filesystem, fun id/2},
            {<<"ulimits">>, #ecs_container_definition.ulimits, fun decode_ulimits_list/2},
            {<<"user">>, #ecs_container_definition.user, fun id/2},
            {<<"volumesFrom">>, #ecs_container_definition.volumes_from, fun decode_volumes_from_list/2},
            {<<"workingDirectory">>, #ecs_container_definition.working_directory, fun id/2}
        ]
    }.

-spec task_definition_record() -> record_desc().
task_definition_record() ->
    {#ecs_task_definition{},
        [
            {<<"containerDefinitions">>, #ecs_task_definition.container_definitions, fun decode_container_definitions_list/2},
            {<<"family">>, #ecs_task_definition.family, fun id/2},
            {<<"networkMode">>, #ecs_task_definition.network_mode, fun decode_atom/2},
            {<<"requiresAttributes">>, #ecs_task_definition.requires_attributes, fun decode_attributes_list/2},
            {<<"revision">>, #ecs_task_definition.revision, fun id/2},
            {<<"status">>, #ecs_task_definition.status, fun id/2},
            {<<"taskDefinitionArn">>, #ecs_task_definition.task_definition_arn, fun id/2},
            {<<"taskRoleArn">>, #ecs_task_definition.task_role_arn, fun id/2},
            {<<"volumes">>, #ecs_task_definition.volumes, fun decode_volumes_list/2}
        ]
    }.

-spec network_binding_record() -> record_desc().
network_binding_record() ->
    {#ecs_network_binding{},
        [
            {<<"bindIP">>, #ecs_network_binding.bind_ip, fun id/2},
            {<<"containerPort">>, #ecs_network_binding.container_port, fun id/2},
            {<<"hostPort">>, #ecs_network_binding.host_port, fun id/2},
            {<<"protocol">>, #ecs_network_binding.protocol, fun decode_atom/2}
        ]
    }.

-spec container_record() -> record_desc().
container_record() ->
    {#ecs_container{},
        [
            {<<"containerArn">>, #ecs_container.container_arn, fun id/2},
            {<<"exitCode">>, #ecs_container.exit_code, fun id/2},
            {<<"lastStatus">>, #ecs_container.last_status, fun id/2},
            {<<"name">>, #ecs_container.name, fun id/2},
            {<<"networkBindings">>, #ecs_container.network_bindings, fun decode_network_bindings_list/2},
            {<<"reason">>, #ecs_container.reason, fun id/2},
            {<<"taskArn">>, #ecs_container.task_arn, fun id/2}
        ]
    }.

-spec container_override_record() -> record_desc().
container_override_record() ->
    {#ecs_container_override{},
        [
            {<<"command">>, #ecs_container_override.command, fun id/2},
            {<<"environment">>, #ecs_container_override.environment, fun decode_attributes_list/2},
            {<<"name">>, #ecs_container_override.name, fun id/2}
        ]
    }.

-spec task_override_record() -> record_desc().
task_override_record() ->
    {#ecs_task_override{},
        [
            {<<"containerOverrides">>, #ecs_task_override.container_overrides, fun decode_container_overrides_list/2},
            {<<"taskRoleArn">>, #ecs_task_override.task_role_arn, fun id/2}
        ]
    }.

-spec task_record() -> record_desc().
task_record() ->
    {#ecs_task{},
        [
            {<<"clusterArn">>, #ecs_task.cluster_arn, fun id/2},
            {<<"containerInstanceArn">>, #ecs_task.container_instance_arn, fun id/2},
            {<<"containers">>, #ecs_task.containers, fun decode_containers_list/2},
            {<<"createdAt">>, #ecs_task.created_at, fun id/2},
            {<<"desiredStatus">>, #ecs_task.desired_status, fun id/2},
            {<<"lastStatus">>, #ecs_task.last_status, fun id/2},
            {<<"overrides">>, #ecs_task.overrides, fun decode_task_overrides/2},
            {<<"startedAt">>, #ecs_task.started_at, fun id/2},
            {<<"startedBy">>, #ecs_task.started_by, fun id/2},
            {<<"stoppedAt">>, #ecs_task.stopped_at, fun id/2},
            {<<"stoppedReason">>, #ecs_task.stopped_reason, fun id/2},
            {<<"taskArn">>, #ecs_task.task_arn, fun id/2},
            {<<"taskDefinitionArn">>, #ecs_task.task_definition_arn, fun id/2}
        ]
    }.

decode_deployment_configuration(V, Opts) ->
    decode_record(deployment_configuration_record(), V, Opts).

decode_deployment_list(V, Opts) ->
    [decode_record(deployment_record(), I, Opts) || I <- V].

decode_events_list(V, Opts) ->
    [decode_record(event_record(), I, Opts) || I <- V].

decode_load_balancers_list(V, Opts) ->
    [decode_record(load_balancer_record(), I, Opts) || I <- V].

decode_attributes_list(V, Opts) ->
    [decode_record(attribute_record(), I, Opts) || I <- V].

decode_resources_list(V, Opts) ->
    [decode_record(resource_record(), I, Opts) || I <- V].

decode_version_info(V, Opts) ->
    decode_record(version_info_record(), V, Opts).

decode_clusters_list(V, Opts) ->
    [decode_record(cluster_record(), I, Opts) || I <- V].

decode_failures_list(V, Opts) ->
    [decode_record(failure_record(), I, Opts) || I <- V].

decode_container_instances_list(V, Opts) ->
    [decode_record(container_instance_record(), I, Opts) || I <- V].

decode_services_list(V, Opts) ->
    [decode_record(service_record(), I, Opts) || I <- V].

decode_volume_host(V, Opts) ->
    decode_record(volume_host_record(), V, Opts).

decode_volumes_list(V, Opts) ->
    [decode_record(volume_record(), I, Opts) || I <- V].

decode_container_definitions_list(V, Opts) ->
    [decode_record(container_definition_record(), I, Opts) || I <- V].

decode_hosts_list(V, Opts) ->
    [decode_record(host_record(), I, Opts) || I <- V].

decode_log_configuration(V, Opts) ->
    decode_record(log_configuration_record(), V, Opts).

decode_mount_points_list(V, Opts) ->
    [decode_record(mount_point_record(), I, Opts) || I <- V].

decode_port_mappings_list(V, Opts) ->
    [decode_record(port_mapping_record(), I, Opts) || I <- V].

decode_atom(V, _Opts) ->
    binary_to_existing_atom(V, utf8).

decode_ulimits_list(V, Opts) ->
    [decode_record(ulimit_record(), I, Opts) || I <- V].

decode_volumes_from_list(V, Opts) ->
    [decode_record(volume_from_record(), I, Opts) || I <- V].

decode_tasks_list(V, Opts) ->
    [decode_record(task_record(), I, Opts) || I <- V].

decode_containers_list(V, Opts) ->
    [decode_record(container_record(), I, Opts) || I <- V].

decode_network_bindings_list(V, Opts) ->
    [decode_record(network_binding_record(), I, Opts) || I <- V].

decode_task_overrides(V, Opts) ->
    decode_record(task_override_record(), V, Opts).

decode_container_overrides_list(V, Opts) ->
    [decode_record(container_override_record(), I, Opts) || I <- V].
%%%------------------------------------------------------------------------------
%%% AWS ECS API Functions
%%%------------------------------------------------------------------------------

%%%------------------------------------------------------------------------------
%% CreateCluster
%%%------------------------------------------------------------------------------
-type create_cluster_opt() :: {cluster_name, string_param()}.
-type create_cluster_opts() :: [create_cluster_opt()].

-spec create_cluster_opts() -> opt_table().
create_cluster_opts() ->
    [
        {cluster_name, <<"clusterName">>, fun to_binary/1}
    ].

-spec create_cluster() -> ecs_return(#ecs_cluster{}).
create_cluster() ->
    create_cluster(default_config()).

-spec create_cluster(create_cluster_opts() | aws_config()) -> ecs_return(#ecs_cluster{}).
create_cluster(#aws_config{} = Config) ->
    create_cluster([], Config);
create_cluster(Opts) ->
    create_cluster(Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% ECS API
%% [http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_CreateCluster.html]
%%
%% ===Example===
%%
%% Create ECS Cluster named "NewCluster"
%%
%% `
%% {ok, Cluster} = erlcloud_ecs:create_cluster([{cluster_name, <<"NewCluster">>}, {out, json}]),
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec create_cluster(Opts :: create_cluster_opts(), Config :: aws_config()) -> ecs_return(#ecs_cluster{}).
create_cluster(Opts, #aws_config{} = Config) ->
    {AwsOpts, EcsOpts} = opts(create_cluster_opts(), Opts),
    Return = ecs_request(
                Config,
                "CreateCluster",
                AwsOpts),
    out(Return, fun(Json, UOpts) -> decode_single_record(cluster_record(), <<"cluster">>, Json, UOpts) end,
        EcsOpts).
    
%%%------------------------------------------------------------------------------
% CreateService
%%%------------------------------------------------------------------------------
-type create_service_opt() :: client_token_opt() |
                              cluster_opt() |
                              deployment_configuration() |
                              load_balancers_opt() |
                              role_opt().
-type create_service_opts() :: [create_service_opt()].

-spec create_service_opts() -> opt_table().
create_service_opts() ->
    [
        client_token_opt(),
        cluster_opt(),
        deployment_configuration_opt(),
        load_balancers_opt(),
        role_opt()
    ].

-spec create_service(ServiceName :: string_param(),
                     TaskDefinition :: string_param(),
                     DesiredCount :: pos_integer()) -> ecs_return(#ecs_service{}).
create_service(ServiceName, TaskDefinition, DesiredCount) ->
    create_service(ServiceName, TaskDefinition, DesiredCount, default_config()).

-spec create_service(ServiceName :: string_param(),
                     TaskDefinition :: string_param(),
                     DesiredCount :: pos_integer(),
                     create_service_opts() | aws_config()) -> ecs_return(#ecs_service{}).
create_service(ServiceName, TaskDefinition, DesiredCount, #aws_config{} = Config) ->
    create_service(ServiceName, TaskDefinition, DesiredCount, [], Config);
create_service(ServiceName, TaskDefinition, DesiredCount, Opts) ->
    create_service(ServiceName, TaskDefinition, DesiredCount, Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% ECS API
%% [http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_CreateService.html]
%%
%% ===Example===
%%
%% Create ECS Service named "ecs-simple-service-test" in a "NewTest" cluster.
%%
%% `
%% {ok, Service} = erlcloud_ecs:create_service("ecs-simple-service-test", "hello_world", 1, [{cluster, "NewTest"}, {out, json}]).
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec create_service(ServiceName :: string_param(),
                     TaskDefinition :: string_param(),
                     DesiredCount :: pos_integer(),
                     Opts :: create_service_opts(), Config :: aws_config()) -> ecs_return(#ecs_service{}).
create_service(ServiceName, TaskDefinition, DesiredCount, Opts, #aws_config{} = Config) ->
    Params = [
                {<<"serviceName">>, to_binary(ServiceName)},
                {<<"taskDefinition">>, to_binary(TaskDefinition)},
                {<<"desiredCount">>, DesiredCount}
           ],
    {AwsOpts, EcsOpts} = opts(create_service_opts(), Opts),
    Return = ecs_request(
                Config,
                "CreateService",
                Params ++ AwsOpts),
    out(Return, fun(Json, UOpts) -> decode_single_record(service_record(), <<"service">>, Json, UOpts) end,
        EcsOpts).

%%%------------------------------------------------------------------------------
%% DeleteCluster 
%%%------------------------------------------------------------------------------
-spec delete_cluster(ClusterName :: string_param()) -> ecs_return(#ecs_cluster{}).
delete_cluster(ClusterName) ->
    delete_cluster(ClusterName, default_config()).

-spec delete_cluster(ClusterName :: string_param(), ecs_opts() | aws_config()) -> ecs_return(#ecs_cluster{}).
delete_cluster(ClusterName, #aws_config{} = Config) ->
    delete_cluster(ClusterName, [], Config);
delete_cluster(ClusterName, Opts) ->
    delete_cluster(ClusterName, Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% ECS API
%% [http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DeleteCluster.html]
%%
%% ===Example===
%%
%% Delete ECS cluster named "NewTest".
%%
%% `
%% {ok, Service} = erlcloud_ecs:create_service("ecs-simple-service-test", "hello_world", 1, [{cluster, "NewTest"}, {out, json}]).
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec delete_cluster(ClusterName :: string_param(), ecs_opts(), aws_config()) -> ecs_return(#ecs_cluster{}).
delete_cluster(ClusterName, Opts, #aws_config{} = Config) ->
    {[], EcsOpts} = opts([], Opts),
    Return = ecs_request(
                Config,
                "DeleteCluster",
                [{<<"cluster">>, to_binary(ClusterName)}]),

    out(Return, fun(Json, UOpts) -> decode_single_record(cluster_record(), <<"cluster">>, Json, UOpts) end,
        EcsOpts).


%%%------------------------------------------------------------------------------
%% DeleteService
%%%------------------------------------------------------------------------------
-type delete_service_opt() :: {cluster, string_param()}.
-type delete_service_opts() :: [delete_service_opt()].

-spec delete_service_opts() -> opt_table().
delete_service_opts() ->
    [
        {cluster, <<"cluster">>, fun to_binary/1}
    ].

-spec delete_service(ServiceName :: string_param()) -> ecs_return( #ecs_service{}).
delete_service(ServiceName) ->
    delete_service(ServiceName, [], default_config()).

-spec delete_service(ServiceName :: string_param(), delete_service_opts() | aws_config()) -> ecs_return(#ecs_service{}).
delete_service(ServiceName, #aws_config{} = Config) ->
    delete_service(ServiceName, [], Config);
delete_service(ServiceName, Opts) ->
    delete_service(ServiceName, Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% ECS API
%% [http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DeleteService.html]
%%
%% ===Example===
%%
%% Delete ECS Service named "ecs-simple-service-test" from "NewTest" cluster.
%%
%% `
%% {ok, Service} = erlcloud_ecs:create_service("ecs-simple-service-test", "hello_world", 1, [{cluster, "NewTest"}, {out, json}]).
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec delete_service(
        ServiceName :: string_param(),
        Opts :: delete_service_opts(),
        Config :: aws_config()) -> ecs_return(#ecs_service{}).
delete_service(ServiceName, Opts, #aws_config{} = Config) ->
    Params = [{<<"service">>, to_binary(ServiceName)}],
    {AwsOpts, EcsOpts} = opts(delete_service_opts(), Opts),

    Return = ecs_request(
                Config,
                "DeleteService",
                Params ++ AwsOpts),
    out(Return, fun(Json, UOpts) -> decode_single_record(service_record(), <<"service">>, Json, UOpts) end,
        EcsOpts).

%%%------------------------------------------------------------------------------
%% DeregisterContainerInstance
%%%------------------------------------------------------------------------------
-type deregister_container_instance_opt() :: {cluster, string_param()} |
                                             {force, boolean()}.
-type deregister_container_instance_opts() :: [deregister_container_instance_opt()].

-spec deregister_container_instance_opts() -> opt_table().
deregister_container_instance_opts() ->
    [
        {cluster, <<"cluster">>, fun to_binary/1},
        {force, <<"cluster">>, fun id/1}
    ].

-spec deregister_container_instance(ContainerInstance :: string_param()) -> ecs_return(#ecs_container_instance{}).
deregister_container_instance(ContainerInstance) ->
    deregister_container_instance(ContainerInstance, [], default_config()).

-spec deregister_container_instance(
                ContainerInstance :: string_param(),
                deregister_container_instance_opts() | aws_config()) -> ecs_return(#ecs_container_instance{}).
deregister_container_instance(ContainerInstance, #aws_config{} = Config) ->
    deregister_container_instance(ContainerInstance, [], Config);

deregister_container_instance(ContainerInstance, Opts) ->
    deregister_container_instance(ContainerInstance, Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% ECS API
%% [http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DeregisterContainerInstance.html]
%%
%% ===Example===
%% 
%% Dergister ECS Container Instance with id "c9c9a6f2-8766-464b-8805-9c57b9368fb0".
%%
%% `
%% {ok, Result} = deregister_container_instance("c9c9a6f2-8766-464b-8805-9c57b9368fb0", [{out, record}]),
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec deregister_container_instance(
                ContainerInstance :: string_param(),
                Opts :: deregister_container_instance_opts(),
                Config :: aws_config()) -> ecs_return(#ecs_container_instance{}).
deregister_container_instance(ContainerInstance, Opts, Config) ->
    Params = [
        {<<"containerInstance">>, to_binary(ContainerInstance)}
    ],
    {AwsOpts, EcsOpts} = opts(deregister_container_instance_opts(), Opts),

    Return = ecs_request(
                Config,
                "DeregisterContainerInstance",
                Params ++ AwsOpts),
    out(Return, fun(Json, UOpts) -> decode_single_record(container_instance_record(), <<"containerInstance">>, Json, UOpts) end,
        EcsOpts).


%%%------------------------------------------------------------------------------
%% DeregisterTaskDefinition
%%%------------------------------------------------------------------------------

-spec deregister_task_definition(TaskDefinition :: string_param()) -> ecs_return(#ecs_task_definition{}).
deregister_task_definition(TaskDefinition) ->
    deregister_task_definition(TaskDefinition, [], default_config()).

-spec deregister_task_definition(
            TaskDefinition :: string_param(),
            ecs_opts() | aws_config()) -> ecs_return(#ecs_task_definition{}).
deregister_task_definition(TaskDefinition, #aws_config{} = Config) ->
    deregister_task_definition(TaskDefinition, [], Config);
deregister_task_definition(TaskDefinition, Opts) ->
    deregister_task_definition(TaskDefinition, Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% ECS API
%% [http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DeregisterTaskDefinition.html]
%%
%% ===Example===
%%
%% Delete ECS task definition named "hello_world:1".
%%
%% `
%% {ok, Service} = erlcloud_ecs:deregister_task_definition("hello_world:1", {out, json}]).
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec deregister_task_definition(
            TaskDefinition :: string_param(),
            ecs_opts(),
            aws_config()) -> ecs_return(#ecs_task_definition{}).
deregister_task_definition(TaskDefinition, Opts, Config) ->
    {[], EcsOpts} = opts([], Opts),
    Return = ecs_request(
                Config,
                "DeregisterTaskDefinition",
                [{<<"taskDefinition">>, to_binary(TaskDefinition)}]),

    out(Return, fun(Json, UOpts) -> decode_single_record(task_definition_record(), <<"taskDefinition">>, Json, UOpts) end,
        EcsOpts).

%%%------------------------------------------------------------------------------
%% DescribeClusters
%%%------------------------------------------------------------------------------
-type describe_clusters_opt() :: {clusters, [string_param()]}.
-type describe_clusters_opts() :: [describe_clusters_opt()].

-spec describe_clusters_opts() -> opt_table().
describe_clusters_opts() ->
    [
        {clusters, <<"clusters">>, fun to_binary/1}
    ].

-spec describe_clusters_record() -> record_desc().
describe_clusters_record() ->
    {#ecs_describe_clusters{},
     [{<<"clusters">>, #ecs_describe_clusters.clusters, fun decode_clusters_list/2},
      {<<"failures">>, #ecs_describe_clusters.failures, fun decode_failures_list/2}
     ]}.

-spec describe_clusters() -> ecs_return(#ecs_describe_clusters{}).
describe_clusters() ->
    describe_clusters([], default_config()).

-spec describe_clusters(describe_clusters_opts() | aws_config()) -> ecs_return(#ecs_describe_clusters{}).
describe_clusters(#aws_config{} = Config) ->
    describe_clusters([], Config);
describe_clusters(Opts) ->
    describe_clusters(Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% ECS API
%% [http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DescribeClusters.html]
%%
%% ===Example===
%%
%% Describe ECS Clusters named "NewCluster" and "TestCluster"
%%
%% `
%% {ok, Clusters} = erlcloud_ecs:describe_clusters([{clusters, ["NewTest", "TestCluster"]}, {out, json}])
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec describe_clusters(Opts :: describe_clusters_opts(), Config :: aws_config()) -> ecs_return(#ecs_describe_clusters{}).
describe_clusters(Opts, #aws_config{} = Config) ->
    {AwsOpts, EcsOpts} = opts(describe_clusters_opts(), Opts),
    Return = ecs_request(
                Config,
                "DescribeClusters",
                AwsOpts),
    out(Return, fun(Json, UOpts) -> decode_record(describe_clusters_record(), Json, UOpts) end, 
        EcsOpts).
 
%%%------------------------------------------------------------------------------
%% DescribeContainerInstances
%%%------------------------------------------------------------------------------
-type describe_container_instances_opt() :: {cluster, string_param()}.
-type describe_container_instances_opts() :: [describe_container_instances_opt()].

-spec describe_container_instances_opts() -> opt_table().
describe_container_instances_opts() ->
    [
        {cluster, <<"cluster">>, fun to_binary/1}
    ].

-spec describe_container_instances_record() -> record_desc().
describe_container_instances_record() ->
    {#ecs_describe_container_instances{},
     [{<<"containerInstances">>, #ecs_describe_container_instances.container_instances, fun decode_container_instances_list/2},
      {<<"failures">>, #ecs_describe_container_instances.failures, fun decode_failures_list/2}
     ]}.

-spec describe_container_instances(Instances :: [string_param()]) -> ecs_return(#ecs_describe_container_instances{}).
describe_container_instances(Instances) ->
    describe_container_instances(Instances, [], default_config()).

-spec describe_container_instances(
        Instances :: [string_param()],
        describe_container_instances_opts() | aws_config()) -> ecs_return(#ecs_describe_container_instances{}).
describe_container_instances(Instances, #aws_config{} = Config) ->
    describe_container_instances(Instances, [], Config);
describe_container_instances(Instances, Opts) ->
    describe_container_instances(Instances, Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% ECS API
%% [http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DescribeContainerInstances.html]
%%
%% ===Example===
%%
%% Describe ECS Container Instances with the the name "709b350e-8c8e-490c-8fa3-d5fc09ac9e0b" in a "TestCluster" cluster
%%
%% `
%% {ok, Instances} = erlcloud_ecs:describe_container_instances(["709b350e-8c8e-490c-8fa3-d5fc09ac9e0b"], [{cluster, "TestCluster"}, {out, json}]),
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec describe_container_instances(
        Instances :: [string_param()],
        Opts :: describe_container_instances_opts(),
        Config :: aws_config()) -> ecs_return(#ecs_describe_container_instances{}).
describe_container_instances(Instances, Opts, #aws_config{} = Config) ->
    {AwsOpts, EcsOpts} = opts(describe_container_instances_opts(), Opts),
    Return = ecs_request(
                Config,
                "DescribeContainerInstances",
                [{<<"containerInstances">>, to_binary(Instances)}] ++ AwsOpts),
    out(Return, fun(Json, UOpts) -> decode_record(describe_container_instances_record(), Json, UOpts) end, 
        EcsOpts).

%%%------------------------------------------------------------------------------
%% DescribeServices
%%%------------------------------------------------------------------------------
-type describe_services_opt() :: {cluster, string_param()}.
-type describe_services_opts() :: [describe_services_opt()].

-spec describe_services_opts() -> opt_table().
describe_services_opts() ->
    [
        {cluster, <<"cluster">>, fun to_binary/1}
    ].

-spec describe_services_record() -> record_desc().
describe_services_record() ->
    {#ecs_describe_services{},
     [{<<"services">>, #ecs_describe_services.services, fun decode_services_list/2},
      {<<"failures">>, #ecs_describe_services.failures, fun decode_failures_list/2}
     ]}.

-spec describe_services(Services :: [string_param()]) -> ecs_return(#ecs_describe_services{}).
describe_services(Services) ->
    describe_services(Services, [], default_config()).

-spec describe_services(
        Services :: [string_param()],
        describe_services_opts() | aws_config()) -> ecs_return(#ecs_describe_services{}).
describe_services(Services, #aws_config{} = Config) ->
    describe_services(Services, [], Config);
describe_services(Services, Opts) ->
    describe_services(Services, Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% ECS API
%% [http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DescribeServices.html]
%%
%% ===Example===
%%
%% Describe ECS Service with the the name "sample-webapp" in a "TestCluster" cluster
%%
%% `
%% {ok, Services} = erlcloud_ecs:describe_services(["sample-webapp"], [{cluster, "TestCluster"}, {out, json}]),
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec describe_services(
        Services :: [string_param()],
        Opts :: describe_services_opts(),
        Config :: aws_config()) -> ecs_return(#ecs_describe_services{}).
describe_services(Services, Opts, #aws_config{} = Config) ->
    {AwsOpts, EcsOpts} = opts(describe_services_opts(), Opts),
    Return = ecs_request(
                Config,
                "DescribeServices",
                [{<<"services">>, to_binary(Services)}] ++ AwsOpts),
    out(Return, fun(Json, UOpts) -> decode_record(describe_services_record(), Json, UOpts) end, 
        EcsOpts).

%%%------------------------------------------------------------------------------
%% DescribeTaskDefinition
%%%------------------------------------------------------------------------------

-spec describe_task_definition(TaskDefinition :: string_param()) -> ecs_return(#ecs_container_definition{}).
describe_task_definition(TaskDefinition) ->
    describe_task_definition(TaskDefinition, default_config()).

-spec describe_task_definition(
            TaskDefinition :: string_param(),
            ecs_opts() | aws_config()) -> ecs_return(#ecs_container_definition{}).
describe_task_definition(TaskDefinition, #aws_config{} = Config) ->
    describe_task_definition(TaskDefinition, [], Config);
describe_task_definition(TaskDefinition, Opts) ->
    describe_task_definition(TaskDefinition, Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% ECS API
%% [http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DescribeTaskDefinition.html]
%%
%% ===Example===
%%
%% Describe "console-sample-app-static" ECS Task
%%
%% `
%% {ok, TaskDefinition} = erlcloud_ecs:describe_task_definition("console-sample-app-static", [{out, record}]).
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec describe_task_definition(
        TaskDefinition :: string_param(),
        Opts :: ecs_opts(),
        Config :: aws_config()) -> ecs_return(#ecs_container_definition{}).
describe_task_definition(TaskDefinition, Opts, #aws_config{} = Config) ->
    {_AwsOpts, EcsOpts} = opts([], Opts),
    Return = ecs_request(
                Config,
                "DescribeTaskDefinition",
                [{<<"taskDefinition">>, to_binary(TaskDefinition)}]),
    out(Return, fun(Json, UOpts) -> decode_single_record(task_definition_record(), <<"taskDefinition">>, Json, UOpts) end,
        EcsOpts).


%%%------------------------------------------------------------------------------
%% DescribeTasks
%%%------------------------------------------------------------------------------
-type describe_tasks_opt() :: {cluster, string_param()}.
-type describe_tasks_opts() :: [describe_tasks_opt()].

-spec describe_tasks_opts() -> opt_table().
describe_tasks_opts() ->
    [
        {cluster, <<"cluster">>, fun to_binary/1}
    ].

-spec describe_tasks_record() -> record_desc().
describe_tasks_record() ->
    {#ecs_describe_tasks{},
     [{<<"tasks">>, #ecs_describe_tasks.tasks, fun decode_tasks_list/2},
      {<<"failures">>, #ecs_describe_tasks.failures, fun decode_failures_list/2}
     ]}.

-spec describe_tasks(Tasks :: [string_param()]) -> ecs_return(#ecs_describe_tasks{}).
describe_tasks(Tasks) ->
    describe_tasks(Tasks, [], default_config()).

-spec describe_tasks(
        Tasks :: [string_param()],
        describe_tasks_opts() | aws_config()) -> ecs_return(#ecs_describe_tasks{}).
describe_tasks(Tasks, #aws_config{} = Config) ->
    describe_tasks(Tasks, [], Config);
describe_tasks(Tasks, Opts) ->
    describe_tasks(Tasks, Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% ECS API
%% [http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DescribeTasks.html]
%%
%% ===Example===
%%
%% Describe "4c89e317-0571-4f07-878b-a8b8c705c347" ECS Task in a "TestCluster" cluster
%%
%% `
%% {ok, Tasks} = erlcloud_ecs:describe_tasks(["4c89e317-0571-4f07-878b-a8b8c705c347"], [{cluster, "TestCluster"}, {out, json}])
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec describe_tasks(
        Tasks :: [string_param()],
        Opts :: describe_tasks_opts(),
        Config :: aws_config()) -> ecs_return(#ecs_describe_tasks{}).
describe_tasks(Tasks, Opts, #aws_config{} = Config) ->
    {AwsOpts, EcsOpts} = opts(describe_tasks_opts(), Opts),
    Return = ecs_request(
                Config,
                "DescribeTasks",
                [{<<"tasks">>, to_binary(Tasks)}] ++ AwsOpts),
    out(Return, fun(Json, UOpts) -> decode_record(describe_tasks_record(), Json, UOpts) end, 
        EcsOpts).


%%%------------------------------------------------------------------------------
%% ListClusters 
%%%------------------------------------------------------------------------------
-type list_clusters_opt() :: {max_results, 1..100} | 
                             {next_token, binary()}.

-type list_clusters_opts() :: [list_clusters_opt()].

-spec list_clusters_opts() -> opt_table().
list_clusters_opts() ->
    [
        {max_results, <<"maxResults">>, fun id/1},
        {next_token, <<"nextToken">>, fun id/1}
    ].

-spec list_clusters_record() -> record_desc().
list_clusters_record() ->
    {#ecs_list_clusters{},
     [{<<"clusterArns">>, #ecs_list_clusters.cluster_arns, fun id/2},
      {<<"nextToken">>, #ecs_list_clusters.next_token, fun id/2}
     ]}.

-spec list_clusters() -> ecs_return(#ecs_list_clusters{}).
list_clusters() ->
    list_clusters([], default_config()).

-spec list_clusters(list_clusters_opts()) -> ecs_return(#ecs_list_clusters{}).
list_clusters(Opts) ->
    list_clusters(Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% ECS API
%% [http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListClusters.html]
%%
%% ===Example===
%%
%% List ECS Clusters
%%
%% `
%% {ok, Result} = erlcloud_ecs:list_clusters([{max_results, 1}, {out, json}]),
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec list_clusters(list_clusters_opts(), aws_config()) -> ecs_return(#ecs_list_clusters{}).
list_clusters(Opts, Config) ->
    {AwsOpts, EcsOpts} = opts(list_clusters_opts(), Opts),
    Return = ecs_request(Config, "ListClusters", AwsOpts),
    out(Return, fun(Json, UOpts) -> decode_record(list_clusters_record(), Json, UOpts) end, 
        EcsOpts).

%%%------------------------------------------------------------------------------
%% ListContainerInstances
%%%------------------------------------------------------------------------------
-type list_container_instances_opt() :: {cluster, string_param()} |
                                        {max_results, 1..100} |
                                        {next_token, binary()}.

-type list_container_instances_opts() :: [list_container_instances_opt()].

-spec list_container_instances_opts() -> opt_table().
list_container_instances_opts() ->
    [
        {cluster, <<"cluster">>, fun to_binary/1},
        {max_results, <<"maxResults">>, fun id/1},
        {next_token, <<"nextToken">>, fun to_binary/1}
    ].

-spec list_container_instances_record() -> record_desc().
list_container_instances_record() ->
    {#ecs_list_container_instances{},
     [{<<"containerInstanceArns">>, #ecs_list_container_instances.container_instance_arns, fun id/2},
      {<<"nextToken">>, #ecs_list_container_instances.next_token, fun id/2}
     ]}.

-spec list_container_instances() -> ecs_return(#ecs_list_container_instances{}).
list_container_instances() ->
    list_container_instances([], default_config()).

-spec list_container_instances(list_container_instances_opts()) -> ecs_return(#ecs_list_container_instances{}).
list_container_instances(Opts) ->
    list_container_instances(Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% ECS API
%% [http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListContainerInstances.html]
%%
%% ===Example===
%%
%% List one container instances from 'TestCluster' ECS Cluster
%%
%% `
%% {ok, Result} = erlcloud_ecs:list_container_instances([{cluster, "TestCluster"}, {max_results, 1}, {out, json}]),
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec list_container_instances(list_container_instances_opts(), aws_config()) -> ecs_return(#ecs_list_container_instances{}).
list_container_instances(Opts, Config) ->
    {AwsOpts, EcsOpts} = opts(list_container_instances_opts(), Opts),
    Return = ecs_request(Config, "ListContainerInstances", AwsOpts),
    out(Return, fun(Json, UOpts) -> decode_record(list_container_instances_record(), Json, UOpts) end, 
        EcsOpts).

%%%------------------------------------------------------------------------------
%% ListServices
%%%------------------------------------------------------------------------------
-type list_services_opt() :: {cluster, string_param()} |
                                        {max_results, 1..100} |
                                        {next_token, binary()}.

-type list_services_opts() :: [list_services_opt()].

-spec list_services_opts() -> opt_table().
list_services_opts() ->
    [
        {cluster, <<"cluster">>, fun to_binary/1},
        {max_results, <<"maxResults">>, fun id/1},
        {next_token, <<"nextToken">>, fun to_binary/1}
    ].

-spec list_services_record() -> record_desc().
list_services_record() ->
    {#ecs_list_services{},
     [{<<"serviceArns">>, #ecs_list_services.service_arns, fun id/2},
      {<<"nextToken">>, #ecs_list_services.next_token, fun id/2}
     ]}.

-spec list_services() -> ecs_return(#ecs_list_services{}).
list_services() ->
    list_services([], default_config()).

-spec list_services(list_services_opts()) -> ecs_return(#ecs_list_services{}).
list_services(Opts) ->
    list_services(Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% ECS API
%% [http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListServices.html]
%%
%% ===Example===
%%
%% List one service from 'TestCluster' ECS Cluster
%%
%% `
%% {ok, Result} = erlcloud_ecs:list_services([{cluster, "TestCluster"}, {max_results, 1}, {out, json}]),
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec list_services(list_services_opts(), aws_config()) -> ecs_return(#ecs_list_services{}).
list_services(Opts, Config) ->
    {AwsOpts, EcsOpts} = opts(list_services_opts(), Opts),
    Return = ecs_request(Config, "ListServices", AwsOpts),
    out(Return, fun(Json, UOpts) -> decode_record(list_services_record(), Json, UOpts) end,
        EcsOpts).

%%%------------------------------------------------------------------------------
%% ListTaskDefinitionFamilies
%%%------------------------------------------------------------------------------
-type list_task_definition_families_opt() :: {family_prefix, string_param()} |
                                             {status, active | inactive | all} |
                                             {max_results, 1..100} |
                                             {next_token, binary()}.

-type list_task_definition_families_opts() :: [list_task_definition_families_opt()].

-spec list_task_definition_families_opts() -> opt_table().
list_task_definition_families_opts() ->
    [
        {family_prefix, <<"familyPrefix">>, fun to_binary/1},
        {status, <<"status">>, fun to_binary/1},
        {max_results, <<"maxResults">>, fun id/1},
        {next_token, <<"nextToken">>, fun to_binary/1}
    ].

-spec list_task_definition_families_record() -> record_desc().
list_task_definition_families_record() ->
    {#ecs_list_task_definition_families{},
     [{<<"families">>, #ecs_list_task_definition_families.families, fun id/2},
      {<<"nextToken">>, #ecs_list_task_definition_families.next_token, fun id/2}
     ]}.


-spec list_task_definition_families() -> ecs_return(#ecs_list_task_definition_families{}).
list_task_definition_families() ->
    list_task_definition_families([], default_config()).

-spec list_task_definition_families(list_task_definition_families_opts()) -> ecs_return(#ecs_list_task_definition_families{}).
list_task_definition_families(Opts) ->
    list_task_definition_families(Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% ECS API
%% [http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListTaskDefinitionFamilies.html]
%%
%% ===Example===
%%
%% List active task families with prefix console
%%
%% `
%% {ok, Result} = erlcloud_ecs:list_task_definition_families([{family_prefix, "console"}, {status, "active"}, {out, json}]).
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec list_task_definition_families(
        list_task_definition_families_opts(),
        aws_config()) -> ecs_return(#ecs_list_task_definition_families{}).
list_task_definition_families(Opts, Config) ->
    {AwsOpts, EcsOpts} = opts(list_task_definition_families_opts(), Opts),
    Return = ecs_request(Config, "ListTaskDefinitionFamilies", AwsOpts),
    out(Return, fun(Json, UOpts) -> decode_record(list_task_definition_families_record(), Json, UOpts) end, 
        EcsOpts).


%%%------------------------------------------------------------------------------
%% ListTaskDefinitions
%%%------------------------------------------------------------------------------
-type list_task_definitions_opt() :: {family_prefix, string_param()} |
                                     {status, active | inactive} |
                                     {sort, asc | desc} |
                                     {max_results, 1..100} |
                                     {next_token, binary()}.

-type list_task_definitions_opts() :: [list_task_definitions_opt()].

-spec list_task_definitions_opts() -> opt_table().
list_task_definitions_opts() ->
    [
        {family_prefix, <<"familyPrefix">>, fun to_binary/1},
        {status, <<"status">>, fun to_binary/1},
        {sort, <<"sort">>, fun to_binary/1},
        {max_results, <<"maxResults">>, fun id/1},
        {next_token, <<"nextToken">>, fun to_binary/1}
    ].

-spec list_task_definitions_record() -> record_desc().
list_task_definitions_record() ->
    {#ecs_list_task_definitions{},
     [{<<"taskDefinitionArns">>, #ecs_list_task_definitions.task_definition_arns, fun id/2},
      {<<"nextToken">>, #ecs_list_task_definitions.next_token, fun id/2}
     ]}.

-spec list_task_definitions() -> ecs_return(#ecs_list_task_definitions{}).
list_task_definitions() ->
    list_task_definitions([], default_config()).

-spec list_task_definitions(list_task_definitions_opts()) -> ecs_return(#ecs_list_task_definitions{}).
list_task_definitions(Opts) ->
    list_task_definitions(Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% ECS API
%% [http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListTaskDefinitions.html]
%%
%% ===Example===
%%
%% List active task definitions with prefix 'console-sample-app-static'
%%
%% `
%% {ok, Result} = erlcloud_ecs:list_task_definitions([{family_prefix, "console-sample-app-static"}, {status, "active"}, {out, json}]),
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec list_task_definitions(list_task_definitions_opts(), aws_config()) -> ecs_return(#ecs_list_task_definitions{}).
list_task_definitions(Opts, Config) ->
    {AwsOpts, EcsOpts} = opts(list_task_definitions_opts(), Opts),
    Return = ecs_request(Config, "ListTaskDefinitions", AwsOpts),
    out(Return, fun(Json, UOpts) -> decode_record(list_task_definitions_record(), Json, UOpts) end, 
        EcsOpts).

%%%------------------------------------------------------------------------------
%% ListTasks
%%%------------------------------------------------------------------------------
-type list_tasks_opt() :: {cluster, string_param()} |
                          {container_instance, string_param()} |
                          {desired_state, running | pending | stopped} |
                          {service_name, string_param()} |
                          {started_by, string_param()} |
                          {family, string_param()} |
                          {sort, asc | desc} |
                          {max_results, 1..100} |
                          {next_token, binary()}.

-type list_tasks_opts() :: [list_tasks_opt()].

-spec list_tasks_opts() -> opt_table().
list_tasks_opts() ->
    [
        {cluster, <<"cluster">>, fun to_binary/1},
        {container_instance, <<"containerInstance">>, fun to_binary/1},
        {desired_status, <<"desiredStatus">>, fun to_binary/1},
        {family, <<"family">>, fun to_binary/1},
        {sort, <<"sort">>, fun to_binary/1},
        {service_name, <<"serviceName">>, fun to_binary/1},
        {started_by, <<"startedBy">>, fun to_binary/1},
        {max_results, <<"maxResults">>, fun id/1},
        {next_token, <<"nextToken">>, fun to_binary/1}
    ].

-spec list_tasks_record() -> record_desc().
list_tasks_record() ->
    {#ecs_list_tasks{},
     [{<<"taskArns">>, #ecs_list_tasks.task_arns, fun id/2},
      {<<"nextToken">>, #ecs_list_tasks.next_token, fun id/2}
     ]}.


-spec list_tasks() -> ecs_return(#ecs_list_tasks{}).
list_tasks() ->
    list_tasks([], default_config()).

-spec list_tasks(list_tasks_opts()) -> ecs_return(#ecs_list_tasks{}).
list_tasks(Opts) ->
    list_tasks(Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% ECS API
%% [http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListTasks.html]
%%
%% ===Example===
%%
%% List tasks for 'TestCluster' ECS Cluster
%%
%% `
%% {ok, Result} = erlcloud_ecs:list_tasks([{cluster, "TestCluster"}, {out, json}]),
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec list_tasks(list_tasks_opts(), aws_config()) -> ecs_return(#ecs_list_tasks{}).
list_tasks(Opts, Config) ->
    {AwsOpts, EcsOpts} = opts(list_tasks_opts(), Opts),
    Return = ecs_request(Config, "ListTasks", AwsOpts),
    out(Return, fun(Json, UOpts) -> decode_record(list_tasks_record(), Json, UOpts) end, 
        EcsOpts).


%%%------------------------------------------------------------------------------
%% RegisterTaskDefinition 
%%%------------------------------------------------------------------------------
-type register_task_definition_opt() :: {network_mode, ecs_network_mode()} |
                                        {task_role_arn, string_param()} |
                                        volumes().
-type register_task_definition_opts() :: [register_task_definition_opt()].

-spec register_task_definition_opts() -> opt_table().
register_task_definition_opts() ->
    [
        {network_mode, <<"networkMode">>, fun to_binary/1},
        {task_role_arn, <<"taskRoleArn">>, fun to_binary/1},
        volumes_opt()
    ].


-spec register_task_definition(
        ContainerDefinitions :: maybe_list(container_definition_opts()),
        Family :: string_param()) -> ecs_return(#ecs_task_definition{}).
register_task_definition(ContainerDefinitions, Family) ->
    register_task_definition(ContainerDefinitions, Family, default_config()).

-spec register_task_definition(
        ContainerDefinitions :: maybe_list(container_definition_opts()),
        Family :: string_param(),
        register_task_definition_opts() | aws_config()) -> ecs_return(#ecs_task_definition{}).
register_task_definition(ContainerDefinitions, Family, #aws_config{} = Config) ->
    register_task_definition(ContainerDefinitions, Family, [], Config);
register_task_definition(ContainerDefinitions, Family, Opts) ->
    register_task_definition(ContainerDefinitions, Family, Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% ECS API
%% [http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_RegisterTaskDefinition.html]
%%
%% ===Example===
%%
%% Register 'hello_world' ECS task definition
%%
%% `
%% {ok, Result} = erlcloud_ecs:register_task_definition([[{memory, 500}, PM, {essential, true}, {name, "wordpress"}, {links, ["mysql"]}, {image, "wordpress"}, {cpu, 10}], [{memory, 500}, {essential, true}, {name, "mysql"}, E, {image, "mysql"}, {cpu, 10}]], "hello_world", [{network_mode, host}]),
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec register_task_definition(
        ContainerDefinitions :: maybe_list(container_definition_opts()),
        Family :: string_param(),
        Opts :: register_task_definition_opts(),
        Config :: aws_config()) -> ecs_return(#ecs_task_definition{}).
register_task_definition(ContainerDefinitions, Family, Opts, Config) ->
    Params = [
                {<<"containerDefinitions">>, encode_container_definitions(ContainerDefinitions)},
                {<<"family">>, to_binary(Family)}
           ],
    {AwsOpts, EcsOpts} = opts(register_task_definition_opts(), Opts),
    Return = ecs_request(
                Config,
                "RegisterTaskDefinition",
                Params ++ AwsOpts),
    out(Return, fun(Json, UOpts) -> decode_single_record(task_definition_record(), <<"taskDefinition">>, Json, UOpts) end,
        EcsOpts).


%%%------------------------------------------------------------------------------
%% RunTask
%%%------------------------------------------------------------------------------
-type run_task_opt() :: {cluster, string_param()} |
                        {count, pos_integer()} |
                        task_overrides_opt() |
                        {started_by, string_param()}.

-type run_task_opts() :: [run_task_opt()].
-spec run_task_opts() -> opt_table().
run_task_opts() ->
    [
        {cluster, <<"cluster">>, fun to_binary/1},
        {count, <<"count">>, fun id/1},
        task_overrides_opt(),
        {started_by, <<"startedBy">>, fun to_binary/1}
    ].

-spec run_task_record() -> record_desc().
run_task_record() ->
    {#ecs_run_task{},
     [{<<"tasks">>, #ecs_run_task.tasks, fun decode_tasks_list/2},
      {<<"failures">>, #ecs_run_task.failures, fun decode_failures_list/2}
     ]}.

-spec run_task(TaskDefinition :: string_param()) -> ecs_return(#ecs_run_task{}).
run_task(TaskDefinition) ->
    run_task(TaskDefinition, [], default_config()).

-spec run_task(TaskDefinition :: string_param(), run_task_opts() | aws_config()) -> ecs_return(#ecs_run_task{}).
run_task(TaskDefinition, #aws_config{} = Config) ->
    run_task(TaskDefinition, [], Config);
run_task(TaskDefinition, Opts) ->
    run_task(TaskDefinition, Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% ECS API
%% [http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_RunTask.html]
%%
%% ===Example===
%%
%% Run ECS task with 'console-sample-app-static' ECS Task Definition in the 'TestCluster'
%%
%% `
%% {ok, Result} = erlcloud_ecs:run_task("console-sample-app-static", [{cluster, "TestCluster"}, {count, 1}, {overrides, [{container_overrides, [{command, ["foo", "bar"]}, {name, "test"}]}, {task_role_arn, "RoleArn"} ] }, {out, record}]) 
%% '
%% @end
%%%------------------------------------------------------------------------------

-spec run_task(
        TaskDefinition :: string_param(),
        Opts :: run_task_opts(),
        Config :: aws_config()) -> ecs_return(#ecs_run_task{}).
run_task(TaskDefinition, Opts, Config) ->
    {AwsOpts, EcsOpts} = opts(run_task_opts(), Opts),
    Return = ecs_request(
                Config,
                "RunTask",
                [{<<"taskDefinition">>, to_binary(TaskDefinition)}] ++ AwsOpts),
    out(Return, fun(Json, UOpts) -> decode_record(run_task_record(), Json, UOpts) end, 
        EcsOpts).

%%%------------------------------------------------------------------------------
%% StartTask
%%%------------------------------------------------------------------------------
-type start_task_opt() :: {cluster, string_param()} |
                          task_overrides_opt() |
                          {started_by, string_param()}.

-type start_task_opts() :: [start_task_opt()].
-spec start_task_opts() -> opt_table().
start_task_opts() ->
    [
        {cluster, <<"cluster">>, fun to_binary/1},
        task_overrides_opt(),
        {started_by, <<"startedBy">>, fun to_binary/1}
    ].

-spec start_task_record() -> record_desc().
start_task_record() ->
    {#ecs_start_task{},
     [{<<"tasks">>, #ecs_start_task.tasks, fun decode_tasks_list/2},
      {<<"failures">>, #ecs_start_task.failures, fun decode_failures_list/2}
     ]}.

-spec start_task(
        TaskDefinition :: string_param(),
        ContainerInstances :: [string_param()]) -> ecs_return(#ecs_start_task{}).
start_task(TaskDefinition, ContainerInstances) ->
    start_task(TaskDefinition, ContainerInstances, [], default_config()).

-spec start_task(
        TaskDefinition :: string_param(),
        ContainerInstances :: [string_param()],
        start_task_opts() | aws_config()) -> ecs_return(#ecs_start_task{}).
start_task(TaskDefinition, ContainerInstances, #aws_config{} = Config) ->
    start_task(TaskDefinition, ContainerInstances, [], Config);
start_task(TaskDefinition, ContainerInstances, Opts) ->
    start_task(TaskDefinition, ContainerInstances, Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% ECS API
%% [http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_StartTask.html]
%%
%% ===Example===
%%
%% Run ECS task with 'console-sample-app-static' ECS Task Definition in the 'TestCluster'
%%
%% `
%% {ok, Result} = erlcloud_ecs:run_task("console-sample-app-static", [{cluster, "TestCluster"}, {count, 1}, {overrides, [{container_overrides, [{command, ["foo", "bar"]}, {name, "test"}]}, {task_role_arn, "RoleArn"} ] }, {out, record}]) 
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec start_task(
        TaskDefinition :: string_param(),
        ContainerInstances :: [string_param()],
        Opts :: start_task_opts(),
        Config :: aws_config()) -> ecs_return(#ecs_start_task{}).
start_task(TaskDefinition, ContainerInstances, Opts, Config) ->
    {AwsOpts, EcsOpts} = opts(start_task_opts(), Opts),
    Return = ecs_request(
                Config,
                "StartTask",
                [
                    {<<"taskDefinition">>, to_binary(TaskDefinition)},
                    {<<"containerInstances">>, to_binary(ContainerInstances)}
                ] ++ AwsOpts),
    out(Return, fun(Json, UOpts) -> decode_record(start_task_record(), Json, UOpts) end, 
        EcsOpts).

%%%------------------------------------------------------------------------------
%% StopTask
%%%------------------------------------------------------------------------------
-type stop_task_opt() :: {cluster, string_param()} |
                         {reason, string_param()}.
-type stop_task_opts() :: [stop_task_opt()].
-spec stop_task_opts() -> opt_table().
stop_task_opts() ->
    [
        {cluster, <<"cluster">>, fun to_binary/1},
        {reason, <<"reason">>, fun to_binary/1}
    ].


-spec stop_task(Task :: string_param()) -> ecs_return(#ecs_task{}).
stop_task(Task) ->
    stop_task(Task, [], default_config()).

-spec stop_task(Task :: string_param(), stop_task_opts() | aws_config()) -> ecs_return(#ecs_task{}).
stop_task(Task, #aws_config{} = Config) ->
    stop_task(Task, [], Config);
stop_task(Task, Opts) ->
    stop_task(Task, Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% ECS API
%% [http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_StoptTask.html]
%%
%% ===Example===
%%
%% Stop '4c89e317-0571-4f07-878b-a8b8c705c347' ECS task in the 'TestCluster'
%%
%% `
%% {ok, Result} = erlcloud_ecs:stop_task("4c89e317-0571-4f07-878b-a8b8c705c347", [{cluster, "TestCluster"}, {out, record}]),
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec stop_task(Task :: string_param(), stop_task_opts(), aws_config()) -> ecs_return(#ecs_task{}).
stop_task(Task, Opts, #aws_config{} = Config) ->
    {AwsOpts, EcsOpts} = opts(stop_task_opts(), Opts),
    Return = ecs_request(
                Config,
                "StopTask",
                [{<<"task">>, to_binary(Task)}] ++ AwsOpts),
    out(Return, fun(Json, UOpts) -> decode_single_record(task_record(), <<"task">>, Json, UOpts) end,
        EcsOpts).

%%%------------------------------------------------------------------------------
%% UpdateContainerAgent 
%%%------------------------------------------------------------------------------
-type update_container_agent_opts() :: [{cluster, string_param()}].
-spec update_container_agent_opts() -> opt_table().
update_container_agent_opts() ->
    [{cluster, <<"cluster">>, fun to_binary/1}].


-spec update_container_agent(ContainerInstance :: string_param()) -> ecs_return(#ecs_container_instance{}).
update_container_agent(ContainerInstance) ->
    update_container_agent(ContainerInstance, [], default_config()).

-spec update_container_agent(
            ContainerInstance :: string_param(),
            update_container_agent_opts() | aws_config()) -> ecs_return(#ecs_container_instance{}).
update_container_agent(ContainerInstance, #aws_config{} = Config) ->
    update_container_agent(ContainerInstance, [], Config);
update_container_agent(ContainerInstance, Opts) ->
    update_container_agent(ContainerInstance, Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% ECS API
%% [http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_UpdateContainerAgent.html]
%%
%% ===Example===
%%
%% Stop '709b350e-8c8e-490c-8fa3-d5fc09ac9e0b' ECS instance in the 'TestCluster'
%%
%% `
%% {ok, Result} = erlcloud_ecs:update_container_agent("709b350e-8c8e-490c-8fa3-d5fc09ac9e0b", [{cluster, "TestCluster"}, {out, record}]),
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec update_container_agent(
            ContainerInstance :: string_param(),
            Opts :: update_container_agent_opts(),
            Config :: aws_config()) -> ecs_return(#ecs_container_instance{}).
update_container_agent(ContainerInstance, Opts, Config) ->
    {AwsOpts, EcsOpts} = opts(update_container_agent_opts(), Opts),
    Return = ecs_request(
                Config,
                "UpdateContainerAgent",
                [{<<"containerInstance">>, to_binary(ContainerInstance)}] ++ AwsOpts),
    out(Return, fun(Json, UOpts) -> decode_single_record(container_instance_record(), <<"containerInstance">>, Json, UOpts) end,
        EcsOpts).

%%%------------------------------------------------------------------------------
%% UpdateService
%%%------------------------------------------------------------------------------
-type update_service_opt() :: {cluster, string_param()} |
                              deployment_configuration() |
                              {desired_count, pos_integer()} |
                              {task_definition, string_param()}.
-type update_service_opts() :: [update_service_opt()].
-spec update_service_opts() -> opt_table().
update_service_opts() ->
    [
        {cluster, <<"cluster">>, fun to_binary/1},
        deployment_configuration_opt(),
        {desired_count, <<"desiredCount">>, fun id/1},
        {task_definition, <<"taskDefinition">>, fun to_binary/1}
    ].


-spec update_service(Service :: string_param()) -> ecs_return(#ecs_service{}).
update_service(Service) ->
    update_service(Service, [], default_config()).

-spec update_service(Service :: string_param(), update_service_opts() | aws_config) -> ecs_return(#ecs_service{}).
update_service(Service, #aws_config{} = Config) ->
    update_service(Service, [], Config);
update_service(Service, Opts) ->
    update_service(Service, Opts, default_config()).

%%%------------------------------------------------------------------------------
%% @doc 
%% ECS API
%% [http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_UpdateService.html]
%%
%% ===Example===
%%
%% Update 'sample-webapp' ECS Service in the 'TestCluster'
%%
%% `
%% {ok, Result} = erlcloud_ecs:update_service("sample-webapp", [{cluster, "TestCluster"}, {desired_count, 1}, {task_definition, "console-sample-app-static:1"}, {deployment_configuration, [{minimum_healthy_percent, 20}]} ]),
%% '
%% @end
%%%------------------------------------------------------------------------------
-spec update_service(
        Service :: string_param(),
        Opts :: update_service_opts(),
        Config :: aws_config()) -> ecs_return(#ecs_service{}).
update_service(Service, Opts, #aws_config{} = Config) ->
    {AwsOpts, EcsOpts} = opts(update_service_opts(), Opts),
    Return = ecs_request(
                Config,
                "UpdateService",
                [{<<"service">>, to_binary(Service)}] ++ AwsOpts),
    out(Return, fun(Json, UOpts) -> decode_single_record(service_record(), <<"service">>, Json, UOpts) end,
        EcsOpts).

%%%------------------------------------------------------------------------------
%%% Internal Functions
%%%------------------------------------------------------------------------------
ecs_request(Config, Operation, Body) ->
    case erlcloud_aws:update_config(Config) of
        {ok, Config1} ->
            ecs_request_no_update(Config1, Operation, Body);
        {error, Reason} ->
            {error, Reason}
    end.

ecs_request_no_update(Config, Operation, Body) ->
    Payload = case Body of
               [] -> <<"{}">>;
               _ -> jsx:encode(lists:flatten(Body))
           end,
    Headers = headers(Config, Operation, Payload),
    Request = #aws_request{service = ecs,
                           uri = uri(Config),
                           method = post,
                           request_headers = Headers,
                           request_body = Payload},
    case erlcloud_aws:request_to_return(erlcloud_retry:request(Config, Request, fun ecs_result_fun/1)) of
        {ok, {_RespHeaders, <<>>}} -> {ok, []};
        {ok, {_RespHeaders, RespBody}} -> {ok, jsx:decode(RespBody)};
        {error, _} = Error-> Error
    end.

-spec ecs_result_fun(Request :: aws_request()) -> aws_request().
ecs_result_fun(#aws_request{response_type = ok} = Request) ->
    Request;
ecs_result_fun(#aws_request{response_type = error,
                                  error_type = aws,
                                  response_status = Status} = Request) when Status >= 500 ->
    Request#aws_request{should_retry = true};
ecs_result_fun(#aws_request{response_type = error, error_type = aws} = Request) ->
    Request#aws_request{should_retry = false}.

headers(Config, Operation, Body) ->
    Headers = [{"host", Config#aws_config.ecs_host},
               {"x-amz-target", lists:append(["AmazonEC2ContainerServiceV", ?API_VERSION, ".", Operation])},
               {"content-type", "application/x-amz-json-1.1"}],
    Region = erlcloud_aws:aws_region_from_host(Config#aws_config.ecs_host),
    erlcloud_aws:sign_v4_headers(Config, Headers, Body, Region, "ecs").

uri(#aws_config{ecs_scheme = Scheme, ecs_host = Host} = Config) ->
    lists:flatten([Scheme, Host, port_spec(Config)]).

port_spec(#aws_config{ecs_port=80}) ->
    "";
port_spec(#aws_config{ecs_port=Port}) ->
    [":", erlang:integer_to_list(Port)].


to_binary(undefined) -> undefined;
to_binary(true) -> true;
to_binary(false) -> false;
to_binary(L) when is_list(L), is_list(hd(L)) -> [to_binary(V) || V <- L];
to_binary(L) when is_list(L) -> list_to_binary(L);
to_binary(B) when is_binary(B) -> B;
to_binary(A) when is_atom(A) -> atom_to_binary(A, latin1).

