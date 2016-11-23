-include("erlcloud.hrl").

-define(LIMIT_MAX, 100).

-type ecs_network_mode() :: bridge | host | none.
-type ecs_key_value_pair() :: {string() | binary(), string() | binary()}.
-type ecs_task_log_driver() :: 'json-file' | syslog | journald | gelf | fluentd | awslogs | splunk.
-type ecs_protocol() :: tcp | udp.
-type ecs_ulimit_name() :: core | cpu | data | fsize | locks | memlock | msgqueue | nice | nofile | nproc | rss | rtprio | rttime | sigpending | stack.
-type token() :: binary().

%%%------------------------------------------------------------------------------
%%
%% Common data types
%%
%%%------------------------------------------------------------------------------
-record(ecs_pagination_options, {
    max_results = ?LIMIT_MAX :: 1..100,
    next_token :: token()
}).
-type(ecs_pagination_options() :: #ecs_pagination_options{}).

-record(ecs_attribute, {
    name :: binary(),
    value :: binary()
}).
%% helper record
-record(ecs_single_return, {
    object :: term()
}).

-record(ecs_deployment_configuration, {
    maximum_percent :: pos_integer(),
    minimum_healthy_percent :: pos_integer()
}).

-record(ecs_deployment, {
    created_at :: pos_integer(),
    desired_count :: pos_integer(),
    id :: binary(),
    pending_count :: pos_integer(),
    running_count :: pos_integer(),
    status :: binary(),
    task_definition:: binary(),
    updated_at :: pos_integer()
}).

-record(ecs_event, {
    created_at :: pos_integer(),
    id :: binary(),
    message :: binary()
}).

-record(ecs_load_balancer, {
    container_name :: binary(),
    container_port :: pos_integer(),
    load_balancer_name :: binary(),
    target_group_arn :: binary()
}).

-record(ecs_cluster, {
    active_services_count :: pos_integer(),
    cluster_arn :: binary(),
    cluster_name :: binary(),
    pending_tasks_count :: pos_integer(),
    registered_container_instances_count :: pos_integer(),
    running_tasks_count :: pos_integer(),
    status :: binary()
}).

-record(ecs_service, {
    cluster_arn :: binary(),
    created_at :: pos_integer(),
    deployment_configuration :: #ecs_deployment_configuration{},
    deployments :: [#ecs_deployment{}],
    desired_count :: pos_integer(),
    events :: [#ecs_event{}],
    load_balancers :: [#ecs_load_balancer{}],
    pending_count :: pos_integer(),
    role_arn :: binary(),
    running_count :: pos_integer(),
    service_arn :: binary(),
    service_name :: binary(),
    status :: binary(),
    task_definition :: binary()
}).

-record(ecs_resource, {
   double_value :: pos_integer(),
   integer_value :: pos_integer(),
   long_value :: pos_integer(),
   name :: binary(),
   string_set_value :: binary(),
   type :: binary()
}).

-record(ecs_version_info, {
    agent_hash :: binary(),
    agent_version :: binary(),
    docker_version :: binary()
}).

-record(ecs_container_instance, {
    agent_connected :: boolean(),
    agent_update_status :: binary(),
    attributes :: [#ecs_attribute{}],
    container_instance_arn :: binary(),
    ec2_instance_id :: binary(),
    pending_tasks_count :: pos_integer(),
    registered_resources :: [#ecs_resource{}],
    remaining_resources :: [#ecs_resource{}],
    running_tasks_count :: pos_integer(),
    status :: binary(),
    version_info :: #ecs_version_info{}
}).

-record(ecs_failure, {
    arn :: binary(),
    reason :: binary()
}).

-record(ecs_describe_clusters, {
    clusters :: [#ecs_cluster{}],
    failures :: [#ecs_failure{}]
}).

-record(ecs_describe_container_instances, {
    container_instances :: [#ecs_container_instance{}],
    failures :: [#ecs_failure{}]
}).

-record(ecs_describe_services, {
    services :: [#ecs_service{}],
    failures :: [#ecs_failure{}]
}).

-record(ecs_host, {
    host_name :: binary(),
    ip_address :: binary()
}).

-record(ecs_log_configuration, {
    log_driver :: binary(),
    options :: [{binary(), binary()}]
}).

-record(ecs_mount_point, {
    container_path :: binary(),
    read_only :: boolean(),
    source_volume :: binary()
}).

-record(ecs_port_mapping, {
    container_port :: pos_integer(),
    host_port :: pos_integer(),
    protocol :: ecs_protocol()
}).

-record(ecs_ulimit, {
    hard_limit :: pos_integer(),
    name :: ecs_ulimit_name(),
    soft_limit :: pos_integer()
}).

-record(ecs_volume_from, {
    read_only :: boolean(),
    source_container :: binary()
}).

-record(ecs_container_definition, {
    command :: [binary()],
    cpu :: pos_integer(),
    disable_networking :: boolean(),
    dns_search_domains :: [binary()],
    docker_labels :: [{binary(), binary()}],
    docker_security_options :: [binary()],
    entry_point :: [binary()],
    environment :: [#ecs_attribute{}],
    essential :: boolean(),
    extra_hosts :: [#ecs_host{}],
    host_name :: binary(),
    image :: binary(),
    links :: [binary()],
    log_configuration :: #ecs_log_configuration{},
    memory :: pos_integer(),
    memory_reservation :: pos_integer(),
    mount_points :: [#ecs_mount_point{}],
    name :: binary(),
    port_mappings :: [#ecs_port_mapping{}],
    privileged :: boolean(),
    readonly_root_filesystem :: boolean(),
    ulimits :: [#ecs_ulimit{}], 
    user :: binary(),
    volumes_from :: [#ecs_volume_from{}],
    working_directory :: binary()
}).

-record(ecs_volume_host, {
    source_path :: binary()
}).

-record(ecs_volume, {
    host :: #ecs_volume_host{},
    name :: binary()
}).

-record(ecs_task_definition, {
    container_definitions :: [#ecs_container_definition{}],
    family :: binary(),
    network_mode :: binary(),
    requires_attributes :: [#ecs_attribute{}],
    revision :: pos_integer(),
    status :: binary(),
    task_definition_arn :: binary(),
    task_role_arn :: binary(),
    volumes :: [#ecs_volume{}]
}).

-record(ecs_network_binding, {
    bind_ip :: binary(),
    container_port :: pos_integer(),
    host_port :: pos_integer(),
    protocol :: ecs_protocol()
}).

-record(ecs_container, {
    container_arn :: binary(),
    exit_code :: pos_integer(),
    last_status :: binary(),
    name :: binary(),
    network_bindings :: [#ecs_network_binding{}],
    reason :: binary(),
    task_arn :: binary()
}).

-record(ecs_container_override, {
    command :: [binary()],
    environment :: [#ecs_attribute{}],
    name :: binary()
}).

-record(ecs_task_override, {
    container_overrides :: [#ecs_container_override{}],
    task_role_arn :: binary() 
}).

-record(ecs_task, {
    cluster_arn :: binary(),
    container_instance_arn :: binary(),
    containers :: [#ecs_container{}],
    created_at :: pos_integer(),
    desired_status:: binary(),
    last_status :: binary(),
    overrides :: #ecs_task_override{},
    started_at :: pos_integer(),
    started_by :: binary(),
    stopped_at :: pos_integer(),
    stopped_reason :: binary(),
    task_arn :: binary(),
    task_definition_arn :: binary()
}).

-record(ecs_describe_tasks, {
    tasks :: [#ecs_task{}],
    failures :: [#ecs_failure{}]
}).

-record(ecs_list_clusters, {
    cluster_arns :: [erlcloud_ecs:arn()],
    next_token :: token()
}).

-record(ecs_list_container_instances, {
    container_instance_arns:: [erlcloud_ecs:arn()],
    next_token :: token()
}).

-record(ecs_list_services, {
    service_arns:: [erlcloud_ecs:arn()],
    next_token :: token()
}).

-record(ecs_list_task_definition_families, {
    families :: [erlcloud_ecs:arn()],
    next_token :: token()
}).

-record(ecs_list_task_definitions, {
    task_definition_arns :: [erlcloud_ecs:arn()],
    next_token :: token()
}).

-record(ecs_list_tasks, {
    task_arns :: [erlcloud_ecs:arn()],
    next_token :: token()
}).

-record(ecs_run_task, {
    tasks :: [#ecs_task{}],
    failures :: [#ecs_failure{}]
}).

-record(ecs_start_task, {
    tasks :: [#ecs_task{}],
    failures :: [#ecs_failure{}]
}).
