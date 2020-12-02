-ifndef(erlcloud_ecs_hrl).
-define(erlcloud_ecs_hrl, 0).

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
    name :: undefined | binary(),
    value :: undefined | binary()
}).
%% helper record
-record(ecs_single_return, {
    object :: term()
}).

-record(ecs_deployment_configuration, {
    maximum_percent :: undefined | pos_integer(),
    minimum_healthy_percent :: undefined | pos_integer()
}).

-record(ecs_deployment, {
    created_at :: undefined | number(),
    desired_count :: undefined | pos_integer(),
    id :: undefined | binary(),
    pending_count :: undefined | non_neg_integer(),
    running_count :: undefined | non_neg_integer(),
    status :: undefined | binary(),
    task_definition:: undefined | binary(),
    updated_at :: undefined | number()
}).

-record(ecs_event, {
    created_at :: undefined | number(),
    id :: undefined | binary(),
    message :: undefined | binary()
}).

-record(ecs_load_balancer, {
    container_name :: undefined | binary(),
    container_port :: undefined | pos_integer(),
    load_balancer_name :: undefined | binary(),
    target_group_arn :: undefined | binary()
}).

-record(ecs_cluster, {
    active_services_count :: undefined | non_neg_integer(),
    cluster_arn :: undefined | binary(),
    cluster_name :: undefined | binary(),
    pending_tasks_count :: undefined | non_neg_integer(),
    registered_container_instances_count :: undefined | non_neg_integer(),
    running_tasks_count :: undefined | non_neg_integer(),
    status :: undefined | binary()
}).

-record(ecs_service, {
    cluster_arn :: undefined | binary(),
    created_at :: undefined | number(),
    deployment_configuration :: undefined | #ecs_deployment_configuration{},
    deployments :: undefined | [#ecs_deployment{}],
    desired_count :: undefined | pos_integer(),
    events :: undefined | [#ecs_event{}],
    load_balancers :: undefined | [#ecs_load_balancer{}],
    pending_count :: undefined | non_neg_integer(),
    role_arn :: undefined | binary(),
    running_count :: undefined | non_neg_integer(),
    service_arn :: undefined | binary(),
    service_name :: undefined | binary(),
    status :: undefined | binary(),
    task_definition :: undefined | binary()
}).

-record(ecs_resource, {
   double_value :: undefined | non_neg_integer(),
   integer_value :: undefined | non_neg_integer(),
   long_value :: undefined | non_neg_integer(),
   name :: undefined | binary(),
   string_set_value :: undefined | [binary()],
   type :: undefined | binary()
}).

-record(ecs_version_info, {
    agent_hash :: undefined | binary(),
    agent_version :: undefined | binary(),
    docker_version :: undefined | binary()
}).

-record(ecs_container_instance, {
    agent_connected :: undefined | boolean(),
    agent_update_status :: undefined | binary(),
    attributes :: undefined | [#ecs_attribute{}],
    container_instance_arn :: undefined | binary(),
    ec2_instance_id :: undefined | binary(),
    pending_tasks_count :: undefined | non_neg_integer(),
    registered_resources :: undefined | [#ecs_resource{}],
    remaining_resources :: undefined | [#ecs_resource{}],
    running_tasks_count :: undefined | non_neg_integer(),
    status :: undefined | binary(),
    version_info :: undefined | #ecs_version_info{}
}).

-record(ecs_failure, {
    arn :: undefined | binary(),
    reason :: undefined | binary()
}).

-record(ecs_describe_clusters, {
    clusters :: undefined | [#ecs_cluster{}],
    failures :: undefined | [#ecs_failure{}]
}).

-record(ecs_describe_container_instances, {
    container_instances :: undefined | [#ecs_container_instance{}],
    failures :: undefined | [#ecs_failure{}]
}).

-record(ecs_describe_services, {
    services :: undefined | [#ecs_service{}],
    failures :: undefined | [#ecs_failure{}]
}).

-record(ecs_host, {
    host_name :: undefined | binary(),
    ip_address :: undefined | binary()
}).

-record(ecs_log_configuration, {
    log_driver :: undefined | binary(),
    options :: undefined | [{binary(), binary()}]
}).

-record(ecs_mount_point, {
    container_path :: undefined | binary(),
    read_only :: undefined | boolean(),
    source_volume :: undefined | binary()
}).

-record(ecs_port_mapping, {
    container_port :: undefined | pos_integer(),
    host_port :: undefined | pos_integer(),
    protocol :: undefined | ecs_protocol()
}).

-record(ecs_ulimit, {
    hard_limit :: undefined | pos_integer(),
    name :: ecs_ulimit_name(),
    soft_limit :: undefined | pos_integer()
}).

-record(ecs_volume_from, {
    read_only :: undefined | boolean(),
    source_container :: undefined | binary()
}).

-record(ecs_container_definition, {
    command :: undefined | [binary()],
    cpu :: undefined | pos_integer(),
    disable_networking :: undefined | boolean(),
    dns_search_domains :: undefined | [binary()],
    docker_labels :: undefined | [{binary(), binary()}],
    docker_security_options :: undefined | [binary()],
    entry_point :: undefined | [binary()],
    environment :: undefined | [#ecs_attribute{}],
    essential :: undefined | boolean(),
    extra_hosts :: undefined | [#ecs_host{}],
    host_name :: undefined | binary(),
    image :: undefined | binary(),
    links :: undefined | [binary()],
    log_configuration :: undefined | #ecs_log_configuration{},
    memory :: undefined | pos_integer(),
    memory_reservation :: undefined | pos_integer(),
    mount_points :: undefined | [#ecs_mount_point{}],
    name :: undefined | binary(),
    port_mappings :: undefined | [#ecs_port_mapping{}],
    privileged :: undefined | boolean(),
    readonly_root_filesystem :: undefined | boolean(),
    ulimits :: undefined | [#ecs_ulimit{}], 
    user :: undefined | binary(),
    volumes_from :: undefined | [#ecs_volume_from{}],
    working_directory :: undefined | binary()
}).

-record(ecs_volume_host, {
    source_path :: undefined | binary()
}).

-record(ecs_volume, {
    host :: undefined | #ecs_volume_host{},
    name :: undefined | binary()
}).

-record(ecs_task_definition, {
    container_definitions :: undefined | [#ecs_container_definition{}],
    family :: undefined | binary(),
    network_mode :: undefined | ecs_network_mode(),
    requires_attributes :: undefined | [#ecs_attribute{}],
    revision :: undefined | pos_integer(),
    status :: undefined | binary(),
    task_definition_arn :: undefined | binary(),
    task_role_arn :: undefined | binary(),
    volumes :: undefined | [#ecs_volume{}]
}).

-record(ecs_network_binding, {
    bind_ip :: undefined | binary(),
    container_port :: undefined | pos_integer(),
    host_port :: undefined | pos_integer(),
    protocol :: undefined | ecs_protocol()
}).

-record(ecs_network_interface, {
    attachment_id :: undefined | binary(),
    private_ipv4_address :: undefined | binary()
}).

-record(ecs_container, {
    container_arn :: undefined | binary(),
    cpu :: undefined | binary(),
    exit_code :: undefined | integer(),
    health_status :: undefined | binary(),
    image :: undefined | binary(),
    last_status :: undefined | binary(),
    memory_reservation :: undefined | binary(),
    name :: undefined | binary(),
    network_bindings :: undefined | [#ecs_network_binding{}],
    network_interfaces :: undefined | [#ecs_network_interface{}],
    reason :: undefined | binary(),
    runtime_id :: undefined | binary(),
    task_arn :: undefined | binary()

}).

-record(ecs_container_override, {
    command :: undefined | [binary()],
    environment :: undefined | [#ecs_attribute{}],
    name :: undefined | binary()
}).

-record(ecs_task_override, {
    container_overrides :: undefined | [#ecs_container_override{}],
    task_role_arn :: undefined | binary() 
}).

-record(ecs_attachment_detail, {
    name :: undefined | binary(),
    value :: undefined | binary()
}).

-record(ecs_attachment, {
    id :: undefined | binary(),
    type :: undefined | binary(),
    status :: undefined | binary(),
    details :: undefined | [#ecs_attachment_detail{}]
}).

-record(ecs_tag, {
    key :: undefined | binary(),
    value :: undefined | binary()
}).

-record(ecs_task, {
    attachments :: undefined | [#ecs_attachment{}],
    availability_zone :: undefined | binary(),
    cluster_arn :: undefined | binary(),
    connectivity :: undefined | binary(),
    connectivity_at :: undefined | number(),
    container_instance_arn :: undefined | binary(),
    containers :: undefined | [#ecs_container{}],
    cpu :: undefined | binary(),
    created_at :: undefined | number(),
    desired_status :: undefined | binary(),
    group :: undefined | binary(),
    health_status :: undefined | binary(),
    last_status :: undefined | binary(),
    launch_type :: undefined | binary(),
    memory :: undefined | binary(),
    overrides :: undefined | #ecs_task_override{},
    platform_version :: undefined | binary(),
    pull_started_at :: undefined | number(),
    pull_stopped_at :: undefined | number(),
    started_at :: undefined | number(),
    started_by :: undefined | binary(),
    stopped_at :: undefined | number(),
    stopped_reason :: undefined | binary(),
    tags :: undefined | [#ecs_tag{}],
    task_arn :: undefined | binary(),
    task_definition_arn :: undefined | binary(),
    version :: undefined | non_neg_integer()
}).

-record(ecs_describe_tasks, {
    tasks :: undefined | [#ecs_task{}],
    failures :: undefined | [#ecs_failure{}]
}).

-record(ecs_list_clusters, {
    cluster_arns :: undefined | [erlcloud_ecs:arn()],
    next_token :: undefined | token()
}).

-record(ecs_list_container_instances, {
    container_instance_arns:: undefined | [erlcloud_ecs:arn()],
    next_token :: undefined | token()
}).

-record(ecs_list_services, {
    service_arns:: undefined | [erlcloud_ecs:arn()],
    next_token :: undefined | token()
}).

-record(ecs_list_task_definition_families, {
    families :: undefined | [erlcloud_ecs:arn()],
    next_token :: undefined | token()
}).

-record(ecs_list_task_definitions, {
    task_definition_arns :: undefined | [erlcloud_ecs:arn()],
    next_token :: undefined | token()
}).

-record(ecs_list_tasks, {
    task_arns :: undefined | [erlcloud_ecs:arn()],
    next_token :: undefined | token()
}).

-record(ecs_run_task, {
    tasks :: undefined | [#ecs_task{}],
    failures :: undefined | [#ecs_failure{}]
}).

-record(ecs_start_task, {
    tasks :: undefined | [#ecs_task{}],
    failures :: undefined | [#ecs_failure{}]
}).

-endif.