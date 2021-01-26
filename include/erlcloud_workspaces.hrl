-ifndef(erlcloud_workspaces_hrl).
-define(erlcloud_workspaces_hrl, 0).

-include("erlcloud.hrl").

-define(WORKSPACES_LIMIT, 25).


%%%------------------------------------------------------------------------------
%%
%% Common data types
%%
%%%------------------------------------------------------------------------------

-record(workspace_modification_state, {
    resource :: undefined | binary(),
    state :: undefined | binary()
}).

-record(workspace_properties, {
    computer_type_name :: undefined | binary(),
    root_volume_size_gib :: undefined | non_neg_integer(),
    running_mode :: undefined | binary(),
    running_mode_auto_stop_timeout_in_minutes :: undefined | non_neg_integer(),
    user_volume_size_gib :: undefined | non_neg_integer()
}).

-record(workspace, {
    bundle_id :: undefined | binary(),
    computer_name :: undefined | binary(),
    directory_id :: undefined | binary(),
    error_code :: undefined | binary(),
    error_message :: undefined | binary(),
    ip_address :: undefined | binary(),
    modification_states :: undefined | [#workspace_modification_state{}],
    root_volume_encryption_enabled :: undefined | boolean(),
    state :: undefined | binary(),
    subnet_id :: undefined | binary(),
    user_name :: undefined | binary(),
    user_volume_encryption_enabled :: undefined | boolean(),
    volume_encryption_key :: undefined | binary(),
    workspace_id :: undefined | binary(),
    workspace_properties :: undefined | #workspace_properties{}
}).

-record(workspaces_selfservice_permissions, {
    change_compute_type :: undefined | binary(),
    increase_volume_size :: undefined | binary(),
    rebuild_workspace :: undefined | binary(),
    restart_workspace :: undefined | binary(),
    switch_running_mode :: undefined | binary()
}).

-record(workspace_access_properties, {
    device_type_android :: undefined | binary(),
    device_type_chrome_os :: undefined | binary(),
    device_type_ios :: undefined | binary(),
    device_type_osx :: undefined | binary(),
    device_type_web :: undefined | binary(),
    device_type_windows :: undefined | binary(),
    device_type_zero_client :: undefined | binary()
}).

-record(workspace_creation_properties, {
    custom_security_group_id :: undefined | binary(),
    default_ou :: undefined | binary(),
    enable_internet_access :: undefined | boolean(),
    enable_maintenance_mode :: undefined | boolean(),
    enable_work_docs :: undefined | boolean(),
    user_enabled_as_local_administrator :: undefined | boolean()
}).

-record(workspace_directory, {
    alias :: undefined | binary(),
    customer_user_name :: undefined | binary(),
    directory_id :: undefined | binary(),
    directory_name :: undefined | binary(),
    directory_type :: undefined | binary(),
    dns_ip_address :: undefined | [binary()],
    iam_role_id :: undefined | binary(),
    ip_group_ids :: undefined | [binary()],
    registration_code :: undefined | binary(),
    selfservice_permissions :: undefined | #workspaces_selfservice_permissions{},
    state :: undefined | binary(),
    subnet_ids :: undefined | [binary()],
    tenancy :: undefined | binary(),
    workspace_access_properties :: undefined | #workspace_access_properties{},
    workspace_creation_properties :: undefined | #workspace_creation_properties{},
    workspace_security_group_id :: undefined | binary()
}).

-record(describe_workspaces, {
    next_token :: undefined | binary(),
    workspaces :: undefined | [#workspace{}]
}).

-record(describe_workspace_directories, {
    next_token :: undefined | binary(),
    workspace_directories :: undefined | [#workspace_directory{}]
}).

-record(workspaces_tag, {
    key :: undefined | binary(),
    value :: undefined | binary()
}).

-endif.