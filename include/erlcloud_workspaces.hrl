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

-record(describe_workspaces, {
    next_token :: undefined | binary(),
    workspaces :: undefined | [#workspace{}]
}).

-record(workspaces_tag, {
    key :: undefined | binary(),
    value :: undefined | binary()
}).

-endif.