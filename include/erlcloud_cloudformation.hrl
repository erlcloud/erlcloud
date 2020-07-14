-ifndef(erlcloud_cloudformation_hrl).
-define(erlcloud_cloudformation_hrl, 0).

-include("erlcloud.hrl").

-record(cloudformation_create_stack_input, {
    capabilities = [] :: [string()], %% list containing CAPABILITY_IAM | CAPABILITY_NAMED_IAM | CAPABILITY_AUTO_EXPAND
    client_request_token :: undefined | string(),
    disable_rollback :: undefined | boolean(),
    enable_termination_protection :: undefined | boolean(),
    notification_arns = [] :: [string()],
    on_failure = "ROLLBACK" :: string(), %% DO_NOTHING | ROLLBACK | DELETE
    parameters = [] :: [cloudformation_parameter()],
    resource_types = [] :: [string()],
    role_arn :: undefined | string(),
    rollback_configuration :: undefined | cloudformation_rollback_configuration(),
    stack_name :: string(),
    stack_policy_body :: undefined | string(),
    stack_policy_url :: undefined | string(),
    tags = []:: [cloudformation_tag()],
    template_body :: undefined | string(),
    template_url :: undefined | string(),
    timeout_in_minutes :: undefined | integer()
}).

-record(cloudformation_update_stack_input, {
    capabilities = [] :: [string()], %% list containing CAPABILITY_IAM | CAPABILITY_NAMED_IAM | CAPABILITY_AUTO_EXPAND
    client_request_token :: undefined | string(),
    notification_arns = [] :: [string()],
    parameters = [] :: [cloudformation_parameter()],
    resource_types = [] :: [string()],
    role_arn :: undefined | string(),
    rollback_configuration :: undefined | cloudformation_rollback_configuration(),
    stack_name :: string(),
    stack_policy_body :: undefined | string(),
    stack_policy_during_update_body :: undefined | string(),
    stack_policy_during_update_url :: undefined | string(),
    stack_policy_url :: undefined | string(),
    tags = []:: [cloudformation_tag()],
    template_body :: undefined | string(),
    template_url :: undefined | string(),
    use_previous_template :: undefined | boolean()
}).

-record(cloudformation_delete_stack_input, {
    client_request_token :: undefined | string(),
    retain_resources = [] :: [string()],
    role_arn :: undefined | string(),
    stack_name :: undefined | string()
}).

-record(cloudformation_parameter, {
    parameter_key :: string(),
    parameter_value :: string(),
    resolved_value :: undefined | string(),
    use_previous_value :: undefined | boolean()
}).

-record(cloudformation_rollback_configuration, {
    monitoring_time_in_minutes :: integer(),
    rollback_triggers:: [cloudformation_rollback_trigger()]
}).

-record(cloudformation_rollback_trigger, {
    arn :: string(),
    type:: string()
}).

-record(cloudformation_tag, {
    key :: string(),
    value :: string()
}).

-type(cloudformation_create_stack_input() :: #cloudformation_create_stack_input{}).
-type(cloudformation_update_stack_input() :: #cloudformation_update_stack_input{}).
-type(cloudformation_delete_stack_input() :: #cloudformation_delete_stack_input{}).
-type(cloudformation_parameter() :: #cloudformation_parameter{}).
-type(cloudformation_rollback_configuration() :: #cloudformation_rollback_configuration{}).
-type(cloudformation_rollback_trigger() :: #cloudformation_rollback_trigger{}).
-type(cloudformation_tag() :: #cloudformation_tag{}).

-endif.
