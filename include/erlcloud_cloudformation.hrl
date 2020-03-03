-ifndef(erlcloud_cloudformation_hrl).
-define(erlcloud_cloudformation_hrl, 0).

-include("erlcloud.hrl").

-record(cloudformation_create_stack_input, {
    capabilities = [] :: [string()], %% list containing CAPABILITY_IAM | CAPABILITY_NAMED_IAM | CAPABILITY_AUTO_EXPAND
    client_request_token :: string(),
    disable_rollback :: boolean(),
    enable_termination_protection :: boolean(),
    notification_arns = [] :: [string()],
    on_failure = "ROLLBACK" :: string(), %% DO_NOTHING | ROLLBACK | DELETE
    parameters = [] :: [cloudformation_parameter()],
    resource_types = [] :: [string()],
    role_arn :: string(),
    rollback_configuration :: undefined | cloudformation_rollback_configuration(),
    stack_name :: string(),
    stack_policy_body :: string(),
    stack_policy_url :: string(),
    tags = []:: [cloudformation_tag()],
    template_body :: string(),
    template_url :: string(),
    timeout_in_minutes :: integer()
}).

-record(cloudformation_update_stack_input, {
    capabilities = [] :: [string()], %% list containing CAPABILITY_IAM | CAPABILITY_NAMED_IAM | CAPABILITY_AUTO_EXPAND
    client_request_token :: string(),
    notification_arns = [] :: [string()],
    parameters = [] :: [cloudformation_parameter()],
    resource_types = [] :: [string()],
    role_arn :: string(),
    rollback_configuration :: cloudformation_rollback_configuration(),
    stack_name :: string(),
    stack_policy_body :: string(),
    stack_policy_during_update_body :: string(),
    stack_policy_during_update_url :: string(),
    stack_policy_url :: string(),
    tags = []:: [cloudformation_tag()],
    template_body :: string(),
    template_url :: string(),
    use_previous_template :: boolean()
}).

-record(cloudformation_delete_stack_input, {
    client_request_token :: string(),
    retain_resources = [] :: [string()],
    role_arn :: string(),
    stack_name :: string()
}).

-record(cloudformation_parameter, {
    parameter_key :: string(),
    parameter_value :: string(),
    resolved_value :: string(),
    use_previous_value :: boolean()
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
