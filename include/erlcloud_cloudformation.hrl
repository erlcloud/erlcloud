-ifndef(erlcloud_ec2_hrl).
-define(erlcloud_ec2_hrl, 0).

-include("erlcloud.hrl").

-record(cloudformation_create_stack_input, {
    capabilities :: [string()], %% list containing CAPABILITY_IAM | CAPABILITY_NAMED_IAM | CAPABILITY_AUTO_EXPAND
    client_request_token :: string(),
    disable_rollback :: boolean(),
    enable_termination_protection :: boolean(),
    notification_arns :: [string()],
    on_failure = "ROLLBACK" :: string(), %% DO_NOTHING | ROLLBACK | DELETE
    parameters :: [cloudformation_parameter()],
    resource_types :: [string()],
    role_arn :: string(),
    rollback_configuration :: cloudformation_rollback_configuration(),
    stack_name :: string(),
    stack_policy_body :: string(),
    stack_policy_url :: string(),
    tags :: [cloudformation_tag()],
    template_body :: string(),
    template_url :: string(),
    timeout_in_minutes :: integer()
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
-type(cloudformation_parameter() :: #cloudformation_parameter{}).
-type(cloudformation_rollback_configuration() :: #cloudformation_rollback_configuration{}).
-type(cloudformation_rollback_trigger() :: #cloudformation_rollback_trigger{}).
-type(cloudformation_tag() :: #cloudformation_tag{}).

-endif.
