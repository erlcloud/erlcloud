-ifndef(erlcloud_as_hrl).
-define(erlcloud_as_hrl, 0).

-include("erlcloud.hrl").

-record(aws_autoscaling_tag, {
          key :: string(),
          propogate_at_launch :: undefined | boolean(),
          resource_id :: undefined | string(),
          resource_type :: undefined | string(),
          value :: string()
}).
-type(aws_autoscaling_tag() :: #aws_autoscaling_tag{}).

-record(aws_autoscaling_instance, {
          instance_id :: string(),
          launch_config_name :: string(),
          group_name :: string(),
          availability_zone :: string(),
          health_status :: string(),
          lifecycle_state :: string()
         }).
-type(aws_autoscaling_instance() :: #aws_autoscaling_instance{}).


-record(aws_autoscaling_group, {
          group_name :: string(),
          availability_zones :: undefined | list(string()),
          load_balancer_names :: list(string()),
          tags :: list(aws_autoscaling_tag()),
          desired_capacity :: undefined | integer(),
          min_size :: undefined | integer(),
          max_size :: undefined | integer(),
          launch_configuration_name :: undefined | string(),
          vpc_zone_id :: undefined | list(string()),
          instances :: list(aws_autoscaling_instance()),
          status :: string()
         }).
-type(aws_autoscaling_group() :: #aws_autoscaling_group{}).

-record(aws_launch_config, {
          name :: string(),
          image_id :: string(),
          instance_type :: string(),
          tenancy :: string(),
          user_data :: undefined | string(),
          security_groups = [] :: list(string()),
          public_ip_address = false :: undefined | boolean(),
          monitoring = false :: undefined | boolean(),
          key_name :: undefined | string()
         }).
-type(aws_launch_config() :: #aws_launch_config{}).


-record(aws_autoscaling_activity, {
          id :: string(),
          group_name :: string(),
          cause :: string(),
          description :: string(),
          details :: string(),
          status_code :: string(),
          status_msg :: string(),
          start_time :: datetime(),
          end_time :: datetime(),
          progress :: integer()
         }).
-type(aws_autoscaling_activity() :: #aws_autoscaling_activity{}).

-record(aws_autoscaling_lifecycle_hook,{
          group_name :: string(),
          lifecycle_hook_name :: string(),
          global_timeout :: non_neg_integer(),
          heartbeat_timeout :: non_neg_integer(),
          default_result :: string(),
          lifecycle_transition :: string()
         }).
-type(aws_autoscaling_lifecycle_hook() :: #aws_autoscaling_lifecycle_hook{}).

-endif.