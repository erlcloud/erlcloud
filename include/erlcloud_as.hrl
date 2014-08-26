%% need this include for datetime() type:
-include("erlcloud.hrl").

-record(aws_autoscaling_group, {
          group_name :: string(),
          availability_zones :: list(string()),
          load_balancer_names :: list(string()),
		  instances :: list(aws_autoscaling_instance()),
          tags :: list(string()),
          desired_capacity :: integer(),
          min_size :: integer(),
          max_size :: integer()
         }).
-type(aws_autoscaling_group() :: #aws_autoscaling_group{}).

-record(aws_launch_config, {
          name :: string(),
          image_id :: string(),
          instance_type :: string(),
          tenancy :: string()
         }).
-type(aws_launch_config() :: #aws_launch_config{}).

%% maps to http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_Instance.html
-record(aws_autoscaling_instance, {
          instance_id :: string(),
          launch_config_name :: string(),
          availability_zone :: string(),
          health_status :: string(),
          lifecycle_state :: string()
         }).
-type(aws_autoscaling_instance() :: #aws_autoscaling_instance{}).

%% maps to http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_AutoScalingInstanceDetails.html
-record(aws_autoscaling_instance_details, {
          instance_id :: string(),
          launch_config_name :: string(),
          group_name :: string(),
          availability_zone :: string(),
          health_status :: string(),
          lifecycle_state :: string()
         }).
-type(aws_autoscaling_instance_details() :: #aws_autoscaling_instance_details{}).

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
