-record(aws_autoscaling_group, {
          group_name::string(),
          availability_zones::list(string()),
          tags::list(string()),
          desired_capacity::integer(),
          min_size::integer(),
          max_size::integer()          
         }).
-type(aws_autoscaling_group()::#aws_autoscaling_group{}).
