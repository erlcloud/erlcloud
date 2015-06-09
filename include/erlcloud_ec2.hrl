-type(ec2_shutdown_behavior() :: stop | terminate | undefined).
-type(ec2_volume_size() :: 1..1024).

-record(ec2_block_device_mapping, {
          device_name::string(),
          virtual_name::string(),
          snapshot_id::string(),
          volume_size::ec2_volume_size(),
          delete_on_termination::boolean()
         }).
-type(ec2_block_device_mapping() :: #ec2_block_device_mapping{}).

%% Network interface (used by launch specification)
-record(ec2_net_if, {
          device_index   :: string(),
          subnet_id      :: string(),
          security_group :: [string()],
          private_ip     :: [string()],
          associate_public_ip :: boolean()
}).

-record(ec2_instance_spec, {
          image_id::string(),
          min_count=1::pos_integer(),
          max_count=1::pos_integer(),
          key_name::string(),
          group_set=["default"]::[string()],
          user_data::binary(),
          instance_type::string(),
          availability_zone::string(),
          placement_group::string(),
          kernel_id::string(),
          ramdisk_id::string(),
          block_device_mapping=[]::[ec2_block_device_mapping()],
          monitoring_enabled=false::boolean(),
          subnet_id::string(),
          disable_api_termination=false::boolean(),
          instance_initiated_shutdown_behavior::ec2_shutdown_behavior(),
          net_if=[] :: [#ec2_net_if{}], 
          ebs_optimized = false :: boolean(),
          iam_instance_profile_name = undefined :: string()
         }).
-record(ec2_image_spec, {
          image_location::string(),
          name::string(),
          description::string(),
          architecture::string(),
          kernel_id::string(),
          ramdisk_id::string(),
          root_device_name::string(),
          block_device_mapping=[]::[ec2_block_device_mapping()]
         }).
-record(ec2_spot_instance_request, {
          spot_price::string(),
          instance_count=1::pos_integer(),
          type=one_time::one_time|persistent,
          valid_from::datetime(),
          valid_until::datetime(),
          launch_group::string(),
          availability_zone_group::string(),
          launch_specification::#ec2_instance_spec{}
         }).
-record(ec2_ingress_spec, {
          ip_protocol::tcp|udp|icmp,
          from_port::-1 | 0..65535,
          to_port::-1 | 0..65535,
          source_security_group_owner_id::string(),
          source_security_group_name::string(),
          cidr_ip::string()
         }).
-record(vpc_ingress_spec, {
          ip_protocol::tcp|udp|icmp,
          from_port::-1 | 0..65535,
          to_port::-1 | 0..65535,
          user_id::[string()],
          group_name::[string()],
          group_id::[string()],
          cidr_ip::[string()]
         }).

-record(ec2_network_acl_spec, {
          network_acl_id::string(),
          rule_number::integer(),
          protocol::integer(),
          rule_action::allow|deny,
          egress=false::boolean(),
          cidr_block::string(),
          icmp_code::integer(),
          icmp_type::integer(),
          port_range_from::integer(),
          port_range_to::integer()
         }).

-type(ec2_image_spec() :: #ec2_image_spec{}).
-type(ec2_instance_spec() :: #ec2_instance_spec{}).
-type(ec2_ingress_spec() :: #ec2_ingress_spec{}).
-type(ec2_spot_instance_request() :: #ec2_spot_instance_request{}).
-type(vpc_ingress_spec() :: #vpc_ingress_spec{}).
-type(ec2_network_acl_spec() :: #ec2_network_acl_spec{}).

-record(ec2_tag, {
          resource_id :: string(),
          resource_type :: string(),
          key :: string(),
          value :: string()
         }).


