-ifndef(erlcloud_ec2_hrl).
-define(erlcloud_ec2_hrl, 0).

-include("erlcloud.hrl").

-type(ec2_shutdown_behavior() :: stop | terminate | undefined).
-type(ec2_volume_size() :: 1..1024).

-record(ec2_block_device_mapping, {
          device_name::string(),
          virtual_name::string(),
          snapshot_id::string(),
          volume_size::ec2_volume_size(),
          delete_on_termination::boolean(),
          encrypted::boolean()
         }).
-type(ec2_block_device_mapping() :: #ec2_block_device_mapping{}).

%% Network interface (used by launch specification)
-record(ec2_net_if, {
          device_index        :: string(),
          subnet_id           :: string(),
          security_group=[]   :: [string()],
          private_ip=[]       :: [string()],
          associate_public_ip :: undefined | boolean()
}).

-record(ec2_instance_spec, {
          image_id::string(),
          min_count=1::pos_integer(),
          max_count=1::pos_integer(),
          key_name::string(),
          group_set=["default"]::[string()],
          user_data::undefined|binary(),
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
          iam_instance_profile_name = undefined :: string(),
          spot_price::string(),
          weighted_capacity::number()
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
-record(spot_fleet_request_config_spec, {
          allocation_strategy::undefined|lowest_price|diversified,
          client_token::string(),
          excess_capacity_termination_policy::no_termination|default,
          iam_fleet_role::string(),
          launch_specification=[]::[#ec2_instance_spec{}],
          spot_price::string(),
          target_capacity::pos_integer(),
          terminate_instances_with_expiration::true|false,
          valid_from::datetime(),
          valid_until::datetime()
         }).
-record(ec2_spot_fleet_request, {
          spot_fleet_request_config::#spot_fleet_request_config_spec{}
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
          user_id::undefined|[string()],
          group_name::undefined|[string()],
          group_id::undefined|[string()],
          cidr_ip::undefined|[string()]
         }).
-record(vpc_egress_spec, {
          ip_protocol::tcp|udp|icmp,
          from_port::-1 | 0..65535,
          to_port::-1 | 0..65535,
          user_id::[string()],
          group_name::[string()],
          group_id::[string()],
          cidr_ip::[string()]
         }).
-record(vpc_xgress_internal_spec, {
          ip_protocol::tcp|udp|icmp,
          from_port::-1 | 0..65535,
          to_port::-1 | 0..65535,
          user_id::undefined|[string()],
          group_name::undefined|[string()],
          group_id::undefined|[string()],
          cidr_ip::undefined|[string()]
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
-type(ec2_spot_fleet_request() :: #ec2_spot_fleet_request{}).
-type(vpc_ingress_spec() :: #vpc_ingress_spec{}).
-type(vpc_egress_spec() :: #vpc_egress_spec{}).
-type(ec2_network_acl_spec() :: #ec2_network_acl_spec{}).

-record(ec2_tag, {
          resource_id :: string(),
          resource_type :: string(),
          key :: string(),
          value :: string()
         }).

-endif.