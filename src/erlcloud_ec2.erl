-module(erlcloud_ec2).

%% EC2 API Functions
-export([
    %% EC2 information.
    describe_availability_zones/0, describe_availability_zones/1,
    describe_availability_zones/2,
%    describe_regions/0, describe_regions/1, describe_regions/2,

    %% Amazon Machine Image (AMI) operations.
%    confirm_product_instance/2, confirm_product_instance/3,
%    create_image/1, create_image/2,
%    deregister_image/1, deregister_image/2,
%    describe_images/0, describe_images/1, describe_images/2,
%    describe_image_attribute/2, describe_image_attribute/3,
%    modify_image_attribute/3, modify_image_attribute/4,
%    register_image/1, register_image/2,
%    reset_image_attribute/2, reset_image_attribute/3,
    
    %% Instance bundling operations.
%    bundle_instance/1, bundle_instance/2,
%    cancel_bundle_task/1, cancel_bundle_task/2,
%    describe_bundle_tasks/0, describe_bundle_tasks/1, describe_bundle_tasks/2,

    %% Instance operations.
    describe_instances/0, describe_instances/1, describe_instances/2,
%    describe_instance_atribute/2, describe_instance_atribute/3,
    get_console_output/1, get_console_output/2,
    get_password_data/1, get_password_data/2,
%    modify_instance_attribute/3, modify_instance_attribute/4,
%    monitor_instances/1, monitor_instances/2,
    reboot_instances/1, reboot_instances/2,
%    reset_instance_attribute/2, reset_instance_attribute/3,
    run_instances/1, run_instances/2,
    start_instances/1, start_instances/2,
    stop_instances/1, stop_instances/2,
    terminate_instances/1, terminate_instances/2,
%    unmonitor_instances/1, unmonitor_instances/2,
    
    %% Keypair operations.
%    create_key_pair/1, create_key_pair/2,
%    delete_key_pair/1, delete_key_pair/2,
%    describe_key_pairs/0, describe_key_pairs/1, describe_key_pairs/2,
    
    %% Security group operations.
%    authorize_security_group_ingress/2, authorize_security_group_ingress/3,
%    create_security_group/2, create_security_group/3,
%    delete_security_group/1, delete_security_group/2,
%    describe_security_groups/0, describe_security_groups/1, describe_security_groups/2,
%    revoke_security_group_ingress/2, revoke_security_group_ingress/3,

    %% Elastic IP addresses.
    allocate_address/0, allocate_address/1,
    associate_address/2, associate_address/3,
    describe_addresses/0, describe_addresses/1, describe_addresses/2,
    disassociate_address/1, disassociate_address/2,
    release_address/1, release_address/2,
    
    %% Reserved instances
%    describe_reserved_instances/1, describe_reserved_instances/2,
%    describe_reserved_instances/3,
%    describe_reserved_instances_offerings/1,
%    describe_reserved_instances_offerings/2,
%    purchase_reserved_instances_offering/2,
%    purchase_reserved_instances_offering/3,
    
    %% EBS volumes
    attach_volume/3, attach_volume/4,
    create_volume/3, create_volume/4,
    describe_volumes/0, describe_volumes/1, describe_volumes/2,
    detach_volume/1, detach_volume/2,
    delete_volume/1, delete_volume/2,
    
    %% EBS snapshots.
    create_snapshot/1, create_snapshot/2, create_snapshot/3,
    delete_snapshot/1, delete_snapshot/2,
    describe_snapshots/0, describe_snapshots/1, describe_snapshots/2%,
%    describe_snapshot_attribute/2, describe_snapshot_attribute/3,
%    modify_snapshot_attribute/3, modify_snapshot_attribute/4,
%    reset_snapshot_attribute/2, reset_snapshot_attribute/3%,
    
    %% Spot instances.
%    cancel_spot_instance_requests/1, cancel_spot_instance_requests/2,
%    create_spot_datafeed_subcription/1, create_spot_datafeed_subscription/2,
%    create_spot_datafeed_subscription/3,
%    delete_spot_datafeed_subscription/0, delete_spot_datafeed_subscription/1,
%    describe_spot_datafeed_subscription/0, describe_spot_datafeed_subscription/1,
%    describe_spot_instance_requests/0, describe_spot_instance_requests/1,
%    describe_spot_price_history/0, describe_spot_price_history/1,
%    describe_spot_price_history/2,
%    request_spot_instances/1, request_spot_instances/2
    
]).

-define(API_VERSION, "2009-11-30").
-record(ec2_config, {url}).
-type(ec2_shutdown_behavior() :: stop | terminate).
-type(ec2_volume_size() :: 1..1024).
-record(ec2_block_device_mapping, {
    device_name::string(),
    virtual_name::string(),
    volume_size::ec2_volume_size(),
    delete_on_termination::boolean()
}).
-type(ec2_block_device_mapping() :: #ec2_block_device_mapping{}).
-record(ec2_instance_spec, {
    imageId::string(),
    minCount=1::pos_integer(),
    maxCount=1::pos_integer(),
    keyName::string(),
    groupSet::[string()],
    userData::binary(),
    instanceType::string(),
    availabilityZone::string(),
    kernelId::string(),
    ramdiskId::string(),
    blockDeviceMapping::[ec2_block_device_mapping()],
    monitoring=false::boolean(),
    subnetId::string(),
    disableApiTermination=false::boolean(),
    instanceInitiatedShutdownBehavior::ec2_shutdown_behavior()
}).
-type(ec2_config() :: #ec2_config{}).
-type(ec2_result() :: term()).
-type(ec2_instance_spec() :: #ec2_instance_spec{}).

default_config() ->
    #ec2_config{url="https://ec2.amazonaws.com"}.

-spec(allocate_address/0 :: () -> ec2_result()).
allocate_address() -> allocate_address(default_config()).

-spec(allocate_address/1 :: (ec2_config()) -> ec2_result()).
allocate_address(Config) ->
    erlang:error(not_implemented).

-spec(associate_address/2 :: (string(), string()) -> ec2_result()).
associate_address(PublicIP, InstanceID) ->
    associate_address(PublicIP, InstanceID, default_config()).

-spec(associate_address/3 :: (string(), string(), ec2_config()) -> ec2_result()).
associate_address(PublicIP, InstanceID, Config)
  when is_list(PublicIP), is_list(InstanceID) ->
    erlang:error(not_implemented).

-spec(attach_volume/3 :: (string(), string(), string()) -> ec2_result()).
attach_volume(VolumeID, InstanceID, Device) ->
    attach_volume(VolumeID, InstanceID, Device, default_config()).

-spec(attach_volume/4 :: (string(), string(), string(), ec2_config()) -> ec2_result()).
attach_volume(VolumeID, InstanceID, Device, Config)
  when is_list(VolumeID), is_list(InstanceID), is_list(Device) ->
    erlang:error(not_implemented).

-spec(create_snapshot/1 :: (string()) -> ec2_result()).
create_snapshot(VolumeID) ->
    create_snapshot(VolumeID, "", default_config()).

-spec(create_snapshot/2 :: (string(), string()) -> ec2_result() ; (string(), ec2_config()) -> ec2_result()).
create_snapshot(VolumeID, Config)
  when is_record(Config, ec2_config) ->
    create_snapshot(VolumeID, "", Config);
create_snapshot(VolumeID, Description) ->
    create_snapshot(VolumeID, Description, default_config()).

-spec(create_snapshot/3 :: (string(), string(), ec2_config()) -> ec2_result()).
create_snapshot(VolumeID, Description, Config)
  when is_list(VolumeID), is_list(Description) ->
    erlang:error(not_implemented).

-spec(create_volume/3 :: (ec2_volume_size(), string(), string()) -> ec2_result()).
create_volume(Size, SnapshotID, AvailabilityZone) ->
    create_volume(Size, SnapshotID, AvailabilityZone, default_config()).

-spec(create_volume/4 :: (ec2_volume_size(), string(), string(), ec2_config()) -> ec2_result()).
create_volume(Size, SnapshotID, AvailabilityZone, Config)
  when Size >= 1, Size =< 1024,
       is_list(SnapshotID) or SnapshotID =:= none,
       is_list(AvailabilityZone) ->
    erlang:error(not_implemented).

-spec(delete_snapshot/1 :: (string()) -> ec2_result()).
delete_snapshot(SnapshotID) -> delete_snapshot(SnapshotID, default_config()).

-spec(delete_snapshot/2 :: (string(), ec2_config()) -> ec2_result()).
delete_snapshot(SnapshotID, Config)
  when is_list(SnapshotID) ->
    erlang:error(not_implemented).

-spec(delete_volume/1 :: (string()) -> ec2_result()).
delete_volume(VolumeID) -> delete_volume(VolumeID, default_config()).

-spec(delete_volume/2 :: (string(), ec2_config()) -> ec2_result()).
delete_volume(VolumeID, Config)
  when is_list(VolumeID) ->
    erlang:error(not_implemented).

-spec(describe_addresses/0 :: () -> ec2_result()).
describe_addresses() -> describe_addresses([]).

-spec(describe_addresses/1 :: ([string()] | ec2_config()) -> ec2_result()).
describe_addresses(Config)
  when is_record(Config, ec2_config) ->
    describe_addresses([], Config);
describe_addresses(PublicIPs) -> describe_addresses(PublicIPs, default_config()).

-spec(describe_addresses/2 :: ([string()], ec2_config()) -> ec2_result()).
describe_addresses(PublicIPs, Config)
  when is_list(PublicIPs) ->
    erlang:error(not_implemented).

-spec(describe_availability_zones/0 :: () -> ec2_result()).
describe_availability_zones() -> describe_availability_zones([]).
-spec(describe_availability_zones/1 :: ([string()] | ec2_config()) -> ec2_result()).
describe_availability_zones(Config)
  when is_record(Config, ec2_config) ->
    describe_availability_zones([], Config);
describe_availability_zones(ZoneNames) ->
    describe_availability_zones(ZoneNames, default_config()).

-spec(describe_availability_zones/2 :: ([string()], ec2_config()) -> ec2_result()).
describe_availability_zones(ZoneNames, Config)
  when is_list(ZoneNames) ->
    erlang:error(not_implemented).

-spec(describe_instances/0 :: () -> ec2_result()).
describe_instances() -> describe_instances([]).

-spec(describe_instances/1 :: ([string()] | ec2_config()) -> ec2_result()).
describe_instances(Config)
  when is_record(Config, ec2_config) ->
    describe_instances([], Config);
describe_instances(InstanceIDs) ->
    describe_instances(InstanceIDs, default_config()).

-spec(describe_instances/2 :: ([string()], ec2_config()) -> ec2_result()).
describe_instances(InstanceIDs, Config)
  when is_list(InstanceIDs) ->
    erlang:error(not_implemented).

-spec(describe_snapshots/0 :: () -> ec2_result()).
describe_snapshots() -> describe_snapshots([]).

-spec(describe_snapshots/1 :: ([string()] | ec2_config()) -> ec2_result()).
describe_snapshots(Config)
  when is_record(Config, ec2_config) ->
    describe_snapshots([], Config);
describe_snapshots(SnapshotIDs) ->
    describe_snapshots(SnapshotIDs, default_config()).

-spec(describe_snapshots/2 :: ([string()], ec2_config()) -> ec2_result()).
describe_snapshots(SnapshotIDs, Config)
  when is_list(SnapshotIDs) ->
    erlang:error(not_implemented).

-spec(describe_volumes/0 :: () -> ec2_result()).
describe_volumes() -> describe_volumes([]).

-spec(describe_volumes/1 :: ([string()] | ec2_config()) -> ec2_result()).
describe_volumes(Config)
  when is_record(Config, ec2_config) ->
    describe_volumes([], Config);
describe_volumes(VolumeIDs) ->
    describe_volumes(VolumeIDs, default_config()).

-spec(describe_volumes/2 :: ([string()], ec2_config()) -> ec2_result()).
describe_volumes(VolumeIDs, Config)
  when is_list(VolumeIDs) ->
    erlang:error(not_implemented).

-spec(detach_volume/1 :: (string()) -> ec2_result()).
detach_volume(VolumeID) -> detach_volume(VolumeID, default_config()).

-spec(detach_volume/2 :: (string(), ec2_config()) -> ec2_result()).
detach_volume(VolumeID, Config)
  when is_list(VolumeID) ->
    erlang:error(not_implemented).

-spec(disassociate_address/1 :: (string()) -> ec2_result()).
disassociate_address(PublicIP) ->
    disassociate_address(PublicIP, default_config()).

-spec(disassociate_address/2 :: (string(), ec2_config()) -> ec2_result()).
disassociate_address(PublicIP, Config)
  when is_list(PublicIP) ->
    erlang:error(not_implemented).

-spec(get_console_output/1 :: (string()) -> ec2_result()).
get_console_output(InstanceID) -> get_console_output(InstanceID, default_config()).

-spec(get_console_output/2 :: (string(), ec2_config()) -> ec2_result()).
get_console_output(InstanceID, Config)
  when is_list(InstanceID) ->
    erlang:error(not_implemented).

-spec(get_password_data/1 :: (string()) -> ec2_result()).
get_password_data(InstanceID) -> get_password_data(InstanceID, default_config()).

-spec(get_password_data/2 :: (string(), ec2_config()) -> ec2_result()).
get_password_data(InstanceID, Config)
  when is_list(InstanceID) ->
    erlang:error(not_implemented).

-spec(reboot_instances/1 :: ([string()]) -> ec2_result()).
reboot_instances(InstanceIDs) -> reboot_instances(InstanceIDs, default_config()).

-spec(reboot_instances/2 :: ([string()], ec2_config()) -> ec2_result()).
reboot_instances(InstanceIDs, Config)
  when is_list(InstanceIDs) ->
    erlang:error(not_implemented).

-spec(release_address/1 :: (string()) -> ec2_result()).
release_address(PublicIP) -> release_address(PublicIP, default_config()).

-spec(release_address/2 :: (string(), ec2_config()) -> ec2_result()).
release_address(PublicIP, Config)
  when is_list(PublicIP) ->
    erlang:error(not_implemented).

-spec(run_instances/1 :: (ec2_instance_spec()) -> ec2_result()).
run_instances(InstanceSpec) -> run_instances(InstanceSpec, default_config()).

-spec(run_instances/2 :: (ec2_instance_spec(), ec2_config()) -> ec2_result()).
run_instances(InstanceSpec, Config)
  when is_record(InstanceSpec, ec2_instance_spec) ->
    erlang:error(not_implemented).

-spec(start_instances/1 :: ([string()]) -> ec2_result()).
start_instances(InstanceIDs) -> start_instances(InstanceIDs, default_config()).

-spec(start_instances/2 :: ([string()], ec2_config()) -> ec2_result()).
start_instances(InstanceIDs, Config)
  when is_list(InstanceIDs) ->
    erlang:error(not_implemented).

-spec(stop_instances/1 :: ([string()]) -> ec2_result()).
stop_instances(InstanceIDs) -> stop_instances(InstanceIDs, default_config()).

-spec(stop_instances/2 :: ([string()], ec2_config()) -> ec2_result()).
stop_instances(InstanceIDs, Config)
  when is_list(InstanceIDs) ->
    erlang:error(not_implemented).

-spec(terminate_instances/1 :: ([string()]) -> ec2_result()).
terminate_instances(InstanceIDs) -> terminate_instances(InstanceIDs, default_config()).

-spec(terminate_instances/2 :: ([string()], ec2_config()) -> ec2_result()).
terminate_instances(InstanceIDs, Config)
  when is_list(InstanceIDs) ->
    erlang:error(not_implemented).
