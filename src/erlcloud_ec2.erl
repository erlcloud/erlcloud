-module(erlcloud_ec2).

-include_lib("xmerl/include/xmerl.hrl").

%% Library initialization.
-export([configure/2, configure/3, new/2, new/3]).

%% EC2 API Functions
-export([
    %% Amazon DevPay
    confirm_product_instance/2, confirm_product_instance/3,

    %% AMIs
    create_image/2, create_image/3, create_image/4, create_image/5,
    deregister_image/1, deregister_image/2,
    describe_image_attribute/2, describe_image_attribute/3,
    describe_images/0, describe_images/1, describe_images/2,
    describe_images/3, describe_images/4,
    modify_image_attribute/3, modify_image_attribute/4,

    %% Availability Zones and Regions
    describe_availability_zones/0, describe_availability_zones/1,
    describe_availability_zones/2,
    describe_regions/0, describe_regions/1, describe_regions/2,

    %% Elastic Block Store
    attach_volume/3, attach_volume/4,
    create_snapshot/1, create_snapshot/2, create_snapshot/3,
    create_volume/3, create_volume/4,
    delete_snapshot/1, delete_snapshot/2,
    delete_volume/1, delete_volume/2,
    describe_snapshot_attribute/2, describe_snapshot_attribute/3,
    describe_snapshots/0, describe_snapshots/1, describe_snapshots/2,
    describe_snapshots/3, describe_snapshots/4,
    describe_volumes/0, describe_volumes/1, describe_volumes/2,
    detach_volume/1, detach_volume/2,
    modify_snapshot_attribute/3, modify_snapshot_attribute/4,
    reset_snapshot_attribute/2, reset_snapshot_attribute/3,

    %% Elastic IP addresses.
    allocate_address/0, allocate_address/1,
    associate_address/2, associate_address/3,
    describe_addresses/0, describe_addresses/1, describe_addresses/2,
    disassociate_address/1, disassociate_address/2,
    release_address/1, release_address/2,

    %% General
    get_console_output/1, get_console_output/2,

    %% Images
    register_image/1, register_image/2,
    reset_image_attribute/2, reset_image_attribute/3,

    %% Instances
    describe_instance_attribute/2, describe_instance_attribute/3,
    describe_instances/0, describe_instances/1, describe_instances/2,
    modify_instance_attribute/3, modify_instance_attribute/4,
    reboot_instances/1, reboot_instances/2,
    reset_instance_attribute/2, reset_instance_attribute/3,
    run_instances/1, run_instances/2,
    start_instances/1, start_instances/2,
    stop_instances/1, stop_instances/2, stop_instances/3,
    terminate_instances/1, terminate_instances/2,

    %% Key Pairs
    create_key_pair/1, create_key_pair/2,
    delete_key_pair/1, delete_key_pair/2,
    describe_key_pairs/0, describe_key_pairs/1, describe_key_pairs/2,

    %% Monitoring
    monitor_instances/1, monitor_instances/2,
    unmonitor_instances/1, unmonitor_instances/2,

    %% Reserved Instances
    describe_reserved_instances/0, describe_reserved_instances/1,
    describe_reserved_instances/2,
    describe_reserved_instances_offerings/0,
    describe_reserved_instances_offerings/1,
    describe_reserved_instances_offerings/2,
    purchase_reserved_instances_offering/1,
    purchase_reserved_instances_offering/2,

    %% Security Groups
    authorize_security_group_ingress/2, authorize_security_group_ingress/3,
    create_security_group/2, create_security_group/3,
    delete_security_group/1, delete_security_group/2,
    describe_security_groups/0, describe_security_groups/1, describe_security_groups/2,
    revoke_security_group_ingress/2, revoke_security_group_ingress/3,

    %% Spot Instances
    cancel_spot_instance_requests/1, cancel_spot_instance_requests/2,
    create_spot_datafeed_subscription/1, create_spot_datafeed_subscription/2,
    create_spot_datafeed_subscription/3,
    delete_spot_datafeed_subscription/0, delete_spot_datafeed_subscription/1,
    describe_spot_datafeed_subscription/0, describe_spot_datafeed_subscription/1,
    describe_spot_instance_requests/0, describe_spot_instance_requests/1,
    describe_spot_price_history/0, describe_spot_price_history/1,
    describe_spot_price_history/2, describe_spot_price_history/3,
    describe_spot_price_history/5,
    request_spot_instances/1, request_spot_instances/2,

    %% Windows
    bundle_instance/6, bundle_instance/7,
    cancel_bundle_task/1, cancel_bundle_task/2,
    describe_bundle_tasks/0, describe_bundle_tasks/1, describe_bundle_tasks/2,
    get_password_data/1, get_password_data/2,

    %% Tagging. Uses different version of AWS API
    create_tags/3
]).

-import(erlcloud_xml, [get_text/1, get_text/2, get_text/3, get_bool/2, get_list/2, get_integer/2]).

-define(API_VERSION, "2009-11-30").
-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include_lib("erlcloud/include/erlcloud_ec2.hrl").

-spec(new/2 :: (string(), string()) -> aws_config()).
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey}.

-spec(new/3 :: (string(), string(), string()) -> aws_config()).
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                ec2_host=Host}.

-spec(configure/2 :: (string(), string()) -> ok).
configure(AccessKeyID, SecretAccessKey) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey)),
    ok.

-spec(configure/3 :: (string(), string(), string()) -> ok).
configure(AccessKeyID, SecretAccessKey, Host) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host)),
    ok.

-spec(allocate_address/0 :: () -> string()).
allocate_address() -> allocate_address(default_config()).

-spec(allocate_address/1 :: (aws_config()) -> string()).
allocate_address(Config) ->
    Doc = ec2_query(Config, "AllocateAddress", []),
    get_text("/AllocateAddressResponse/publicIp", Doc).

-spec(associate_address/2 :: (string(), string()) -> ok).
associate_address(PublicIP, InstanceID) ->
    associate_address(PublicIP, InstanceID, default_config()).

-spec(associate_address/3 :: (string(), string(), aws_config()) -> ok).
associate_address(PublicIP, InstanceID, Config)
  when is_list(PublicIP), is_list(InstanceID) ->
    ec2_simple_query(Config, "AssociateAddress", [{"InstanceId", InstanceID}, {"PublicIp", PublicIP}]).

-spec(attach_volume/3 :: (string(), string(), string()) -> proplist()).
attach_volume(VolumeID, InstanceID, Device) ->
    attach_volume(VolumeID, InstanceID, Device, default_config()).

-spec(attach_volume/4 :: (string(), string(), string(), aws_config()) -> proplist()).
attach_volume(VolumeID, InstanceID, Device, Config)
  when is_list(VolumeID), is_list(InstanceID), is_list(Device) ->
    Doc = ec2_query(Config, "AttachVolume",
        [
            {"InstanceId", InstanceID},
            {"Device", Device},
            {"VolumeId", VolumeID}
        ]),
    extract_volume_status(hd(xmerl_xpath:string("/AttachVolumeResponse", Doc))).

extract_volume_status(Node) ->
    erlcloud_xml:decode(
        [
            {volume_id, "volumeId", text},
            {instance_id, "instanceId", text},
            {device, "device", text},
            {status, "status", text},
            {attach_time, "attachTime", time}
        ],
        Node
    ).

-spec(authorize_security_group_ingress/2 :: (string(), ec2_ingress_spec()) -> ok).
authorize_security_group_ingress(GroupName, IngressSpec) ->
    authorize_security_group_ingress(GroupName, IngressSpec, default_config()).

-spec(authorize_security_group_ingress/3 :: (string(), ec2_ingress_spec(), aws_config()) -> ok).
authorize_security_group_ingress(GroupName, IngressSpec, Config)
  when is_list(GroupName), is_record(IngressSpec, ec2_ingress_spec) ->
    Params = [{"GroupName", GroupName}|ingress_spec_params(IngressSpec)],
    ec2_simple_query(Config, "AuthorizeSecurityGroupIngress", Params).

-spec(bundle_instance/6 :: (string(), string(), string(), string(), string(), string()) -> proplist()).
bundle_instance(InstanceID, Bucket, Prefix, AccessKeyID, UploadPolicy,
                UploadPolicySignature) ->
    bundle_instance(InstanceID, Bucket, Prefix, AccessKeyID, UploadPolicy,
                UploadPolicySignature, default_config()).

-spec(bundle_instance/7 :: (string(), string(), string(), string(), string(), string(), aws_config()) -> proplist()).
bundle_instance(InstanceID, Bucket, Prefix, AccessKeyID, UploadPolicy,
                UploadPolicySignature, Config) ->
    Doc = ec2_query(Config, "BundleInstance",
        [{"InstanceId", InstanceID}, {"Storage.S3.Bucket", Bucket},
         {"Storage.S3.Prefix", Prefix}, {"Storage.S3.AWSAccessKeyId", AccessKeyID},
         {"Storage.S3.UploadPolicy", UploadPolicy},
         {"Storage.S3.UploadPolicySignature", UploadPolicySignature}]),
    extract_bundle_task(xmerl_xpath:string("/BundleInstanceResponse/bundleInstanceTask", Doc)).

extract_bundle_task([Node]) ->
    [
        {instance_id, get_text("instanceId", Node)},
        {bundle_id, get_text("bundleId", Node)},
        {state, get_text("state", Node)},
        {start_time, erlcloud_xml:get_time("startTime", Node)},
        {update_time, erlcloud_xml:get_time("updateTime", Node)},
        {progress, get_text("progress", Node)},
        {bucket, get_text("storage/S3/bucket", Node)},
        {prefix, get_text("storage/S3/prefix", Node)}
    ].

ingress_spec_params(Spec) ->
    [
        {"IpProtocol", Spec#ec2_ingress_spec.ip_protocol},
        {"FromPort", Spec#ec2_ingress_spec.from_port},
        {"ToPort", Spec#ec2_ingress_spec.to_port},
        {"SourceSecurityGroupOwnerId", Spec#ec2_ingress_spec.source_security_group_owner_id},
        {"SourceSecurityGroupName", Spec#ec2_ingress_spec.source_security_group_name},
        {"CidrIp", Spec#ec2_ingress_spec.cidr_ip}
    ].

-spec(cancel_bundle_task/1 :: (string()) -> proplist()).
cancel_bundle_task(BundleID) ->
    cancel_bundle_task(BundleID, default_config()).

-spec(cancel_bundle_task/2 :: (string(), aws_config()) -> proplist()).
cancel_bundle_task(BundleID, Config)
  when is_list(BundleID) ->
    Doc = ec2_query(Config, "CancelBundleTask", [{"BundleId", BundleID}]),
    extract_bundle_task(xmerl_xpath:string("/CancelBundleTaskResponse/bundleInstanceTask", Doc)).

-spec(cancel_spot_instance_requests/1 :: ([string()]) -> [proplist()]).
cancel_spot_instance_requests(SpotInstanceRequestIDs) ->
    cancel_spot_instance_requests(SpotInstanceRequestIDs, default_config()).

-spec(cancel_spot_instance_requests/2 :: ([string()], aws_config()) -> [proplist()]).
cancel_spot_instance_requests(SpotInstanceRequestIDs, Config)
  when is_list(SpotInstanceRequestIDs) ->
    Doc = ec2_query(Config, "CancelSpotInstanceRequests",
                    erlcloud_aws:param_list(SpotInstanceRequestIDs, "SpotInstanceRequestId")),
    [extract_spot_instance_state(Item) ||
     Item <- xmerl_xpath:string("/CancelSpotInstanceRequestsResponse/spotInstanceRequestSet/item", Doc)].

extract_spot_instance_state(Node) ->
    [
        {spot_instance_request_id, get_text("spotInstanceRequestId", Node)},
        {state, get_text("state", Node)}
    ].

-spec(confirm_product_instance/2 :: (string(), string()) -> proplist()).
confirm_product_instance(ProductCode, InstanceID) ->
    confirm_product_instance(ProductCode, InstanceID, default_config()).

-spec(confirm_product_instance/3 :: (string(), string(), aws_config()) -> proplist()).
confirm_product_instance(ProductCode, InstanceID, Config)
  when is_list(ProductCode), is_list(InstanceID) ->
    Params = [{"ProductCode", ProductCode}, {"InstanceId", InstanceID}],
    Doc = ec2_query(Config, "ConfirmProductInstance", Params),
    [
        {return, get_bool("/ConfirmProductInstanceResponse/return", Doc)},
        {owner_id, get_text("/ConfirmProductInstanceResponse/ownerId", Doc)}
    ].

-spec(create_key_pair/1 :: (string()) -> proplist()).
create_key_pair(KeyName) -> create_key_pair(KeyName, default_config()).

-spec(create_key_pair/2 :: (string(), aws_config()) -> proplist()).
create_key_pair(KeyName, Config)
  when is_list(KeyName) ->
    Doc = ec2_query(Config, "CreateKeyPair", [{"KeyName", KeyName}]),
    [
        {key_name, get_text("/CreateKeyPairResponse/keyName", Doc)},
        {key_fingerprint, get_text("/CreateKeyPairResponse/keyFingerprint", Doc)},
        {key_material, get_text("/CreateKeyPairResponse/keyMaterial", Doc)}
    ].

-spec(create_image/2 :: (string(), string()) -> proplist()).
create_image(InstanceID, Name) -> create_image(InstanceID, Name, default_config()).

-spec(create_image/3 :: (string(), string(), string() | aws_config()) -> proplist()).
create_image(InstanceID, Name, Config)
  when is_record(Config, aws_config) ->
    create_image(InstanceID, Name, none, Config);
create_image(InstanceID, Name, Description) ->
    create_image(InstanceID, Name, Description, default_config()).

-spec(create_image/4 :: (string(), string(), string() | none, boolean() | aws_config()) -> proplist()).
create_image(InstanceID, Name, Description, Config)
  when is_record(Config, aws_config) ->
    create_image(InstanceID, Name, Description, false, Config);
create_image(InstanceID, Name, Description, NoReboot) ->
    create_image(InstanceID, Name, Description, NoReboot, default_config()).

-spec(create_image/5 :: (string(), string(), string() | none, boolean(), aws_config()) -> proplist()).
create_image(InstanceID, Name, Description, NoReboot, Config)
  when is_list(InstanceID), is_list(Name),
       is_list(Description) orelse Description =:= none,
       is_boolean(NoReboot) ->
    Params = [{"InstanceId", InstanceID}, {"Name", Name},
              {"Description", Description}, {"NoReboot", NoReboot}],
    Doc = ec2_query(Config, "CreateImage", Params),
    [{image_id, get_text("/CreateImageResponse/imageId", Doc)}].

-spec(create_security_group/2 :: (string(), string()) -> ok).
create_security_group(GroupName, GroupDescription) ->
    create_security_group(GroupName, GroupDescription, default_config()).

-spec(create_security_group/3 :: (string(), string(), aws_config()) -> ok).
create_security_group(GroupName, GroupDescription, Config)
  when is_list(GroupName), is_list(GroupDescription) ->
    ec2_simple_query(Config, "CreateSecurityGroup",
        [{"GroupName", GroupName}, {"GroupDescription", GroupDescription}]).

-spec(create_snapshot/1 :: (string()) -> proplist()).
create_snapshot(VolumeID) ->
    create_snapshot(VolumeID, "", default_config()).

-spec(create_snapshot/2 :: (string(), string()) -> proplist() ; (string(), aws_config()) -> proplist()).
create_snapshot(VolumeID, Config)
  when is_record(Config, aws_config) ->
    create_snapshot(VolumeID, "", Config);
create_snapshot(VolumeID, Description) ->
    create_snapshot(VolumeID, Description, default_config()).

-spec(create_snapshot/3 :: (string(), string(), aws_config()) -> proplist()).
create_snapshot(VolumeID, Description, Config)
  when is_list(VolumeID), is_list(Description) ->
    Doc = ec2_query(Config, "CreateSnapshot",
        [
            {"VolumeId", VolumeID},
            {"Description", Description}
        ]),
    [
        {snapshot_id, get_text("/CreateSnapshotResponse/snapshotId", Doc)},
        {volume_id, get_text("/CreateSnapshotResponse/volumeId", Doc)},
        {volume_size, get_integer("/CreateSnapshotResponse/volumeSize", Doc)},
        {status, get_text("/CreateSnapshotResponse/status", Doc)},
        {start_time, erlcloud_xml:get_time("/CreateSnapshotResponse/attachTime", Doc)},
        {progress, get_text("/CreateSnapshotResponse/progress", Doc)},
        {owner_id, get_text("/CreateSnapshotResponse/ownerId", Doc)},
        {description, get_text("/CreateSnapshotResponse/description", Doc)}
    ].

-spec(create_spot_datafeed_subscription/1 :: (string()) -> proplist()).
create_spot_datafeed_subscription(Bucket) ->
    create_spot_datafeed_subscription(Bucket, none).

-spec(create_spot_datafeed_subscription/2 :: (string(), string() | none | aws_config()) -> proplist()).
create_spot_datafeed_subscription(Bucket, Config)
  when is_record(Config, aws_config) ->
    create_spot_datafeed_subscription(Bucket, none, Config);
create_spot_datafeed_subscription(Bucket, Prefix) ->
    create_spot_datafeed_subscription(Bucket, Prefix, default_config()).

-spec(create_spot_datafeed_subscription/3 :: (string(), string() | none, aws_config()) -> proplist()).
create_spot_datafeed_subscription(Bucket, Prefix, Config)
  when is_list(Bucket),
       is_list(Prefix) orelse Prefix =:= none ->
    Doc = ec2_query(Config, "CreateSpotDatafeedSubscription",
        [{"Bucket", Bucket}, {Prefix, Prefix}]),
    extract_spot_datafeed_subscription(xmerl_xpath:string("/CreateSpotDatafeedSubscriptionResponse/spotDatafeedSubscription", Doc)).

extract_spot_datafeed_subscription([Node]) ->
    [
        {owner_id, get_text("ownerId", Node)},
        {bucket, get_text("bucket", Node)},
        {prefix, get_text("prefix", Node)},
        {state, get_text("state", Node)},
        {fault, [{code, get_text("fault/code", Node)},
                 {message, get_text("fault/message", Node)}
                ]}
    ].

-spec(create_volume/3 :: (ec2_volume_size(), string(), string()) -> proplist()).
create_volume(Size, SnapshotID, AvailabilityZone) ->
    create_volume(Size, SnapshotID, AvailabilityZone, default_config()).

-spec(create_volume/4 :: (ec2_volume_size(), string(), string(), aws_config()) -> proplist()).
create_volume(Size, SnapshotID, AvailabilityZone, Config)
  when Size >= 1, Size =< 1024,
       is_list(SnapshotID) orelse SnapshotID =:= none,
       is_list(AvailabilityZone) ->
    Params = [
        {"Size", integer_to_list(Size)},
        {"AvailabilityZone", AvailabilityZone},
        {"SnapshotId", SnapshotID}
    ],
    Doc = ec2_query(Config, "CreateVolume", Params),
    [{volume_id, get_text("volumeId", Doc)},
     {size, get_integer("size", Doc)},
     {snapshot_id, get_text("snapshotId", Doc, none)},
     {availability_zone, get_text("availabilityZone", Doc, none)},
     {status, get_text("status", Doc, none)},
     {create_time, erlcloud_xml:get_time("createTime", Doc)}
    ].

-spec(delete_key_pair/1 :: (string()) -> ok).
delete_key_pair(KeyName) -> delete_key_pair(KeyName, default_config()).

-spec(delete_key_pair/2 :: (string(), aws_config()) -> ok).
delete_key_pair(KeyName, Config)
  when is_list(KeyName) ->
    ec2_simple_query(Config, "DeleteKeyPair", [{"KeyName", KeyName}]).

-spec(delete_security_group/1 :: (string()) -> ok).
delete_security_group(GroupName) -> delete_security_group(GroupName, default_config()).

-spec(delete_security_group/2 :: (string(), aws_config()) -> ok).
delete_security_group(GroupName, Config)
  when is_list(GroupName) ->
    ec2_simple_query(Config, "DeleteSecurityGroup", [{"GroupName", GroupName}]).

-spec(delete_snapshot/1 :: (string()) -> ok).
delete_snapshot(SnapshotID) -> delete_snapshot(SnapshotID, default_config()).

-spec(delete_snapshot/2 :: (string(), aws_config()) -> ok).
delete_snapshot(SnapshotID, Config)
  when is_list(SnapshotID) ->
    ec2_simple_query(Config, "DeleteSnapshot", [{"SnapshotId", SnapshotID}]).

-spec(delete_spot_datafeed_subscription/0 :: () -> ok).
delete_spot_datafeed_subscription() -> delete_spot_datafeed_subscription(default_config()).

-spec(delete_spot_datafeed_subscription/1 :: (aws_config()) -> ok).
delete_spot_datafeed_subscription(Config) ->
    ec2_simple_query(Config, "DeleteSpotDatafeedSubscription", []).

-spec(delete_volume/1 :: (string()) -> ok).
delete_volume(VolumeID) -> delete_volume(VolumeID, default_config()).

-spec(delete_volume/2 :: (string(), aws_config()) -> ok).
delete_volume(VolumeID, Config)
  when is_list(VolumeID) ->
    ec2_simple_query(Config, "DeleteVolume", [{"VolumeId", VolumeID}]).

-spec(deregister_image/1 :: (string()) -> ok).
deregister_image(ImageID) -> deregister_image(ImageID, default_config()).

-spec(deregister_image/2 :: (string(), aws_config()) -> ok).
deregister_image(ImageID, Config)
  when is_list(ImageID) ->
    ec2_simple_query(Config, "DeregisterImage", [{"ImageId", ImageID}]).

-spec(describe_addresses/0 :: () -> proplist()).
describe_addresses() -> describe_addresses([]).

-spec(describe_addresses/1 :: ([string()] | aws_config()) -> proplist()).
describe_addresses(Config)
  when is_record(Config, aws_config) ->
    describe_addresses([], Config);
describe_addresses(PublicIPs) -> describe_addresses(PublicIPs, default_config()).

-spec(describe_addresses/2 :: ([string()], aws_config()) -> proplist()).
describe_addresses(PublicIPs, Config)
  when is_list(PublicIPs) ->
    Doc = ec2_query(Config, "DescribeAddresses", erlcloud_aws:param_list(PublicIPs, "PublicIp")),
    Items = xmerl_xpath:string("/DescribeAddressesResponse/addressesSet/item", Doc),
    [[{public_ip, get_text("publicIp", Item)}, {instance_id, get_text("instanceId", Item, none)}] || Item <- Items].

-spec(describe_availability_zones/0 :: () -> proplist()).
describe_availability_zones() -> describe_availability_zones([]).
-spec(describe_availability_zones/1 :: ([string()] | aws_config()) -> proplist()).
describe_availability_zones(Config)
  when is_record(Config, aws_config) ->
    describe_availability_zones([], Config);
describe_availability_zones(ZoneNames) ->
    describe_availability_zones(ZoneNames, default_config()).

-spec(describe_availability_zones/2 :: ([string()], aws_config()) -> proplist()).
describe_availability_zones(ZoneNames, Config)
  when is_list(ZoneNames) ->
    Doc = ec2_query(Config, "DescribeAvailabilityZones", erlcloud_aws:param_list(ZoneNames, "ZoneName")),
    Items = xmerl_xpath:string("/DescribeAvailabilityZonesResponse/availabilityZoneInfo/item", Doc),
    [[{zone_name, get_text("zoneName", Item)},
      {zone_state, get_text("zoneState", Item)},
      {region_name, get_text("regionName", Item)},
      {messages, get_list("messageSet/item/message", Item)}
     ] || Item <- Items].

-spec(describe_bundle_tasks/0 :: () -> [proplist()]).
describe_bundle_tasks() ->
    describe_bundle_tasks([]).

-spec(describe_bundle_tasks/1 :: ([string()] | aws_config()) -> [proplist()]).
describe_bundle_tasks(Config)
  when is_record(Config, aws_config) ->
    describe_bundle_tasks([], Config);
describe_bundle_tasks(BundleIDs) ->
    describe_bundle_tasks(BundleIDs, default_config()).

-spec(describe_bundle_tasks/2 :: ([string()], aws_config()) -> [proplist()]).
describe_bundle_tasks(BundleIDs, Config) ->
    Doc = ec2_query(Config, "DescribeBundleTasks", erlcloud_aws:param_list(BundleIDs, "BundleId")),
    [extract_bundle_task(Item) || Item <- xmerl_xpath:string("/DescribeBundleTasksResponse/bundleInstanceTasksSet/item", Doc)].

-spec(describe_image_attribute/2 :: (string(), atom()) -> proplist()).
describe_image_attribute(ImageID, Attribute) ->
    describe_image_attribute(ImageID, Attribute, default_config()).

-spec(describe_image_attribute/3 :: (string(), atom(), aws_config()) -> term()).
describe_image_attribute(ImageID, Attribute, Config)
  when is_list(ImageID), is_atom(Attribute) ->
    AttributeName = case Attribute of
        launch_permission -> "launchPermission";
        product_codes -> "productCodes";
        block_device_mapping -> "blockDeviceMapping"
    end,
    Doc = ec2_query(Config, "DescribeImageAttribute", [{"ImageId", ImageID}, {"Attribute", AttributeName}]),
    case Attribute of
        launch_permission ->
            extract_permissions(xmerl_xpath:string("/DescribeImageAttributeResponse/launchPermission/item", Doc));
        product_codes ->
            get_list("/DescribeImageAttributeResponse/productCodes/item/productCode", Doc);
        block_device_mapping ->
            [extract_block_device_mapping(Node) || Node <- xmerl_xpath:string("/DescribeImageAttributeResponse/blockDeviceMapping/item", Doc)]
    end.

extract_permissions(Nodes) ->
    extract_permissions(Nodes, []).

extract_permissions([], Accum) ->
    lists:reverse(Accum);
extract_permissions([#xmlElement{name="group"} = Node|Nodes], Accum) ->
    extract_permissions(Nodes, [{group, get_text(Node)}|Accum]);
extract_permissions([#xmlElement{name="userId"} = Node|Nodes], Accum) ->
    extract_permissions(Nodes, [{user_id, get_text(Node)}|Accum]);
extract_permissions([_|Nodes], Accum) ->
    extract_permissions(Nodes, Accum).

-spec(describe_images/0 :: () -> proplist()).
describe_images() -> describe_images([], "self").

-spec(describe_images/1 :: ([string()] | aws_config()) -> proplist()).
describe_images(Config)
  when is_record(Config, aws_config) ->
    describe_images([], "self", none, Config);
describe_images(ImageIDs) ->
    describe_images(ImageIDs, none, none, default_config()).

-spec(describe_images/2 :: ([string()], aws_config()) -> proplist() ;
                           ([string()], string() | none) -> proplist()).
describe_images(ImageIDs, Config)
  when is_record(Config, aws_config) ->
    describe_images(ImageIDs, none, none, Config);
describe_images(ImageIDs, Owner) ->
    describe_images(ImageIDs, Owner, none, default_config()).

-spec(describe_images/3 :: ([string()], string() | none, aws_config()) -> proplist() ;
                           ([string()], string() | none, string() | none) -> proplist()).
describe_images(ImageIDs, Owner, Config)
  when is_record(Config, aws_config) ->
    describe_images(ImageIDs, Owner, none, Config);
describe_images(ImageIDs, Owner, ExecutableBy) ->
    describe_images(ImageIDs, Owner, ExecutableBy, default_config()).

-spec(describe_images/4 :: ([string()], string() | none, string() | none, aws_config()) -> proplist  ()).
describe_images(ImageIDs, Owner, ExecutableBy, Config)
  when is_list(ImageIDs),
        is_list(Owner) orelse Owner =:= none,
        is_list(ExecutableBy) orelse ExecutableBy =:= none ->
    Params = [
        {"ExecutableBy", ExecutableBy}, {"Owner", Owner}|
        erlcloud_aws:param_list(ImageIDs, "ImageId")
    ],
    Doc = ec2_query(Config, "DescribeImages", Params),
    [extract_image(Item) || Item <- xmerl_xpath:string("/DescribeImagesResponse/imagesSet/item", Doc)].

extract_image(Node) ->
    [{image_id, get_text("imageId", Node)},
     {image_location, get_text("imageLocation", Node)},
     {image_state, get_text("imageState", Node)},
     {image_owner_id, get_text("imageOwnerId", Node)},
     {is_public, get_bool("isPublic", Node)},
     {architecture, get_text("architecture", Node)},
     {image_type, get_text("imageType", Node)},
     {kernel_id, get_text("kernelId", Node)},
     {ramdisk_id, get_text("ramdiskId", Node)},
     {image_owner_alias, get_text("imageOwnerAlias", Node, none)},
     {name, get_text("name", Node)},
     {description, get_text("description", Node)},
     {root_device_type, get_text("rootDeviceType", Node)},
     {root_device_name, get_text("rootDeviceName", Node)},
     {block_device_mapping, [extract_block_device_mapping(Item) || Item <- xmerl_xpath:string("blockDeviceMapping/item", Node)]}
    ].

extract_block_device_mapping(Node) ->
    #ec2_block_device_mapping{
        device_name=get_text("deviceName", Node),
        virtual_name=get_text("virtualName", Node),
        snapshot_id=get_text("ebs/snapshotId", Node, none),
        volume_size=list_to_integer(get_text("ebs/volumeSize", Node, "0")),
        delete_on_termination=get_bool("ebs/deleteOnTermination", Node)
    }.

-spec(describe_instance_attribute/2 :: (string(), atom()) -> proplist()).
describe_instance_attribute(InstanceID, Attribute) ->
    describe_instance_attribute(InstanceID, Attribute, default_config()).

-spec(describe_instance_attribute/3 :: (string(), atom(), aws_config()) -> term()).
describe_instance_attribute(InstanceID, Attribute, Config)
  when is_list(InstanceID), is_atom(Attribute) ->
    AttributeName = case Attribute of
        instance_type -> "instanceType";
        kernel -> "kernel";
        ramdisk -> "ramdisk";
        user_data -> "userData";
        disable_api_termination -> "disableApiTermination";
        instance_initiated_shutdown_behavior -> "instanceInitiatedShutdownBehavior";
        root_device_name -> "rootDeviceName";
        block_device_mapping -> "blockDeviceMapping"
    end,
    Doc = ec2_query(Config, "DescribeInstanceAttribute", [{"InstanceId", InstanceID}, {"Attribute", AttributeName}]),
    Node = case Attribute of
        block_device_mapping -> hd(xmerl_xpath:string("/DescribeInstanceAttributeResponse/" ++ AttributeName, Doc));
        _ -> hd(xmerl_xpath:string("/DescribeInstanceAttributeResponse/" ++ AttributeName ++ "/value", Doc))
    end,
    case Attribute of
        user_data -> base64:decode(get_text(Node));
        disable_api_termination -> list_to_existing_atom(get_text(Node));
        instance_initiated_shutdown_behavior -> list_to_existing_atom(get_text(Node));
        block_device_mapping ->
            [extract_block_device_mapping_status(Item) || Item <- xmerl_xpath:string("item", Node)];
        _ -> get_text(Node)
    end.

-spec(describe_instances/0 :: () -> proplist()).
describe_instances() -> describe_instances([]).

-spec(describe_instances/1 :: ([string()] | aws_config()) -> proplist()).
describe_instances(Config)
  when is_record(Config, aws_config) ->
    describe_instances([], Config);
describe_instances(InstanceIDs) ->
    describe_instances(InstanceIDs, default_config()).

-spec(describe_instances/2 :: ([string()], aws_config()) -> proplist()).
describe_instances(InstanceIDs, Config)
  when is_list(InstanceIDs) ->
    Doc = ec2_query(Config, "DescribeInstances", erlcloud_aws:param_list(InstanceIDs, "InstanceId")),
    Reservations = xmerl_xpath:string("/DescribeInstancesResponse/reservationSet/item", Doc),
    [extract_reservation(Item) || Item <- Reservations].

extract_reservation(Node) ->
    [{reservation_id, get_text("reservationId", Node)},
     {owner_id, get_text("ownerId", Node)},
     {group_set, get_list("groupSet/item/groupId", Node)},
     {instances_set, [extract_instance(Item) || Item <- xmerl_xpath:string("instancesSet/item", Node)]}
    ].

extract_instance(Node) ->
    [{instance_id, get_text("instanceId", Node)},
     {image_id, get_text("imageId", Node)},
     {instance_state, [
       {code, list_to_integer(get_text("instanceState/code", Node, "0"))},
       {name, get_text("instanceState/name", Node)}
     ]},
     {private_dns_name, get_text("privateDnsName", Node)},
     {dns_name, get_text("dnsName", Node)},
     {reason, get_text("reason", Node, none)},
     {key_name, get_text("keyName", Node, none)},
     {ami_launch_index, list_to_integer(get_text("amiLaunchIndex", Node, "0"))},
     {product_codes, get_list("productCodes/item/productCode", Node)},
     {instance_type, get_text("instanceType", Node)},
     {launch_time, erlcloud_xml:get_time("launchTime", Node)},
     {placement, [{availability_zone, get_text("placement/availabilityZone", Node)}]},
     {kernel_id, get_text("kernelId", Node)},
     {ramdisk_id, get_text("ramdiskId", Node)},
     {monitoring, [{enabled, get_bool("monitoring/enabled", Node)}, {state, get_text("monitoring/state", Node)}]},
     {subnet_id, get_text("subnetId", Node)},
     {vpc_id, get_text("vpcId", Node)},
     {private_ip_address, get_text("privateIpAddress", Node)},
     {ip_address, get_text("ipAddress", Node)},
     {state_reason, [{code, get_text("stateReason/code", Node)}, {message, get_text("stateReason/message", Node)}]},
     {architecture, get_text("architecture", Node)},
     {root_device_type, get_text("rootDeviceType", Node)},
     {root_device_name, get_text("rootDeviceName", Node)},
     {block_device_mapping, [extract_block_device_mapping_status(Item) || Item <- xmerl_xpath:string("blockDeviceMapping/item", Node)]},
     {instance_lifecycle, get_text("instanceLifecycle", Node, none)},
     {spot_instance_request_id, get_text("spotInstanceRequestId", Node, none)}
    ].

extract_block_device_mapping_status(Node) ->
    [
        {device_name, get_text("deviceName", Node)},
        {volume_id, get_text("ebs/volumeId", Node)},
        {status, get_text("ebs/status", Node)},
        {attach_time, erlcloud_xml:get_time("ebs/attachTime", Node)},
        {delete_on_termination, get_bool("ebs/deleteOnTermination", Node)}
    ].

-spec(describe_key_pairs/0 :: () -> proplist()).
describe_key_pairs() -> describe_key_pairs([]).

-spec(describe_key_pairs/1 :: ([string()] | aws_config()) -> proplist()).
describe_key_pairs(Config)
  when is_record(Config, aws_config) ->
    describe_key_pairs([], Config);
describe_key_pairs(KeyNames) -> describe_key_pairs(KeyNames, default_config()).

-spec(describe_key_pairs/2 :: ([string()], aws_config()) -> proplist()).
describe_key_pairs(KeyNames, Config)
  when is_list(KeyNames) ->
    Doc = ec2_query(Config, "DescribeKeyPairs", erlcloud_aws:param_list(KeyNames, "KeyName")),
    Items = xmerl_xpath:string("/DescribeKeyPairsResponse/keySet/item", Doc),
    [
        [
            {key_name, get_text("keyName", Item)},
            {key_fingerprint, get_text("keyFingerprint", Item)}
        ] || Item <- Items
    ].

-spec(describe_regions/0 :: () -> proplist()).
describe_regions() -> describe_regions([]).
-spec(describe_regions/1 :: ([string()] | aws_config()) -> proplist()).
describe_regions(Config)
  when is_record(Config, aws_config) ->
    describe_regions([], Config);
describe_regions(RegionNames) ->
    describe_regions(RegionNames, default_config()).

-spec(describe_regions/2 :: ([string()], aws_config()) -> proplist()).
describe_regions(RegionNames, Config)
  when is_list(RegionNames) ->
    Doc = ec2_query(Config, "DescribeRegions", erlcloud_aws:param_list(RegionNames, "RegionName")),
    Items = xmerl_xpath:string("/DescribeRegionsResponse/regionInfo/item", Doc),
    [[{region_name, get_text("regionName", Item)},
      {region_endpoint, get_text("regionEndpoint", Item)}
     ] || Item <- Items].

-spec(describe_reserved_instances/0 :: () -> proplist()).
describe_reserved_instances() -> describe_reserved_instances([]).

-spec(describe_reserved_instances/1 :: ([string()] | aws_config()) -> proplist()).
describe_reserved_instances(Config)
  when is_record(Config, aws_config) ->
    describe_reserved_instances([], Config);
describe_reserved_instances(ReservedInstanceIDs) ->
    describe_reserved_instances(ReservedInstanceIDs, default_config()).

-spec(describe_reserved_instances/2 :: ([string()], aws_config()) -> proplist()).
describe_reserved_instances(ReservedInstanceIDs, Config)
  when is_list(ReservedInstanceIDs) ->
    Doc = ec2_query(Config, "DescribeReservedInstances", erlcloud_aws:param_list(ReservedInstanceIDs, "ReservedInstanceId")),
    ReservedInstances = xmerl_xpath:string("/DescribeReservedInstancesResponse/reservedInstancesSet/item", Doc),
    [extract_reserved_instance(Item) || Item <- ReservedInstances].

extract_reserved_instance(Node) ->
    [
        {reserved_instance_id, get_text("reservedInstanceId", Node)},
        {instance_type, get_text("instanceType", Node)},
        {availability_zone, get_text("availabilityZone", Node)},
        {start, erlcloud_xml:get_time("start", Node)},
        {duration, get_integer("duration", Node)},
        {fixed_price, get_text("fixedPrice", Node)},
        {usage_price, get_text("usagePrice", Node)},
        {instance_count, get_integer("instanceCount", Node)},
        {product_description, get_text("productDescription", Node)},
        {state, get_text("state", Node)}
    ].

-spec(describe_reserved_instances_offerings/0 :: () -> proplist()).
describe_reserved_instances_offerings() -> describe_reserved_instances_offerings([]).

-spec(describe_reserved_instances_offerings/1 :: ([{atom(), string()}] | aws_config()) -> proplist()).
describe_reserved_instances_offerings(Config)
  when is_record(Config, aws_config) ->
    describe_reserved_instances_offerings([], Config);
describe_reserved_instances_offerings(Selector) ->
    describe_reserved_instances_offerings(Selector, default_config()).

-spec(describe_reserved_instances_offerings/2 :: ([{atom(), string()}], aws_config()) -> proplist()).
describe_reserved_instances_offerings(Selector, Config)
  when is_list(Selector) ->
    InstanceTypes = [Value || {Key, Value} <- Selector, Key =:= instance_type],
    AvailabilityZones = [Value || {Key, Value} <- Selector, Key =:= availability_zone],
    Descs = [Value || {Key, Value} <- Selector, Key =:= product_description],
    Params = erlcloud_aws:param_list(InstanceTypes, "InstanceType") ++
             erlcloud_aws:param_list(AvailabilityZones, "AvailabilityZone") ++
             erlcloud_aws:param_list(Descs, "ProductDescription"),
    Doc = ec2_query(Config, "DescribeReservedInstancesOfferings", Params),
    [extract_reserved_instances_offering(Node) ||
     Node <- xmerl_xpath:string("/DescribeReservedInstancesOfferingsResponse/reservedInstancesOfferingsSet/item", Doc)].

extract_reserved_instances_offering(Node) ->
    [
        {reserved_instances_offering_id, get_text("reservedInstancesOfferingId", Node)},
        {instance_type, get_text("instanceType", Node)},
        {availability_zone, get_text("availabilityZone", Node)},
        {duration, get_integer("duration", Node)},
        {fixed_price, get_text("fixedPrice", Node)},
        {usage_price, get_text("usagePrice", Node)},
        {product_description, get_text("productDescription", Node)}
    ].

-spec(describe_security_groups/0 :: () -> [proplist()]).
describe_security_groups() ->
    describe_security_groups([]).

-spec(describe_security_groups/1 :: ([string()] | aws_config()) -> [proplist()]).
describe_security_groups(Config)
  when is_record(Config, aws_config) ->
    describe_security_groups([], Config);
describe_security_groups(GroupNames) ->
    describe_security_groups(GroupNames, default_config()).

-spec(describe_security_groups/2 :: ([string()], aws_config()) -> [proplist()]).
describe_security_groups(GroupNames, Config)
  when is_list(GroupNames) ->
    Doc = ec2_query(Config, "DescribeSecurityGroups", erlcloud_aws:param_list(GroupNames, "GroupName")),
    [extract_security_group(Node) ||
     Node <- xmerl_xpath:string("/DescribeSecurityGroupsResponse/securityGroupInfo/item", Doc)].

extract_security_group(Node) ->
    [
        {owner_id, get_text("ownerId", Node)},
        {group_name, get_text("groupName", Node)},
        {group_description, get_text("groupDescription", Node)},
        {ip_permissions,
         [extract_ip_permissions(Item) || Item <- xmerl_xpath:string("ipPermissions/item", Node)]}
    ].

extract_ip_permissions(Node) ->
    [
        {ip_protocol, list_to_existing_atom(get_text("ipProtocol", Node))},
        {from_port, get_integer("fromPort", Node)},
        {to_port, get_integer("toPort", Node)},
        {users, get_list("groups/item/userId", Node)},
        {ip_ranges, get_list("ipRanges/item/cidrIp", Node)}
    ].

-spec(describe_snapshot_attribute/2 :: (string(), atom()) -> proplist()).
describe_snapshot_attribute(SnapshotID, Attribute) ->
    describe_snapshot_attribute(SnapshotID, Attribute, default_config()).

-spec(describe_snapshot_attribute/3 :: (string(), atom(), aws_config()) -> term()).
describe_snapshot_attribute(SnapshotID, create_volume_permission, Config)
  when is_list(SnapshotID) ->
    Doc = ec2_query(Config, "DescribeSnapshotAttribute", [{"snapshotId", SnapshotID}, {"Attribute", "createVolumePermission"}]),
    extract_permissions(xmerl_xpath:string("/DescribeSnapshotAttributeResponse/createVolumePermission/item", Doc)).

-spec(describe_snapshots/0 :: () -> [proplist()]).
describe_snapshots() -> describe_snapshots([], "self").

-spec(describe_snapshots/1 :: ([string()] | aws_config()) -> proplist()).
describe_snapshots(Config)
  when is_record(Config, aws_config) ->
    describe_snapshots([], "self", Config);
describe_snapshots(SnapshotIDs) ->
    describe_snapshots(SnapshotIDs, none, none, default_config()).

-spec(describe_snapshots/2 :: ([string()], aws_config()) -> [proplist()] ;
                              ([string()], string() | none) -> [proplist()]).
describe_snapshots(SnapshotIDs, Config)
  when is_record(Config, aws_config) ->
    describe_snapshots(SnapshotIDs, none, none, Config);
describe_snapshots(SnapshotIDs, Owner) ->
    describe_snapshots(SnapshotIDs, Owner, none, default_config()).

-spec(describe_snapshots/3 :: ([string()], string() | none, aws_config()) -> [proplist()] ;
                              ([string()], string() | none, string() | none) -> [proplist()]).
describe_snapshots(SnapshotIDs, Owner, Config)
  when is_record(Config, aws_config) ->
    describe_snapshots(SnapshotIDs, Owner, none, Config);
describe_snapshots(SnapshotIDs, Owner, RestorableBy) ->
    describe_snapshots(SnapshotIDs, Owner, RestorableBy, default_config()).

-spec(describe_snapshots/4 :: ([string()], string() | none, string() | none, aws_config()) -> [proplist()]).
describe_snapshots(SnapshotIDs, Owner, RestorableBy, Config)
  when is_list(SnapshotIDs),
       is_list(Owner) orelse Owner =:= none,
       is_list(RestorableBy) orelse RestorableBy =:= none ->
    Params = [{"Owner", Owner}, {"RestorableBy", RestorableBy}|
              erlcloud_aws:param_list(SnapshotIDs, "SnapshotId")],
    Doc = ec2_query(Config, "DescribeSnapshots", Params),
    [extract_snapshot(Item) || Item <- xmerl_xpath:string("/DescribeSnapshotsResponse/snapshotSet/item", Doc)].

extract_snapshot(Node) ->
    [{snapshot_id, get_text("snapshotId", Node)},
     {volume_id, get_text("volumeId", Node)},
     {status, get_text("status", Node)},
     {start_time, erlcloud_xml:get_time("startTime", Node)},
     {progress, get_text("progress", Node, none)},
     {owner_id, get_text("ownerId", Node)},
     {volume_size, get_integer("volumeSize", Node)},
     {description, get_text("description", Node)},
     {owner_alias, get_text("ownerAlias", Node, none)}
    ].

-spec(describe_spot_datafeed_subscription/0 :: () -> proplist()).
describe_spot_datafeed_subscription() ->
    describe_spot_datafeed_subscription(default_config()).

-spec(describe_spot_datafeed_subscription/1 :: (aws_config()) -> proplist()).
describe_spot_datafeed_subscription(Config) ->
    Doc = ec2_query(Config, "DescribeSpotDatafeedSubscription", []),
    extract_spot_datafeed_subscription(xmerl_xpath:string("/DescribeSpotDatafeedSubscriptionResponse/spotDatafeedSubscription", Doc)).

-spec(describe_spot_instance_requests/0 :: () -> [proplist()]).
describe_spot_instance_requests() ->
    describe_spot_instance_requests([]).

-spec(describe_spot_instance_requests/1 :: ([string()] | aws_config()) -> [proplist()]).
describe_spot_instance_requests(Config)
  when is_record(Config, aws_config) ->
    describe_spot_instance_requests([], Config);
describe_spot_instance_requests(SpotInstanceRequestIDs) ->
    describe_spot_instance_requests(SpotInstanceRequestIDs, default_config()).

-spec(describe_spot_instance_requests/2 :: ([string()], aws_config()) -> [proplist()]).
describe_spot_instance_requests(SpotInstanceRequestIDs, Config)
  when is_list(SpotInstanceRequestIDs) ->
    Doc = ec2_query(Config, "DescribeSpotInstanceRequests",
                    erlcloud_aws:param_list(SpotInstanceRequestIDs, "SpotInstanceRequestId")),
    [extract_spot_instance_request(Item) ||
     Item <- xmerl_xpath:string("/DescribeSpotInstanceRequestsResponse/spotInstanceRequestSet/item", Doc)].

extract_spot_instance_request(Node) ->
    [
        {spot_instance_request_id, get_text("spotInstanceRequestId", Node)},
        {spot_price, get_text("spotPrice", Node)},
        {type, extract_spot_instance_request_type(get_text("type", Node))},
        {state, get_text("state", Node)},
        {fault, [{code, get_text("fault/code", Node)},
                 {message, get_text("fault/message", Node)}
                ]},
        {valid_from, erlcloud_xml:get_time("validFrom", Node)},
        {valid_until, erlcloud_xml:get_time("validUntil", Node)},
        {launch_group, get_text("launchGroup", Node, none)},
        {availability_zone_group, get_text("availabilityZoneGroup", Node, none)},
        {create_time, erlcloud_xml:get_time("createTime", Node)},
        {product_description, get_text("productDescription", Node)},
        {launch_specification, extract_launch_specification(hd(xmerl_xpath:string("launchSpecification", Node)))},
        {instance_id, get_text("instanceId", Node, none)}
    ].

extract_spot_instance_request_type("one-time") -> one_time;
extract_spot_instance_request_type("persistent") -> persistent.

extract_launch_specification(Node) ->
    [
        {image_id, get_text("imageId", Node)},
        {key_name, get_text("keyName", Node, none)},
        {group_set, get_list("groupSet/item/groupId", Node)},
        {user_data, base64:decode(get_text("userData/data", Node))},
        {instance_type, get_text("instanceType", Node)},
        {availability_zone, get_text("placement/availabilityZone", Node)},
        {kernel_id, get_text("kernelId", Node)},
        {ramdisk_id, get_text("ramdiskId", Node)},
        {subnet_id, get_text("subnetId", Node)}
    ].

-spec(describe_spot_price_history/0 :: () -> proplist()).
describe_spot_price_history() ->
    describe_spot_price_history(none).

-spec(describe_spot_price_history/1 :: (datetime() | none | aws_config()) -> proplist()).
describe_spot_price_history(Config)
  when is_record(Config, aws_config) ->
    describe_spot_price_history(none, Config);
describe_spot_price_history(StartTime) ->
    describe_spot_price_history(StartTime, none).

-spec(describe_spot_price_history/2 :: (datetime() | none, datetime() | none | aws_config()) -> proplist()).
describe_spot_price_history(StartTime, Config)
  when is_record(Config, aws_config) ->
    describe_spot_price_history(StartTime, none, Config);
describe_spot_price_history(StartTime, EndTime) ->
    describe_spot_price_history(StartTime, EndTime, []).

-spec(describe_spot_price_history/3 :: (datetime() | none, datetime() | none, [string()] | aws_config()) -> proplist()).
describe_spot_price_history(StartTime, EndTime, Config)
  when is_record(Config, aws_config) ->
    describe_spot_price_history(StartTime, EndTime, [], Config);
describe_spot_price_history(StartTime, EndTime, InstanceTypes) ->
    describe_spot_price_history(StartTime, EndTime, InstanceTypes, none).

-spec(describe_spot_price_history/4 :: (datetime() | none, datetime() | none, [string()], string() | none | aws_config()) -> proplist()).
describe_spot_price_history(StartTime, EndTime, InstanceTypes, Config)
  when is_record(Config, aws_config) ->
    describe_spot_price_history(StartTime, EndTime, InstanceTypes, none, Config);
describe_spot_price_history(StartTime, EndTime, InstanceTypes, ProductDescription) ->
    describe_spot_price_history(StartTime, EndTime, InstanceTypes,
                                ProductDescription, default_config()).

-spec(describe_spot_price_history/5 :: (datetime() | none, datetime() | none, [string()], string() | none, aws_config()) -> proplist()).
describe_spot_price_history(StartTime, EndTime, InstanceTypes,
                            ProductDescription, Config)
  when is_list(InstanceTypes),
       is_list(ProductDescription) orelse ProductDescription =:= none ->
    Doc = ec2_query(Config, "DescribeSpotPriceHistory",
        [{"StartTime", StartTime}, {"EndTime", EndTime},
         {"ProductDescription", ProductDescription}|
         erlcloud_aws:param_list(InstanceTypes, "InstanceType")]),
    [extract_spot_price_history(Item) ||
     Item <- xmerl_xpath:string("/DescribeSpotPriceHistoryResponse/spotPriceHistorySet/item", Doc)].

extract_spot_price_history(Node) ->
    [
        {instance_type, get_text("instanceType", Node)},
        {product_description, get_text("productDescription", Node)},
        {spot_price, get_text("spotPrice", Node)},
        {timestamp, erlcloud_xml:get_time("timestamp", Node)}
    ].

-spec(describe_volumes/0 :: () -> proplist()).
describe_volumes() -> describe_volumes([]).

-spec(describe_volumes/1 :: ([string()] | aws_config()) -> proplist()).
describe_volumes(Config)
  when is_record(Config, aws_config) ->
    describe_volumes([], Config);
describe_volumes(VolumeIDs) ->
    describe_volumes(VolumeIDs, default_config()).

-spec(describe_volumes/2 :: ([string()], aws_config()) -> proplist()).
describe_volumes(VolumeIDs, Config)
  when is_list(VolumeIDs) ->
    Doc = ec2_query(Config, "DescribeVolumes", erlcloud_aws:param_list(VolumeIDs, "VolumeId")),
    [extract_volume(Item) || Item <- xmerl_xpath:string("/DescribeVolumesResponse/volumeSet/item", Doc)].

extract_volume(Node) ->
    [{volume_id, get_text("volumeId", Node)},
     {size, get_integer("size", Node)},
     {snapshot_id, get_text("snapshotId", Node, none)},
     {availability_zone, get_text("availabilityZone", Node, none)},
     {status, get_text("status", Node, none)},
     {create_time, erlcloud_xml:get_time("createTime", Node)},
     {attachment_set,
      [[{volume_id, get_text("volumeId", Item)},
        {instance_id, get_text("instanceId",Item)},
        {device, get_text("device", Item)},
        {status, get_text("status", Item)},
        {attach_time, erlcloud_xml:get_time("attachTime", Item)}
       ] ||
       Item <- xmerl_xpath:string("attachmentSet/item", Node)
      ]
     }
    ].

-spec(detach_volume/1 :: (string()) -> proplist()).
detach_volume(VolumeID) -> detach_volume(VolumeID, default_config()).

-spec(detach_volume/2 :: (string(), aws_config()) -> proplist()).
detach_volume(VolumeID, Config)
  when is_list(VolumeID) ->
    Params = [{"VolumeId", VolumeID}],
    Doc = ec2_query(Config, "DetachVolume", Params),
    extract_volume_status(hd(xmerl_xpath:string("/DetachVolumeResponse", Doc))).

-spec(disassociate_address/1 :: (string()) -> ok).
disassociate_address(PublicIP) ->
    disassociate_address(PublicIP, default_config()).

-spec(disassociate_address/2 :: (string(), aws_config()) -> ok).
disassociate_address(PublicIP, Config)
  when is_list(PublicIP) ->
    ec2_simple_query(Config, "DisassociateAddress", [{"PublicIp", PublicIP}]).

-spec(get_console_output/1 :: (string()) -> proplist()).
get_console_output(InstanceID) -> get_console_output(InstanceID, default_config()).

-spec(get_console_output/2 :: (string(), aws_config()) -> proplist()).
get_console_output(InstanceID, Config)
  when is_list(InstanceID) ->
    Doc = ec2_query(Config, "GetConsoleOutput", [{"InstanceId", InstanceID}]),
    [{instance_id, get_text("/GetConsoleOutputResponse/instanceId", Doc)},
     {timestamp, erlcloud_xml:get_time("/GetConsoleOutputResponse/timestamp", Doc)},
     {output, base64:decode(get_text("/GetConsoleOutputResponse/output", Doc))}
    ].

-spec(get_password_data/1 :: (string()) -> proplist()).
get_password_data(InstanceID) -> get_password_data(InstanceID, default_config()).

-spec(get_password_data/2 :: (string(), aws_config()) -> proplist()).
get_password_data(InstanceID, Config)
  when is_list(InstanceID) ->
    Doc = ec2_query(Config, "GetPasswordData", [{"InstanceId", InstanceID}]),
    [{instance_id, get_text("/GetPasswordDataResponse/instanceId", Doc)},
     {timestamp, erlcloud_xml:get_time("/GetPasswordDataResponse/timestamp", Doc)},
     {password_data, get_text("/GetPasswordDataResponse/passwordData", Doc)}
    ].

-spec(modify_image_attribute/3 :: (string(), atom(), term()) -> ok).
modify_image_attribute(ImageID, Attribute, Value) ->
    modify_image_attribute(ImageID, Attribute, Value, default_config()).

-spec(modify_image_attribute/4 :: (string(), atom(), term(), aws_config()) -> ok).
modify_image_attribute(ImageID, Attribute, Value, Config) ->
    {AttributeName, OperationType, Values} = case {Attribute, Value} of
        {launch_permission, {Operation, Permissions}}
          when Operation =:= add orelse Operation =:= remove,
               is_list(Permissions) ->
            {"launchPermission", Operation, permission_list(Permissions)};
        {description, Desc} ->
            {"description", none, [{"Description", Desc}]};
        {product_codes, ProductCodes} ->
            {"productCodes", none, erlcloud_aws:param_list(ProductCodes, "ProductCode")}
    end,

    Params = [{"ImageId", ImageID},
              {"Attribute", AttributeName},
              {"OperationType", OperationType}|
              Values
             ],
    ec2_simple_query(Config, "ModifyImageAttribute", Params).

-spec(modify_instance_attribute/3 :: (string(), atom(), term()) -> ok).
modify_instance_attribute(InstanceID, Attribute, Value) ->
    modify_instance_attribute(InstanceID, Attribute, Value, default_config()).

-spec(modify_instance_attribute/4 :: (string(), atom(), term(), aws_config()) -> ok).
modify_instance_attribute(InstanceID, Attribute, Value, Config) ->
    {AttributeName, AParams} = case Attribute of
        instance_type when is_list(Value) -> {"instanceType", [{"Value", Value}]};
        kernel when is_list(Value) -> {"kernel", [{"Value", Value}]};
        ramdisk when is_list(Value) -> {"ramdisk", [{"Value", Value}]};
        user_data when is_list(Value); is_binary(Value) -> {"userData", [{"Value", base64:encode(Value)}]};
        disable_api_termination when is_boolean(Value) ->
            {"disableApiTermination", [{"Value", Value}]};
        instance_initiated_shutdown_behavior
          when Value =:= stop; Value =:= terminate ->
            {"instanceInitiatedShutdownBehavior", [{"Value", Value}]};
        root_device_name when is_list(Value) -> {"rootDeviceName", Value};
        block_device_mapping when is_list(Value) ->
            {"blockDeviceMapping", block_device_params(Value)}
    end,
    Params = [{"InstanceID", InstanceID}, {"Attribute", AttributeName}|AParams],
    ec2_simple_query(Config, "ModifyInstanceAttribute", Params).

permission_list(Permissions) ->
    UserIDs = [UserID || {user_id, UserID} <- Permissions],
    Groups = [Group || {group, Group} <- Permissions],
    erlcloud_aws:param_list(UserIDs, "UserId") ++ erlcloud_aws:param_list(Groups, "Group").

-spec(modify_snapshot_attribute/3 :: (string(), atom(), term()) -> ok).
modify_snapshot_attribute(SnapshotID, Attribute, Value) ->
    modify_snapshot_attribute(SnapshotID, Attribute, Value, default_config()).

-spec(modify_snapshot_attribute/4 :: (string(), atom(), term(), aws_config()) -> ok).
modify_snapshot_attribute(SnapshotID, create_volume_permission,
                          {Operation, Permissions}, Config)
  when Operation =:= add orelse Operation =:= remove,
       is_list(Permissions) ->
    Params = [{"SnapshotId", SnapshotID},
              {"Attribute", "createVolumePermission"},
              {"OperationType", Operation}|
              permission_list(Permissions)
             ],
    ec2_simple_query(Config, "ModifySnapshotAttribute", Params).

-spec(monitor_instances/1 :: ([string()]) -> proplist()).
monitor_instances(InstanceIDs) ->
    monitor_instances(InstanceIDs, default_config()).

-spec(monitor_instances/2 :: ([string()], aws_config()) -> [proplist()]).
monitor_instances(InstanceIDs, Config) ->
    Doc = ec2_query(Config, "MonitorInstances", erlcloud_aws:param_list(InstanceIDs, "InstanceId")),
    [extract_monitor_state(Node) || Node <- xmerl_xpath:string("/MonitorInstancesResponse/instancesSet/item", Doc)].

extract_monitor_state(Node) ->
    [
        {instance_id, get_text("instanceId", Node)},
        {monitoring_state, get_text("monitoring/state", Node)}
    ].

-spec(purchase_reserved_instances_offering/1 :: ([string() | {string(), pos_integer()}]) -> [string()]).
purchase_reserved_instances_offering(ReservedInstancesOfferings) ->
    purchase_reserved_instances_offering(ReservedInstancesOfferings, default_config()).

-spec(purchase_reserved_instances_offering/2 :: ([string() | {string(), pos_integer()}], aws_config()) -> [string()]).
purchase_reserved_instances_offering(ReservedInstancesOfferings, Config)
  when is_list(ReservedInstancesOfferings), length(ReservedInstancesOfferings) > 0 ->
    Params = lists:flatten(
        [case Offering of
            {OfferingID, Count} ->
                [
                    {"ReservedInstancesOfferingId." ++ integer_to_list(I), OfferingID},
                    {"InstanceCount." ++ integer_to_list(I), Count}
                ];
            OfferingID when is_list(OfferingID) ->
                [{"ReservedInstancesOfferingId." ++ integer_to_list(I), OfferingID}]
         end || {I, Offering} <- lists:zip(lists:seq(0, length(ReservedInstancesOfferings) - 1),
                                           ReservedInstancesOfferings)]),
    Doc = ec2_query(Config, "PurchaseReservedInstancesOffering", Params),
    get_list("/PurchaseReservedInstancesOfferingResponse/reservedInstancesId", Doc).

-spec(reboot_instances/1 :: ([string()]) -> ok).
reboot_instances(InstanceIDs) -> reboot_instances(InstanceIDs, default_config()).

-spec(reboot_instances/2 :: ([string()], aws_config()) -> ok).
reboot_instances(InstanceIDs, Config)
  when is_list(InstanceIDs) ->
    ec2_simple_query(Config, "RebootInstances", erlcloud_aws:param_list(InstanceIDs, "InstanceId")).

-spec(register_image/1 :: (ec2_image_spec()) -> proplist()).
register_image(ImageSpec) -> register_image(ImageSpec, default_config()).

-spec(register_image/2 :: (ec2_image_spec(), aws_config()) -> proplist()).
register_image(ImageSpec, Config) ->
    Params = [
        {"ImageLocation", ImageSpec#ec2_image_spec.image_location},
        {"Name", ImageSpec#ec2_image_spec.name},
        {"Description", ImageSpec#ec2_image_spec.description},
        {"Architecture", ImageSpec#ec2_image_spec.architecture},
        {"KernelId", ImageSpec#ec2_image_spec.kernel_id},
        {"RamdiskId", ImageSpec#ec2_image_spec.ramdisk_id},
        {"RootDeviceName", ImageSpec#ec2_image_spec.root_device_name}
    ],
    BDParams = block_device_params(ImageSpec#ec2_image_spec.block_device_mapping),
    Doc = ec2_query(Config, "RegisterImage", Params ++ BDParams),
    [{image_id, get_text("/CreateImageResponse/imageId", Doc)}].

-spec(release_address/1 :: (string()) -> ok).
release_address(PublicIP) -> release_address(PublicIP, default_config()).

-spec(request_spot_instances/1 :: (ec2_spot_instance_request()) -> [proplist()]).
request_spot_instances(Request) ->
    request_spot_instances(Request, default_config()).

-spec(request_spot_instances/2 :: (ec2_spot_instance_request(), aws_config()) -> [proplist()]).
request_spot_instances(Request, Config) ->
    InstanceSpec = Request#ec2_spot_instance_request.launch_specification,
    Params = [
        {"SpotPrice", Request#ec2_spot_instance_request.spot_price},
        {"InstanceCount", Request#ec2_spot_instance_request.instance_count},
        {"Type", case Request#ec2_spot_instance_request.type of one_time -> "one-time" ; persistent -> "persistent" end},
        {"ValidFrom", Request#ec2_spot_instance_request.valid_from},
        {"ValidUntil", Request#ec2_spot_instance_request.valid_until},
        {"LaunchGroup", Request#ec2_spot_instance_request.launch_group},
        {"AvailabilityZoneGroup", Request#ec2_spot_instance_request.availability_zone_group},
        {"LaunchSpecification.ImageId", InstanceSpec#ec2_instance_spec.image_id},
        {"LaunchSpecification.KeyName", InstanceSpec#ec2_instance_spec.key_name},
        {"LaunchSpecification.UserData",
         case InstanceSpec#ec2_instance_spec.user_data of
             undefined -> undefined;
             Data -> base64:encode(Data)
         end},
        {"LaunchSpecification.InstanceType", InstanceSpec#ec2_instance_spec.instance_type},
        {"LaunchSpecification.KernelId", InstanceSpec#ec2_instance_spec.kernel_id},
        {"LaunchSpecification.Ramdiskd", InstanceSpec#ec2_instance_spec.ramdisk_id},
        {"LaunchSpecification.Monitoring.Enabled", InstanceSpec#ec2_instance_spec.monitoring_enabled},
        {"LaunchSpecification.SubnetId", InstanceSpec#ec2_instance_spec.subnet_id},
        {"LaunchSpecification.Placement.AvailabilityZone", InstanceSpec#ec2_instance_spec.availability_zone}
    ],
    GParams = erlcloud_aws:param_list(InstanceSpec#ec2_instance_spec.group_set, "LaunchSpecification.SecurityGroup"),
    BDParams = [
        {"LaunchSpecification." ++ Key, Value} ||
        {Key, Value} <- block_device_params(InstanceSpec#ec2_instance_spec.block_device_mapping)],

    Doc = ec2_query(Config, "RequestSpotInstances", Params ++ BDParams ++ GParams),
    [extract_spot_instance_request(Item) ||
     Item <- xmerl_xpath:string("/RequestSpotInstancesResponse/spotInstanceRequestSet/item", Doc)].

-spec(release_address/2 :: (string(), aws_config()) -> ok).
release_address(PublicIP, Config)
  when is_list(PublicIP) ->
    ec2_simple_query(Config, "ReleaseAddress", [{"PublicIp", PublicIP}]).

-spec(reset_image_attribute/2 :: (string(), atom()) -> ok).
reset_image_attribute(ImageID, Attribute) ->
    reset_image_attribute(ImageID, Attribute, default_config()).

-spec(reset_image_attribute/3 :: (string(), atom(), aws_config()) -> ok).
reset_image_attribute(ImageID, launch_permission, Config) ->
    ec2_simple_query(Config, "ResetImageAttribute",
        [{"ImageId", ImageID}, {"Attribute", "launchPermission"}]).

-spec(reset_instance_attribute/2 :: (string(), atom()) -> ok).
reset_instance_attribute(InstanceID, Attribute) ->
    reset_instance_attribute(InstanceID, Attribute, default_config()).

-spec(reset_instance_attribute/3 :: (string(), atom(), aws_config()) -> ok).
reset_instance_attribute(InstanceID, Attribute, Config)
  when is_list(InstanceID),
       Attribute =:= kernel orelse Attribute =:= ramdisk ->
    ec2_simple_query(Config, "ResetInstanceAttribute",
        [{"InstanceId", InstanceID}, {"Attribute", Attribute}]).

-spec(reset_snapshot_attribute/2 :: (string(), atom()) -> ok).
reset_snapshot_attribute(SnapshotID, Attribute) ->
    reset_snapshot_attribute(SnapshotID, Attribute, default_config()).

-spec(reset_snapshot_attribute/3 :: (string(), atom(), aws_config()) -> ok).
reset_snapshot_attribute(SnapshotID, create_volume_permission, Config)
  when is_list(SnapshotID) ->
    ec2_simple_query(Config, "ResetSnapshotAttribute",
        [{"snapshotId", SnapshotID}, {"Attribute", "createVolumePermission"}]).

-spec(revoke_security_group_ingress/2 :: (string(), ec2_ingress_spec()) -> ok).
revoke_security_group_ingress(GroupName, IngressSpec) ->
    revoke_security_group_ingress(GroupName, IngressSpec, default_config()).

-spec(revoke_security_group_ingress/3 :: (string(), ec2_ingress_spec(), aws_config()) -> ok).
revoke_security_group_ingress(GroupName, IngressSpec, Config)
  when is_list(GroupName), is_record(IngressSpec, ec2_ingress_spec) ->
    Params = [{"GroupName", GroupName}|ingress_spec_params(IngressSpec)],
    ec2_simple_query(Config, "RevokeSecurityGroupIngress", Params).

-spec(run_instances/1 :: (ec2_instance_spec()) -> proplist()).
run_instances(InstanceSpec) -> run_instances(InstanceSpec, default_config()).

-spec(run_instances/2 :: (ec2_instance_spec(), aws_config()) -> proplist()).
run_instances(InstanceSpec, Config)
  when is_record(InstanceSpec, ec2_instance_spec) ->
    Params = [
        {"ImageId", InstanceSpec#ec2_instance_spec.image_id},
        {"MaxCount", InstanceSpec#ec2_instance_spec.max_count},
        {"MinCount", InstanceSpec#ec2_instance_spec.min_count},
        {"KeyName", InstanceSpec#ec2_instance_spec.key_name},
        {"UserData",
         case InstanceSpec#ec2_instance_spec.user_data of
             undefined -> undefined;
             Data -> base64:encode(Data)
         end},
        {"InstanceType", InstanceSpec#ec2_instance_spec.instance_type},
        {"KernelId", InstanceSpec#ec2_instance_spec.kernel_id},
        {"Ramdiskd", InstanceSpec#ec2_instance_spec.ramdisk_id},
        {"Monitoring.Enabled", InstanceSpec#ec2_instance_spec.monitoring_enabled},
        {"SubnetId", InstanceSpec#ec2_instance_spec.subnet_id},
        {"Placement.AvailabilityZone", InstanceSpec#ec2_instance_spec.availability_zone},
        {"DisableApiTermination", InstanceSpec#ec2_instance_spec.disable_api_termination},
        {"InstanceInitiatedShutdownBehavior", InstanceSpec#ec2_instance_spec.instance_initiated_shutdown_behavior}
    ],
    GParams = erlcloud_aws:param_list(InstanceSpec#ec2_instance_spec.group_set, "SecurityGroup"),
    BDParams = block_device_params(InstanceSpec#ec2_instance_spec.block_device_mapping),
    Doc = ec2_query(Config, "RunInstances", Params ++ GParams ++ BDParams),
    extract_reservation(hd(xmerl_xpath:string("/RunInstancesResponse", Doc))).

% -spec(run_instances/2 :: ([string()], TagsList::[{key,value}], aws_config()) -> proplist()).
create_tags(ResourceIds, TagsList, Config) when is_list(ResourceIds)->
   {Tags, _} = lists:foldl(fun({Key, Value}, {Acc, Index}) ->
                                   I = integer_to_list(Index),
                                   TKKey = "Tag."++I++".Key",
                                   TVKey = "Tag."++I++".Value",
                                   {[{TKKey, Key}, {TVKey, Value} | Acc], Index+1}
                       end, {[], 1}, TagsList),
    {Resources, _} = lists:foldl(fun(ResourceId, {Acc, Index}) ->
                                   I = integer_to_list(Index),
                                   TKey = "ResourceId."++I,
                                   {[{TKey, ResourceId} | Acc], Index+1}
                       end, {[], 1}, ResourceIds),
    ec2_query(Config, "CreateTags", Resources ++ Tags, "2010-08-31").

block_device_params(Mappings) ->
    erlcloud_aws:param_list(
        [[{"DeviceName", Mapping#ec2_block_device_mapping.device_name},
          {"VirtualName", Mapping#ec2_block_device_mapping.virtual_name}] ++
         if Mapping#ec2_block_device_mapping.snapshot_id =/= none,
            Mapping#ec2_block_device_mapping.snapshot_id =/= undefined ->
             [{"Ebs.SnapshotId", Mapping#ec2_block_device_mapping.snapshot_id},
              {"Ebs.DeleteOnTermination", Mapping#ec2_block_device_mapping.delete_on_termination}];
            is_integer(Mapping#ec2_block_device_mapping.volume_size) ->
             [{"Ebs.VolumeSize", Mapping#ec2_block_device_mapping.volume_size},
              {"Ebs.DeleteOnTermination", Mapping#ec2_block_device_mapping.delete_on_termination}];
            true ->
             []
         end ||
         Mapping <- Mappings], "BlockDeviceMapping").

-spec(start_instances/1 :: ([string()]) -> proplist()).
start_instances(InstanceIDs) -> start_instances(InstanceIDs, default_config()).

-spec(start_instances/2 :: ([string()], aws_config()) -> proplist()).
start_instances(InstanceIDs, Config)
  when is_list(InstanceIDs) ->
    Doc = ec2_query(Config, "StartInstances", erlcloud_aws:param_list(InstanceIDs, "InstanceId")),
    [extract_instance_state_change(Node) || Node <- xmerl_xpath:string("/StartInstancesResponse/instancesSet/item", Doc)].

-spec(stop_instances/1 :: ([string()]) -> proplist()).
stop_instances(InstanceIDs) -> stop_instances(InstanceIDs, default_config()).

-spec(stop_instances/2 :: ([string()], boolean() | aws_config()) -> proplist()).
stop_instances(InstanceIDs, Config)
  when is_record(Config, aws_config) ->
    stop_instances(InstanceIDs, false, Config);
stop_instances(InstanceIDs, Force) ->
    stop_instances(InstanceIDs, Force, default_config()).

-spec(stop_instances/3 :: ([string()], boolean(), aws_config()) -> proplist()).
stop_instances(InstanceIDs, Force, Config)
  when is_list(InstanceIDs), is_boolean(Force) ->
    Doc = ec2_query(Config, "StopInstances",
      [{"Force", atom_to_list(Force)}|erlcloud_aws:param_list(InstanceIDs, "InstanceId")]),
    [extract_instance_state_change(Node) || Node <- xmerl_xpath:string("/StopInstancesResponse/instancesSet/item", Doc)].

-spec(terminate_instances/1 :: ([string()]) -> proplist()).
terminate_instances(InstanceIDs) -> terminate_instances(InstanceIDs, default_config()).

-spec(terminate_instances/2 :: ([string()], aws_config()) -> proplist()).
terminate_instances(InstanceIDs, Config)
  when is_list(InstanceIDs) ->
    Doc = ec2_query(Config, "TerminateInstances", erlcloud_aws:param_list(InstanceIDs, "InstanceId")),
    [extract_instance_state_change(Node) || Node <- xmerl_xpath:string("/TerminateInstancesResponse/instancesSet/item", Doc)].

extract_instance_state_change(Node) ->
    [
        {instance_id, get_text("instanceId", Node)},
        {current_state,
            {code, list_to_integer(get_text("currentState/code", Node, "0"))},
            {name, get_text("currentState/name", Node)}
        },
        {previous_state,
            {code, list_to_integer(get_text("previousState/code", Node, "0"))},
            {name, get_text("previousState/name", Node)}
        }
    ].

-spec(unmonitor_instances/1 :: ([string()]) -> proplist()).
unmonitor_instances(InstanceIDs) ->
    unmonitor_instances(InstanceIDs, default_config()).

-spec(unmonitor_instances/2 :: ([string()], aws_config()) -> [proplist()]).
unmonitor_instances(InstanceIDs, Config) ->
    Doc = ec2_query(Config, "UnmonitorInstances", erlcloud_aws:param_list(InstanceIDs, "InstanceId")),
    [extract_monitor_state(Node) || Node <- xmerl_xpath:string("/UnmonitorInstancesResponse/instancesSet/item", Doc)].

ec2_simple_query(Config, Action, Params) ->
    ec2_query(Config, Action, Params),
    ok.

ec2_query(Config, Action, Params) ->
    ec2_query(Config, Action, Params, ?API_VERSION).

ec2_query(Config, Action, Params, ApiVersion) ->
    QParams = [{"Action", Action}, {"Version", ApiVersion}|Params],
    erlcloud_aws:aws_request_xml(post, Config#aws_config.ec2_host,
        "/", QParams, Config#aws_config.access_key_id,
        Config#aws_config.secret_access_key).

default_config() -> erlcloud_aws:default_config().
