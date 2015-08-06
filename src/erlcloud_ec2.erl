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
         describe_images/3, describe_images/4, describe_images/5,
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
         describe_instances/3,
         modify_instance_attribute/3, modify_instance_attribute/4,
         modify_instance_attribute/5,
         reboot_instances/1, reboot_instances/2,
         reset_instance_attribute/2, reset_instance_attribute/3,
         run_instances/1, run_instances/2,
         start_instances/1, start_instances/2,
         stop_instances/1, stop_instances/2, stop_instances/3,
         terminate_instances/1, terminate_instances/2,
         describe_instance_status/1, describe_instance_status/2,
         describe_instance_status/3,

         %% Key Pairs
         create_key_pair/1, create_key_pair/2,
         import_key_pair/2, import_key_pair/3,
         delete_key_pair/1, delete_key_pair/2,
         describe_key_pairs/0, describe_key_pairs/1, describe_key_pairs/2,

         %% Monitoring
         monitor_instances/1, monitor_instances/2,
         unmonitor_instances/1, unmonitor_instances/2,

         %% Network Interfaces
         describe_network_interfaces/0, describe_network_interfaces/1, 
         describe_network_interfaces/2,
         describe_network_interfaces_filtered/3,

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
         create_security_group/2, create_security_group/3, create_security_group/4,
         delete_security_group/1, delete_security_group/2, delete_security_group/3,
         describe_security_groups/0, describe_security_groups/1, describe_security_groups/2,
         describe_security_groups_filtered/1, describe_security_groups_filtered/2,
         revoke_security_group_ingress/2, revoke_security_group_ingress/3,

         %% Spot Instances
         cancel_spot_instance_requests/1, cancel_spot_instance_requests/2,
         create_spot_datafeed_subscription/1, create_spot_datafeed_subscription/2,
         create_spot_datafeed_subscription/3,
         delete_spot_datafeed_subscription/0, delete_spot_datafeed_subscription/1,
         describe_spot_datafeed_subscription/0, describe_spot_datafeed_subscription/1,
         describe_spot_instance_requests/0, describe_spot_instance_requests/1,
         describe_spot_instance_requests/2,
         describe_spot_price_history/0, describe_spot_price_history/1,
         describe_spot_price_history/2, describe_spot_price_history/3,
         describe_spot_price_history/5,
         request_spot_instances/1, request_spot_instances/2,

         %% Windows
         bundle_instance/6, bundle_instance/7,
         cancel_bundle_task/1, cancel_bundle_task/2,
         describe_bundle_tasks/0, describe_bundle_tasks/1, describe_bundle_tasks/2,
         get_password_data/1, get_password_data/2,

         %% VPC
         describe_subnets/0, describe_subnets/1, describe_subnets/2,
         create_subnet/2, create_subnet/3, create_subnet/4,
         delete_subnet/1, delete_subnet/2,
         describe_vpcs/0, describe_vpcs/1, describe_vpcs/2,
         create_vpc/1, create_vpc/2, create_vpc/3,
         delete_vpc/1, delete_vpc/2,
         describe_dhcp_options/0, describe_dhcp_options/1, describe_dhcp_options/2,
         associate_dhcp_options/2, associate_dhcp_options/3,
         describe_internet_gateways/0, describe_internet_gateways/1,
         describe_internet_gateways/2,
         create_internet_gateway/0, create_internet_gateway/1,
         attach_internet_gateway/2, attach_internet_gateway/3,
         delete_internet_gateway/1, delete_internet_gateway/2,
         detach_internet_gateway/2, detach_internet_gateway/3,
         describe_route_tables/0, describe_route_tables/1, describe_route_tables/2,
         create_route_table/1, create_route_table/2,
         delete_route_table/1, delete_route_table/2,
         create_route/4, create_route/5, delete_route/2, delete_route/3,
         associate_route_table/2, associate_route_table/3,

         %% VPC/Network ACLs
         create_network_acl/1, create_network_acl/2,
         delete_network_acl/1, delete_network_acl/2,
         describe_network_acls/0, describe_network_acls/1, describe_network_acls/2,
         create_network_acl_entry/1, create_network_acl_entry/2,
         replace_network_acl_entry/1, replace_network_acl_entry/2,
         delete_network_acl_entry/2, delete_network_acl_entry/3,
         delete_network_acl_entry/4,
         replace_network_acl_association/2, replace_network_acl_association/3,

         %% Tagging. Uses different version of AWS API
         create_tags/2, create_tags/3,
         describe_tags/0, describe_tags/1, describe_tags/2,
         delete_tags/2, delete_tags/3,
        
         %% VPN gateways
         describe_vpn_gateways/0, describe_vpn_gateways/1, describe_vpn_gateways/2,
         describe_vpn_connections/0, describe_vpn_connections/1, describe_vpn_connections/2,
        
        %% Customer gateways
         describe_customer_gateways/0, describe_customer_gateways/1, describe_customer_gateways/2

        ]).

-import(erlcloud_xml, [get_text/1, get_text/2, get_text/3, get_bool/2, get_list/2, get_integer/2]).

-define(API_VERSION, "2009-11-30").
% -define(NEW_API_VERSION, "2012-10-01").
% -define(NEW_API_VERSION, "2013-10-15").
% -define(NEW_API_VERSION, "2014-02-01").
% -define(NEW_API_VERSION, "2014-06-15").
-define(NEW_API_VERSION, "2014-10-01").
-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include_lib("erlcloud/include/erlcloud_ec2.hrl").

-type(filter_list() :: [{string(),[string()]}]).

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

%%
%%
-spec(allocate_address/0 :: () -> {ok, string()} | {error, any()}).
allocate_address() -> allocate_address(none, default_config()).

-spec(allocate_address/1 :: (aws_config()) -> {ok, string()} | {error, any()}).
allocate_address(Config) when is_record(Config, aws_config) ->
    allocate_address(none, Config);
allocate_address(Domain) when is_atom(Domain) ->
    allocate_address(Domain, default_config()).

-spec(allocate_address/2 :: (none | vpc, aws_config()) -> {ok, string()} | {ok, {string() | string()}} | {error, any()}).
allocate_address(Domain, Config) ->
    Params = case Domain of
                 vpc -> [{"Domain", "vpc"}];
                 none -> []
             end,
    case ec2_query(Config, "AllocateAddress", Params, ?NEW_API_VERSION) of
        {ok, Doc} ->
            case Domain of
                vpc ->
                    {ok, {get_text("/AllocateAddressResponse/publicIp", Doc),
                          get_text("/AllocateAddressResponse/allocationId", Doc)}};
                none ->
                    {ok, get_text("/AllocateAddressResponse/publicIp", Doc)}
            end;
        {error, _} = Error ->
            Error
    end.

%%
%%
-spec(associate_address/2 :: (string(), string()) -> ok | {error, any()}).
associate_address(PublicIP, InstanceID) ->
    associate_address(PublicIP, InstanceID, default_config()).

-spec(associate_address/3 :: (string(), string(), string() | aws_config()) -> ok | {error, any()}).
associate_address(PublicIP, InstanceID, Config)
  when is_list(PublicIP), is_list(InstanceID), is_record(Config, aws_config) ->
    associate_address(PublicIP, InstanceID, none, Config);
associate_address(PublicIP, InstanceID, AllocationID)
  when is_list(PublicIP), is_list(InstanceID), is_list(AllocationID) ->
    associate_address(PublicIP, InstanceID, AllocationID, default_config()).

-spec(associate_address/4 :: (string(), string(), string() | none, aws_config()) -> ok | {error, any()}).
associate_address(PublicIP, InstanceID, AllocationID, Config) ->
    AllocationParam = case AllocationID of
                          none -> [{ "PublicIp", PublicIP} ];
                          ID -> [{ "AllocationId", ID }]
                      end,
    ec2_simple_query(Config, "AssociateAddress",
                     [{"InstanceId", InstanceID} | AllocationParam],
                     ?NEW_API_VERSION).

%%
%%
-spec(associate_dhcp_options/2 :: (string(), string()) -> ok | {error, any()}).
associate_dhcp_options(OptionsID, VpcID) ->
    associate_dhcp_options(OptionsID, VpcID, default_config()).

-spec(associate_dhcp_options/3 :: (string(), string(), aws_config()) -> ok | {error, any()}).
associate_dhcp_options(OptionsID, VpcID, Config) ->
    ec2_simple_query(Config, "AssociateDhcpOptions",
                     [{"DhcpOptionsId", OptionsID}, {"VpcId", VpcID}]).

%%
%%
-spec(associate_route_table/2 :: (string(), string()) -> {ok, string()} | {error, any()}).
associate_route_table(RouteTableID, SubnetID) ->
    associate_route_table(RouteTableID, SubnetID, default_config()).

-spec(associate_route_table/3 :: (string(), string(), aws_config()) -> {ok, string()} | {error, any()}).
associate_route_table(RouteTableID, SubnetID, Config) ->
    Params = [{"RouteTableId", RouteTableID}, {"SubnetId", SubnetID}],
    case ec2_query(Config, "AssociateRouteTable", Params, ?NEW_API_VERSION) of
        {ok, Doc} ->
            {ok, get_text("/AssociateRouteTableResponse/associationId", Doc)};
        {error, _} = Error ->
            Error
    end.

%%
%%
-spec(attach_internet_gateway/2 :: (string(), string()) -> {ok, proplist()} | {error, any()}).
attach_internet_gateway(GatewayID, VpcID) ->
    attach_internet_gateway(GatewayID, VpcID, default_config()).

-spec(attach_internet_gateway/3 :: (string(), string(), aws_config()) -> {ok, proplist()} | {error, any()}).
attach_internet_gateway(GatewayID, VpcID, Config) ->
    ec2_simple_query(Config, "AttachInternetGateway",
                     [{"InternetGatewayId", GatewayID},
                      {"VpcId", VpcID}], ?NEW_API_VERSION).


%%
%%
-spec(attach_volume/3 :: (string(), string(), string()) -> {ok, proplist()} | {error, any()}).
attach_volume(VolumeID, InstanceID, Device) ->
    attach_volume(VolumeID, InstanceID, Device, default_config()).

-spec(attach_volume/4 :: (string(), string(), string(), aws_config()) -> {ok, proplist()} | {error, any()}).
attach_volume(VolumeID, InstanceID, Device, Config)
  when is_list(VolumeID), is_list(InstanceID), is_list(Device) ->
    case ec2_query(Config, "AttachVolume", [{"InstanceId", InstanceID}, {"Device", Device}, {"VolumeId", VolumeID}]) of
        {ok, Doc} ->
            {ok, extract_volume_status(hd(xmerl_xpath:string("/AttachVolumeResponse", Doc)))};
        {error, _} = Error ->
            Error
    end.

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

%%
%%
-spec(authorize_security_group_ingress/2 :: (string(), ec2_ingress_spec()) -> ok | {error, any()}).
authorize_security_group_ingress(GroupName, IngressSpec) ->
    authorize_security_group_ingress(GroupName, IngressSpec, default_config()).

-spec(authorize_security_group_ingress/3 :: (string(), ec2_ingress_spec() | [ vpc_ingress_spec() ], aws_config()) -> ok | {error, any()}).
authorize_security_group_ingress(GroupName, IngressSpec, Config)
  when is_list(GroupName), is_record(IngressSpec, ec2_ingress_spec) ->
    Params = [{"GroupName", GroupName}|ingress_spec_params(IngressSpec)],
    ec2_simple_query(Config, "AuthorizeSecurityGroupIngress", Params);
authorize_security_group_ingress(GroupID, VPCIngressSpec, Config)
  when is_list(GroupID), is_list(VPCIngressSpec) ->
    Params = [{"GroupId", GroupID} | vpc_ingress_spec_to_params(VPCIngressSpec)],
    ec2_simple_query(Config, "AuthorizeSecurityGroupIngress", Params, ?NEW_API_VERSION).

ingress_spec_params(Spec) ->
    [
     {"IpProtocol", Spec#ec2_ingress_spec.ip_protocol},
     {"FromPort", Spec#ec2_ingress_spec.from_port},
     {"ToPort", Spec#ec2_ingress_spec.to_port},
     {"SourceSecurityGroupOwnerId", Spec#ec2_ingress_spec.source_security_group_owner_id},
     {"SourceSecurityGroupName", Spec#ec2_ingress_spec.source_security_group_name},
     {"CidrIp", Spec#ec2_ingress_spec.cidr_ip}
    ].

vpc_ingress_spec_to_params(Spec) ->
    vpc_ingress_spec_to_params(Spec, 1, []).

vpc_ingress_spec_to_params([], _, Res) -> Res;
vpc_ingress_spec_to_params([H|T], Count, Res) ->
    Prefix = lists:flatten(["IpPermissions", $., integer_to_list(Count), $.]),
    P1 = [ {lists:flatten([Prefix, "IpProtocol"]),
            H#vpc_ingress_spec.ip_protocol},
           {lists:flatten([Prefix, "FromPort"]), H#vpc_ingress_spec.from_port},
           {lists:flatten([Prefix, "ToPort"]), H#vpc_ingress_spec.to_port}
         ],
    UserP = vpc_ingress_details_to_params(
              H#vpc_ingress_spec.user_id, Count, "Groups", "UserId"),
    GNameP = vpc_ingress_details_to_params(
               H#vpc_ingress_spec.group_name, Count, "Groups", "GroupName"),
    GIdP = vpc_ingress_details_to_params(
             H#vpc_ingress_spec.group_id, Count, "Groups", "GroupId"),
    CidrP = vpc_ingress_details_to_params(
              H#vpc_ingress_spec.cidr_ip, Count, "IpRanges", "CidrIp"),
    vpc_ingress_spec_to_params(T, Count + 1, lists:flatten([P1, UserP, GNameP,
                                                            GIdP, CidrP, Res])).

vpc_ingress_details_to_params(Values, Count, Prefix, Suffix) ->
    vpc_ingress_details_to_params(Values, Count, Prefix, Suffix, 1, []).

vpc_ingress_details_to_params(undefined, _, _, _, _, _) -> [];
vpc_ingress_details_to_params([], _, _, _, _, Res) -> lists:flatten(Res);
vpc_ingress_details_to_params([H|T], Count, Prefix, Suffix, DetailCount, Res) ->
    Key = lists:flatten(["IpPermissions", $., integer_to_list(Count), $.,
                         Prefix, $., integer_to_list(DetailCount), $., Suffix]),
    Param = { Key, H },
    vpc_ingress_details_to_params(T, Count, Prefix, Suffix, DetailCount + 1,
                                  [ Param | Res ]).

%%
%%
-spec(bundle_instance/6 :: (string(), string(), string(), string(), string(), string()) -> {ok, proplist()} | {error, any()}).
bundle_instance(InstanceID, Bucket, Prefix, AccessKeyID, UploadPolicy,
                UploadPolicySignature) ->
    bundle_instance(InstanceID, Bucket, Prefix, AccessKeyID, UploadPolicy,
                    UploadPolicySignature, default_config()).

-spec(bundle_instance/7 :: (string(), string(), string(), string(), string(), string(), aws_config()) -> {ok, proplist()} | {error, any()}).
bundle_instance(InstanceID, Bucket, Prefix, AccessKeyID, UploadPolicy,
                UploadPolicySignature, Config) ->
    case ec2_query(Config, "BundleInstance",
                    [{"InstanceId", InstanceID}, {"Storage.S3.Bucket", Bucket},
                     {"Storage.S3.Prefix", Prefix}, {"Storage.S3.AWSAccessKeyId", AccessKeyID},
                     {"Storage.S3.UploadPolicy", UploadPolicy},
                     {"Storage.S3.UploadPolicySignature", UploadPolicySignature}]) of
        {ok, Doc} ->
            {ok, extract_bundle_task(xmerl_xpath:string("/BundleInstanceResponse/bundleInstanceTask", Doc))};
        {error, _} = Error ->
            Error
    end.

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

%%
%%
-spec(cancel_bundle_task/1 :: (string()) -> {ok, proplist()} | {error, any()}).
cancel_bundle_task(BundleID) ->
    cancel_bundle_task(BundleID, default_config()).

-spec(cancel_bundle_task/2 :: (string(), aws_config()) -> {ok, proplist()} | {error, any()}).
cancel_bundle_task(BundleID, Config)
  when is_list(BundleID) ->
    case ec2_query(Config, "CancelBundleTask", [{"BundleId", BundleID}]) of
        {ok, Doc} ->
            {ok, extract_bundle_task(xmerl_xpath:string("/CancelBundleTaskResponse/bundleInstanceTask", Doc))};
        {error, _} = Error ->
            Error
    end.

%%
%%
-spec(cancel_spot_instance_requests/1 :: ([string()]) -> {ok, [proplist()]} | {error, any()}).
cancel_spot_instance_requests(SpotInstanceRequestIDs) ->
    cancel_spot_instance_requests(SpotInstanceRequestIDs, default_config()).

-spec(cancel_spot_instance_requests/2 :: ([string()], aws_config()) -> {ok, [proplist()]} | {error, any()}).
cancel_spot_instance_requests(SpotInstanceRequestIDs, Config)
  when is_list(SpotInstanceRequestIDs) ->
    case ec2_query(Config, "CancelSpotInstanceRequests",
                    erlcloud_aws:param_list(SpotInstanceRequestIDs, "SpotInstanceRequestId")) of
        {ok, Doc} ->
            {ok, [extract_spot_instance_state(Item) ||
                Item <- xmerl_xpath:string("/CancelSpotInstanceRequestsResponse/spotInstanceRequestSet/item", Doc)]};
        {error, _} = Error ->
            Error
    end.

extract_spot_instance_state(Node) ->
    [
     {spot_instance_request_id, get_text("spotInstanceRequestId", Node)},
     {state, get_text("state", Node)}
    ].

%%
%%
-spec(confirm_product_instance/2 :: (string(), string()) -> {ok, proplist()} | {error, any()}).
confirm_product_instance(ProductCode, InstanceID) ->
    confirm_product_instance(ProductCode, InstanceID, default_config()).

-spec(confirm_product_instance/3 :: (string(), string(), aws_config()) -> {ok, proplist()} | {error, any()}).
confirm_product_instance(ProductCode, InstanceID, Config)
  when is_list(ProductCode), is_list(InstanceID) ->
    Params = [{"ProductCode", ProductCode}, {"InstanceId", InstanceID}],
    case ec2_query(Config, "ConfirmProductInstance", Params) of
        {ok, Doc} ->
            {ok, [
                {return, get_bool("/ConfirmProductInstanceResponse/return", Doc)},
                {owner_id, get_text("/ConfirmProductInstanceResponse/ownerId", Doc)}
            ]};
        {error, _} = Error ->
            Error
    end.

%%
%%
-spec(create_key_pair/1 :: (string()) -> {ok, proplist()} | {error, any()}).
create_key_pair(KeyName) -> create_key_pair(KeyName, default_config()).

-spec(create_key_pair/2 :: (string(), aws_config()) -> proplist()).
create_key_pair(KeyName, Config)
  when is_list(KeyName) ->
    case ec2_query(Config, "CreateKeyPair", [{"KeyName", KeyName}]) of
        {ok, Doc} ->
            {ok, [
                {key_name, get_text("/CreateKeyPairResponse/keyName", Doc)},
                {key_fingerprint, get_text("/CreateKeyPairResponse/keyFingerprint", Doc)},
                {key_material, get_text("/CreateKeyPairResponse/keyMaterial", Doc)}
            ]};
        {error, _} = Error ->
            Error
    end.

%%
%%
-spec(import_key_pair/2 :: (string(), string()) -> {ok, proplist()} | {error, any()}).
import_key_pair(KeyName, PublicKeyMaterial) -> import_key_pair(KeyName, PublicKeyMaterial, default_config()).

-spec(import_key_pair/3 :: (string(), string(), aws_config()) -> {ok, proplist()} | {error, any()}).
import_key_pair(KeyName, PublicKeyMaterial, Config)
  when is_record(Config, aws_config) ->
    Params = [{"KeyName", KeyName}, {"PublicKeyMaterial", base64:encode_to_string(PublicKeyMaterial)}],
    case ec2_query(Config, "ImportKeyPair", Params, ?NEW_API_VERSION) of
        {ok, Doc} ->
            {ok, [
                {key_fingerprint, get_text("/ImportKeyPairResponse/keyFingerprint", Doc)},
                {key_name, get_text("/ImportKeyPairResponse/keyName", Doc)}
            ]};
        {error, _} = Error ->
            Error
    end.

%%
%%
-spec(create_image/2 :: (string(), string()) -> {ok, proplist()} | {error, any()}).
create_image(InstanceID, Name) -> create_image(InstanceID, Name, default_config()).

-spec(create_image/3 :: (string(), string(), string() | aws_config()) -> {ok, proplist()} | {error, any()}).
create_image(InstanceID, Name, Config)
  when is_record(Config, aws_config) ->
    create_image(InstanceID, Name, none, Config);
create_image(InstanceID, Name, Description) ->
    create_image(InstanceID, Name, Description, default_config()).

-spec(create_image/4 :: (string(), string(), string() | none, boolean() | aws_config()) -> {ok, proplist()} | {error, any()}).
create_image(InstanceID, Name, Description, Config)
  when is_record(Config, aws_config) ->
    create_image(InstanceID, Name, Description, false, Config);
create_image(InstanceID, Name, Description, NoReboot) ->
    create_image(InstanceID, Name, Description, NoReboot, default_config()).

-spec(create_image/5 :: (string(), string(), string() | none, boolean(), aws_config()) -> {ok, proplist()} | {error, any()}).
create_image(InstanceID, Name, Description, NoReboot, Config)
  when is_list(InstanceID), is_list(Name),
       is_list(Description) orelse Description =:= none,
       is_boolean(NoReboot) ->
    Params = [{"InstanceId", InstanceID}, {"Name", Name},
              {"Description", Description}, {"NoReboot", NoReboot}],
    case ec2_query(Config, "CreateImage", Params) of
        {ok, Doc} ->
            {ok, [{image_id, get_text("/CreateImageResponse/imageId", Doc)}]};
        {error, _} = Error ->
            Error
    end.

%%
%%
-spec(create_internet_gateway/0 :: () -> {ok, proplist()} | {error, any()}).
create_internet_gateway() ->
    create_internet_gateway(default_config()).

-spec(create_internet_gateway/1 :: (aws_config()) -> proplist()).
create_internet_gateway(Config) ->
    case ec2_query(Config, "CreateInternetGateway", [], ?NEW_API_VERSION) of
        {ok, Doc} ->
            Path = "/CreateInternetGatewayResponse/internetGateway/internetGatewayId",
            {ok, [{internet_gateway_id, get_text(Path, Doc)}]};
        {error, _} = Error ->
            Error
    end.

%%
%%
-spec(create_network_acl/1 :: (string()) -> {ok, proplist()} | {error, any()}).
create_network_acl(VpcID) ->
    create_network_acl(VpcID, default_config()).

-spec(create_network_acl/2 :: (string(), aws_config()) -> {ok, proplist()} | {error, any()}).
create_network_acl(VpcID, Config) ->
    case ec2_query(Config, "CreateNetworkAcl", [{"VpcId", VpcID}], ?NEW_API_VERSION) of
        {ok, Doc} ->
            Path = "/CreateNetworkAclResponse/networkAcl",
            [Node] = xmerl_xpath:string(Path, Doc),
            {ok, extract_acl_response(Node)};
        {error, _} = Error ->
            Error
    end.

extract_acl_response(Node) ->
    [{network_acl_id, get_text("networkAclId", Node)},
     {vpc_id, get_text("vpcId", Node)},
     {default, get_text("default", Node)},
     {association_set, [ extract_acl_association_item(Item)
                         || Item <- xmerl_xpath:string("associationSet/item", Node)]},
     {entry_set, [ extract_acl_entry_item(Item)
                   || Item <- xmerl_xpath:string("entrySet/item", Node)]},
     {tag_set, 
      [extract_tag_item(Item)
       || Item <- xmerl_xpath:string("tagSet/item", Node)]}
].

extract_acl_association_item(Node) ->
    [{network_acl_association_id, get_text("networkAclAssociationId", Node)},
     {network_acl_id, get_text("networkAclId", Node)},
     {subnet_id, get_text("subnetId", Node)}].

extract_acl_entry_item(Node) ->
    [{rule_number, get_text("ruleNumber", Node)},
     {protocol, get_text("protocol", Node)},
     {rule_action, get_text("ruleAction", Node)},
     {egress, get_text("egress", Node)},
     {cidr_block, get_text("cidrBlock", Node)},
     {port_range, extract_port_range(xmerl_xpath:string("portRange", Node))}
    ].

extract_port_range([]) ->
    [];

extract_port_range([Node]) ->
    [{from, get_text("from", Node)},
     {to, get_text("to", Node)}
].

%%
%%
-spec(create_network_acl_entry/1 :: (ec2_network_acl_spec()) -> ok | {error, any()}).
create_network_acl_entry(Spec) ->
    create_network_acl_entry(Spec, default_config()).

-spec(create_network_acl_entry/2 :: (ec2_network_acl_spec(), aws_config()) -> ok | {error, any()}).
create_network_acl_entry(Spec, Config) ->
    Params = network_acl_spec_to_params(Spec),
    ec2_simple_query(Config, "CreateNetworkAclEntry", Params, ?NEW_API_VERSION).

network_acl_spec_to_params(Spec) ->
    [{ "NetworkAclId", Spec#ec2_network_acl_spec.network_acl_id },
     { "RuleNumber", Spec#ec2_network_acl_spec.rule_number },
     { "Protocol", Spec#ec2_network_acl_spec.protocol },
     { "RuleAction", Spec#ec2_network_acl_spec.rule_action },
     { "Egress", Spec#ec2_network_acl_spec.egress },
     { "CidrBlock", Spec#ec2_network_acl_spec.cidr_block },
     { "Icmp.Code", Spec#ec2_network_acl_spec.icmp_code },
     { "Icmp.Type", Spec#ec2_network_acl_spec.icmp_type },
     { "PortRange.From", Spec#ec2_network_acl_spec.port_range_from },
     { "PortRange.To", Spec#ec2_network_acl_spec.port_range_to }].

%%
%%
-spec(create_route/4 :: (string(), string(), gateway_id | instance_id | network_interface_id, string()) -> ok | {error, any()}).
create_route(RouteTableID, DestCidrBl, Attachment, Val) ->
    create_route(RouteTableID, DestCidrBl, Attachment, Val, default_config()).

-spec(create_route/5 :: (string(), string(), gateway_id | instance_id | network_interface_id, string(), aws_config()) -> ok | {error, any()}).
create_route(RouteTableID, DestCidrBl, Attachment, Val, Config) ->
    ASpec= case Attachment of
               gateway_id -> {"GatewayId", Val};
               instance_id -> {"InstanceId", Val};
               network_interface_id -> {"NetworkInterfaceId", Val};
               _ -> {}
           end,
    Params = [ASpec, {"RouteTableId", RouteTableID},
              {"DestinationCidrBlock", DestCidrBl}],
    ec2_simple_query(Config, "CreateRoute", Params, ?NEW_API_VERSION).


%%
%%
-spec(create_route_table/1 :: (string()) -> {ok, [proplist()]} | {error, any()}).
create_route_table(VpcID) ->
    create_route_table(VpcID, default_config()).

-spec(create_route_table/2 :: (string(), aws_config()) -> {ok, [proplist()]} | {error, any()}).
create_route_table(VpcID, Config) ->
    case ec2_query(Config, "CreateRouteTable", [{"VpcId", VpcID}], ?NEW_API_VERSION) of
        {ok, Doc} ->
            Path = "/CreateRouteTableResponse/routeTable",
            {ok, [extract_route(RT) || RT <- xmerl_xpath:string(Path, Doc)]};
        {error, _} = Error ->
            Error
    end.


%%
%%
-spec(create_subnet/2 :: (string(), string()) -> {ok, proplist()} | {error, any()}).
create_subnet(VpcID, CIDR) when is_list(VpcID), is_list(CIDR) ->
    create_subnet(VpcID, CIDR, none, default_config()).

-spec(create_subnet/3 :: (string(), string(), string() | aws_config()) -> {ok, proplist()} | {error, any()}).
create_subnet(VpcID, CIDR, Config) when is_record(Config, aws_config) ->
    create_subnet(VpcID, CIDR, none, Config);
create_subnet(VpcID, CIDR, Zone) when is_list(Zone) ->
    create_subnet(VpcID, CIDR, Zone, default_config()).

-spec(create_subnet/4 :: (string(), string(), string() | none, aws_config()) -> {ok, proplist()} | {error, any()}).
create_subnet(VpcID, CIDR, Zone, Config) when
      is_list(VpcID), is_list(CIDR), is_list(Zone) orelse Zone =:= none ->
    Params = [{"VpcId", VpcID}, {"CidrBlock", CIDR},
              {"AvailabilityZone", Zone}],
    case ec2_query(Config, "CreateSubnet", Params) of
        {ok, Doc} ->
            Node = hd(xmerl_xpath:string("/CreateSubnetResponse/subnet", Doc)),
            {ok, [extract_subnet(Node)]};
        {error, _} = Error ->
            Error
    end.

%%
%%
-spec(create_security_group/2 :: (string(), string()) -> ok | {error, any()}).
create_security_group(GroupName, GroupDescription) ->
    create_security_group(GroupName, GroupDescription, none, default_config()).

-spec(create_security_group/3 :: (string(), string(), aws_config() | string() | none) -> ok | {error, any()}).
create_security_group(GroupName, GroupDescription, Config)
  when is_record(Config, aws_config) ->
    create_security_group(GroupName, GroupDescription, none, Config);
create_security_group(GroupName, GroupDescription, VpcID) ->
    create_security_group(GroupName, GroupDescription, VpcID, default_config()).

-spec(create_security_group/4 :: (string(), string(), string() | none, aws_config()) -> ok | {error, any()}).
create_security_group(GroupName, GroupDescription, VpcID, Config)
  when is_list(GroupName), is_list(GroupDescription) ->
    case ec2_query(Config, "CreateSecurityGroup",
                    [{"GroupName", GroupName}, {"GroupDescription", GroupDescription},
                     {"VpcId", VpcID}], ?NEW_API_VERSION) of
        {ok, Doc} ->
            case VpcID of
                none -> ok;
                _    -> {ok, get_text("/CreateSecurityGroupResponse/groupId", Doc)}
            end;
        {error, _} = Error ->
            Error
    end.

%%
%%
-spec(create_snapshot/1 :: (string()) -> {ok, proplist()} | {error, any()}).
create_snapshot(VolumeID) ->
    create_snapshot(VolumeID, "", default_config()).

-spec(create_snapshot/2 :: (string(), string()) -> proplist() ; (string(), aws_config()) -> {ok, proplist()} | {error, any()}).
create_snapshot(VolumeID, Config)
  when is_record(Config, aws_config) ->
    create_snapshot(VolumeID, "", Config);
create_snapshot(VolumeID, Description) ->
    create_snapshot(VolumeID, Description, default_config()).

-spec(create_snapshot/3 :: (string(), string(), aws_config()) -> {ok, proplist()} | {error, any()}).
create_snapshot(VolumeID, Description, Config)
  when is_list(VolumeID), is_list(Description) ->
    case ec2_query(Config, "CreateSnapshot", [{"VolumeId", VolumeID}, {"Description", Description}]) of
        {ok, Doc} -> 
            {ok, [
                 {snapshot_id, get_text("/CreateSnapshotResponse/snapshotId", Doc)},
                 {volume_id, get_text("/CreateSnapshotResponse/volumeId", Doc)},
                 {volume_size, get_integer("/CreateSnapshotResponse/volumeSize", Doc)},
                 {status, get_text("/CreateSnapshotResponse/status", Doc)},
                 {start_time, erlcloud_xml:get_time("/CreateSnapshotResponse/attachTime", Doc)},
                 {progress, get_text("/CreateSnapshotResponse/progress", Doc)},
                 {owner_id, get_text("/CreateSnapshotResponse/ownerId", Doc)},
                 {description, get_text("/CreateSnapshotResponse/description", Doc)}
            ]};
        {error, _} = Error ->
            Error
    end.

%%
%%
-spec(create_spot_datafeed_subscription/1 :: (string()) -> {ok, proplist()} | {error, any()}).
create_spot_datafeed_subscription(Bucket) ->
    create_spot_datafeed_subscription(Bucket, none).

-spec(create_spot_datafeed_subscription/2 :: (string(), string() | none | aws_config()) -> {ok, proplist()} | {error, any()}).
create_spot_datafeed_subscription(Bucket, Config)
  when is_record(Config, aws_config) ->
    create_spot_datafeed_subscription(Bucket, none, Config);
create_spot_datafeed_subscription(Bucket, Prefix) ->
    create_spot_datafeed_subscription(Bucket, Prefix, default_config()).

-spec(create_spot_datafeed_subscription/3 :: (string(), string() | none, aws_config()) -> {ok, proplist()} | {error, any()}).
create_spot_datafeed_subscription(Bucket, Prefix, Config)
  when is_list(Bucket),
       is_list(Prefix) orelse Prefix =:= none ->
    case ec2_query(Config, "CreateSpotDatafeedSubscription", [{"Bucket", Bucket}, {Prefix, Prefix}]) of
        {ok, Doc} ->
            {ok, extract_spot_datafeed_subscription(xmerl_xpath:string("/CreateSpotDatafeedSubscriptionResponse/spotDatafeedSubscription", Doc))};
        {error, _} = Error ->
            Error
    end.

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

%%
%%
-spec(create_volume/3 :: (ec2_volume_size(), string(), string()) -> {ok, proplist()} | {error, any()}).
create_volume(Size, SnapshotID, AvailabilityZone) ->
    create_volume(Size, SnapshotID, AvailabilityZone, default_config()).

-spec(create_volume/4 :: (ec2_volume_size(), string(), string(), aws_config()) -> {ok, proplist()} | {error, any()}).
create_volume(Size, SnapshotID, AvailabilityZone, Config)
  when Size >= 1, Size =< 1024,
       is_list(SnapshotID) orelse SnapshotID =:= none,
       is_list(AvailabilityZone) ->
    Params = [
              {"Size", integer_to_list(Size)},
              {"AvailabilityZone", AvailabilityZone},
              {"SnapshotId", SnapshotID}
             ],
    case ec2_query(Config, "CreateVolume", Params) of
        {ok, Doc} ->
            {ok, [
                {volume_id, get_text("volumeId", Doc)},
                {size, get_integer("size", Doc)},
                {snapshot_id, get_text("snapshotId", Doc, none)},
                {availability_zone, get_text("availabilityZone", Doc, none)},
                {status, get_text("status", Doc, none)},
                {create_time, erlcloud_xml:get_time("createTime", Doc)}
            ]};
        {error, _} = Error ->
            Error
    end.

%%
%%
-spec(create_vpc/1 :: (string()) -> {ok, proplist()} | {error, any()}).
create_vpc(CIDR) ->
    create_vpc(CIDR, none, default_config()).

-spec(create_vpc/2 :: (string(), string() | none | aws_config()) -> {ok, proplist()} | {error, any()}).
create_vpc(CIDR, InsTen) when is_list(InsTen) ->
    create_vpc(CIDR, InsTen, default_config());
create_vpc(CIDR, Config) when is_record(Config, aws_config) ->
    create_vpc(CIDR, none, Config).

-spec(create_vpc/3 :: (string(), string() | none, aws_config()) -> {ok, proplist()} | {error, any()}).
create_vpc(CIDR, InsTen, Config) when
      is_list(CIDR), is_list(InsTen) orelse InsTen =:= none ->
    case ec2_query(Config, "CreateVpc", [{"CidrBlock", CIDR}, {"instanceTenancy", InsTen}]) of
        {ok, Doc} ->
            {ok, [
                {vpc_id, get_text("/CreateVpcResponse/vpc/vpcId", Doc)},
                {state, get_text("/CreateVpcResponse/vpc/state", Doc)},
                {cidr_block, get_text("/CreateVpcResponse/vpc/cidrBlock", Doc)},
                {dhcp_options_id, get_text("/CreateVpcResponse/vpc/dhcpOptionsId", Doc)}
            ]};
        {error, _} = Error ->
            Error
    end.

%%
%%
-spec(delete_internet_gateway/1 :: (string()) -> ok | {error, any()}).
delete_internet_gateway(GatewayID) ->
    delete_internet_gateway(GatewayID, default_config()).

-spec(delete_internet_gateway/2 :: (string(), aws_config()) -> ok | {error, any()}).
delete_internet_gateway(GatewayID, Config) ->
    ec2_simple_query(Config, "DeleteInternetGateway",
                     [{"InternetGatewayId", GatewayID}], ?NEW_API_VERSION).

%%
%%
-spec(delete_key_pair/1 :: (string()) -> ok | {error, any()}).
delete_key_pair(KeyName) -> delete_key_pair(KeyName, default_config()).

-spec(delete_key_pair/2 :: (string(), aws_config()) -> ok | {error, any()}).
delete_key_pair(KeyName, Config)
  when is_list(KeyName) ->
    ec2_simple_query(Config, "DeleteKeyPair", [{"KeyName", KeyName}]).

%%
%%
-spec(delete_network_acl/1 :: (string()) -> ok | {error, any()}).
delete_network_acl(NetworkAclId) ->
    delete_network_acl(NetworkAclId, default_config()).

-spec(delete_network_acl/2 :: (string(), aws_config()) -> ok | {error, any()}).
delete_network_acl(NetworkAclId, Config) ->
    ec2_simple_query(Config, "DeleteNetworkAcl",
                     [{"NetworkAclId", NetworkAclId}], ?NEW_API_VERSION).

%%
%%
-spec(delete_network_acl_entry/2 :: (string(), string()) -> ok | {error, any()}).
delete_network_acl_entry(NetworkAclID, RuleNumber) ->
    delete_network_acl_entry(NetworkAclID, RuleNumber, false, default_config()).

-spec(delete_network_acl_entry/3 :: (string(), string(), boolean() | aws_config()) -> ok | {error, any()}).
delete_network_acl_entry(NetworkAclID, RuleNumber, Config)
  when is_record(Config, aws_config) ->
    delete_network_acl_entry(NetworkAclID, RuleNumber, false, Config);
delete_network_acl_entry(NetworkAclID, RuleNumber, Egress) ->
    delete_network_acl_entry(NetworkAclID, RuleNumber, Egress, default_config()).

-spec(delete_network_acl_entry/4 :: (string(), string(), boolean(), aws_config()) -> ok | {error, any()}).
delete_network_acl_entry(NetworkAclID, RuleNumber, Egress, Config) ->
    Params = [{"NetworkAclId", NetworkAclID},
              {"RuleNumber", RuleNumber},
              {"Egress", Egress}],
    ec2_simple_query(Config, "DeleteNetworkAclEntry", Params, ?NEW_API_VERSION).

%%
%%
-spec(delete_route/2 :: (string(), string()) -> ok | {error, any()}).
delete_route(RouteTableID, DestCidrBlock) ->
    delete_route(RouteTableID, DestCidrBlock, default_config()).

-spec(delete_route/3 :: (string(), string(), aws_config()) -> ok | {error, any()}).
delete_route(RouteTableID, DestCidrBlock, Config) ->
    Params = [{"RouteTableId", RouteTableID},
              {"DestinationCidrBlock", DestCidrBlock}],
    ec2_simple_query(Config, "DeleteRoute", Params, ?NEW_API_VERSION).

%%
%%
-spec(delete_route_table/1 :: (string()) -> ok | {error, any()}).
delete_route_table(RouteTableID) ->
    delete_route_table(RouteTableID, default_config()).

-spec(delete_route_table/2 :: (string(), aws_config()) -> ok | {error, any()}).
delete_route_table(RouteTableID, Config) ->
    ec2_simple_query(Config, "DeleteRouteTable",
                     [{"RouteTableId", RouteTableID}], ?NEW_API_VERSION).

%%
%%
-spec(delete_security_group/1 :: (string()) -> ok | {error, any()}).
delete_security_group(GroupName) ->
    delete_security_group(groupName, GroupName, default_config()).

-spec(delete_security_group/2 :: (groupId | groupName | string(), string() | aws_config()) -> ok | {error, any()}).
delete_security_group(GroupName, Config)
  when is_list(GroupName), is_record(Config, aws_config) ->
    delete_security_group(groupName, GroupName, Config);
delete_security_group(Param, GroupName)
  when is_atom(Param), is_list(GroupName) ->
    delete_security_group(Param, GroupName, default_config()).

-spec(delete_security_group/3 :: (groupId | groupName, string(), aws_config()) -> ok | {error, any()}).
delete_security_group(Param, GroupName, Config) ->
    ParamStr = atom_to_list(Param),
    Key = [string:to_upper(hd(ParamStr)) | tl(ParamStr)],
    ec2_simple_query(Config, "DeleteSecurityGroup", [{Key, GroupName}], ?NEW_API_VERSION).

%%
%%    
-spec(delete_snapshot/1 :: (string()) -> ok | {error, any()}).
delete_snapshot(SnapshotID) -> delete_snapshot(SnapshotID, default_config()).

-spec(delete_snapshot/2 :: (string(), aws_config()) -> ok | {error, any()}).
delete_snapshot(SnapshotID, Config)
  when is_list(SnapshotID) ->
    ec2_simple_query(Config, "DeleteSnapshot", [{"SnapshotId", SnapshotID}]).

%%
%%
-spec(delete_spot_datafeed_subscription/0 :: () -> ok | {error, any()}).
delete_spot_datafeed_subscription() -> delete_spot_datafeed_subscription(default_config()).

-spec(delete_spot_datafeed_subscription/1 :: (aws_config()) -> ok | {error, any()}).
delete_spot_datafeed_subscription(Config) ->
    ec2_simple_query(Config, "DeleteSpotDatafeedSubscription", []).

%%
%%
-spec(delete_subnet/1 :: (string()) -> ok | {error, any()}).
delete_subnet(SubnetID) when is_list(SubnetID) ->
    delete_subnet(SubnetID, default_config()).

-spec(delete_subnet/2 :: (string(), aws_config()) -> ok | {error, any()}).
delete_subnet(SubnetID, Config) when is_list(SubnetID) ->
    ec2_simple_query(Config, "DeleteSubnet", [{"SubnetId", SubnetID}]).

%%
%%
-spec(delete_volume/1 :: (string()) -> ok | {error, any()}).
delete_volume(VolumeID) -> delete_volume(VolumeID, default_config()).

-spec(delete_volume/2 :: (string(), aws_config()) -> ok | {error, any()}).
delete_volume(VolumeID, Config)
  when is_list(VolumeID) ->
    ec2_simple_query(Config, "DeleteVolume", [{"VolumeId", VolumeID}]).

%%
%%
-spec(delete_vpc/1 :: (string()) -> ok | {error, any()}).
delete_vpc(ID) ->
    delete_vpc(ID, default_config()).

-spec(delete_vpc/2 :: (string(), aws_config()) -> ok | {error, any()}).
delete_vpc(ID, Config) ->
    ec2_simple_query(Config, "DeleteVpc", [{"VpcId", ID}]).

%%
%%
-spec(deregister_image/1 :: (string()) -> ok | {error, any()}).
deregister_image(ImageID) -> deregister_image(ImageID, default_config()).

-spec(deregister_image/2 :: (string(), aws_config()) -> ok | {error, any()}).
deregister_image(ImageID, Config)
  when is_list(ImageID) ->
    ec2_simple_query(Config, "DeregisterImage", [{"ImageId", ImageID}]).

%%
%%
-spec(describe_addresses/0 :: () -> {ok, proplist()} | {error, any()}).
describe_addresses() -> describe_addresses([]).

-spec(describe_addresses/1 :: ([string()] | aws_config()) -> {ok, proplist()} | {error, any()}).
describe_addresses(Config)
  when is_record(Config, aws_config) ->
    describe_addresses([], Config);
describe_addresses(PublicIPs) -> describe_addresses(PublicIPs, default_config()).

-spec(describe_addresses/2 :: ([string()], aws_config()) -> {ok, proplist()} | {error, any()}).
describe_addresses(PublicIPs, Config)
  when is_list(PublicIPs) ->
    case ec2_query(Config, "DescribeAddresses", erlcloud_aws:param_list(PublicIPs, "PublicIp")) of
        {ok, Doc} ->
            Items = xmerl_xpath:string("/DescribeAddressesResponse/addressesSet/item", Doc),
            {ok, [[{public_ip, get_text("publicIp", Item)}, {instance_id, get_text("instanceId", Item, none)}] || Item <- Items]};
        {error, _} = Error ->
            Error
    end.

%%
%%
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
    case ec2_query(Config, "DescribeAvailabilityZones", erlcloud_aws:param_list(ZoneNames, "ZoneName")) of
        {ok, Doc} ->
            Items = xmerl_xpath:string("/DescribeAvailabilityZonesResponse/availabilityZoneInfo/item", Doc),
            {ok, [[{zone_name, get_text("zoneName", Item)},
              {zone_state, get_text("zoneState", Item)},
              {region_name, get_text("regionName", Item)},
              {messages, get_list("messageSet/item/message", Item)}
             ] || Item <- Items]};
        {error, _} = Error ->
            Error
    end.

%%
%%
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
    case ec2_query(Config, "DescribeBundleTasks", erlcloud_aws:param_list(BundleIDs, "BundleId")) of
        {ok, Doc} ->
            {ok, [extract_bundle_task(Item) || Item <- xmerl_xpath:string("/DescribeBundleTasksResponse/bundleInstanceTasksSet/item", Doc)]};
        {error, _} = Error ->
            Error
    end.

%%
%%
-spec(describe_dhcp_options/0 :: () -> proplist()).
describe_dhcp_options() ->
    describe_dhcp_options(none, default_config()).

-spec(describe_dhcp_options/1 :: (aws_config() | filter_list() | none) -> proplist()).
describe_dhcp_options(Config) when is_record(Config, aws_config) ->
    describe_dhcp_options(none, Config);
describe_dhcp_options(Filter) when is_list(Filter) ->
    describe_dhcp_options(Filter, default_config()).

-spec(describe_dhcp_options/2 :: (none | filter_list(), aws_config()) -> proplist()).
describe_dhcp_options(Filter, Config) ->
    Params = list_to_ec2_filter(Filter),
    case ec2_query(Config, "DescribeDhcpOptions", Params, ?NEW_API_VERSION) of
        {ok,  Doc} ->
            Path = "/DescribeDhcpOptionsResponse/dhcpOptionsSet/item",
            {ok, [ extract_dhcp_opts(Item) || Item <- xmerl_xpath:string(Path, Doc) ]};
        {error, _} = Error ->
            Error
    end.

extract_dhcp_opts(Node) ->
    Path = "dhcpConfigurationSet/item",
    [ {dhcp_options_id, get_text("dhcpOptionsId", Node)},
      {configuration_set, [ extract_dhcp_opt_kv(Item) ||
                              Item <- xmerl_xpath:string(Path, Node) ]} ].
extract_dhcp_opt_kv(Node) ->
    [ {key, get_text("key", Node)},
      {value, get_text("valueSet/item/value", Node)} ].

%%
%%
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
    case ec2_query(Config, "DescribeImageAttribute", [{"ImageId", ImageID}, {"Attribute", AttributeName}]) of
        {ok, Doc} ->
            case Attribute of
                launch_permission ->
                    {ok, extract_permissions(xmerl_xpath:string("/DescribeImageAttributeResponse/launchPermission/item", Doc))};
                product_codes ->
                    {ok, get_list("/DescribeImageAttributeResponse/productCodes/item/productCode", Doc)};
                block_device_mapping ->
                    {ok, [extract_block_device_mapping(Node) || Node <- xmerl_xpath:string("/DescribeImageAttributeResponse/blockDeviceMapping/item", Doc)]}
            end;
        {error, _} = Error ->
            Error
    end.

extract_permissions(Nodes) ->
    extract_permissions(Nodes, []).

extract_permissions([], Accum) ->
    lists:reverse(Accum);
extract_permissions([Node|Nodes], Accum) ->
    case {erlcloud_xml:get_text("userId", Node), erlcloud_xml:get_text("groupId", Node)} of
        {[], []} -> extract_permissions(Nodes, Accum);
        {UserId, []} -> extract_permissions(Nodes, [{user_id, UserId}|Accum]);
        {[], GroupId} -> extract_permissions(Nodes, [{groupId, GroupId}|Accum])
    end.

%%
%%
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
       is_list(ExecutableBy) orelse ExecutableBy =:= none,
       is_record(Config, aws_config) ->
    describe_images(ImageIDs, Owner, ExecutableBy, none, Config).

-spec(describe_images/5 :: ([string()], 
                            string() | none, 
                            string() | none, 
                            filter_list() | none, 
                            aws_config()) -> proplist()).
describe_images(ImageIDs, Owner, ExecutableBy, Filters, Config)
  when is_list(ImageIDs),
       is_list(Owner) orelse Owner =:= none,
       is_list(ExecutableBy) orelse ExecutableBy =:= none,
       is_list(Filters) orelse Filters =:= none,
       is_record(Config, aws_config)->
    Params = [
              {"ExecutableBy", ExecutableBy}, {"Owner", Owner} |
              erlcloud_aws:param_list(ImageIDs, "ImageId")
             ] ++ list_to_ec2_filter(Filters),
    case ec2_query(Config, "DescribeImages", Params, ?NEW_API_VERSION) of
        {ok, Doc} ->
            {ok, [extract_image(Item) || Item <- xmerl_xpath:string("/DescribeImagesResponse/imagesSet/item", Doc)]};
        {error, _} = Error ->
            Error
    end.

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
     {platform, get_text("platform", Node)},
     {block_device_mapping, [extract_block_device_mapping(Item) || Item <- xmerl_xpath:string("blockDeviceMapping/item", Node)]},
     {product_codes, [extract_product_code(Item) || Item <- xmerl_xpath:string("productCodes/item", Node)]}
    ].

extract_block_device_mapping(Node) ->
    #ec2_block_device_mapping{
                              device_name=get_text("deviceName", Node),
                              virtual_name=get_text("virtualName", Node),
                              snapshot_id=get_text("ebs/snapshotId", Node, none),
                              volume_size=list_to_integer(get_text("ebs/volumeSize", Node, "0")),
                              delete_on_termination=get_bool("ebs/deleteOnTermination", Node)
                             }.

extract_product_code(Node) ->
    [
        {product_code, get_text("productCode", Node)},
        {type, get_text("type", Node)}
    ].

%%
%%
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
    case ec2_query(Config, "DescribeInstanceAttribute", [{"InstanceId", InstanceID}, {"Attribute", AttributeName}]) of
        {ok, Doc} ->
            case xmerl_xpath:string(attribute_xpath(Attribute, AttributeName), Doc) of
                                                        % attribute might not be defined
                [] ->
                    undefined;
                [Node | _] ->
                    case Attribute of
                        user_data -> base64:decode(get_text(Node));
                        disable_api_termination -> list_to_existing_atom(get_text(Node));
                        instance_initiated_shutdown_behavior -> list_to_existing_atom(get_text(Node));
                        block_device_mapping ->
                            [extract_block_device_mapping_status(Item) || Item <- xmerl_xpath:string("item", Node)];
                        _ -> get_text(Node)
                    end
            end;
        {error, _} = Error ->
            Error
    end.

attribute_xpath(block_device_mapping, AttributeName) ->
    "/DescribeInstanceAttributeResponse/" ++ AttributeName;
attribute_xpath(_, AttributeName) ->
    "/DescribeInstanceAttributeResponse/" ++ AttributeName ++ "/value".


%%
%%
-spec(describe_instances/0 :: () -> proplist()).
describe_instances() -> describe_instances([]).

-spec(describe_instances/1 :: ([string()] | aws_config()) -> proplist()).
describe_instances(Config)
  when is_record(Config, aws_config) ->
    describe_instances([], Config);
describe_instances(InstanceIDs) ->
    describe_instances(InstanceIDs, [], default_config()).

-spec(describe_instances/2 :: ([string()], filter_list() | aws_config()) -> proplist()).
describe_instances(InstanceIDs, Config)
  when is_record(Config, aws_config) ->
    describe_instances(InstanceIDs, [], Config);
describe_instances(InstanceIDs, Filter) ->
    describe_instances(InstanceIDs, Filter, default_config()).

-spec(describe_instances/3 :: ([string()], filter_list(), aws_config()) -> proplist()).
describe_instances(InstanceIDs, Filter, Config)
  when is_list(InstanceIDs) ->
    Params = erlcloud_aws:param_list(InstanceIDs, "InstanceId") ++ list_to_ec2_filter(Filter),
    case ec2_query(Config, "DescribeInstances", Params, ?NEW_API_VERSION) of
        {ok, Doc} ->
            Reservations = xmerl_xpath:string("/DescribeInstancesResponse/reservationSet/item", Doc),
            {ok, [extract_reservation(Item) || Item <- Reservations]};
        {error, Reason} ->
            {error, Reason}
    end.

extract_reservation(Node) ->
    [{reservation_id, get_text("reservationId", Node)},
     {owner_id, get_text("ownerId", Node)},
     %% {group_set, get_list("groupSet/item/groupId", Node)},
     {instances_set, [extract_instance(Item) || Item <- xmerl_xpath:string("instancesSet/item", Node)]}
    ].

extract_instance(Node) ->
    [{instance_id, get_text("instanceId", Node)},
     {group_set, [extract_group(Item) || Item <- xmerl_xpath:string("groupSet/item", Node)]},
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
     {spot_instance_request_id, get_text("spotInstanceRequestId", Node, none)},
     {iam_instance_profile, [
                             {arn, get_text("iamInstanceProfile/arn", Node)},
                             {id, get_text("iamInstanceProfile/id", Node)}
                            ]},
     {tag_set, 
      [extract_tag_item(Item)
       || Item <- xmerl_xpath:string("tagSet/item", Node)]},
     {network_interface_set, [extract_network_interface(Item) || Item <- xmerl_xpath:string("networkInterfaceSet/item", Node)]}
    ].

extract_group(Node) ->
     [{group_id, get_text("groupId", Node)},
      {group_name, get_text("groupName", Node)}
     ].

extract_block_device_mapping_status(Node) ->
    [
     {device_name, get_text("deviceName", Node)},
     {volume_id, get_text("ebs/volumeId", Node)},
     {status, get_text("ebs/status", Node)},
     {attach_time, erlcloud_xml:get_time("ebs/attachTime", Node)},
     {delete_on_termination, get_bool("ebs/deleteOnTermination", Node)}
    ].

%%
%%
-spec(describe_instance_status/1 :: (string()) -> proplist()).
describe_instance_status(InstanceID) ->
    describe_instance_status([{"InstanceId", InstanceID}], [], default_config()).

-spec(describe_instance_status/2 :: (proplist(), filter_list()) -> proplist()).
describe_instance_status(Params, Filter) ->
    describe_instance_status(Params, Filter, default_config()).

-spec(describe_instance_status/3 :: (proplist(), filter_list(), aws_config()) -> proplist()).
describe_instance_status(Params, Filter, Config) ->
    AllParams = Params ++ list_to_ec2_filter(Filter),
    case ec2_query(Config, "DescribeInstanceStatus", AllParams, ?NEW_API_VERSION) of
        {ok, Doc} ->
            Path = "/DescribeInstanceStatusResponse/instanceStatusSet/item",
            {ok, [ extract_instance_status(Item) || Item <- xmerl_xpath:string(Path, Doc) ]};
        {error, _} = Error ->
            Error
    end.

extract_instance_status(Node) ->
    %% XXX: abbreviated.
    [ { instance_id, get_text("instanceId", Node) },
      { availability_zone, get_text("availabilityZone", Node) },
      { instance_state_code, get_text("instanceState/code", Node) },
      { instance_state_name, get_text("instanceState/name", Node) } ].

%%
%%
-spec(describe_internet_gateways/0 :: () -> proplist()).
describe_internet_gateways() ->
    describe_internet_gateways(none, default_config()).

-spec(describe_internet_gateways/1 :: (filter_list | aws_config()) -> proplist()).
describe_internet_gateways(Config) when is_record(Config, aws_config) ->
    describe_internet_gateways(none, Config);
describe_internet_gateways(Filter) ->
    describe_internet_gateways(Filter, default_config()).

-spec(describe_internet_gateways/2 :: (none | filter_list(), aws_config()) -> [proplist()]).
describe_internet_gateways(Filter, Config) ->
    Params = list_to_ec2_filter(Filter),
    case ec2_query(Config, "DescribeInternetGateways", Params, ?NEW_API_VERSION) of
        {ok, Doc} ->
            ResultPath = "/DescribeInternetGatewaysResponse/internetGatewaySet/item",
            {ok, [ extract_igw(Item) || Item <- xmerl_xpath:string(ResultPath, Doc) ]};
        {error, _} = Error ->
            Error
    end.

extract_igw(Node) ->
    Items = xmerl_xpath:string("attachmentSet/item", Node),
    [ {internet_gateway_id, get_text("internetGatewayId", Node)},
      {attachment_set, [ extract_igw_attachments(Item) || Item <- Items]},
      {tag_set, 
       [extract_tag_item(Item)
        || Item <- xmerl_xpath:string("tagSet/item", Node)]}
 ].

extract_igw_attachments(Node) ->
    [ {vpc_id, get_text("vpcId", Node)},
      {state, get_text("state", Node)} ].

%%
%%
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
    case ec2_query(Config, "DescribeKeyPairs", erlcloud_aws:param_list(KeyNames, "KeyName")) of
        {ok, Doc} ->
            Items = xmerl_xpath:string("/DescribeKeyPairsResponse/keySet/item", Doc),
            {ok, [
             [
              {key_name, get_text("keyName", Item)},
              {key_fingerprint, get_text("keyFingerprint", Item)}
             ] || Item <- Items
            ]};
        {error, _} = Error ->
            Error
    end.

%%
%%
-spec(describe_network_acls/0 :: () -> [proplist()]).
describe_network_acls() ->
    describe_network_acls(none, default_config()).

-spec(describe_network_acls/1 :: (filter_list() | aws_config()) -> [proplist()]).
describe_network_acls(Config) when is_record(Config, aws_config) ->
    describe_network_acls(none, Config);
describe_network_acls(Filter) ->
    describe_network_acls(Filter, default_config()).

-spec(describe_network_acls/2 :: (filter_list(), aws_config()) -> [proplist()]).
describe_network_acls(Filter, Config) ->
    Params = list_to_ec2_filter(Filter),
    case ec2_query(Config, "DescribeNetworkAcls", Params, ?NEW_API_VERSION) of
        {ok, Doc} ->
            Path = "/DescribeNetworkAclsResponse/networkAclSet/item",
            {ok, [extract_acl_response(Item) || Item <- xmerl_xpath:string(Path, Doc)]};
        {error, _} = Error ->
            Error
    end.

%%
%%
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
    case ec2_query(Config, "DescribeRegions", erlcloud_aws:param_list(RegionNames, "RegionName")) of
        {ok, Doc} ->
            Items = xmerl_xpath:string("/DescribeRegionsResponse/regionInfo/item", Doc),
            {ok, [[{region_name, get_text("regionName", Item)},
              {region_endpoint, get_text("regionEndpoint", Item)}
             ] || Item <- Items]};
        {error, _} = Error ->
            Error
    end.

%
% Network Interfaces API
%
-spec(describe_network_interfaces/0 :: () -> [proplist()]).
describe_network_interfaces() ->
    describe_network_interfaces(none).

-spec(describe_network_interfaces/1 :: (list() | aws_config()) -> [proplist()]).
describe_network_interfaces(Config)
    when is_record(Config, aws_config) ->
      describe_network_interfaces([], Config);
describe_network_interfaces(NetworkInterfacesIds) ->
    describe_network_interfaces(NetworkInterfacesIds, default_config()).

-spec(describe_network_interfaces/2 :: (list(), aws_config()) -> [proplist()]).
%%
%%
%% Example: describe_network_interfaces(["eni-1c111111", "eni-222e2222"], Config).
describe_network_interfaces(NetworkInterfacesIds, Config)
    when is_record(Config, aws_config) ->
        describe_network_interfaces_filtered(NetworkInterfacesIds, none, Config).

-spec(describe_network_interfaces_filtered/3 :: (list(), filter_list() | node, aws_config()) -> [proplist()]).
%%
%% Example: describe_network_interfaces_filtered([], [{"subnet-id", ["subnet-e11e11e1"]}], Config)
%%
describe_network_interfaces_filtered(NetworkInterfacesIds, Filter, Config) 
    when is_record(Config, aws_config) ->
       Params = lists:append(erlcloud_aws:param_list(NetworkInterfacesIds, "NetworkInterfaceId") ,
                              list_to_ec2_filter(Filter)),
       case ec2_query(Config, "DescribeNetworkInterfaces", Params, ?NEW_API_VERSION) of
          {ok, Doc} ->
              NetworkInterfaces = xmerl_xpath:string("/DescribeNetworkInterfacesResponse/networkInterfaceSet/item", Doc),
              {ok, [extract_network_interface(Item) || Item <- NetworkInterfaces]};
          {error, _} = Error ->
              Error
       end.

-spec(extract_network_interface/1 :: (Node::list()) -> proplist()).
extract_network_interface(Node) ->
    [
     {network_interface_id, get_text("networkInterfaceId", Node)},
     {subnet_id, get_text("subnetId", Node)},
     {vpc_id, get_text("vpcId", Node)},
     {availability_zone, get_text("availabilityZone", Node)},
     {description, get_text("description", Node)},
     {owner_id, get_text("ownerId", Node)},
     {requester_managed, get_bool("requesterManaged", Node)},
     {status, get_text("status", Node)},
     {mac_address, get_text("macAddress", Node)},
     {private_ip_address, get_text("privateIpAddress", Node)},
     {source_dest_check, get_bool("sourceDestCheck", Node)},
     {groups_set, [extract_group(Item) || Item <- xmerl_xpath:string("groupSet/item", Node)]},
     {attachment, extract_attachment(Node)},
     {association, extract_association(Node)},
     {tag_set, [extract_tag_item(Item) || Item <- xmerl_xpath:string("tagSet/item", Node)]},
     {private_ip_addresses_set, 
            [extract_private_ip_address(Item) || Item <- xmerl_xpath:string("privateIpAddressesSet/item", Node)]}
    ].

-spec(extract_attachment/1 :: (Node::list()) -> proplist()).
extract_attachment(Node) ->
    [
     {attachment_id, get_text("attachment/attachmentId", Node)},
     {instance_id, get_text("attachment/instanceId", Node)},
     {instance_owner_id, get_text("attachment/instanceOwnerId", Node)},
     {device_index, get_text("attachment/deviceIndex", Node)},
     {status, get_text("attachment/status", Node)},
     {attach_time, erlcloud_xml:get_time("attachment/attachTime", Node)},
     {delete_on_termination, get_bool("attachment/deleteOnTermination", Node)}
    ].

-spec(extract_private_ip_address/1 :: (Node::list()) -> proplist()).
extract_private_ip_address(Node) ->
    [
     {private_ip_address, get_text("privateIpAddress", Node)},
     {primary, get_bool("primary", Node)}
    ]. 

-spec(extract_association/1 :: (Node::list()) -> proplist()).
extract_association(Node) ->
    [
     {public_ip, get_text("association/publicIp", Node)},
     {public_dns_name, get_text("association/publicDnsName", Node)},
     {ip_owner_id, get_text("association/ipOwnerId", Node)},
     {allocation_id, get_text("association/allocationId", Node)},
     {association_id, get_text("association/associationId", Node)}
    ].

%%
%%
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
    case ec2_query(Config, "DescribeReservedInstances", erlcloud_aws:param_list(ReservedInstanceIDs, "ReservedInstanceId")) of
        {ok, Doc} ->
            ReservedInstances = xmerl_xpath:string("/DescribeReservedInstancesResponse/reservedInstancesSet/item", Doc),
            {ok, [extract_reserved_instance(Item) || Item <- ReservedInstances]};
        {error, _} = Error ->
            Error
    end.

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

%%
%%
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
    case ec2_query(Config, "DescribeReservedInstancesOfferings", Params) of
        {ok, Doc} ->
            {ok, [extract_reserved_instances_offering(Node) ||
                Node <- xmerl_xpath:string("/DescribeReservedInstancesOfferingsResponse/reservedInstancesOfferingsSet/item", Doc)]};
        {error, _} = Error ->
            Error
    end.

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

%%
%%
-spec(describe_route_tables/0 :: () -> [proplist()]).
describe_route_tables() ->
    describe_route_tables(none, default_config()).

-spec(describe_route_tables/1 :: (filter_list() | none | aws_config()) -> [proplist()]).
describe_route_tables(Config) when is_record(Config, aws_config) ->
    describe_route_tables(none, Config);
describe_route_tables(Filter) ->
    describe_route_tables(Filter, default_config()).

-spec(describe_route_tables/2 :: (filter_list() | none, aws_config()) -> [proplist()]).
describe_route_tables(Filter, Config) ->
    Params = list_to_ec2_filter(Filter),
    case ec2_query(Config, "DescribeRouteTables", Params, ?NEW_API_VERSION) of
        {ok, Doc} ->
            Path = "/DescribeRouteTablesResponse/routeTableSet/item",
            {ok, [extract_route(Item) || Item <- xmerl_xpath:string(Path, Doc)]};
        {error, _} = Error ->
            Error
    end.

extract_route(Node) ->
    [
     {route_table_id, get_text("routeTableId", Node)},
     {vpc_id, get_text("vpcId", Node)},
     {route_set, [extract_route_set(Item) ||
                     Item <- xmerl_xpath:string("routeSet/item", Node)]},
     {association_set,
      [extract_route_assn(Item)
       || Item <-xmerl_xpath:string("associationSet/item", Node)]},
     {tag_set, 
      [extract_tag_item(Item)
       || Item <- xmerl_xpath:string("tagSet/item", Node)]}
    ].

extract_route_set(Node) ->
    [
     {destination_cidr_block, get_text("destinationCidrBlock", Node)},
     {gateway_id, get_text("gatewayId", Node)},
     {instance_id, get_text("instanceId", Node)},
     {state, get_text("state", Node)},
     {origin, get_text("origin", Node)}
    ].

extract_route_assn(Node) ->
    [
     {route_table_association_id, get_text("routeTableAssociationId", Node)},
     {route_table_id, get_text("routeTableId", Node)},
     {main, get_text("main", Node)},
     {subnet_id, get_text("subnetId", Node)}
    ].

%%
%%
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
    case ec2_query(Config, "DescribeSecurityGroups", erlcloud_aws:param_list(GroupNames, "GroupName"), ?NEW_API_VERSION) of
        {ok, Doc} ->
            {ok, [extract_security_group(Node) ||
                Node <- xmerl_xpath:string("/DescribeSecurityGroupsResponse/securityGroupInfo/item", Doc)]};
        {error, _} = Error ->
            Error
    end.

%%
%%
-spec(describe_security_groups_filtered/1 :: (filter_list()) -> [proplist()]).
describe_security_groups_filtered(Filter) ->
    describe_security_groups_filtered(Filter, default_config()).

-spec(describe_security_groups_filtered/2 :: (filter_list(), aws_config()) -> [proplist()]).
describe_security_groups_filtered(Filter, Config)->
    Params = list_to_ec2_filter(Filter),
    case ec2_query(Config, "DescribeSecurityGroups", Params, ?NEW_API_VERSION) of
        {ok, Doc} ->
            Path = "/DescribeSecurityGroupsResponse/securityGroupInfo/item",
            {ok, [extract_security_group(Node) || Node <- xmerl_xpath:string(Path, Doc)]};
        {error, _} = Error ->
            Error
    end.

extract_security_group(Node) ->
    [
     {owner_id, get_text("ownerId", Node)},
     {group_id, get_text("groupId", Node)},
     {group_name, get_text("groupName", Node)},
     {group_description, get_text("groupDescription", Node)},
     {vpc_id, get_text("vpcId", Node)},
     {ip_permissions,
      [extract_ip_permissions(Item) || Item <- xmerl_xpath:string("ipPermissions/item", Node)]},
     {ip_permissions_egress,
      [extract_ip_permissions(Item) || Item <- xmerl_xpath:string("ipPermissionsEgress/item", Node)]},
     {tag_set, 
      [extract_tag_item(Item)
       || Item <- xmerl_xpath:string("tagSet/item", Node)]}
    ].

extract_ip_permissions(Node) ->
    [
     {ip_protocol, list_to_atom(get_text("ipProtocol", Node))},
     {from_port, get_integer("fromPort", Node)},
     {to_port, get_integer("toPort", Node)},
     {users, get_list("groups/item/userId", Node)},
     {groups,
      [extract_user_id_group_pair(Item) || Item <- xmerl_xpath:string("groups/item", Node)]},
     {ip_ranges, get_list("ipRanges/item/cidrIp", Node)}
    ].

extract_user_id_group_pair(Node) ->
    [
      {user_id, get_text("userId", Node)},
      {group_id, get_text("groupId", Node)},
      {group_name, get_text("groupName", Node)}
    ].
%%
%%
-spec(describe_snapshot_attribute/2 :: (string(), atom()) -> proplist()).
describe_snapshot_attribute(SnapshotID, Attribute) ->
    describe_snapshot_attribute(SnapshotID, Attribute, default_config()).

-spec(describe_snapshot_attribute/3 :: (string(), atom(), aws_config()) -> term()).
describe_snapshot_attribute(SnapshotID, create_volume_permission, Config)
  when is_list(SnapshotID) ->
    case ec2_query(Config, "DescribeSnapshotAttribute", [{"snapshotId", SnapshotID}, {"Attribute", "createVolumePermission"}]) of
        {ok, Doc} ->
            {ok, extract_permissions(xmerl_xpath:string("/DescribeSnapshotAttributeResponse/createVolumePermission/item", Doc))};
        {error, _} = Error ->
            Error
    end.

%%
%%
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
    case ec2_query(Config, "DescribeSnapshots", Params) of
        {ok, Doc} ->
            {ok, [extract_snapshot(Item) || Item <- xmerl_xpath:string("/DescribeSnapshotsResponse/snapshotSet/item", Doc)]};
        {error, _} = Error ->
            Error
    end.

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

%%
%%
-spec(describe_spot_datafeed_subscription/0 :: () -> proplist()).
describe_spot_datafeed_subscription() ->
    describe_spot_datafeed_subscription(default_config()).

-spec(describe_spot_datafeed_subscription/1 :: (aws_config()) -> proplist()).
describe_spot_datafeed_subscription(Config) ->
    case ec2_query(Config, "DescribeSpotDatafeedSubscription", []) of
        {ok, Doc} ->
            {ok, extract_spot_datafeed_subscription(xmerl_xpath:string("/DescribeSpotDatafeedSubscriptionResponse/spotDatafeedSubscription", Doc))};
        {error, _} = Error ->
            Error
    end.

%%
%%
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
    case ec2_query(Config, "DescribeSpotInstanceRequests", erlcloud_aws:param_list(SpotInstanceRequestIDs, "SpotInstanceRequestId"), ?NEW_API_VERSION) of
        {ok, Doc} ->
            {ok, [extract_spot_instance_request(Item) ||
                    Item <- xmerl_xpath:string("/DescribeSpotInstanceRequestsResponse/spotInstanceRequestSet/item", Doc)]};
        {error, Reason} -> 
            {error, Reason}
    end.

extract_spot_instance_request(Node) ->
    [
        {spot_instance_request_id, get_text("spotInstanceRequestId", Node)},
        {spot_price, get_text("spotPrice", Node)},
        {type, extract_spot_instance_request_type(get_text("type", Node))},
        {state, get_text("state", Node)},
        {status, get_text("status/code", Node)},
        {fault, [
            {code, get_text("fault/code", Node)},
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

%%
%%
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
    case ec2_query(Config, "DescribeSpotPriceHistory",
                    [{"StartTime", StartTime}, {"EndTime", EndTime},
                     {"ProductDescription", ProductDescription}|
                     erlcloud_aws:param_list(InstanceTypes, "InstanceType")]) of
        {ok, Doc} ->
            {ok, [extract_spot_price_history(Item) ||
                    Item <- xmerl_xpath:string("/DescribeSpotPriceHistoryResponse/spotPriceHistorySet/item", Doc)]};
        {error, _} = Error ->
            Error
    end.

extract_spot_price_history(Node) ->
    [
     {instance_type, get_text("instanceType", Node)},
     {product_description, get_text("productDescription", Node)},
     {spot_price, get_text("spotPrice", Node)},
     {timestamp, erlcloud_xml:get_time("timestamp", Node)}
    ].

%%
%%
-spec(describe_subnets/0 :: () -> proplist()).
describe_subnets() ->
    describe_subnets(none, default_config()).

-spec(describe_subnets/1 :: ([none | filter_list() | string()] | aws_config()) -> proplist()).
describe_subnets(Config) when is_record(Config, aws_config) ->
    describe_subnets(none, Config);
describe_subnets(Filter) when is_list(Filter) ->
    describe_subnets(Filter, default_config()).

-spec(describe_subnets/2 :: (none | filter_list(), aws_config()) -> proplist()).
describe_subnets(Filter, Config) ->
    Params = list_to_ec2_filter(Filter),
    case ec2_query(Config, "DescribeSubnets", Params, ?NEW_API_VERSION) of
        {ok, Doc} ->
            Subnets = xmerl_xpath:string("/DescribeSubnetsResponse/subnetSet/item", Doc),
            {ok, [extract_subnet(Item) || Item <- Subnets]};
        {error, _} = Error ->
            Error
    end.

extract_subnet(Node) ->
    [{subnet_id, get_text("subnetId", Node)},
     {state, get_text("state", Node)},
     {vpc_id, get_text("vpcId", Node)},
     {available_ip_address_count, get_text("availableIpAddressCount", Node)},
     {availability_zone, get_text("availabilityZone", Node)},
     {cidr_block, get_text("cidrBlock", Node)},
     {tag_set, 
      [extract_tag_item(Item)
       || Item <- xmerl_xpath:string("tagSet/item", Node)]}
].

%%
%%
-spec(describe_volumes/0 :: () -> {ok, proplist()} | {error, any()}).
describe_volumes() -> describe_volumes([]).

-spec(describe_volumes/1 :: ([string()] | aws_config()) -> {ok, proplist()} | {error, any()}).
describe_volumes(Config)
  when is_record(Config, aws_config) ->
    describe_volumes([], Config);
describe_volumes(VolumeIDs) ->
    describe_volumes(VolumeIDs, default_config()).

-spec(describe_volumes/2 :: ([string()], aws_config()) -> {ok, proplist()} | {error, any()}).
describe_volumes(VolumeIDs, Config)
  when is_list(VolumeIDs) ->
    case ec2_query(Config, "DescribeVolumes", erlcloud_aws:param_list(VolumeIDs, "VolumeId")) of
        {ok, Doc} ->
            {ok, [extract_volume(Item) || Item <- xmerl_xpath:string("/DescribeVolumesResponse/volumeSet/item", Doc)]};
        {error, Reason} ->
            {error, Reason}
    end.

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

%%
%%
-spec(describe_vpcs/0 :: () -> proplist()).
describe_vpcs() ->
    describe_vpcs(default_config()).

-spec(describe_vpcs/1 :: (filter_list() | aws_config()) -> proplist()).
describe_vpcs(Config) when is_record(Config, aws_config) ->
    describe_vpcs(none, Config);
describe_vpcs(Filter) ->
    describe_vpcs(Filter, default_config()).

-spec(describe_vpcs/2 :: (filter_list() | none, aws_config()) -> proplist()).
describe_vpcs(Filter, Config) ->
    Params = list_to_ec2_filter(Filter),
    case ec2_query(Config, "DescribeVpcs", Params, ?NEW_API_VERSION) of
        {ok, Doc} ->
            Items = xmerl_xpath:string("/DescribeVpcsResponse/vpcSet/item", Doc),
            {ok, [ extract_vpc(Item) || Item <- Items ]};
        {error, _} = Error ->
            Error
    end.

extract_vpc(Node) ->
    [ {vpc_id, get_text("vpcId", Node)},
      {state, get_text("state", Node)},
      {cidr_block, get_text("cidrBlock", Node)},
      {dhcp_options_id, get_text("dhcpOptionsId", Node)},
      {instance_tenancy, get_text("instanceTenancy", Node)},
      {is_default, get_bool("isDefault", Node)},
      {tag_set, 
        [extract_tag_item(Item)
         || Item <- xmerl_xpath:string("tagSet/item", Node)]}
 ].

%%
%%
-spec(detach_internet_gateway/2 :: (string(), string()) -> ok).
detach_internet_gateway(GatewayID, VpcID) ->
    detach_internet_gateway(GatewayID, VpcID, default_config()).

-spec(detach_internet_gateway/3 :: (string(), string(), aws_config()) -> ok).
detach_internet_gateway(GatewayID, VpcID, Config) ->
    ec2_simple_query(Config, "DetachInternetGateway",
                     [{"InternetGatewayId", GatewayID}, {"VpcId", VpcID}],
                     ?NEW_API_VERSION).

%%
%%
-spec(detach_volume/1 :: (string()) -> proplist()).
detach_volume(VolumeID) -> detach_volume(VolumeID, default_config()).

-spec(detach_volume/2 :: (string(), aws_config()) -> proplist()).
detach_volume(VolumeID, Config)
  when is_list(VolumeID) ->
    Params = [{"VolumeId", VolumeID}],
    case ec2_query(Config, "DetachVolume", Params) of
        {ok, Doc} ->
            {ok, extract_volume_status(hd(xmerl_xpath:string("/DetachVolumeResponse", Doc)))};
        {error, _} = Error ->
            Error
    end.

%%
%%
-spec(disassociate_address/1 :: (string()) -> ok).
disassociate_address(PublicIP) ->
    disassociate_address(PublicIP, default_config()).

-spec(disassociate_address/2 :: (string(), aws_config()) -> ok).
disassociate_address(PublicIP, Config)
  when is_list(PublicIP) ->
    ec2_simple_query(Config, "DisassociateAddress", [{"PublicIp", PublicIP}]).

%%
%%
-spec(get_console_output/1 :: (string()) -> proplist()).
get_console_output(InstanceID) -> get_console_output(InstanceID, default_config()).

-spec(get_console_output/2 :: (string(), aws_config()) -> proplist()).
get_console_output(InstanceID, Config)
  when is_list(InstanceID) ->
    case ec2_query(Config, "GetConsoleOutput", [{"InstanceId", InstanceID}]) of
        {ok, Doc} ->
            {ok, [
                {instance_id, get_text("/GetConsoleOutputResponse/instanceId", Doc)},
                {timestamp, erlcloud_xml:get_time("/GetConsoleOutputResponse/timestamp", Doc)},
                {output, base64:decode(get_text("/GetConsoleOutputResponse/output", Doc))}
            ]};
        {error, _} = Error ->
            Error
    end.

%%
%%
-spec(get_password_data/1 :: (string()) -> proplist()).
get_password_data(InstanceID) -> get_password_data(InstanceID, default_config()).

-spec(get_password_data/2 :: (string(), aws_config()) -> proplist()).
get_password_data(InstanceID, Config)
  when is_list(InstanceID) ->
    case ec2_query(Config, "GetPasswordData", [{"InstanceId", InstanceID}]) of
        {ok, Doc} ->
            {ok, [
                {instance_id, get_text("/GetPasswordDataResponse/instanceId", Doc)},
                {timestamp, erlcloud_xml:get_time("/GetPasswordDataResponse/timestamp", Doc)},
                {password_data, get_text("/GetPasswordDataResponse/passwordData", Doc)}
            ]};
        {error, _} = Error ->
            Error
    end.

%%
%%
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

%%
%%
-spec(modify_instance_attribute/3 :: (string(), atom(), term()) -> ok).
modify_instance_attribute(InstanceID, Attribute, Value) ->
    modify_instance_attribute(InstanceID, Attribute, Value, default_config()).

-spec(modify_instance_attribute/4 :: (newstyle | string(), string()|atom(), string()|atom()|term(), string()|aws_config()) -> ok). %%FIXME
modify_instance_attribute(newstyle, InstanceID, Attribute, Value) ->
    modify_instance_attribute(newstyle, InstanceID, Attribute, Value, default_config());
modify_instance_attribute(InstanceID, Attribute, Value, Config) ->
    {AttributeName, AParams} =
        case Attribute of
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

-spec(modify_instance_attribute/5 :: (newstyle, string(), string(), string(), aws_config()) -> ok).
modify_instance_attribute(newstyle, InstanceID, Attribute, Value, Config) -> %%FIXME
    ec2_simple_query(Config, "ModifyInstanceAttribute",
                     [{"InstanceId", InstanceID}, {Attribute, Value}], ?NEW_API_VERSION).

permission_list(Permissions) ->
    UserIDs = [UserID || {user_id, UserID} <- Permissions],
    Groups = [Group || {group, Group} <- Permissions],
    erlcloud_aws:param_list(UserIDs, "UserId") ++ erlcloud_aws:param_list(Groups, "Group").

%%
%%
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

%%
%%
-spec(monitor_instances/1 :: ([string()]) -> proplist()).
monitor_instances(InstanceIDs) ->
    monitor_instances(InstanceIDs, default_config()).

-spec(monitor_instances/2 :: ([string()], aws_config()) -> [proplist()]).
monitor_instances(InstanceIDs, Config) ->
    case ec2_query(Config, "MonitorInstances", erlcloud_aws:param_list(InstanceIDs, "InstanceId")) of
        {ok, Doc} ->
            {ok, [extract_monitor_state(Node) || Node <- xmerl_xpath:string("/MonitorInstancesResponse/instancesSet/item", Doc)]};
        {error, _} = Error ->
            Error
    end.

extract_monitor_state(Node) ->
    [
     {instance_id, get_text("instanceId", Node)},
     {monitoring_state, get_text("monitoring/state", Node)}
    ].

%%
%%
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
    case ec2_query(Config, "PurchaseReservedInstancesOffering", Params) of
        {ok, Doc} ->
            {ok, get_list("/PurchaseReservedInstancesOfferingResponse/reservedInstancesId", Doc)};
        {error, _} = Error ->
            Error
    end.

%%
%%
-spec(reboot_instances/1 :: ([string()]) -> ok).
reboot_instances(InstanceIDs) -> reboot_instances(InstanceIDs, default_config()).

-spec(reboot_instances/2 :: ([string()], aws_config()) -> ok).
reboot_instances(InstanceIDs, Config)
  when is_list(InstanceIDs) ->
    ec2_simple_query(Config, "RebootInstances", erlcloud_aws:param_list(InstanceIDs, "InstanceId")).

%%
%%
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
    case ec2_query(Config, "RegisterImage", Params ++ BDParams) of
        {ok, Doc} ->
            {ok, [{image_id, get_text("/CreateImageResponse/imageId", Doc)}]};
        {error, _} = Error ->
            Error
    end.

%%
%%
-spec(release_address/1 :: (string()) -> ok).
release_address(PublicIP) -> release_address(PublicIP, default_config()).

%%
%%
-spec(replace_network_acl_association/2 :: (string(), string()) -> string()).
replace_network_acl_association(AssociationID, NetworkAclID) ->
    replace_network_acl_association(AssociationID, NetworkAclID, default_config()).

-spec(replace_network_acl_association/3 :: (string(), string(), aws_config()) -> string()).
replace_network_acl_association(AssociationID, NetworkAclID, Config) ->
    Params = [{"AssociationId", AssociationID},
              {"NetworkAclId", NetworkAclID}],
    case ec2_query(Config, "ReplaceNetworkAclAssociation", Params, ?NEW_API_VERSION) of
        {ok, Doc} ->
            Path = "/ReplaceNetworkAclAssociationResponse/newAssociationId",
            {ok, get_text(Path, Doc)};
        {error, _} = Error ->
            Error
    end.

%%
%%
-spec(replace_network_acl_entry/1 :: (ec2_network_acl_spec()) -> ok).
replace_network_acl_entry(Spec) ->
    replace_network_acl_entry(Spec, default_config()).

-spec(replace_network_acl_entry/2 :: (ec2_network_acl_spec(), aws_config()) -> ok).
replace_network_acl_entry(Spec, Config) ->
    Params = network_acl_spec_to_params(Spec),
    ec2_simple_query(Config, "ReplaceNetworkAclEntry", Params, ?NEW_API_VERSION).

%%
%%
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
              {"LaunchSpecification.RamdiskId", InstanceSpec#ec2_instance_spec.ramdisk_id},
              {"LaunchSpecification.Monitoring.Enabled", InstanceSpec#ec2_instance_spec.monitoring_enabled},
              {"LaunchSpecification.Placement.AvailabilityZone", InstanceSpec#ec2_instance_spec.availability_zone},
              {"LaunchSpecification.Placement.GroupName", InstanceSpec#ec2_instance_spec.placement_group},
              {"LaunchSpecification.EbsOptimized", InstanceSpec#ec2_instance_spec.ebs_optimized},
              {"LaunchSpecification.IamInstanceProfile.Name", InstanceSpec#ec2_instance_spec.iam_instance_profile_name}
             ],
    NetParams = case InstanceSpec#ec2_instance_spec.net_if of
        [] ->
            [
                {"LaunchSpecification.SubnetId", InstanceSpec#ec2_instance_spec.subnet_id}
            ] ++ erlcloud_aws:param_list(InstanceSpec#ec2_instance_spec.group_set, "LaunchSpecification.SecurityGroup");
        List      ->
            net_if_params(List, "LaunchSpecification.NetworkInterface")
    end,         
    BDParams = [
                {"LaunchSpecification." ++ Key, Value} ||
                   {Key, Value} <- block_device_params(InstanceSpec#ec2_instance_spec.block_device_mapping)],

    case ec2_query(Config, "RequestSpotInstances", Params ++ BDParams ++ NetParams, ?NEW_API_VERSION) of
        {ok, Doc} ->
            {ok, [extract_spot_instance_request(Item) ||
                    Item <- xmerl_xpath:string("/RequestSpotInstancesResponse/spotInstanceRequestSet/item", Doc)]};
        {error, Reason} ->
            {error, Reason}
    end.

%%
%%
-spec(release_address/2 :: (string(), aws_config()) -> ok).
release_address(PublicIP, Config)
  when is_list(PublicIP) ->
    ec2_simple_query(Config, "ReleaseAddress", [{"PublicIp", PublicIP}]).

%%
%%
-spec(reset_image_attribute/2 :: (string(), atom()) -> ok).
reset_image_attribute(ImageID, Attribute) ->
    reset_image_attribute(ImageID, Attribute, default_config()).

-spec(reset_image_attribute/3 :: (string(), atom(), aws_config()) -> ok).
reset_image_attribute(ImageID, launch_permission, Config) ->
    ec2_simple_query(Config, "ResetImageAttribute",
                     [{"ImageId", ImageID}, {"Attribute", "launchPermission"}]).

%%
%%
-spec(reset_instance_attribute/2 :: (string(), atom()) -> ok).
reset_instance_attribute(InstanceID, Attribute) ->
    reset_instance_attribute(InstanceID, Attribute, default_config()).

-spec(reset_instance_attribute/3 :: (string(), atom(), aws_config()) -> ok).
reset_instance_attribute(InstanceID, Attribute, Config)
  when is_list(InstanceID),
       Attribute =:= kernel orelse Attribute =:= ramdisk ->
    ec2_simple_query(Config, "ResetInstanceAttribute",
                     [{"InstanceId", InstanceID}, {"Attribute", Attribute}]).

%%
%%
-spec(reset_snapshot_attribute/2 :: (string(), atom()) -> ok).
reset_snapshot_attribute(SnapshotID, Attribute) ->
    reset_snapshot_attribute(SnapshotID, Attribute, default_config()).

-spec(reset_snapshot_attribute/3 :: (string(), atom(), aws_config()) -> ok).
reset_snapshot_attribute(SnapshotID, create_volume_permission, Config)
  when is_list(SnapshotID) ->
    ec2_simple_query(Config, "ResetSnapshotAttribute",
                     [{"snapshotId", SnapshotID}, {"Attribute", "createVolumePermission"}]).

%%
%%
-spec(revoke_security_group_ingress/2 :: (string(), ec2_ingress_spec()) -> ok).
revoke_security_group_ingress(GroupName, IngressSpec) ->
    revoke_security_group_ingress(GroupName, IngressSpec, default_config()).

-spec(revoke_security_group_ingress/3 :: (string(), ec2_ingress_spec(), aws_config()) -> ok).
revoke_security_group_ingress(GroupName, IngressSpec, Config)
  when is_list(GroupName), is_record(IngressSpec, ec2_ingress_spec) ->
    Params = [{"GroupName", GroupName}|ingress_spec_params(IngressSpec)],
    ec2_simple_query(Config, "RevokeSecurityGroupIngress", Params).

%%
%%
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
              {"RamdiskId", InstanceSpec#ec2_instance_spec.ramdisk_id},
              {"Monitoring.Enabled", InstanceSpec#ec2_instance_spec.monitoring_enabled},
              {"Placement.AvailabilityZone", InstanceSpec#ec2_instance_spec.availability_zone},
              {"Placement.GroupName", InstanceSpec#ec2_instance_spec.placement_group},
              {"DisableApiTermination", InstanceSpec#ec2_instance_spec.disable_api_termination},
              {"InstanceInitiatedShutdownBehavior", InstanceSpec#ec2_instance_spec.instance_initiated_shutdown_behavior},
              {"EbsOptimized", InstanceSpec#ec2_instance_spec.ebs_optimized},
              {"IamInstanceProfile.Name", InstanceSpec#ec2_instance_spec.iam_instance_profile_name}
             ],
    NetParams = case InstanceSpec#ec2_instance_spec.net_if of
        [] ->
            [
                {"SubnetId", InstanceSpec#ec2_instance_spec.subnet_id} 
            ] ++ erlcloud_aws:param_list(InstanceSpec#ec2_instance_spec.group_set, "SecurityGroupId");
        List      ->
            net_if_params(List, "NetworkInterface")
    end,         
    BDParams  = block_device_params(InstanceSpec#ec2_instance_spec.block_device_mapping),
    case ec2_query(Config, "RunInstances", Params ++ NetParams ++ BDParams, ?NEW_API_VERSION) of
        {ok, Doc} ->
            {ok, extract_reservation(hd(xmerl_xpath:string("/RunInstancesResponse", Doc)))};
        {error, _} = Error ->
            Error
    end.

net_if_params(#ec2_net_if{private_ip=undefined}=X) ->
    [
        {"DeviceIndex", X#ec2_net_if.device_index},
        {"SubnetId",    X#ec2_net_if.subnet_id},
        {"AssociatePublicIpAddress", X#ec2_net_if.associate_public_ip}
    ] ++ erlcloud_aws:param_list(X#ec2_net_if.security_group, "SecurityGroupId");
net_if_params(#ec2_net_if{}=X) ->
    [
        {"DeviceIndex", X#ec2_net_if.device_index},
        {"SubnetId",    X#ec2_net_if.subnet_id},
        {"AssociatePublicIpAddress", X#ec2_net_if.associate_public_ip},
        {"PrivateIpAddress", hd(X#ec2_net_if.private_ip)}
    ] ++ erlcloud_aws:param_list(
        [ [{"PrivateIpAddress", IP}] || IP <- tl(X#ec2_net_if.private_ip)], "PrivateIpAddresses"
    ) ++ erlcloud_aws:param_list(X#ec2_net_if.security_group, "SecurityGroupId").
net_if_params(List, Prefix) ->
    erlcloud_aws:param_list([net_if_params(X) || X <- List], Prefix).

%%
%%
-spec(create_tags/2 :: ([string()], [{string(), string()}]) -> proplist()).
create_tags(ResourceIds, TagsList) when is_list(ResourceIds) ->
    create_tags(ResourceIds, TagsList, default_config()).

-spec(create_tags/3 :: ([string()], [{string(), string()}], aws_config()) -> proplist()).
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
    case ec2_query(Config, "CreateTags", Resources ++ Tags, ?NEW_API_VERSION) of
        {ok, Doc} ->
            {ok, [{return, get_text("/CreateTagsResponse/return", Doc)}]};
        {error, _} = Error ->
            Error
    end.

%%
%%
-spec(delete_tags/2 :: ([string()], [{string(), string()}]) -> proplist()).
delete_tags(ResourceIds, TagsList) when is_list(ResourceIds) ->
    delete_tags(ResourceIds, TagsList, default_config()).

-spec(delete_tags/3 :: ([string()], [{string(), string()}], aws_config()) -> proplist()).
delete_tags(ResourceIds, TagsList, Config) when is_list(ResourceIds)->
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
    case ec2_query(Config, "DeleteTags", Resources ++ Tags, ?NEW_API_VERSION) of
        {ok, Doc} ->
            {ok, [{return, get_text("/DeleteTagsResponse/return", Doc)}]};
        {error, _} = Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @see describe_tags/2.
%% @end
%%------------------------------------------------------------------------------
%% describe_tags follows new API patterns. It returning {ok, _} or {error, _}
%% and by using records instead of property lists.
-type filter_name() :: key | resource_id | resource_type | value.
-type filter() :: {filter_name(), [string()]}.
-spec describe_tags() -> {ok, [#ec2_tag{}]} | {error, tuple()}.
describe_tags() ->
    describe_tags([], default_config()).

%%------------------------------------------------------------------------------
%% @doc
%% @see describe_tags/2.
%% @end
%%------------------------------------------------------------------------------
-spec describe_tags([filter()] | aws_config()) -> {ok, [#ec2_tag{}]} | {error, tuple()}.
describe_tags(#aws_config{} = Config) ->
    describe_tags([], Config);
describe_tags(Filters) ->
    describe_tags(Filters, default_config()).

%%------------------------------------------------------------------------------
%% @doc EC2 API:
%% [http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeTags.html]
%%
%% ===Example===
%% Get "Tag1" and "Tag2" for a specific instance
%%
%% <code>
%% {ok, Tags} = erlcloud_ec2:describe_tags([{resource_id, [InstanceId]}, {key, ["Tag1", "Tag2"]}], Config), <br/>
%% Tag1 = lists:keyfind("Tag1", #ec2_tag.key, Tags), <br/>
%% Tag1Value = Tag1#ec2_tag.value,
%% </code>
%% @end
%%------------------------------------------------------------------------------
-spec describe_tags([filter()], aws_config()) -> {ok, [#ec2_tag{}]} | {error, tuple()}.
describe_tags(Filters, Config) ->
    {Params, _} =
        lists:foldl(
          fun({Name, Values}, {Acc, Index}) ->
                  I = integer_to_list(Index),
                  Key = "Filter."++I++".Name",
                  Prefix = "Filter."++I++".Value.",
                  {value_list_params(Values, Prefix) ++ [{Key, filter_name(Name)} | Acc], Index + 1}
          end, {[], 1}, Filters),

    case ec2_query(Config, "DescribeTags", Params, "2012-12-01") of
        {ok, Doc} ->
            Tags = xmerl_xpath:string("/DescribeTagsResponse/tagSet/item", Doc),
            {ok, [extract_tag(Tag) || Tag <- Tags]};
        {error, Reason} ->
            {error, Reason}
    end.

-spec filter_name(filter_name()) -> string().
filter_name(key) -> "key";
filter_name(resource_id) -> "resource-id";
filter_name(resource_type) -> "resource-type";
filter_name(value) -> "value".

-spec value_list_params([string()], string()) -> [{string(), string()}].
value_list_params(Values, Prefix) ->
    {Params, _} = lists:foldl(fun(Value, {Acc, Index}) ->
                                      I = integer_to_list(Index),
                                      Key = Prefix ++ I,
                                      {[{Key, Value} | Acc], Index + 1}
                              end, {[], 1}, Values),
    Params.

-spec extract_tag(term()) -> #ec2_tag{}.
extract_tag(Node) ->
    #ec2_tag{
             resource_id = get_text("resourceId", Node),
             resource_type = get_text("resourceType", Node),
             key = get_text("key", Node),
             value = get_text("value", Node)}.

extract_tag_item(Node) ->
    [
     {key, get_text("key", Node)},
     {value, get_text("value", Node)}
    ].

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

%%
%%
-spec(start_instances/1 :: ([string()]) -> proplist()).
start_instances(InstanceIDs) -> start_instances(InstanceIDs, default_config()).

-spec(start_instances/2 :: ([string()], aws_config()) -> proplist()).
start_instances(InstanceIDs, Config)
  when is_list(InstanceIDs) ->
    case ec2_query(Config, "StartInstances", erlcloud_aws:param_list(InstanceIDs, "InstanceId")) of
        {ok, Doc} ->
            {ok, [extract_instance_state_change(Node) || Node <- xmerl_xpath:string("/StartInstancesResponse/instancesSet/item", Doc)]};
        {error, _} = Error ->
            Error
    end.

%%
%%
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
    case ec2_query(Config, "StopInstances",
                    [{"Force", atom_to_list(Force)}|erlcloud_aws:param_list(InstanceIDs, "InstanceId")]) of
        {ok, Doc} ->
            {ok, [extract_instance_state_change(Node) || Node <- xmerl_xpath:string("/StopInstancesResponse/instancesSet/item", Doc)]};
        {error, _} = Error ->
            Error
    end.

%%
%%
-spec(terminate_instances/1 :: ([string()]) -> proplist()).
terminate_instances(InstanceIDs) -> terminate_instances(InstanceIDs, default_config()).

-spec(terminate_instances/2 :: ([string()], aws_config()) -> proplist()).
terminate_instances(InstanceIDs, Config)
  when is_list(InstanceIDs) ->
    case ec2_query(Config, "TerminateInstances", erlcloud_aws:param_list(InstanceIDs, "InstanceId")) of
        {ok,  Doc} ->
            {ok, [extract_instance_state_change(Node) || Node <- xmerl_xpath:string("/TerminateInstancesResponse/instancesSet/item", Doc)]};
        {error, _} = Error ->
            Error
    end.

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

%%
%%
-spec(unmonitor_instances/1 :: ([string()]) -> proplist()).
unmonitor_instances(InstanceIDs) ->
    unmonitor_instances(InstanceIDs, default_config()).

-spec(unmonitor_instances/2 :: ([string()], aws_config()) -> [proplist()]).
unmonitor_instances(InstanceIDs, Config) ->
    case ec2_query(Config, "UnmonitorInstances", erlcloud_aws:param_list(InstanceIDs, "InstanceId")) of
        {ok, Doc} ->
            {ok, [extract_monitor_state(Node) || Node <- xmerl_xpath:string("/UnmonitorInstancesResponse/instancesSet/item", Doc)]};
        {error, _} = Error ->
            Error
    end.

ec2_simple_query(Config, Action, Params) ->
    case ec2_query(Config, Action, Params) of
        {ok,    _} -> 
            ok;
        {error, _} = Error ->
            Error
    end.

ec2_simple_query(Config, Action, Params, ApiVersion) ->
    case ec2_query(Config, Action, Params, ApiVersion) of
        {ok,    _} ->
            ok;
        {error, _} = Error ->
            Error
    end. 

ec2_query(Config, Action, Params) ->
    ec2_query(Config, Action, Params, ?API_VERSION).

ec2_query(Config, Action, Params, ApiVersion) ->
    QParams = [{"Action", Action}, {"Version", ApiVersion}|Params],
    erlcloud_aws:aws_request_xml4(post, Config#aws_config.ec2_host,
                                  "/", QParams, "ec2", Config).

default_config() -> erlcloud_aws:default_config().

list_to_ec2_filter(none) ->
    [];
list_to_ec2_filter(List) ->
    list_to_ec2_filter(List, 1, []).

list_to_ec2_filter([], _Count, Res) ->
    Res;
list_to_ec2_filter([{N, V}|T], Count, Res) ->
    Tup = {io_lib:format("Filter.~p.Name", [Count]), N},
    Vals = list_to_ec2_values(V, Count, 1, []),
    list_to_ec2_filter(T, Count + 1, lists:flatten([Tup, Vals, Res])).

list_to_ec2_values([], _Count, _VCount, Res) ->
    Res;
list_to_ec2_values([H|T], Count, VCount, Res) ->
    Tup = {io_lib:format("Filter.~p.Value.~p", [Count, VCount]), H},
    list_to_ec2_values(T, Count, VCount + 1, [Tup|Res]).


%%
%%
-spec(describe_vpn_gateways/0 :: () -> {ok, [proplist()]} | {error, any()}).
describe_vpn_gateways() ->
    describe_vpn_gateways(none, default_config()).

-spec(describe_vpn_gateways/1 :: (filter_list | aws_config()) -> {ok, [proplist()]} | {error, any()}).
describe_vpn_gateways(Config) when is_record(Config, aws_config) ->
    describe_vpn_gateways(none, Config);
describe_vpn_gateways(Filter) ->
    describe_vpn_gateways(Filter, default_config()).

-spec(describe_vpn_gateways/2 :: (none | filter_list(), aws_config()) -> {ok, [proplist()]} | {error, any()}).
describe_vpn_gateways(Filter, Config) ->
    Params = list_to_ec2_filter(Filter),
    case ec2_query(Config, "DescribeVpnGateways", Params, ?NEW_API_VERSION) of
        {ok, Doc} ->
            ResultPath = "/DescribeVpnGatewaysResponse/vpnGatewaySet/item",
            {ok, [ extract_vgw(Item) || Item <- xmerl_xpath:string(ResultPath, Doc) ]};
        {error, _} = Error ->
            Error
    end.

extract_vgw(Node) ->
    Items = xmerl_xpath:string("attachments/item", Node),
    [ {vpn_gateway_id, get_text("vpnGatewayId", Node)},
      {vpn_gateway_type, get_text("type", Node)},
      {vpn_gateway_state, get_text("state", Node)},
      {vpn_az, get_text("availabilityZone", Node)},
      {vpc_attachment_set, [ extract_vgw_attachments(Item) || Item <- Items]},
      {tag_set, 
       [extract_tag_item(Item)
        || Item <- xmerl_xpath:string("tagSet/item", Node)]}
 ].

extract_vgw_attachments(Node) ->
    [ {vpc_id, get_text("vpcId", Node)},
      {state, get_text("state", Node)} ].

%%
%%
-spec(describe_vpn_connections/0 :: () -> {ok, [proplist()]} | {error, any()}).
describe_vpn_connections() ->
    describe_vpn_connections(none, default_config()).

-spec(describe_vpn_connections/1 :: (filter_list | aws_config()) -> {ok, [proplist()]} | {error, any()}).
describe_vpn_connections(Config) when is_record(Config, aws_config) ->
    describe_vpn_connections(none, Config);
describe_vpn_connections(Filter) ->
    describe_vpn_connections(Filter, default_config()).

-spec(describe_vpn_connections/2 :: (none | filter_list(), aws_config()) -> {ok, [proplist()]} | {error, any()}).
describe_vpn_connections(Filter, Config) ->
    Params = list_to_ec2_filter(Filter),
    case ec2_query(Config, "DescribeVpnConnections", Params, ?NEW_API_VERSION) of
        {ok, Doc} ->
            ResultPath = "/DescribeVpnConnectionsResponse/vpnConnectionSet/item",
            {ok, [ extract_vpn_connection(Item) || Item <- xmerl_xpath:string(ResultPath, Doc) ]};
        {error, _} = Error ->
            Error
    end.

extract_vpn_connection(Node) ->
    [ {vpn_connection_id, get_text("vpnConnectionId", Node)},
      {vpn_connection_state, get_text("state", Node)},
      {customer_gateway_configuration, get_text("customerGatewayConfiguration", Node)},
      {vpn_connection_type, get_text("type", Node)},
      {customer_gateway_id, get_text("customerGatewayId", Node)},
      {vpn_gateway_id, get_text("vpnGatewayId", Node)},
      {tag_set, 
       [extract_tag_item(Item)
        || Item <- xmerl_xpath:string("tagSet/item", Node)]}
 ].

%%
%%
-spec(describe_customer_gateways/0 :: () -> {ok, [proplist()]} | {error, any()}).
describe_customer_gateways() ->
    describe_customer_gateways(none, default_config()).

-spec(describe_customer_gateways/1 :: (filter_list | aws_config()) -> {ok, [proplist()]} | {error, any()}).
describe_customer_gateways(Config) when is_record(Config, aws_config) ->
    describe_customer_gateways(none, Config);
describe_customer_gateways(Filter) ->
    describe_customer_gateways(Filter, default_config()).

-spec(describe_customer_gateways/2 :: (none | filter_list(), aws_config()) -> {ok, [proplist()]} | {error, any()}).
describe_customer_gateways(Filter, Config) ->
    Params = list_to_ec2_filter(Filter),
    case ec2_query(Config, "DescribeCustomerGateways", Params, ?NEW_API_VERSION) of
        {ok, Doc} ->
            ResultPath = "/DescribeCustomerGatewaysResponse/customerGatewaySet/item",
            {ok, [ extract_cgw(Item) || Item <- xmerl_xpath:string(ResultPath, Doc) ]};
        {error, _} = Error ->
            Error
    end.

extract_cgw(Node) ->
    [ {customer_gateway_id, get_text("customerGatewayId", Node)},
      {customer_gateway_state, get_text("state", Node)},
      {customer_gateway_type, get_text("type", Node)},
      {customer_gateway_ip, get_text("ipAddress", Node)},
      {customer_gateway_bgpasn, get_text("bgpAsn", Node)},
      {tag_set, 
       [extract_tag_item(Item)
        || Item <- xmerl_xpath:string("tagSet/item", Node)]}
 ].
