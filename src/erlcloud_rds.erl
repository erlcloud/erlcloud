-module(erlcloud_rds).


-include("erlcloud_aws.hrl").


-define(API_VERSION, "2014-10-31").


-type access_key_id() :: string().
-type secret_access_key() :: string().

-type db_instance() :: proplists:proplist().
-type db_security_group() :: proplists:proplist().
-type db_subnet_group() :: proplists:proplist().
-type marker() :: string() | none | undefined.

-type describe_params() :: proplists:proplist().
-type error_reason() :: term().
-type error_res() :: {error, error_reason()}.


%% Library initialization
-export([
    configure/2,
    new/2
]).


%% RDS API Functions
-export([
    describe_db_instances/0,
    describe_db_instances/1,
    describe_db_instances/2,
    describe_db_instances_all/0,
    describe_db_instances_all/1,
    describe_db_security_groups/0,
    describe_db_security_groups/1,
    describe_db_security_groups/2,
    describe_db_security_groups_all/0,
    describe_db_security_groups_all/1,
    describe_db_subnet_groups/0,
    describe_db_subnet_groups/1,
    describe_db_subnet_groups/2,
    describe_db_subnet_groups_all/0,
    describe_db_subnet_groups_all/1
]).


%%==============================================================================
%% Library initialization
%%==============================================================================


-spec configure(access_key_id(), secret_access_key()) -> ok.
configure(AccessKeyID, SecretAccessKey) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey)),
    ok.


-spec new(access_key_id(), secret_access_key()) -> #aws_config{}.
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{
        access_key_id = AccessKeyID,
        secret_access_key = SecretAccessKey
    }.


%%==============================================================================
%% RDS API Functions
%%==============================================================================


-spec describe_db_instances() ->
    {ok, [db_instance()], marker()} | error_res().
describe_db_instances() ->
    describe_db_instances([], default_config()).


-spec describe_db_instances(describe_params() | aws_config()) ->
    {ok, [db_instance()], marker()} | error_res().
describe_db_instances(Params) when is_list(Params) ->
    describe_db_instances(Params, default_config());
describe_db_instances(#aws_config{} = Config) ->
    describe_db_instances([], Config).


-spec describe_db_instances(describe_params(), aws_config()) ->
    {ok, [db_instance()], marker()} | error_res().
describe_db_instances(Params, Config) ->
    Query = lists:map(
        fun
            ({db_instance_identifier, Id}) -> {"DBInstanceIdentifier", Id};
            ({marker, Marker}) -> {"Marker", Marker};
            ({max_records, MaxRecords}) -> {"MaxRecords", MaxRecords}
        end,
        Params
    ),
    case rds_query(Config, "DescribeDBInstances", Query) of
        {ok, Doc} ->
            DBInstances = extract_db_instances(
                xmerl_xpath:string(
                    "/DescribeDBInstancesResponse/DescribeDBInstancesResult"
                    "/DBInstances/DBInstance",
                    Doc
                )
            ),
            NewMarker = erlcloud_xml:get_text(
                "/DescribeDBInstancesResponse/DescribeDBInstancesResult/Marker",
                Doc,
                undefined
            ),
            {ok, DBInstances, NewMarker};
        {error, Reason} ->
            {error, Reason}
    end.


-spec describe_db_instances_all() ->
    {ok, [db_instance()]} | error_res().
describe_db_instances_all() ->
    describe_db_instances_all(default_config()).


-spec describe_db_instances_all(aws_config()) ->
    {ok, [db_instance()]} | error_res().
describe_db_instances_all(Config) ->
    describe_all(fun describe_db_instances/2, Config, undefined, []).


-spec describe_db_security_groups() ->
    {ok, [db_security_group()], marker()} | error_res().
describe_db_security_groups() ->
    describe_db_security_groups([], default_config()).


-spec describe_db_security_groups(describe_params() | aws_config()) ->
    {ok, [db_security_group()], marker()} | error_res().
describe_db_security_groups(Params) when is_list(Params) ->
    describe_db_security_groups(Params, default_config());
describe_db_security_groups(#aws_config{} = Config) ->
    describe_db_security_groups([], Config).


-spec describe_db_security_groups(describe_params(), aws_config()) ->
    {ok, [db_security_group()], marker()} | error_res().
describe_db_security_groups(Params, Config) ->
    Query = lists:map(
        fun
            ({db_security_group_name, Name}) -> {"DBSecurityGroupName", Name};
            ({marker, Marker}) -> {"Marker", Marker};
            ({max_records, MaxRecords}) -> {"MaxRecords", MaxRecords}
        end,
        Params
    ),
    case rds_query(Config, "DescribeDBSecurityGroups", Query) of
        {ok, Doc} ->
            DBSecurityGroups = extract_db_security_groups(
                xmerl_xpath:string(
                    "/DescribeDBSecurityGroupsResponse"
                    "/DescribeDBSecurityGroupsResult/DBSecurityGroups"
                    "/DBSecurityGroup",
                    Doc
                )
            ),
            NewMarker = erlcloud_xml:get_text(
                "/DescribeDBSecurityGroupsResponse"
                "/DescribeDBSecurityGroupsResult/Marker",
                Doc,
                undefined
            ),
            {ok, DBSecurityGroups, NewMarker};
        {error, Reason} ->
            {error, Reason}
    end.


-spec describe_db_security_groups_all() ->
    {ok, [db_security_group()]} | error_res().
describe_db_security_groups_all() ->
    describe_db_security_groups_all(default_config()).


-spec describe_db_security_groups_all(aws_config()) ->
    {ok, [db_security_group()]} | error_res().
describe_db_security_groups_all(Config) ->
    describe_all(fun describe_db_security_groups/2, Config, undefined, []).


-spec describe_db_subnet_groups() ->
    {ok, [db_subnet_group()], marker()} | error_res().
describe_db_subnet_groups() ->
    describe_db_subnet_groups([], default_config()).


-spec describe_db_subnet_groups(describe_params() | aws_config()) ->
    {ok, [db_subnet_group()], marker()} | error_res().
describe_db_subnet_groups(Params) when is_list(Params) ->
    describe_db_subnet_groups(Params, default_config());
describe_db_subnet_groups(#aws_config{} = Config) ->
    describe_db_subnet_groups([], Config).


-spec describe_db_subnet_groups(describe_params(), aws_config()) ->
    {ok, [db_subnet_group()], marker()} | error_res().
describe_db_subnet_groups(Params, Config) ->
    Query = lists:map(
        fun
            ({db_subnet_group_name, Name}) -> {"DBSubnetGroupName", Name};
            ({marker, Marker}) -> {"Marker", Marker};
            ({max_records, MaxRecords}) -> {"MaxRecords", MaxRecords}
        end,
        Params
    ),
    case rds_query(Config, "DescribeDBSubnetGroups", Query) of
        {ok, Doc} ->
            DBSubnetGroups = extract_db_subnet_groups(
                xmerl_xpath:string(
                    "/DescribeDBSubnetGroupsResponse"
                    "/DescribeDBSubnetGroupsResult/DBSubnetGroups"
                    "/DBSubnetGroup",
                    Doc
                )
            ),
            NewMarker = erlcloud_xml:get_text(
                "/DescribeDBSubnetGroupsResponse"
                "/DescribeDBSubnetGroupsResult/Marker",
                Doc,
                undefined
            ),
            {ok, DBSubnetGroups, NewMarker};
        {error, Reason} ->
            {error, Reason}
    end.


-spec describe_db_subnet_groups_all() ->
    {ok, [db_subnet_group()]} | error_res().
describe_db_subnet_groups_all() ->
    describe_db_subnet_groups_all(default_config()).


-spec describe_db_subnet_groups_all(aws_config()) ->
    {ok, [db_subnet_group()]} | error_res().
describe_db_subnet_groups_all(Config) ->
    describe_all(fun describe_db_subnet_groups/2, Config, undefined, []).


%%==============================================================================
%% Internal functions
%%==============================================================================


rds_query(Config, Action, Params) ->
    Query = [{"Action", Action}, {"Version", ?API_VERSION} | Params],
    erlcloud_aws:aws_request_xml4(
        post, Config#aws_config.rds_host, "/", Query, "rds", Config).


default_config() ->
    erlcloud_aws:default_config().


describe_all(Fun, Config, Marker, Acc) ->
    case Fun([{marker, Marker}], Config) of
        {ok, Res, undefined} ->
            {ok, lists:append(Acc, Res)};
        {ok, Res, NewMarker} ->
            describe_all(Fun, Config, NewMarker, lists:append(Acc, Res));
        {error, Reason} ->
            {error, Reason}
    end.


extract_db_instances(XmlNodes) ->
    lists:map(fun extract_db_instance/1, XmlNodes).


extract_db_instance(XmlNode) ->
    erlcloud_xml:decode([
        {allocated_storage,
         "AllocatedStorage",
         optional_integer},
        {auto_minor_version_upgrade,
         "AutoMinorVersionUpgrade",
         optional_boolean},
        {availability_zone,
         "AvailabilityZone",
         optional_text},
        {backup_retention_period,
         "BackupRetentionPeriod",
         optional_integer},
        {ca_certificate_identifier,
         "CACertificateIdentifier",
         optional_text},
        {character_set_name,
         "CharacterSetName",
         optional_text},
        {db_cluster_identifier,
         "DBClusterIdentifier",
         optional_text},
        {db_instance_class,
         "DBInstanceClass",
         optional_text},
        {db_instance_identifier,
         "DBInstanceIdentifier",
         optional_text},
        {db_instance_status,
         "DBInstanceStatus",
         optional_text},
        {db_name,
         "DBName",
         optional_text},
        {db_parameter_groups,
         "DBParameterGroups/DBParameterGroup",
         {optional_map, fun extract_db_parameter_group_status/1}},
        {db_security_groups,
         "DBSecurityGroups/DBSecurityGroup",
         {optional_map, fun extract_db_security_group_membership/1}},
        {db_subnet_group,
         "DBSubnetGroup",
         {single, fun extract_db_subnet_group/1}},
        {db_instance_port,
         "DbInstancePort",
         optional_integer},
        {dbi_resource_id,
         "DbiResourceId",
         optional_text},
        {endpoint,
         "Endpoint",
         {single, fun extract_endpoint/1}},
        {engine,
         "Engine",
         optional_text},
        {engine_version,
         "EngineVersion",
         optional_text},
        {instance_create_time,
         "InstanceCreateTime",
         time},
        {iops,
         "Iops",
         optional_integer},
        {kms_key_id,
         "KmsKeyId",
         optional_text},
        {latest_restorable_time,
         "LatestRestorableTime",
         time},
        {license_model,
         "LicenseModel",
         optional_text},
        {master_username,
         "MasterUsername",
         optional_text},
        {multi_az,
         "MultiAZ",
         optional_boolean},
        {option_group_memberships,
         "OptionGroupMemberships/OptionGroupMembership",
         {optional_map, fun extract_option_group_membership/1}},
        {pending_modified_values,
         "PendingModifiedValues",
         {single, fun extract_pending_modified_values/1}},
        {preferred_backup_window,
         "PreferredBackupWindow",
         optional_text},
        {preferred_maintenance_window,
         "PreferredMaintenanceWindow",
         optional_text},
        {publicly_accessible,
         "PubliclyAccessible",
         optional_boolean},
        {read_replica_db_instance_identifiers,
         "ReadReplicaDBInstanceIdentifiers/ReadReplicaDBInstanceIdentifier",
         list},
        {read_replica_source_db_instance_identifier,
         "ReadReplicaSourceDBInstanceIdentifier",
         optional_text},
        {secondary_availability_zone,
         "SecondaryAvailabilityZone",
         optional_text},
        {status_infos,
         "StatusInfos/DBInstanceStatusInfo",
         {optional_map, fun extract_db_instance_status_info/1}},
        {storage_encrypted,
         "StorageEncrypted",
         optional_boolean},
        {storage_type,
         "StorageType",
         optional_text},
        {tde_credential_arn,
         "TdeCredentialArn",
         optional_text},
        {vpc_security_groups,
         "VpcSecurityGroups/VpcSecurityGroupMembership",
         {optional_map, fun extract_vpc_security_group_membership/1}}
    ], XmlNode).


extract_db_parameter_group_status(XmlNode) ->
    erlcloud_xml:decode([
        {db_parameter_group_name,
         "DBParameterGroupName",
         optional_text},
        {parameter_apply_status,
         "ParameterApplyStatus",
         optional_text}
    ], XmlNode).


extract_db_security_group_membership(XmlNode) ->
    erlcloud_xml:decode([
        {db_security_group_name,
         "DBSecurityGroupName",
         optional_text},
        {status,
         "Status",
         optional_text}
    ], XmlNode).


extract_db_subnet_groups(XmlNodes) ->
    lists:map(fun extract_db_subnet_group/1, XmlNodes).


extract_db_subnet_group(XmlNode) ->
    erlcloud_xml:decode([
        {db_subnet_group_description,
         "DBSubnetGroupDescription",
         optional_text},
        {db_subnet_group_name,
         "DBSubnetGroupName",
         optional_text},
        {subnet_group_status,
         "SubnetGroupStatus",
         optional_text},
        {subnets,
         "Subnets/Subnet",
         {optional_map, fun extract_subnet/1}},
        {vpc_id,
         "VpcId",
         optional_text}
    ], XmlNode).


extract_subnet(XmlNode) ->
    erlcloud_xml:decode([
        {subnet_availability_zone,
         "SubnetAvailabilityZone",
         {single, fun extract_subnet_availability_zone/1}},
        {subnet_identifier,
         "SubnetIdentifier",
         optional_text},
        {subnet_status,
         "SubnetStatus",
         optional_text}
    ], XmlNode).


extract_subnet_availability_zone(XmlNode) ->
    erlcloud_xml:decode([
        {name,
         "Name",
         optional_text}
    ], XmlNode).


extract_endpoint(XmlNode) ->
    erlcloud_xml:decode([
        {address,
         "Address",
         optional_text},
        {port,
         "Port",
         optional_integer}
    ], XmlNode).


extract_option_group_membership(XmlNode) ->
    erlcloud_xml:decode([
        {option_group_name,
         "OptionGroupName",
         optional_text},
        {status,
         "Status",
         optional_text}
    ], XmlNode).


extract_pending_modified_values(XmlNode) ->
    erlcloud_xml:decode([
        {allocated_storage,
         "AllocatedStorage",
         optional_integer},
        {backup_retention_period,
         "BackupRetentionPeriod",
         optional_integer},
        {ca_certificate_identifier,
         "CACertificateIdentifier",
         optional_text},
        {db_instance_class,
         "DBInstanceClass",
         optional_text},
        {db_instance_identifier,
         "DBInstanceIdentifier",
         optional_text},
        {engine_version,
         "EngineVersion",
         optional_text},
        {iops,
         "Iops",
         optional_integer},
        {master_user_password,
         "MasterUserPassword",
         optional_text},
        {multi_az,
         "MultiAZ",
         optional_boolean},
        {port,
         "Port",
         optional_integer},
        {storage_type,
         "StorageType",
         optional_text}
    ], XmlNode).


extract_db_instance_status_info(XmlNode) ->
    erlcloud_xml:decode([
        {message,
         "Message",
         optional_text},
        {normal,
         "Normal",
         optional_boolean},
        {status,
         "Status",
         optional_text},
        {status_type,
         "StatusType",
         optional_text}
    ], XmlNode).


extract_vpc_security_group_membership(XmlNode) ->
    erlcloud_xml:decode([
        {status,
         "Status",
         optional_text},
        {vpc_security_group_id,
         "VpcSecurityGroupId",
         optional_text}
    ], XmlNode).


extract_db_security_groups(XmlNodes) ->
    lists:map(fun extract_db_security_group/1, XmlNodes).


extract_db_security_group(XmlNode) ->
    erlcloud_xml:decode([
        {db_security_group_description,
         "DBSecurityGroupDescription",
         optional_text},
        {db_security_group_name,
         "DBSecurityGroupName",
         optional_text},
        {ec2_security_groups,
         "EC2SecurityGroups/EC2SecurityGroup",
         {optional_map, fun extract_ec2_security_group/1}},
        {ip_ranges,
         "IPRanges/IPRange",
         {optional_map, fun extract_ip_range/1}},
        {owner_id,
         "OwnerId",
         optional_text},
        {vpc_id,
         "VpcId",
         optional_text}
    ], XmlNode).


extract_ec2_security_group(XmlNode) ->
    erlcloud_xml:decode([
        {ec2_security_group_id,
         "EC2SecurityGroupId",
         optional_text},
        {ec2_security_group_name,
         "EC2SecurityGroupName",
         optional_text},
        {ec2_security_group_owner_id,
         "EC2SecurityGroupOwnerId",
         optional_text},
        {status,
         "Status",
         optional_text}
    ], XmlNode).


extract_ip_range(XmlNode) ->
    erlcloud_xml:decode([
        {cidr_ip,
         "CIDRIP",
         optional_text},
        {status,
         "Status",
         optional_text}
    ], XmlNode).

