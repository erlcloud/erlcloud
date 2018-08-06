-module(erlcloud_redshift).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

-define(API_VERSION, "2012-12-01").

-export([configure/2, configure/3, new/2, new/3]).

-export([describe_clusters/0, describe_clusters/1, describe_clusters/2]).
-export([describe_clusters_all/0, describe_clusters_all/1,
         describe_clusters_all/2]).
-export([describe_cluster/1, describe_cluster/2, describe_cluster/3]).

-type(clusters_marker_return() :: {ok, list(proplists:proplist())} |
                                  {ok, list(proplists:proplist()), list()} |
                                  {error, term()}).
-type(clusters_return() :: {ok, list(proplists:proplist())} |
                                  {error, term()}).
-type(cluster_return() :: {ok, proplists:proplist()} | {error, term()}).


-spec new(string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey}.

-spec new(string(), string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                redshift_host=Host}.

-spec configure(string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey)),
    ok.

-spec configure(string(), string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host)),
    ok.

-spec redshift_query(aws_config(), string(), list({string(), string()})) ->
    {ok, term()} | {error, term()}.
redshift_query(Config, Action, Params) ->
    QParams = [{"Action", Action}, {"Version", ?API_VERSION} | Params],
    erlcloud_aws:aws_request_xml4(get, Config#aws_config.redshift_host,
                                  "/", QParams, "redshift", Config).

-spec describe_cluster(list()) -> cluster_return().
describe_cluster(Id) ->
    describe_cluster(Id, erlcloud_aws:default_config()).

-spec describe_cluster(list(), list() | aws_config()) -> cluster_return().
describe_cluster(Id, Options) when is_list(Options) ->
    describe_cluster(Id, Options, erlcloud_aws:default_config());
describe_cluster(Id, AwsConfig) ->
    describe_cluster(Id, [], AwsConfig).

-spec describe_cluster(list(), list(), aws_config()) -> cluster_return().
describe_cluster(Id, Options, AwsConfig) ->
    case describe_clusters([{"ClusterIdentifier", Id} | Options], AwsConfig) of
        {ok, [Result]} -> {ok, Result};
        {error, _} = E -> E
    end.

-spec describe_clusters() -> clusters_marker_return().
describe_clusters() ->
    describe_clusters(erlcloud_aws:default_config()).

-spec describe_clusters(list() | aws_config()) -> clusters_marker_return().
describe_clusters(Options) when is_list(Options) ->
    describe_clusters(Options, erlcloud_aws:default_config());
describe_clusters(AwsConfig) ->
    describe_clusters([], AwsConfig).

-spec describe_clusters(list(), aws_config()) -> clusters_marker_return().
describe_clusters(Options, AwsConfig) ->
    case redshift_query(AwsConfig, "DescribeClusters", Options) of
        {ok, Doc} ->
            Clusters = xmerl_xpath:string("/DescribeClustersResponse"
                                          "/DescribeClustersResult"
                                          "/Clusters/Cluster", Doc),
            Marker   = erlcloud_xml:get_text("/DescribeClustersResponse"
                                             "/DescribeClustersResult"
                                             "/Marker", Doc),
            ProcessedClusters = [extract_cluster(C) || C <- Clusters],
            case Marker of
                [] ->
                    {ok, ProcessedClusters};
                M ->
                    {ok, ProcessedClusters, M}
            end;
        {error, _} = E ->
            E
    end.

-spec describe_clusters_all() -> clusters_return().
describe_clusters_all() ->
    describe_clusters_all(erlcloud_aws:default_config()).

-spec describe_clusters_all(list() | aws_config()) -> clusters_return().
describe_clusters_all(Options) when is_list(Options)  ->
    describe_clusters_all(Options, erlcloud_aws:default_config());
describe_clusters_all(AwsConfig) ->
    describe_clusters_all([], AwsConfig).

-spec describe_clusters_all(list(), aws_config()) -> clusters_return().
describe_clusters_all(Options, AwsConfig) ->
    describe_all(fun describe_clusters/2, Options, AwsConfig, []).

extract_cluster_nodes(Xml) ->
    erlcloud_xml:decode(
      [
       {node_role, "NodeRole", optional_text},
       {private_ip_address, "PrivateIPAddress", optional_text},
       {public_ip_address, "PublicIPAddress", optional_text}
      ], Xml).

extract_cluster_parameter_groups(Xml) ->
    erlcloud_xml:decode(
      [
       {cluster_parameter_status_list, "ClusterParameterStatusList/member",
        {optional_map, fun extract_cluster_parameter_status/1}},
       {parameter_apply_status, "ParameterApplyStatus", optional_text},
       {parameter_group_name, "ParameterGroupName", optional_text}
      ], Xml).

extract_cluster_parameter_status(Xml) ->
    erlcloud_xml:decode(
      [
       {parameter_apply_error_description, "ParameterApplyErrorDescription",
        optional_text},
       {parameter_apply_status, "ParameterApplyStatus", optional_text},
       {parameter_name, "ParameterName", optional_text}
      ], Xml).

extract_cluster_snapshot_copy_status(Xml) ->
    erlcloud_xml:decode(
      [
       {destination_region, "DestinationRegion", optional_text},
       {retention_period, "RetentionPeriod", optional_integer},
       {snapshot_copy_grant_name, "SnapshotCopyGrantName", optional_text}
      ], Xml).

extract_elastic_ip_status(Xml) ->
    erlcloud_xml:decode(
      [
       {elastic_ip, "ElasticIp", optional_text},
       {status, "Status", optional_text}
      ], Xml).

extract_endpoint(Xml) ->
    erlcloud_xml:decode(
      [
       {address, "Address", optional_text},
       {port, "Port", optional_integer}
      ], Xml).

extract_hsm_status(Xml) ->
    erlcloud_xml:decode(
      [
       {hsm_client_certificate_identifier, "HsmClientCertificateIdentifier",
        optional_text},
       {hsm_configuration_identifier, "HsmConfigurationIdentifier",
        optional_text},
       {status, "Status", optional_text}
      ], Xml).

extract_pending_modified_values(Xml) ->
    erlcloud_xml:decode(
      [
       {automated_snapshot_retention_period, "AutomatedSnapshotRetentionPeriod",
        optional_integer},
       {cluster_identifier, "ClusterIdentifier", optional_text},
       {cluster_type, "ClusterType", optional_text},
       {cluster_version, "ClusterVersion", optional_text},
       {master_user_password, "MasterUserPassword", optional_text},
       {node_type, "NodeType", optional_text},
       {number_of_nodes, "NumberOfNodes", optional_integer},
       {publicly_accessible, "PubliclyAccessible", optional_boolean}
      ], Xml).

extract_restore_status(Xml) ->
    erlcloud_xml:decode(
      [
       {current_restore_rate_in_mega_bytes_per_second,
        "CurrentRestoreRateInMegaBytesPerSecond", float},
       {elapsed_time_in_seconds, "ElapsedTimeInSeconds", optional_integer},
       {estimated_time_to_completion_in_seconds,
        "EstimatedTimeToCompletionInSeconds", optional_integer},
       {progress_in_mega_bytes, "ProgressInMegaBytes", optional_integer},
       {snapshot_size_in_mega_bytes,
        "SnapshotSizeInMegaBytes", optional_integer},
       {status, "Status", optional_text}
      ], Xml).

extract_tags(Xml) ->
    erlcloud_xml:decode(
      [
       {key, "Key", optional_text},
       {value, "Value", optional_text}
      ], Xml).

extract_vpc_security_groups(Xml) ->
    erlcloud_xml:decode(
      [
       {status, "Status", optional_text},
       {vpc_security_group_id, "VpcSecurityGroupId", optional_text}
      ], Xml).

extract_cluster(Xml) ->
    erlcloud_xml:decode(
      [
       {allow_version_upgrade, "AllowVersionUpgrade", optional_boolean},
       {automated_snapshot_retention_period, "AutomatedSnapshotRetentionPeriod",
        optional_integer},
       {availability_zone, "AvailabilityZone", optional_text},
       {cluster_create_time, "ClusterCreateTime", optional_text},
       {cluster_identifier, "ClusterIdentifier", optional_text},
       {cluster_nodes, "ClusterNodes/member",
        {optional_map, fun extract_cluster_nodes/1}},
       {cluster_parameter_groups, "ClusterParameterGroups/ClusterParameterGroup",
        {optional_map, fun extract_cluster_parameter_groups/1}},
       {cluster_public_key, "ClusterPublicKey", optional_text},
       {cluster_revision_number, "ClusterRevisionNumber", optional_text},
%%       {cluster_security_groups, "ClusterSecurityGroups", },  Not supported as only EC2 classic
       {cluster_snapshot_copy_status, "ClusterSnapshotCopyStatus",
        {single, fun extract_cluster_snapshot_copy_status/1}},
       {cluster_status, "ClusterStatus", optional_text},
       {cluster_subnet_group_name, "ClusterSubnetGroupName", optional_text},
       {cluster_version, "ClusterVersion", optional_text},
       {db_name, "DBName", optional_text},
       {elastic_ip_status, "ElasticIpStatus",
        {single, fun extract_elastic_ip_status/1}},
       {encrypted, "Encrypted", optional_boolean},
       {endpoint, "Endpoint", {single, fun extract_endpoint/1}},
       {hsm_status, "HmsStatus", {single, fun extract_hsm_status/1}},
       {kms_key_id, "KmsKeyId", optional_text},
       {master_username, "MasterUsername", optional_text},
       {modify_status, "ModifyStatus", optional_text},
       {node_type, "NodeType", optional_text},
       {number_of_nodes, "NumberOfNodes", optional_integer},
       {pending_modified_values, "PendingModifiedValues",
        {single, fun extract_pending_modified_values/1}},
       {preferred_maintenance_window, "PreferredMaintenanceWindow",
        optional_text},
       {publicly_accessible, "PubliclyAccessible", optional_boolean},
       {restore_status, "RestoreStatus", {single, fun extract_restore_status/1}},
       {tags, "Tags/Tag", {optional_map, fun extract_tags/1}},
       {vpc_id, "VpcId", optional_text},
       {vpc_security_groups, "VpcSecurityGroups/VpcSecurityGroup",
        {optional_map, fun extract_vpc_security_groups/1}}
      ],
      Xml).

describe_all(Fun, Options, Config, Acc) ->
    describe_all(Fun, [], Options, Config, Acc).

describe_all(Fun, Args, Options, Config, Acc) when is_list(Args) ->
    case apply(Fun, Args ++ [Options] ++ [Config]) of
        {ok, Res} ->
            {ok, lists:foldl(fun erlang:'++'/2, [], [Res | Acc])};
        {ok, Res, Marker} ->
            Options1 = key_replace_or_add("Marker", Marker, Options),
            describe_all(Fun, Args, Options1,
                         Config, [Res | Acc]);
        {error, Reason} ->
            {error, Reason}
    end.

key_replace_or_add(Key, Value, List) ->
    case lists:keymember(Key, 1, List) of
        true ->
            lists:keyreplace(Key, 1, List, {Key, Value});
        false ->
            [{Key, Value} | List]
    end.
