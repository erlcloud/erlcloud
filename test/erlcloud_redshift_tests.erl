-module(erlcloud_redshift_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud_aws.hrl").

-define(ALL_FEILDS_XML, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<DescribeClustersResponse xmlns=\"http://redshift.amazonaws.com/doc/2012-12-01/\">
  <DescribeClustersResult>
    <Clusters>
      <Cluster>
        <ClusterNodes>
          <member>
            <PrivateIPAddress>10.0.0.202</PrivateIPAddress>
            <NodeRole>SHARED</NodeRole>
            <PublicIPAddress>52.71.10.11</PublicIPAddress>
          </member>
        </ClusterNodes>
        <ClusterParameterGroups>
        <ClusterParameterGroup>
            <ParameterApplyStatus>in-sync</ParameterApplyStatus>
            <ParameterGroupName>default.redshift-1.0</ParameterGroupName>
            <ClusterParameterStatusList>
              <member>
                <ParameterApplyStatus>applying</ParameterApplyStatus>
                <ParameterName>enable_user_activity_logging</ParameterName>
              </member>
              <member>
                <ParameterApplyStatus>applying</ParameterApplyStatus>
                <ParameterName>max_cursor_result_set_size</ParameterName>
              </member>
              <member>
                <ParameterApplyStatus>applying</ParameterApplyStatus>
                <ParameterName>query_group</ParameterName>
              </member>
              <member>
                <ParameterApplyStatus>applying</ParameterApplyStatus>
                <ParameterName>datestyle</ParameterName>
              </member>
              <member>
                <ParameterApplyStatus>applying</ParameterApplyStatus>
                <ParameterName>extra_float_digits</ParameterName>
              </member>
              <member>
                <ParameterApplyStatus>applying</ParameterApplyStatus>
                <ParameterName>search_path</ParameterName>
              </member>
              <member>
                <ParameterApplyStatus>applying</ParameterApplyStatus>
                <ParameterName>statement_timeout</ParameterName>
              </member>
              <member>
                <ParameterApplyStatus>applying</ParameterApplyStatus>
                <ParameterName>wlm_json_configuration</ParameterName>
              </member>
              <member>
                <ParameterApplyStatus>applying</ParameterApplyStatus>
                <ParameterName>require_ssl</ParameterName>
              </member>
           </ClusterParameterStatusList>
        </ClusterParameterGroup>
        </ClusterParameterGroups>
        <Endpoint>
          <Port>5439</Port>
          <Address>kelt.us-east-1.redshift.amazonaws.com</Address>
        </Endpoint>
        <Tags>
          <Tag>
            <Value>test</Value>
            <Key>test</Key>
          </Tag>
        </Tags>
        <ClusterSnapshotCopyStatus>
          <RetentionPeriod>1</RetentionPeriod>
          <DestinationRegion>eu-west-1</DestinationRegion>
        </ClusterSnapshotCopyStatus>
        <RestoreStatus>
          <Status>starting</Status>
          <ProgressInMegaBytes>0</ProgressInMegaBytes>
          <CurrentRestoreRateInMegaBytesPerSecond>0.0</CurrentRestoreRateInMegaBytesPerSecond>
          <EstimatedTimeToCompletionInSeconds>0</EstimatedTimeToCompletionInSeconds>
          <ElapsedTimeInSeconds>0</ElapsedTimeInSeconds>
          <SnapshotSizeInMegaBytes>0</SnapshotSizeInMegaBytes>
        </RestoreStatus>
        <ElasticIpStatus>
          <ElasticIp>52.71.33.243</ElasticIp>
          <Status>active</Status>
        </ElasticIpStatus>
        <HsmStatus>
          <HsmClientCertificateIdentifier>Test Cert</HsmClientCertificateIdentifier>
          <HsmConfigurationIdentifier>Test Config</HsmConfigurationIdentifier>
          <Status>applying</Status>
        </HsmStatus>
        <VpcSecurityGroups>
          <VpcSecurityGroup>
            <Status>active</Status>
            <VpcSecurityGroupId>sg-43018527</VpcSecurityGroupId>
          </VpcSecurityGroup>
        </VpcSecurityGroups>
        <PendingModifiedValues>
          <MasterUserPassword>****</MasterUserPassword>
        </PendingModifiedValues>
        <ClusterVersion>1.0</ClusterVersion>
        <VpcSecurityGroups/>
        <ClusterStatus>creating</ClusterStatus>
        <NumberOfNodes>2</NumberOfNodes>
        <AutomatedSnapshotRetentionPeriod>1</AutomatedSnapshotRetentionPeriod>
        <PubliclyAccessible>true</PubliclyAccessible>
        <Encrypted>false</Encrypted>
        <DBName>dev</DBName>
        <PreferredMaintenanceWindow>sun:10:30-sun:11:00</PreferredMaintenanceWindow>
        <ClusterSecurityGroups>
          <ClusterSecurityGroup>
            <Status>active</Status>
            <ClusterSecurityGroupName>default</ClusterSecurityGroupName>
          </ClusterSecurityGroup>
        </ClusterSecurityGroups>
        <AvailabilityZone>us-east-1a</AvailabilityZone>
        <NodeType>ds1.xlarge</NodeType>
        <ClusterIdentifier>examplecluster</ClusterIdentifier>
        <AllowVersionUpgrade>true</AllowVersionUpgrade>
        <MasterUsername>masteruser</MasterUsername>
      </Cluster>
    </Clusters>
  </DescribeClustersResult>
  <ResponseMetadata>
    <RequestId>837d45d6-64f0-11e2-b07c-f7fbdd006c67</RequestId>
  </ResponseMetadata>
</DescribeClustersResponse>").
-define(MARKER, "TESTMARKER").
-define(TEST_ID, "TESTID").

redshift_test_() ->
    {foreach,
     fun setup/0,
     fun meck:unload/1,
     [fun describe_clusters/1]
    }.

setup() ->
    meck:new(ECA = erlcloud_aws, [non_strict]),
    meck:expect(erlcloud_aws, default_config, 0, #aws_config{}),
    meck:expect(erlcloud_aws, aws_request_xml4, mocks()),
    [ECA].

mocks() ->
    [mocked_clusters_1(), mocked_clusters_2(),
     mocked_marker_1(), mocked_marker_2(),
     mocked_cluster()].

mocked_clusters_1() ->
    {[get, '_', "/",
      [{"Action", "DescribeClusters"},
       {"Version", '_'}], '_', '_'],
     make_response(?ALL_FEILDS_XML)}.

mocked_clusters_2() ->
    {[get, '_', "/",
      [{"Action", "DescribeClusters"},
       {"Version", '_'},
       {"ClusterIdentifier", "examplecluster"}], '_', '_'],
     make_response(?ALL_FEILDS_XML)}.

mocked_marker_1() ->
    {[get, '_', "/",
      [{"Action", "DescribeClusters"},
       {"Version", '_'},
       {"MaxRecords", 20}], '_', '_'],
     make_response("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<DescribeClustersResponse xmlns=\"http://redshift.amazonaws.com/doc/2012-12-01/\">
  <DescribeClustersResult>
    <Marker>" ?MARKER "</Marker>
  </DescribeClustersResult>
  <ResponseMetadata>
    <RequestId>837d45d6-64f0-11e2-b07c-f7fbdd006c67</RequestId>
  </ResponseMetadata>
</DescribeClustersResponse>")}.

mocked_marker_2() ->
    {[get, '_', "/",
      [{"Action", "DescribeClusters"},
       {"Version", '_'},
       {"Marker", ?MARKER},
       {"MaxRecords", 20}], '_', '_'],
     make_response(?ALL_FEILDS_XML)}.

mocked_cluster() ->
    {[get, '_', "/",
      [{"Action", "DescribeClusters"},
       {"Version", '_'},
       {"ClusterIdentifier", ?TEST_ID}], '_', '_'],
     make_response(?ALL_FEILDS_XML)}.

make_response(Xml) ->
    {ok, element(1, xmerl_scan:string(Xml))}.

describe_clusters(_) ->
    [
     fun() ->
             {ok, Result} = erlcloud_redshift:describe_clusters(),
             {ok, Result1} = erlcloud_redshift:describe_clusters(
                               [{"ClusterIdentifier", "examplecluster"}]),
             {ok, Result2} = erlcloud_redshift:describe_clusters_all(
                               [{"MaxRecords", 20}]),
             {ok, Result3} = erlcloud_redshift:describe_cluster(?TEST_ID),
             Expected = [[{allow_version_upgrade,true},
                          {automated_snapshot_retention_period,1},
                          {availability_zone,"us-east-1a"},
                          {cluster_identifier,"examplecluster"},
                          {cluster_nodes,[[{node_role,"SHARED"},
                                           {private_ip_address,"10.0.0.202"},
                                           {public_ip_address,"52.71.10.11"}]]},
                          {cluster_parameter_groups,
                           [[{cluster_parameter_status_list,
                              [[{parameter_apply_status,"applying"},
                                {parameter_name,"enable_user_activity_logging"}],
                               [{parameter_apply_status,"applying"},
                                {parameter_name,"max_cursor_result_set_size"}],
                               [{parameter_apply_status,"applying"},
                                {parameter_name,"query_group"}],
                               [{parameter_apply_status,"applying"},
                                {parameter_name,"datestyle"}],
                               [{parameter_apply_status,"applying"},
                                {parameter_name,"extra_float_digits"}],
                               [{parameter_apply_status,"applying"},
                                {parameter_name,"search_path"}],
                               [{parameter_apply_status,"applying"},
                                {parameter_name,"statement_timeout"}],
                               [{parameter_apply_status,"applying"},
                                {parameter_name,"wlm_json_configuration"}],
                               [{parameter_apply_status,"applying"},
                                {parameter_name,"require_ssl"}]]},
                             {parameter_apply_status,"in-sync"},
                             {parameter_group_name,"default.redshift-1.0"}]]},
                          {cluster_snapshot_copy_status,
                           [{destination_region,"eu-west-1"},
                            {retention_period,1}]},
                          {cluster_status,"creating"},
                          {cluster_version,"1.0"},
                          {db_name,"dev"},
                          {elastic_ip_status,[{elastic_ip,"52.71.33.243"},
                                              {status,"active"}]},
                          {encrypted,false},
                          {endpoint,[{address,
                                      "kelt.us-east-1.redshift.amazonaws.com"},
                                     {port,5439}]},
                          {master_username,"masteruser"},
                          {node_type,"ds1.xlarge"},
                          {number_of_nodes,2},
                          {pending_modified_values,
                           [{master_user_password,"****"}]},
                          {preferred_maintenance_window,"sun:10:30-sun:11:00"},
                          {publicly_accessible,true},
                          {restore_status,
                           [{current_restore_rate_in_mega_bytes_per_second,0.0},
                            {elapsed_time_in_seconds,0},
                            {estimated_time_to_completion_in_seconds,0},
                            {progress_in_mega_bytes,0},
                            {snapshot_size_in_mega_bytes,0},
                            {status,"starting"}]},
                          {tags,[[{key,"test"},
                                  {value,"test"}]]},
                          {vpc_security_groups,
                           [[{status,"active"},
                             {vpc_security_group_id,"sg-43018527"}]]}]],
             ?assertEqual(Expected,     Result),
             ?assertEqual(Expected,     Result1),
             ?assertEqual(Expected,     Result2),
             ?assertEqual(hd(Expected), Result3)
     end
    ].
