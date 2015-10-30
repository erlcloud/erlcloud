%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_rds_tests).


-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").


%% Unit tests for ec2.
%% These tests work by using meck to mock erlcloud_httpc. There are two classes
%% of test: input and output.
%%
%% Input tests verify that different function args produce the desired query
%% parameters.
%%
%% An input test list provides a list of funs and the parameters that are
%% expected to result.
%%
%% Output tests verify that the http response produces the correct return
%% from the fun.
%% An output test lists provides a list of response bodies and the
%% expected return.


%% The _rds_test macro provides line number annotation to a test,
%% similar to _test, but doesn't wrap in a fun
-define(_rds_test(T), {?LINE, T}).
%% The _f macro is a terse way to wrap code in a fun.
%% Similar to _test but doesn't annotate with a line number
-define(_f(F), fun() -> F end).


-type expected_param() :: {string(), string()}.
-type input_test_spec() :: {pos_integer(), {fun(), [expected_param()]} |
                           {string(), fun(), [expected_param()]}}.
-type output_test_spec() :: {pos_integer(), {string(), term()} |
                            {string(), string(), term()}}.


%%==============================================================================
%% Test entry points
%%==============================================================================


describe_db_instances_test_() ->
    {foreach, fun start/0, fun stop/1, [
        fun describe_db_instances_input_tests/1,
        fun describe_db_instances_output_tests/1,
        fun describe_db_security_groups_input_tests/1,
        fun describe_db_security_groups_output_tests/1,
        fun describe_db_subnet_groups_input_tests/1,
        fun describe_db_subnet_groups_output_tests/1
    ]}.


start() ->
    meck:new(erlcloud_httpc),
    ok.


stop(_) ->
    meck:unload(erlcloud_httpc).


%%==============================================================================
%% Input test helpers
%%==============================================================================


%% common_params returns the list of parameters that are not validated by
%% these tests. They should be checked by lower level unit tests.
-spec common_params() -> [string()].
common_params() ->
    ["AWSAccessKeyId",
     "SignatureMethod",
     "SignatureVersion",
     "Timestamp",
     "Version",
     "Signature"].


%% validate_param checks that the query parameter is either a common param or
%% expected by the test case. If expected, returns expected with the param
%% deleted to be used in subsequent calls.
-spec validate_param(string(), [expected_param()]) -> [expected_param()].
validate_param(Param, Expected) ->
    {Key, Value} =
        case string:tokens(Param, "=") of
            [K, V] ->
                {K, V};
            [K] ->
                {K, ""}
        end,
    case lists:member(Key, common_params()) of
        true ->
            Expected;
        false ->
            Expected1 = lists:delete({Key, Value}, Expected),
            case length(Expected) - 1 =:= length(Expected1) of
                true -> ok;
                false ->
                    ?debugFmt("Parameter not expected: ~p", [{Key, Value}])
            end,
            ?assertEqual(length(Expected) - 1, length(Expected1)),
            Expected1
    end.


%% verifies that the parameters in the body match the expected parameters
-spec validate_params(binary(), [expected_param()]) -> ok.
validate_params(Body, Expected) ->
    ParamList = string:tokens(binary_to_list(Body), "&"),
    Remain = lists:foldl(fun validate_param/2, Expected, ParamList),
    ?assertEqual([], Remain).


%% returns the mock of the erlcloud_httpc function input tests expect to be
%% called. Validates the query body and responds with the provided response.
-spec input_expect(string(), [expected_param()]) -> fun().
input_expect(Response, Expected) ->
    fun(_Url, post, _Headers, Body, _Timeout, _Config) ->
        validate_params(Body, Expected),
        {ok, {{200, "OK"}, [], list_to_binary(Response)}}
    end.


%% input_test converts an input_test specifier into an eunit test generator
-spec input_test(string(), input_test_spec()) -> tuple().
input_test(Response, {Line, {Descr, Fun, Params}}) when is_list(Descr) ->
    {Descr, {Line, fun() ->
        meck:expect(erlcloud_httpc, request, input_expect(Response, Params)),
        %% Configure to make sure there is a key.
        %% Would like to do this in start, but that isn't called
        %% in the same process
        erlcloud_ec2:configure(string:copies("A", 20), string:copies("a", 40)),
        Fun()
    end}}.


%% input_tests converts a list of input_test specifiers into
%% an eunit test generator
-spec input_tests(string(), [input_test_spec()]) -> [tuple()].
input_tests(Response, Tests) ->
    [input_test(Response, Test) || Test <- Tests].


%%==============================================================================
%% Output test helpers
%%==============================================================================


%% returns the mock of the erlcloud_httpc function output tests expect
%% to be called.
-spec output_expect(string()) -> fun().
output_expect(Response) ->
    fun(_Url, post, _Headers, _Body, _Timeout, _Config) ->
        {ok, {{200, "OK"}, [], list_to_binary(Response)}}
    end.


%% output_test converts an output_test specifier into an eunit test generator
-spec output_test(fun(), output_test_spec()) -> tuple().
output_test(Fun, {Line, {Description, Response, Result}}) ->
    {Description, {Line, fun() ->
        meck:expect(erlcloud_httpc, request, output_expect(Response)),
        erlcloud_ec2:configure(string:copies("A", 20), string:copies("a", 40)),
        Actual = Fun(),
        ?assertEqual(Result, Actual)
    end}}.


%%==============================================================================
%% Actual test specifiers
%%==============================================================================


describe_db_instances_input_tests(_) ->
    Response =
        "<DescribeDBInstancesResponse xmlns=\"http://rds.amazonaws.com/doc/2014-10-31/\">
             <DescribeDBInstancesResult>
                 <DBInstances/>
             </DescribeDBInstancesResult>
             <ResponseMetadata>
                 <RequestId>0cbf3229-f989-11e4-a78c-f76e72479ffa</RequestId>
             </ResponseMetadata>
         </DescribeDBInstancesResponse>",

    Tests = [
        ?_rds_test(
            {"Tests describing db instances with no parameters",
             ?_f(erlcloud_rds:describe_db_instances()),
             [{"Action", "DescribeDBInstances"}]}
        ),
        ?_rds_test(
            {"Tests describing db instances with db instance id provided",
             ?_f(erlcloud_rds:describe_db_instances([
                 {db_instance_identifier, string:copies("a", 20)}
             ])),
             [{"Action", "DescribeDBInstances"},
              {"DBInstanceIdentifier", string:copies("a", 20)}]}
        ),
        ?_rds_test(
            {"Tests describing db instances with db instance id "
             "and continuation marker provided",
             ?_f(erlcloud_rds:describe_db_instances([
                 {db_instance_identifier, string:copies("a", 20)},
                 {marker, string:copies("b", 20)}
             ])),
             [{"Action", "DescribeDBInstances"},
              {"DBInstanceIdentifier", string:copies("a", 20)},
              {"Marker", string:copies("b", 20)}]}
        ),
        ?_rds_test(
            {"Tests describing db instances with db instance id, "
             "continuation marker and records limit provided",
             ?_f(erlcloud_rds:describe_db_instances([
                 {db_instance_identifier, string:copies("a", 20)},
                 {marker, string:copies("b", 20)},
                 {max_records, 100}
             ])),
             [{"Action", "DescribeDBInstances"},
              {"DBInstanceIdentifier", string:copies("a", 20)},
              {"Marker", string:copies("b", 20)},
              {"MaxRecords", "100"}]}
        )
    ],

    input_tests(Response, Tests).


describe_db_instances_output_tests(_) ->
    Test = ?_rds_test({
        "Tests describing all db instances",
        "<DescribeDBInstancesResponse xmlns=\"http://rds.amazonaws.com/doc/2014-10-31/\">
            <DescribeDBInstancesResult>
                <DBInstances>
                    <DBInstance>
                        <BackupRetentionPeriod>7</BackupRetentionPeriod>
                        <DBInstanceStatus>available</DBInstanceStatus>
                        <MultiAZ>false</MultiAZ>
                        <VpcSecurityGroups>
                            <VpcSecurityGroupMembership>
                                <Status>active</Status>
                                <VpcSecurityGroupId>sg-3b020c5e</VpcSecurityGroupId>
                            </VpcSecurityGroupMembership>
                        </VpcSecurityGroups>
                        <DBInstanceIdentifier>us-west-2-mysql-test-01</DBInstanceIdentifier>
                        <PreferredBackupWindow>11:00-11:30</PreferredBackupWindow>
                        <PreferredMaintenanceWindow>sat:08:03-sat:08:33</PreferredMaintenanceWindow>
                        <AvailabilityZone>us-west-2c</AvailabilityZone>
                        <CACertificateIdentifier>rds-ca-2015</CACertificateIdentifier>
                        <LatestRestorableTime>2015-05-19T17:10:00Z</LatestRestorableTime>
                        <ReadReplicaDBInstanceIdentifiers/>
                        <Engine>mysql</Engine>
                        <PendingModifiedValues/>
                        <LicenseModel>general-public-license</LicenseModel>
                        <DbiResourceId>db-HB5FPKLCFRW2HF2OAYZRDYLSEI</DbiResourceId>
                        <DBSubnetGroup>
                            <VpcId>vpc-7a7dc81f</VpcId>
                            <SubnetGroupStatus>Complete</SubnetGroupStatus>
                            <DBSubnetGroupDescription>default</DBSubnetGroupDescription>
                            <DBSubnetGroupName>default</DBSubnetGroupName>
                            <Subnets>
                                <Subnet>
                                    <SubnetStatus>Active</SubnetStatus>
                                    <SubnetIdentifier>subnet-e43fb281</SubnetIdentifier>
                                    <SubnetAvailabilityZone>
                                        <Name>us-west-2b</Name>
                                    </SubnetAvailabilityZone>
                                </Subnet>
                                <Subnet>
                                    <SubnetStatus>Active</SubnetStatus>
                                    <SubnetIdentifier>subnet-ebb777b2</SubnetIdentifier>
                                    <SubnetAvailabilityZone>
                                        <Name>us-west-2c</Name>
                                    </SubnetAvailabilityZone>
                                </Subnet>
                                <Subnet>
                                    <SubnetStatus>Active</SubnetStatus>
                                    <SubnetIdentifier>subnet-0006a577</SubnetIdentifier>
                                    <SubnetAvailabilityZone>
                                        <Name>us-west-2a</Name>
                                    </SubnetAvailabilityZone>
                                </Subnet>
                            </Subnets>
                        </DBSubnetGroup>
                        <Endpoint>
                            <Port>3306</Port>
                            <Address>us-west-2-mysql-test-01.csjsnznjb5ln.us-west-2.rds.amazonaws.com</Address>
                        </Endpoint>
                        <EngineVersion>5.6.22</EngineVersion>
                        <DBParameterGroups>
                            <DBParameterGroup>
                                <ParameterApplyStatus>in-sync</ParameterApplyStatus>
                                <DBParameterGroupName>default.mysql5.6</DBParameterGroupName>
                            </DBParameterGroup>
                        </DBParameterGroups>
                        <OptionGroupMemberships>
                            <OptionGroupMembership>
                                <OptionGroupName>default:mysql-5-6</OptionGroupName>
                                <Status>in-sync</Status>
                            </OptionGroupMembership>
                        </OptionGroupMemberships>
                        <PubliclyAccessible>true</PubliclyAccessible>
                        <DBSecurityGroups/>
                        <DBName>test_db</DBName>
                        <AutoMinorVersionUpgrade>true</AutoMinorVersionUpgrade>
                        <StorageType>gp2</StorageType>
                        <DbInstancePort>0</DbInstancePort>
                        <InstanceCreateTime>2015-05-19T12:06:45.971Z</InstanceCreateTime>
                        <AllocatedStorage>5</AllocatedStorage>
                        <StorageEncrypted>false</StorageEncrypted>
                        <MasterUsername>admin</MasterUsername>
                        <DBInstanceClass>db.t2.micro</DBInstanceClass>
                    </DBInstance>
                </DBInstances>
            </DescribeDBInstancesResult>
            <ResponseMetadata>
                <RequestId>7a83f2a8-fe4a-11e4-bdf0-3b866ab818a9</RequestId>
            </ResponseMetadata>
        </DescribeDBInstancesResponse>",
        {ok, [[
            {allocated_storage, 5},
            {auto_minor_version_upgrade, true},
            {availability_zone, "us-west-2c"},
            {backup_retention_period, 7},
            {ca_certificate_identifier, "rds-ca-2015"},
            {db_instance_class, "db.t2.micro"},
            {db_instance_identifier, "us-west-2-mysql-test-01"},
            {db_instance_status, "available"},
            {db_name, "test_db"},
            {db_parameter_groups, [[
                {db_parameter_group_name, "default.mysql5.6"},
                {parameter_apply_status, "in-sync"}
            ]]},
            {db_subnet_group, [
                {db_subnet_group_description,"default"},
                {db_subnet_group_name,"default"},
                {subnet_group_status,"Complete"},
                {subnets,[
                    [{subnet_availability_zone, [{name, "us-west-2b"}]},
                     {subnet_identifier, "subnet-e43fb281"},
                     {subnet_status, "Active"}],
                    [{subnet_availability_zone, [{name, "us-west-2c"}]},
                     {subnet_identifier, "subnet-ebb777b2"},
                     {subnet_status, "Active"}],
                    [{subnet_availability_zone, [{name, "us-west-2a"}]},
                     {subnet_identifier,"subnet-0006a577"},
                     {subnet_status,"Active"}]
                ]},
                {vpc_id,"vpc-7a7dc81f"}
            ]},
            {db_instance_port, 0},
            {dbi_resource_id, "db-HB5FPKLCFRW2HF2OAYZRDYLSEI"},
            {endpoint,[
                {address, "us-west-2-mysql-test-01.csjsnznjb5ln.us-west-2.rds.amazonaws.com"},
                {port, 3306}
            ]},
            {engine, "mysql"},
            {engine_version, "5.6.22"},
            {instance_create_time, {{2015, 5, 19}, {12, 6, 45}}},
            {latest_restorable_time, {{2015, 5, 19}, {17, 10, 0}}},
            {license_model, "general-public-license"},
            {master_username, "admin"},
            {multi_az, false},
            {option_group_memberships, [
                [{option_group_name, "default:mysql-5-6"},
                 {status, "in-sync"}]
            ]},
            {pending_modified_values, []},
            {preferred_backup_window, "11:00-11:30"},
            {preferred_maintenance_window, "sat:08:03-sat:08:33"},
            {publicly_accessible, true},
            {read_replica_db_instance_identifiers, []},
            {storage_encrypted, false},
            {storage_type, "gp2"},
            {vpc_security_groups,[
                [{status,"active"},
                 {vpc_security_group_id,"sg-3b020c5e"}]
            ]}
        ]]}
    }),

    output_test(?_f(erlcloud_rds:describe_db_instances_all()), Test).


describe_db_security_groups_input_tests(_) ->
    Response =
        "<DescribeDBSecurityGroupsResponse xmlns=\"http://rds.amazonaws.com/doc/2014-10-31/\">
             <DescribeDBSecurityGroupsResult>
                 <DBSecurityGroups/>
             </DescribeDBSecurityGroupsResult>
             <ResponseMetadata>
                 <RequestId>8598ddf0-f982-11e4-bb76-4da4d340eb83</RequestId>
             </ResponseMetadata>
         </DescribeDBSecurityGroupsResponse>",

    Tests = [
        ?_rds_test(
            {"Tests describing db security groups with no parameters",
             ?_f(erlcloud_rds:describe_db_security_groups()),
             [{"Action", "DescribeDBSecurityGroups"}]}
        ),
        ?_rds_test(
            {"Tests describing db security groups with "
             "db security group name provided",
             ?_f(erlcloud_rds:describe_db_security_groups([
                 {db_security_group_name, string:copies("a", 20)}
             ])),
             [{"Action", "DescribeDBSecurityGroups"},
              {"DBSecurityGroupName", string:copies("a", 20)}]}
        ),
        ?_rds_test(
            {"Tests describing db security groups with db security group name "
             "and continuation marker provided",
             ?_f(erlcloud_rds:describe_db_security_groups([
                 {db_security_group_name, string:copies("a", 20)},
                 {marker, string:copies("b", 20)}
             ])),
             [{"Action", "DescribeDBSecurityGroups"},
              {"DBSecurityGroupName", string:copies("a", 20)},
              {"Marker", string:copies("b", 20)}]}
        ),
        ?_rds_test(
            {"Tests describing db security groups with db security group name, "
             "continuation marker and records limit provided",
             ?_f(erlcloud_rds:describe_db_security_groups([
                 {db_security_group_name, string:copies("a", 20)},
                 {marker, string:copies("b", 20)},
                 {max_records, 100}
             ])),
             [{"Action", "DescribeDBSecurityGroups"},
              {"DBSecurityGroupName", string:copies("a", 20)},
              {"Marker", string:copies("b", 20)},
              {"MaxRecords", "100"}]}
        )
    ],

    input_tests(Response, Tests).


describe_db_security_groups_output_tests(_) ->
    Test = ?_rds_test({
        "Tests describing all db security groups",
        "<DescribeDBSecurityGroupsResponse xmlns=\"http://rds.amazonaws.com/doc/2014-10-31/\">
            <DescribeDBSecurityGroupsResult>
                <DBSecurityGroups>
                    <DBSecurityGroup>
                        <DBSecurityGroupDescription>some description 01</DBSecurityGroupDescription>
                        <DBSecurityGroupName>some name 01</DBSecurityGroupName>
                        <EC2SecurityGroups/>
                        <IPRanges>
                            <IPRange>
                                <CIDRIP>192.0.0.0/24</CIDRIP>
                                <Status>authorized</Status>
                            </IPRange>
                            <IPRange>
                                <CIDRIP>190.0.1.0/29</CIDRIP>
                                <Status>authorized</Status>
                            </IPRange>
                            <IPRange>
                                <CIDRIP>190.0.2.0/29</CIDRIP>
                                <Status>authorized</Status>
                            </IPRange>
                            <IPRange>
                                <CIDRIP>10.0.0.0/8</CIDRIP>
                                <Status>authorized</Status>
                            </IPRange>
                        </IPRanges>
                        <OwnerId>803739043321</OwnerId>
                    </DBSecurityGroup>
                    <DBSecurityGroup>
                        <DBSecurityGroupDescription>some description 02</DBSecurityGroupDescription>
                        <DBSecurityGroupName>some name 02</DBSecurityGroupName>
                        <EC2SecurityGroups/>
                        <IPRanges/>
                        <OwnerId>803938574930</OwnerId>
                        <VpcId>vpc-1a2b3c4d</VpcId>
                    </DBSecurityGroup>
                </DBSecurityGroups>
            </DescribeDBSecurityGroupsResult>
            <ResponseMetadata>
                <RequestId>b76e692c-b98c-11d3-a907-5a2c468b9cb0</RequestId>
            </ResponseMetadata>
        </DescribeDBSecurityGroupsResponse>",
        {ok, [
            [{db_security_group_description, "some description 01"},
             {db_security_group_name, "some name 01"},
             {ip_ranges, [
                 [{cidr_ip, "192.0.0.0/24"}, {status, "authorized"}],
                 [{cidr_ip, "190.0.1.0/29"}, {status, "authorized"}],
                 [{cidr_ip, "190.0.2.0/29"}, {status, "authorized"}],
                 [{cidr_ip, "10.0.0.0/8"}, {status, "authorized"}]
             ]},
             {owner_id, "803739043321"}],
            [{db_security_group_description, "some description 02"},
             {db_security_group_name, "some name 02"},
             {owner_id, "803938574930"},
             {vpc_id, "vpc-1a2b3c4d"}]
        ]}
    }),

    output_test(?_f(erlcloud_rds:describe_db_security_groups_all()), Test).


describe_db_subnet_groups_input_tests(_) ->
    Response =
        "<DescribeDBSubnetGroupsResponse xmlns=\"http://rds.amazonaws.com/doc/2014-10-31/\">
             <DescribeDBSubnetGroupsResult>
                 <DBSubnetGroups/>
             </DescribeDBSubnetGroupsResult>
             <ResponseMetadata>
                 <RequestId>3493b81f-fbf6-11e4-982e-b9bf1678b080</RequestId>
             </ResponseMetadata>
         </DescribeDBSubnetGroupsResponse>",

    Tests = [
        ?_rds_test(
            {"Tests describing db subnet groups with no parameters",
             ?_f(erlcloud_rds:describe_db_subnet_groups()),
             [{"Action", "DescribeDBSubnetGroups"}]}
        ),
        ?_rds_test(
            {"Tests describing db subnet groups with "
             "db subnet group name provided",
             ?_f(erlcloud_rds:describe_db_subnet_groups([
                 {db_subnet_group_name, string:copies("a", 20)}
             ])),
             [{"Action", "DescribeDBSubnetGroups"},
              {"DBSubnetGroupName", string:copies("a", 20)}]}
        ),
        ?_rds_test(
            {"Tests describing db subnet groups with db subnet group name "
             "and continuation marker provided",
             ?_f(erlcloud_rds:describe_db_subnet_groups([
                 {db_subnet_group_name, string:copies("a", 20)},
                 {marker, string:copies("b", 20)}
             ])),
             [{"Action", "DescribeDBSubnetGroups"},
              {"DBSubnetGroupName", string:copies("a", 20)},
              {"Marker", string:copies("b", 20)}]}
        ),
        ?_rds_test(
            {"Tests describing db subnet groups with db subnet group name, "
             "continuation marker and records limit provided",
             ?_f(erlcloud_rds:describe_db_subnet_groups([
                 {db_subnet_group_name, string:copies("a", 20)},
                 {marker, string:copies("b", 20)},
                 {max_records, 100}
             ])),
             [{"Action", "DescribeDBSubnetGroups"},
              {"DBSubnetGroupName", string:copies("a", 20)},
              {"Marker", string:copies("b", 20)},
              {"MaxRecords", "100"}]}
        )
    ],

    input_tests(Response, Tests).


describe_db_subnet_groups_output_tests(_) ->
    Test = ?_rds_test({
        "Tests describing all db subnet groups",
        "<DescribeDBSubnetGroupsResponse xmlns=\"http://rds.amazonaws.com/doc/2014-10-31/\">
            <DescribeDBSubnetGroupsResult>
                <DBSubnetGroups>
                    <DBSubnetGroup>
                        <DBSubnetGroupDescription>some description 01</DBSubnetGroupDescription>
                        <DBSubnetGroupName>some name 01</DBSubnetGroupName>
                        <SubnetGroupStatus>Complete</SubnetGroupStatus>
                        <Subnets>
                            <Subnet>
                                <SubnetAvailabilityZone>
                                    <Name>us-west-2a</Name>
                                </SubnetAvailabilityZone>
                                <SubnetIdentifier>subnet-e8b3e5b1</SubnetIdentifier>
                                <SubnetStatus>Active</SubnetStatus>
                            </Subnet>
                        </Subnets>
                        <VpcId>vpc-e7abbdce</VpcId>
                    </DBSubnetGroup>
                    <DBSubnetGroup>
                        <DBSubnetGroupDescription>some description 02</DBSubnetGroupDescription>
                        <DBSubnetGroupName>some name 02</DBSubnetGroupName>
                        <SubnetGroupStatus>Complete</SubnetGroupStatus>
                        <Subnets>
                            <Subnet>
                                <SubnetAvailabilityZone>
                                    <Name>us-west-2a</Name>
                                </SubnetAvailabilityZone>
                                <SubnetIdentifier>subnet-d281ef8a</SubnetIdentifier>
                                <SubnetStatus>Active</SubnetStatus>
                            </Subnet>
                            <Subnet>
                                <SubnetAvailabilityZone>
                                    <Name>us-west-2c</Name>
                                </SubnetAvailabilityZone>
                                <SubnetIdentifier>subnet-b381ef9f</SubnetIdentifier>
                                <SubnetStatus>Active</SubnetStatus>
                            </Subnet>
                        </Subnets>
                        <VpcId>vpc-c1e17bb8</VpcId>
                    </DBSubnetGroup>
                </DBSubnetGroups>
            </DescribeDBSubnetGroupsResult>
            <ResponseMetadata>
                <RequestId>b783db3b-b98c-11d3-fbc7-5c0aad74da7c</RequestId>
            </ResponseMetadata>
        </DescribeDBSubnetGroupsResponse>",
        {ok, [
            [{db_subnet_group_description, "some description 01"},
             {db_subnet_group_name, "some name 01"},
             {subnet_group_status, "Complete"},
             {subnets, [
                 [{subnet_availability_zone, [{name, "us-west-2a"}]},
                  {subnet_identifier, "subnet-e8b3e5b1"},
                  {subnet_status, "Active"}]
             ]},
             {vpc_id, "vpc-e7abbdce"}],
            [{db_subnet_group_description, "some description 02"},
             {db_subnet_group_name, "some name 02"},
             {subnet_group_status, "Complete"},
             {subnets, [
                 [{subnet_availability_zone, [{name, "us-west-2a"}]},
                  {subnet_identifier, "subnet-d281ef8a"},
                  {subnet_status, "Active"}],
                 [{subnet_availability_zone, [{name, "us-west-2c"}]},
                  {subnet_identifier, "subnet-b381ef9f"},
                  {subnet_status, "Active"}]
             ]},
             {vpc_id, "vpc-c1e17bb8"}]
        ]}
    }),

    output_test(?_f(erlcloud_rds:describe_db_subnet_groups_all()), Test).