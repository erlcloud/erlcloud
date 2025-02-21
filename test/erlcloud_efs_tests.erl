-module(erlcloud_efs_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlcloud_aws.hrl").

-define(TEST_AWS_CONFIG, #aws_config{
    access_key_id = "TEST_ACCESS_KEY_ID",
    secret_access_key = "TEST_ACCESS_KEY",
    security_token = "TEST_SECURITY_TOKEN"
}).

api_test_() ->
    {
        foreach,
        fun() -> meck:new(erlcloud_httpc) end,
        fun(_) -> meck:unload() end,
        [
            fun describe_file_systems_tests/1
        ]
    }.

describe_file_systems_tests(_) ->
    [
        {
            "DescribeFileSystems",
            fun() ->
                EfsUrl = "https://elasticfilesystem.us-east-1.amazonaws.com:443/2015-02-01/file-systems",
                AwsConfig = ?TEST_AWS_CONFIG,
                meck:expect(
                    erlcloud_httpc,
                    request,
                    fun(Url, Method, _Hdrs, Body, _Timeout, AwsCfg) when
                        Url =:= EfsUrl, Method =:= get, Body =:= <<>>, AwsCfg =:= AwsConfig
                    ->
                        ResponseContent = <<
                            "{"
                              "\"FileSystems\":["
                                "{"
                                  "\"AvailabilityZoneId\":null,"
                                  "\"AvailabilityZoneName\":null,"
                                  "\"CreationTime\":1.7343072E9,"
                                  "\"CreationToken\":\"11111111-1111-1111-1111-111111111111\","
                                  "\"Encrypted\":true,"
                                  "\"FileSystemArn\":\"arn:aws:elasticfilesystem:us-east-1:111111111111:file-system/fs-00000000000000001\","
                                  "\"FileSystemId\":\"fs-00000000000000001\","
                                  "\"FileSystemProtection\":{"
                                    "\"ReplicationOverwriteProtection\":\"ENABLED\""
                                  "},"
                                  "\"KmsKeyId\":\"arn:aws:kms:us-east-1:111111111111:key/aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa\","
                                  "\"LifeCycleState\":\"available\","
                                  "\"Name\":\"efs-001\","
                                  "\"NumberOfMountTargets\":0,"
                                  "\"OwnerId\":\"111111111111\","
                                  "\"PerformanceMode\":\"generalPurpose\","
                                  "\"ProvisionedThroughputInMibps\":null,"
                                  "\"SizeInBytes\":{"
                                    "\"Timestamp\":null,"
                                    "\"Value\":6144,"
                                    "\"ValueInArchive\":0,"
                                    "\"ValueInIA\":0,"
                                    "\"ValueInStandard\":6144"
                                  "},"
                                  "\"Tags\":["
                                    "{"
                                      "\"Key\":\"Name\","
                                      "\"Value\":\"efs-001\""
                                    "}"
                                  "],"
                                  "\"ThroughputMode\":\"elastic\""
                                "},"
                                "{"
                                  "\"AvailabilityZoneId\":null,"
                                  "\"AvailabilityZoneName\":null,"
                                  "\"CreationTime\":1.7343072E9,"
                                  "\"CreationToken\":\"22222222-2222-2222-2222-222222222222\","
                                  "\"Encrypted\":true,"
                                  "\"FileSystemArn\":\"arn:aws:elasticfilesystem:us-east-1:111111111111:file-system/fs-00000000000000002\","
                                  "\"FileSystemId\":\"fs-00000000000000002\","
                                  "\"FileSystemProtection\":{"
                                    "\"ReplicationOverwriteProtection\":\"ENABLED\""
                                  "},"
                                  "\"KmsKeyId\":\"arn:aws:kms:us-east-1:111111111111:key/aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa\","
                                  "\"LifeCycleState\":\"available\","
                                  "\"Name\":\"efs-002\","
                                  "\"NumberOfMountTargets\":0,"
                                  "\"OwnerId\":\"111111111111\","
                                  "\"PerformanceMode\":\"generalPurpose\","
                                  "\"ProvisionedThroughputInMibps\":null,"
                                  "\"SizeInBytes\":{"
                                    "\"Timestamp\":null,"
                                    "\"Value\":6144,"
                                    "\"ValueInArchive\":0,"
                                    "\"ValueInIA\":0,"
                                    "\"ValueInStandard\":6144"
                                  "},"
                                  "\"Tags\":["
                                    "{"
                                      "\"Key\":\"Name\","
                                      "\"Value\":\"efs-002\""
                                    "}"
                                  "],"
                                  "\"ThroughputMode\":\"elastic\""
                                "}"
                              "],"
                              "\"Marker\":null,"
                              "\"NextMarker\":null"
                            "}"
                        >>,
                        {ok, {{200, "OK"}, [], ResponseContent}}
                    end
                ),
                Params = [],
                Result = erlcloud_efs:describe_file_systems(AwsConfig, Params),
                FileSystems = [
                    [
                        {<<"AvailabilityZoneId">>, null},
                        {<<"AvailabilityZoneName">>, null},
                        {<<"CreationTime">>, 1.7343072e9},
                        {<<"CreationToken">>, <<"11111111-1111-1111-1111-111111111111">>},
                        {<<"Encrypted">>, true},
                        {<<"FileSystemArn">>,
                            <<"arn:aws:elasticfilesystem:us-east-1:111111111111:file-system/fs-00000000000000001">>},
                        {<<"FileSystemId">>, <<"fs-00000000000000001">>},
                        {<<"FileSystemProtection">>, [
                            {<<"ReplicationOverwriteProtection">>, <<"ENABLED">>}
                        ]},
                        {<<"KmsKeyId">>,
                            <<"arn:aws:kms:us-east-1:111111111111:key/aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa">>},
                        {<<"LifeCycleState">>, <<"available">>},
                        {<<"Name">>, <<"efs-001">>},
                        {<<"NumberOfMountTargets">>, 0},
                        {<<"OwnerId">>, <<"111111111111">>},
                        {<<"PerformanceMode">>, <<"generalPurpose">>},
                        {<<"ProvisionedThroughputInMibps">>, null},
                        {<<"SizeInBytes">>, [
                            {<<"Timestamp">>, null},
                            {<<"Value">>, 6144},
                            {<<"ValueInArchive">>, 0},
                            {<<"ValueInIA">>, 0},
                            {<<"ValueInStandard">>, 6144}
                        ]},
                        {<<"Tags">>, [[{<<"Key">>, <<"Name">>}, {<<"Value">>, <<"efs-001">>}]]},
                        {<<"ThroughputMode">>, <<"elastic">>}
                    ],
                    [
                        {<<"AvailabilityZoneId">>, null},
                        {<<"AvailabilityZoneName">>, null},
                        {<<"CreationTime">>, 1.7343072e9},
                        {<<"CreationToken">>, <<"22222222-2222-2222-2222-222222222222">>},
                        {<<"Encrypted">>, true},
                        {<<"FileSystemArn">>,
                            <<"arn:aws:elasticfilesystem:us-east-1:111111111111:file-system/fs-00000000000000002">>},
                        {<<"FileSystemId">>, <<"fs-00000000000000002">>},
                        {<<"FileSystemProtection">>, [
                            {<<"ReplicationOverwriteProtection">>, <<"ENABLED">>}
                        ]},
                        {<<"KmsKeyId">>,
                            <<"arn:aws:kms:us-east-1:111111111111:key/aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa">>},
                        {<<"LifeCycleState">>, <<"available">>},
                        {<<"Name">>, <<"efs-002">>},
                        {<<"NumberOfMountTargets">>, 0},
                        {<<"OwnerId">>, <<"111111111111">>},
                        {<<"PerformanceMode">>, <<"generalPurpose">>},
                        {<<"ProvisionedThroughputInMibps">>, null},
                        {<<"SizeInBytes">>, [
                            {<<"Timestamp">>, null},
                            {<<"Value">>, 6144},
                            {<<"ValueInArchive">>, 0},
                            {<<"ValueInIA">>, 0},
                            {<<"ValueInStandard">>, 6144}
                        ]},
                        {<<"Tags">>, [[{<<"Key">>, <<"Name">>}, {<<"Value">>, <<"efs-002">>}]]},
                        {<<"ThroughputMode">>, <<"elastic">>}
                    ]
                ],
                ?assertEqual({ok, FileSystems, undefined}, Result)
            end
        },
        {
            "DescribeFileSystems [all]",
            fun() ->
                EfsUrl1 = "https://elasticfilesystem.us-east-1.amazonaws.com:443/2015-02-01/file-systems?MaxItems=1",
                EfsUrl2 = "https://elasticfilesystem.us-east-1.amazonaws.com:443/2015-02-01/file-systems?Marker=TEST_NEXT_MARKER&MaxItems=1",
                AwsConfig = ?TEST_AWS_CONFIG,
                meck:expect(
                    erlcloud_httpc,
                    request,
                    fun
                        (Url, Method, _Hdrs, Body, _Timeout, AwsCfg) when
                            Url =:= EfsUrl1, Method =:= get, Body =:= <<>>, AwsCfg =:= AwsConfig
                        ->
                            ResponseContent = <<
                                "{"
                                  "\"FileSystems\":["
                                    "{"
                                      "\"AvailabilityZoneId\":null,"
                                      "\"AvailabilityZoneName\":null,"
                                      "\"CreationTime\":1.7343072E9,"
                                      "\"CreationToken\":\"11111111-1111-1111-1111-111111111111\","
                                      "\"Encrypted\":true,"
                                      "\"FileSystemArn\":\"arn:aws:elasticfilesystem:us-east-1:111111111111:file-system/fs-00000000000000001\","
                                      "\"FileSystemId\":\"fs-00000000000000001\","
                                      "\"FileSystemProtection\":{"
                                        "\"ReplicationOverwriteProtection\":\"ENABLED\""
                                      "},"
                                      "\"KmsKeyId\":\"arn:aws:kms:us-east-1:111111111111:key/aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa\","
                                      "\"LifeCycleState\":\"available\","
                                      "\"Name\":\"efs-001\","
                                      "\"NumberOfMountTargets\":0,"
                                      "\"OwnerId\":\"111111111111\","
                                      "\"PerformanceMode\":\"generalPurpose\","
                                      "\"ProvisionedThroughputInMibps\":null,"
                                      "\"SizeInBytes\":{"
                                        "\"Timestamp\":null,"
                                        "\"Value\":6144,"
                                        "\"ValueInArchive\":0,"
                                        "\"ValueInIA\":0,"
                                        "\"ValueInStandard\":6144"
                                      "},"
                                      "\"Tags\":["
                                        "{"
                                          "\"Key\":\"Name\","
                                          "\"Value\":\"efs-001\""
                                        "}"
                                      "],"
                                      "\"ThroughputMode\":\"elastic\""
                                    "}"
                                  "],"
                                  "\"Marker\":null,"
                                  "\"NextMarker\":\"TEST_NEXT_MARKER\""
                                "}"
                            >>,
                            {ok, {{200, "OK"}, [], ResponseContent}};
                        (Url, Method, _Hdrs, Body, _Timeout, AwsCfg) when
                            Url =:= EfsUrl2, Method =:= get, Body =:= <<>>, AwsCfg =:= AwsConfig
                        ->
                            ResponseContent = <<
                                "{"
                                  "\"FileSystems\":["
                                    "{"
                                      "\"AvailabilityZoneId\":null,"
                                      "\"AvailabilityZoneName\":null,"
                                      "\"CreationTime\":1.7343072E9,"
                                      "\"CreationToken\":\"22222222-2222-2222-2222-222222222222\","
                                      "\"Encrypted\":true,"
                                      "\"FileSystemArn\":\"arn:aws:elasticfilesystem:us-east-1:111111111111:file-system/fs-00000000000000002\","
                                      "\"FileSystemId\":\"fs-00000000000000002\","
                                      "\"FileSystemProtection\":{"
                                        "\"ReplicationOverwriteProtection\":\"ENABLED\""
                                      "},"
                                      "\"KmsKeyId\":\"arn:aws:kms:us-east-1:111111111111:key/aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa\","
                                      "\"LifeCycleState\":\"available\","
                                      "\"Name\":\"efs-002\","
                                      "\"NumberOfMountTargets\":0,"
                                      "\"OwnerId\":\"111111111111\","
                                      "\"PerformanceMode\":\"generalPurpose\","
                                      "\"ProvisionedThroughputInMibps\":null,"
                                      "\"SizeInBytes\":{"
                                        "\"Timestamp\":null,"
                                        "\"Value\":6144,"
                                        "\"ValueInArchive\":0,"
                                        "\"ValueInIA\":0,"
                                        "\"ValueInStandard\":6144"
                                      "},"
                                      "\"Tags\":["
                                        "{"
                                          "\"Key\":\"Name\","
                                          "\"Value\":\"efs-002\""
                                        "}"
                                      "],"
                                      "\"ThroughputMode\":\"elastic\""
                                    "}"
                                  "],"
                                  "\"Marker\":\"TEST_NEXT_MARKER\","
                                  "\"NextMarker\":null"
                                "}"
                            >>,
                            {ok, {{200, "OK"}, [], ResponseContent}}
                    end
                ),
                Params = [{<<"MaxItems">>, 1}],
                Result = erlcloud_efs:describe_file_systems_all(AwsConfig, Params),
                FileSystems = [
                    [
                        {<<"AvailabilityZoneId">>, null},
                        {<<"AvailabilityZoneName">>, null},
                        {<<"CreationTime">>, 1.7343072e9},
                        {<<"CreationToken">>, <<"11111111-1111-1111-1111-111111111111">>},
                        {<<"Encrypted">>, true},
                        {<<"FileSystemArn">>,
                            <<"arn:aws:elasticfilesystem:us-east-1:111111111111:file-system/fs-00000000000000001">>},
                        {<<"FileSystemId">>, <<"fs-00000000000000001">>},
                        {<<"FileSystemProtection">>, [
                            {<<"ReplicationOverwriteProtection">>, <<"ENABLED">>}
                        ]},
                        {<<"KmsKeyId">>,
                            <<"arn:aws:kms:us-east-1:111111111111:key/aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa">>},
                        {<<"LifeCycleState">>, <<"available">>},
                        {<<"Name">>, <<"efs-001">>},
                        {<<"NumberOfMountTargets">>, 0},
                        {<<"OwnerId">>, <<"111111111111">>},
                        {<<"PerformanceMode">>, <<"generalPurpose">>},
                        {<<"ProvisionedThroughputInMibps">>, null},
                        {<<"SizeInBytes">>, [
                            {<<"Timestamp">>, null},
                            {<<"Value">>, 6144},
                            {<<"ValueInArchive">>, 0},
                            {<<"ValueInIA">>, 0},
                            {<<"ValueInStandard">>, 6144}
                        ]},
                        {<<"Tags">>, [[{<<"Key">>, <<"Name">>}, {<<"Value">>, <<"efs-001">>}]]},
                        {<<"ThroughputMode">>, <<"elastic">>}
                    ],
                    [
                        {<<"AvailabilityZoneId">>, null},
                        {<<"AvailabilityZoneName">>, null},
                        {<<"CreationTime">>, 1.7343072e9},
                        {<<"CreationToken">>, <<"22222222-2222-2222-2222-222222222222">>},
                        {<<"Encrypted">>, true},
                        {<<"FileSystemArn">>,
                            <<"arn:aws:elasticfilesystem:us-east-1:111111111111:file-system/fs-00000000000000002">>},
                        {<<"FileSystemId">>, <<"fs-00000000000000002">>},
                        {<<"FileSystemProtection">>, [
                            {<<"ReplicationOverwriteProtection">>, <<"ENABLED">>}
                        ]},
                        {<<"KmsKeyId">>,
                            <<"arn:aws:kms:us-east-1:111111111111:key/aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa">>},
                        {<<"LifeCycleState">>, <<"available">>},
                        {<<"Name">>, <<"efs-002">>},
                        {<<"NumberOfMountTargets">>, 0},
                        {<<"OwnerId">>, <<"111111111111">>},
                        {<<"PerformanceMode">>, <<"generalPurpose">>},
                        {<<"ProvisionedThroughputInMibps">>, null},
                        {<<"SizeInBytes">>, [
                            {<<"Timestamp">>, null},
                            {<<"Value">>, 6144},
                            {<<"ValueInArchive">>, 0},
                            {<<"ValueInIA">>, 0},
                            {<<"ValueInStandard">>, 6144}
                        ]},
                        {<<"Tags">>, [[{<<"Key">>, <<"Name">>}, {<<"Value">>, <<"efs-002">>}]]},
                        {<<"ThroughputMode">>, <<"elastic">>}
                    ]
                ],
                ?assertEqual({ok, FileSystems}, Result)
            end
        },
        {
            "DescribeFileSystems [filesystem not found]",
            fun() ->
                AwsConfig = ?TEST_AWS_CONFIG,
                EfsUrl = "https://elasticfilesystem.us-east-1.amazonaws.com:443/2015-02-01/file-systems?FileSystemId=fs-1234bad",
                meck:expect(
                    erlcloud_httpc,
                    request,
                    fun(Url, Method, _Hdrs, Body, _Timeout, AwsCfg) when
                        Url =:= EfsUrl, Method =:= get, Body =:= <<>>, AwsCfg =:= AwsConfig
                    ->
                        ResponseContent = <<
                            "{"
                              "\"ErrorCode\":\"FileSystemNotFound\","
                              "\"Message\":\"File system 'fs-1234bad' does not exist.\""
                            "}"
                        >>,
                        ResponseHeaders = [
                            {"x-amzn-errortype", "FileSystemNotFound:"},
                            {"content-type", "application/json"}
                        ],
                        {ok, {{404, "Not Found"}, ResponseHeaders, ResponseContent}}
                    end
                ),
                Params = [{<<"FileSystemId">>, <<"fs-1234bad">>}],
                Result = erlcloud_efs:describe_file_systems(AwsConfig, Params),
                ErrorType = <<"FileSystemNotFound">>,
                ErrorMessage = <<"File system 'fs-1234bad' does not exist.">>,
                ?assertEqual({error, {ErrorType, ErrorMessage}}, Result)
            end
        }
    ].
