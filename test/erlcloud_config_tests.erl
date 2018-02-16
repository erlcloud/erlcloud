-module(erlcloud_config_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlcloud_aws.hrl").

%% API
-export([]).

-define(COMPLIANCE_TYPES, <<"ComplianceTypes">>).
-define(CONFIG_RULE_NAMES, <<"ConfigRuleNames">>).
-define(CONFIG_RULE_NAME, <<"ConfigRuleName">>).
-define(LIMIT, <<"Limit">>).
-define(RESOURCE_ID, <<"ResourceId">>).
-define(RESOURCE_TYPE, <<"ResourceType">>).
-define(RESOURCE_NAME, <<"ResourceName">>).
-define(RESOURCE_IDS, <<"ResourceIds">>).
-define(RESOURCE_TYPES, <<"ResourceTypes">>).
-define(CONFIGURATION_RECORDER_NAMES, <<"ConfigurationRecorderNames">>).
-define(DELIVERY_CHANNEL_NAMES, <<"DeliveryChannelNames">>).
-define(CHRONOLOGICAL_ORDER, <<"chronologicalOrder">>).
-define(EARLIER_TIME, <<"earlierTime">>).
-define(LATER_TIME, <<"laterTime">>).
-define(INCLUDE_DELETED_RESOURCES, <<"includeDeletedResources">>).

-define(TEST_COMPLIANCE_TYPES, [<<"COMPLIANT">>, <<"NON_COMPLIANT">>]).
-define(TEST_CONFIG_RULE_NAMES, [<<"acm-certificate-expiration-check">>, <<"autoscaling-group-elb-healthcheck-required">>]).
-define(TEST_CONFIG_RULE_NAME, <<"acm-certificate-expiration-check">>).
-define(TEST_LIMIT, 99).
-define(TEST_RESOURCE_NAME, <<"string">>).
-define(TEST_RESOURCE_ID, <<"i-0160f625a53e99c41">>).
-define(TEST_RESOURCE_TYPE, <<"AWS::EC2::Instance">>).
-define(TEST_RESOURCE_TYPES, [<<"AWS::EC2::Instance">>]).

-define(EHTTPC, erlcloud_httpc).


-define(DESCRIBE_COMPLIANCE_BY_CONFIG_RULE_RESP,
    #{<<"ComplianceByConfigRules">> => [
            #{<<"Compliance">> =>
                #{<<"ComplianceContributorCount">> =>
                    #{
                        <<"CapExceeded">> => false,
                        <<"CappedCount">> => 10
                    },
                    <<"ComplianceType">> => <<"COMPLIANT">>
                },
                <<"ConfigRuleName">> => <<"acm-certificate-expiration-check">>
            }
        ],
        <<"NextToken">> => <<"">>
    }
).

-define(DESCRIBE_COMPLIANCE_BY_RESOURCE_RESP,
    #{<<"ComplianceByResources">> => [
            #{<<"Compliance">> => #{
                    <<"ComplianceContributorCount">> => #{
                        <<"CapExceeded">> => false,
                        <<"CappedCount">> => 10
                    },
                    <<"ComplianceType">> => <<"COMPLIANT">>
                },
                <<"ResourceId">> => ?TEST_RESOURCE_ID,
                <<"ResourceType">> => ?TEST_RESOURCE_TYPE
            }
        ],
        <<"NextToken">> => <<"string">>
    }
).

-define(DESCRIBE_CONFIG_RULE_EVALUATION_STATUS_RESP,
    #{<<"ConfigRulesEvaluationStatus">> => [
            #{
                <<"ConfigRuleArn">> => <<"string">>,
                <<"ConfigRuleId">> => <<"string">>,
                <<"ConfigRuleName">> => <<"string">>,
                <<"FirstActivatedTime">> => 1234567890,
                <<"FirstEvaluationStarted">> => true,
                <<"LastErrorCode">> => <<"string">>,
                <<"LastErrorMessage">> => <<"string">>,
                <<"LastFailedEvaluationTime">> => 1234567890,
                <<"LastFailedInvocationTime">> => 1234567890,
                <<"LastSuccessfulEvaluationTime">> => 1234567890,
                <<"LastSuccessfulInvocationTime">> => 1234567890
            }
        ],
        <<"NextToken">> => <<"string">>
    }
).


-define(DESCRIBE_CONFIG_RULES_RESP,
    #{<<"ConfigRules">> => [
        #{<<"ConfigRuleArn">> => <<"string">>,
              <<"ConfigRuleId">> => <<"string">>,
              <<"ConfigRuleName">> => <<"string">>,
              <<"ConfigRuleState">> => <<"string">>,
              <<"Description">> => <<"string">>,
              <<"InputParameters">> => <<"string">>,
              <<"MaximumExecutionFrequency">> => <<"string">>,
              <<"Scope">> => #{
                   <<"ComplianceResourceId">> => <<"string">>,
                   <<"ComplianceResourceTypes">> => [<<"string">>, <<"string">> ],
                   <<"TagKey">> => <<"string">>,
                   <<"TagValue">> => <<"string">>
              },
              <<"Source">> => #{
                   <<"Owner">> => <<"string">>,
                   <<"SourceDetails">> => [
                        #{<<"EventSource">> => <<"string">>,
                            <<"MaximumExecutionFrequency">> => <<"string">>,
                            <<"MessageType">> => <<"string">>
                        }
                   ],
                   <<"SourceIdentifier">> => <<"string">>
                }
            }
        ],
        <<"NextToken">> => <<"string">>
    }
).

-define(DESCRIBE_CONFIGURATION_RECORDERS_RESP,
    #{<<"ConfigurationRecorders">> => [
        #{
            <<"name">> => <<"string">>,
            <<"recordingGroup">> => #{
                <<"allSupported">> => true,
                <<"includeGlobalResourceTypes">> => true,
                <<"resourceTypes">> => [ <<"string">> ]
            },
            <<"roleARN">> => <<"string">>
        }
    ]}
).

-define(DESCRIBE_CONFIGURATION_RECORDER_STATUS_RESP,
    #{<<"ConfigurationRecordersStatus">> => [
        #{
            <<"lastErrorCode">> => <<"string">>,
            <<"lastErrorMessage">> => <<"string">>,
            <<"lastStartTime">> => 1234567890,
            <<"lastStatus">> => <<"string">>,
            <<"lastStatusChangeTime">> => 1234567890,
            <<"lastStopTime">> => 1234567890,
            <<"name">> => <<"string">>,
            <<"recording">> => true
        }
    ]}
).

-define(DESCRIBE_DELIVERY_CHANNELS_RESP,
    #{<<"DeliveryChannels">> => [
        #{
            <<"configSnapshotDeliveryProperties">> => #{
                <<"deliveryFrequency">> => <<"string">>
            },
            <<"name">> => <<"string">>,
            <<"s3BucketName">> => <<"string">>,
            <<"s3KeyPrefix">> => <<"string">>,
            <<"snsTopicARN">> => <<"string">>
        }
    ]}
).


-define(DESCRIBE_DELIVERY_CHANNEL_STATUS_RESP,
    #{<<"DeliveryChannelsStatus">> => [
        #{
            <<"configHistoryDeliveryInfo">> => #{
                <<"lastAttemptTime">> => 1234567890,
                <<"lastErrorCode">> => <<"string">>,
                <<"lastErrorMessage">> => <<"string">>,
                <<"lastStatus">> => <<"string">>,
                <<"lastSuccessfulTime">> => 1234567890,
                <<"nextDeliveryTime">> => 1234567890
            },
            <<"configSnapshotDeliveryInfo">> => #{
                <<"lastAttemptTime">> => 1234567890,
                <<"lastErrorCode">> => <<"string">>,
                <<"lastErrorMessage">> => <<"string">>,
                <<"lastStatus">> => <<"string">>,
                <<"lastSuccessfulTime">> => 1234567890,
                <<"nextDeliveryTime">> => 1234567890
            },
            <<"configStreamDeliveryInfo">> => #{
                <<"lastErrorCode">> => <<"string">>,
                <<"lastErrorMessage">> => <<"string">>,
                <<"lastStatus">> => <<"string">>,
                <<"lastStatusChangeTime">> => 1234567890
            },
            <<"name">> => <<"string">>
        }
    ]}
).

-define(GET_COMPLIANCE_DETAILS_BY_CONFIG_RULE_RESP,
    #{<<"EvaluationResults">> => [
            #{
                <<"Annotation">> => <<"string">>,
                <<"ComplianceType">> => <<"COMPLIANT">>,
                <<"ConfigRuleInvokedTime">> => 1234567890,
                <<"EvaluationResultIdentifier">> => #{
                    <<"EvaluationResultQualifier">> => #{
                        <<"ConfigRuleName">> => <<"string">>,
                        <<"ResourceId">> => <<"string">>,
                        <<"ResourceType">> => <<"string">>
                    },
                    <<"OrderingTimestamp">> => 1234567890
                },
                <<"ResultRecordedTime">> => 1234567890,
                <<"ResultToken">> => <<"string">>
            }
        ],
        <<"NextToken">> => <<"">>
    }
).

-define(GET_COMPLIANCE_DETAILS_BY_RESOURCE_RESP,
    #{<<"EvaluationResults">> => [
            #{
                <<"Annotation">> => <<"string">>,
                <<"ComplianceType">> => <<"COMPLIANT">>,
                <<"ConfigRuleInvokedTime">> => 1234567890,
                <<"EvaluationResultIdentifier">> => #{
                    <<"EvaluationResultQualifier">> => #{
                        <<"ConfigRuleName">> => <<"string">>,
                        <<"ResourceId">> => <<"string">>,
                        <<"ResourceType">> => <<"string">>
                    },
                    <<"OrderingTimestamp">> => 1234567890
                },
                <<"ResultRecordedTime">> => 1234567890,
                <<"ResultToken">> => <<"string">>
            }
        ],
        <<"NextToken">> => <<"string">>
    }
).

-define(GET_COMPLIANCE_SUMMARY_BY_CONFIG_RULE_RESP,
    #{<<"ComplianceSummary">> => #{
            <<"ComplianceSummaryTimestamp">> => 01234567890,
            <<"CompliantResourceCount">> => #{
                <<"CapExceeded">> => false,
                <<"CappedCount">> => 01234567890
            },
            <<"NonCompliantResourceCount">> => #{
                <<"CapExceeded">> => true,
                <<"CappedCount">> => 01234567890
            }
        }
    }
).

-define(GET_COMPLIANCE_SUMMARY_BY_RESOURCE_TYPE_RESP,
    #{<<"ComplianceSummariesByResourceType">> => [
        #{<<"ComplianceSummary">> => #{
                <<"ComplianceSummaryTimestamp">> => 01234567890,
                <<"CompliantResourceCount">> => #{
                    <<"CapExceeded">> => false,
                    <<"CappedCount">> => 01234567890
                },
                <<"NonCompliantResourceCount">> => #{
                    <<"CapExceeded">> => true,
                    <<"CappedCount">> => 01234567890
                }
            },
            <<"ResourceType">> => <<"string">>
        }
    ]}
).

-define(GET_DISCOVERED_RESOURCE_COUNTS_RESP,
    #{<<"nextToken">> => <<"string">>,
        <<"resourceCounts">> => [
            #{
                <<"count">> => 10,
                <<"resourceType">> => <<"string">>
            }
        ],
        <<"totalDiscoveredResources">> => 1234567890
    }
).

-define(GET_RESOURCE_CONFIG_HISTORY_RESP,
    #{<<"chronologicalOrder">> => <<"string">>,
        <<"earlierTime">> => 01234567890,
        <<"laterTime">> => 01234567890,
        <<"limit">> => 01234567890,
        <<"nextToken">> => <<"string">>,
        <<"resourceId">> => <<"string">>,
        <<"resourceType">> => <<"string">>
    }
).

-define(LIST_DISCOVERED_RESOURCES_RESP,
    #{<<"nextToken">> => <<"string">>,
        <<"resourceIdentifiers">> => [
            #{<<"resourceDeletionTime">> => 01234567890,
                <<"resourceId">> => <<"string">>,
                <<"resourceName">> => <<"string">>,
                <<"resourceType">> => <<"string">>
            }
        ]
    }
).

setup() ->
    configure("test-access-key", "test-secret-key"),
    meck:new(?EHTTPC, [passthrough]),
    meck:expect(?EHTTPC, request, 6, fun do_erlcloud_httpc_request/6),
    [?EHTTPC].

erlcloud_config_test_() ->
    {
        foreach,
        fun setup/0,
        fun meck:unload/1,
        [
            fun test_describe_compliance_by_config_rule/0,
            fun test_describe_compliance_by_resource/0,
            fun test_describe_config_rule_evaluation_status/0,
            fun test_describe_config_rules/0,
            fun test_describe_configuration_recorders/0,
            fun test_describe_configuration_recorder_status/0,
            fun test_describe_delivery_channels/0,
            fun test_describe_delivery_channel_status/0,
            fun test_get_compliance_details_by_config_rule/0,
            fun test_get_compliance_details_by_resource/0,
            fun test_get_compliance_summary_by_config_rule/0,
            fun test_get_compliance_summary_by_resource_type/0,
            fun test_get_discovered_resource_counts/0,
            fun test_get_resource_config_history/0,
            fun test_list_discovered_resources/0
        ]
    }.

test_describe_compliance_by_config_rule() ->
    Request = #{?COMPLIANCE_TYPES => ?TEST_COMPLIANCE_TYPES,
        ?CONFIG_RULE_NAMES        => ?TEST_CONFIG_RULE_NAMES,
        ?NEXT_TOKEN_LABEL         => <<"">>},
    Expected = {ok, ?DESCRIBE_COMPLIANCE_BY_CONFIG_RULE_RESP},
    TestFun  =
        fun() ->
            erlcloud_config:describe_compliance_by_config_rule(Request, default_config())
        end,
    do_test(Request, Expected, TestFun).

test_describe_compliance_by_resource() ->
    Request = #{?COMPLIANCE_TYPES => ?TEST_COMPLIANCE_TYPES,
        ?LIMIT                    => ?TEST_LIMIT,
        ?NEXT_TOKEN_LABEL         => <<"string">>,
        ?RESOURCE_ID              => ?TEST_RESOURCE_ID,
        ?RESOURCE_TYPE            => ?TEST_RESOURCE_TYPE},
    Expected = {ok, ?DESCRIBE_COMPLIANCE_BY_RESOURCE_RESP},
    TestFun  =
        fun() ->
            erlcloud_config:describe_compliance_by_resource(Request, default_config())
        end,
    do_test(Request, Expected, TestFun).

test_describe_config_rule_evaluation_status() ->
    Request = #{?CONFIG_RULE_NAMES => ?TEST_CONFIG_RULE_NAMES,
        ?LIMIT                     => 10,
        ?NEXT_TOKEN_LABEL          => <<"string">>},
    Expected = {ok, ?DESCRIBE_CONFIG_RULE_EVALUATION_STATUS_RESP},
    TestFun  =
        fun() ->
            erlcloud_config:describe_config_rule_evaluation_status(Request, default_config())
        end,
    do_test(Request, Expected, TestFun).

test_describe_config_rules() ->
    Request = #{?CONFIG_RULE_NAMES => ?TEST_CONFIG_RULE_NAMES,
        ?NEXT_TOKEN_LABEL          => <<"string">>},
    Expected = {ok, ?DESCRIBE_CONFIG_RULES_RESP},
    TestFun  =
        fun() ->
            erlcloud_config:describe_config_rules(Request, default_config())
        end,
    do_test(Request, Expected, TestFun).

test_describe_configuration_recorders() ->
    Request = #{?CONFIGURATION_RECORDER_NAMES => [<<"name1">>, <<"name2">>]},
    Expected = {ok, ?DESCRIBE_CONFIGURATION_RECORDERS_RESP},
    TestFun  =
        fun() ->
            erlcloud_config:describe_configuration_recorders(Request, default_config())
        end,
    do_test(Request, Expected, TestFun).

test_describe_configuration_recorder_status() ->
    Request = #{?CONFIGURATION_RECORDER_NAMES => [<<"name1">>, <<"name2">>]},
    Expected = {ok, ?DESCRIBE_CONFIGURATION_RECORDER_STATUS_RESP},
    TestFun  =
        fun() ->
            erlcloud_config:describe_configuration_recorder_status(Request, default_config())
        end,
    do_test(Request, Expected, TestFun).

test_describe_delivery_channels() ->
    Request = #{?DELIVERY_CHANNEL_NAMES => [<<"name1">>, <<"name2">>]},
    Expected = {ok, ?DESCRIBE_DELIVERY_CHANNELS_RESP},
    TestFun  =
        fun() ->
            erlcloud_config:describe_delivery_channels(Request, default_config())
        end,
    do_test(Request, Expected, TestFun).

test_describe_delivery_channel_status() ->
    Request = #{?DELIVERY_CHANNEL_NAMES => [<<"name1">>, <<"name2">>]},
    Expected = {ok, ?DESCRIBE_DELIVERY_CHANNEL_STATUS_RESP},
    TestFun  =
        fun() ->
            erlcloud_config:describe_delivery_channel_status(Request, default_config())
        end,
    do_test(Request, Expected, TestFun).

test_get_compliance_details_by_config_rule() ->
    Request = #{?COMPLIANCE_TYPES => ?TEST_COMPLIANCE_TYPES,
        ?CONFIG_RULE_NAME         => ?TEST_CONFIG_RULE_NAME,
        ?LIMIT                    => 88,
        ?NEXT_TOKEN_LABEL         => <<"">>},
    Expected = {ok, ?GET_COMPLIANCE_DETAILS_BY_CONFIG_RULE_RESP},
    TestFun  =
        fun() ->
            erlcloud_config:get_compliance_details_by_config_rule(?TEST_CONFIG_RULE_NAME, Request, default_config())
        end,
    do_test(Request, Expected, TestFun).

test_get_compliance_details_by_resource() ->
    Request = #{?COMPLIANCE_TYPES => ?TEST_COMPLIANCE_TYPES,
        ?NEXT_TOKEN_LABEL         => <<"string">>,
        ?RESOURCE_ID              => ?TEST_RESOURCE_ID,
        ?RESOURCE_TYPE            => ?TEST_RESOURCE_TYPE},
    Expected = {ok, ?GET_COMPLIANCE_DETAILS_BY_RESOURCE_RESP},
    TestFun  =
        fun() ->
            erlcloud_config:get_compliance_details_by_resource(?TEST_RESOURCE_ID,
                ?TEST_RESOURCE_TYPE,
                Request,
                default_config())
        end,
    do_test(Request, Expected, TestFun).

test_get_compliance_summary_by_config_rule() ->
    Request = #{},
    Expected = {ok, ?GET_COMPLIANCE_SUMMARY_BY_CONFIG_RULE_RESP},
    TestFun  =
        fun() ->
            erlcloud_config:get_compliance_summary_by_config_rule(default_config())
        end,
    do_test(Request, Expected, TestFun).

test_get_compliance_summary_by_resource_type() ->
    Request = #{?RESOURCE_TYPES => ?TEST_RESOURCE_TYPES},
    Expected = {ok, ?GET_COMPLIANCE_SUMMARY_BY_RESOURCE_TYPE_RESP},
    TestFun  =
        fun() ->
            erlcloud_config:get_compliance_summary_by_resource_type(Request, default_config())
        end,
    do_test(Request, Expected, TestFun).

test_get_discovered_resource_counts() ->
    Request = #{?LIMIT      => 77,
        ?NEXT_TOKEN_LABEL   => <<"string">>,
        ?RESOURCE_TYPES     => ?TEST_RESOURCE_TYPES},
    Expected = {ok, ?GET_DISCOVERED_RESOURCE_COUNTS_RESP},
    TestFun  =
        fun() ->
            erlcloud_config:get_discovered_resource_counts(Request, default_config())
        end,
    do_test(Request, Expected, TestFun).

test_get_resource_config_history() ->
    Request = #{?CHRONOLOGICAL_ORDER => <<"Reverse">>,
        ?EARLIER_TIME                => 1234567890,
        ?LATER_TIME                  => 1234567890,
        ?LIMIT                       => 66,
        ?NEXT_TOKEN_LABEL            => <<"string">>,
        ?RESOURCE_ID                 => ?TEST_RESOURCE_ID,
        ?RESOURCE_TYPE               => ?TEST_RESOURCE_TYPE},
    Expected = {ok, ?GET_RESOURCE_CONFIG_HISTORY_RESP},
    TestFun  =
        fun() ->
            erlcloud_config:get_resource_config_history(?TEST_RESOURCE_ID,
                ?TEST_RESOURCE_TYPE,
                Request,
                default_config())
        end,
    do_test(Request, Expected, TestFun).

test_list_discovered_resources() ->
    Request = #{?INCLUDE_DELETED_RESOURCES => true,
        ?LIMIT                             => ?TEST_LIMIT,
        ?NEXT_TOKEN_LABEL                  => <<"string">>,
        ?RESOURCE_IDS                      => [],
        ?RESOURCE_NAME                     => ?TEST_RESOURCE_NAME,
        ?RESOURCE_TYPE                     => ?TEST_RESOURCE_TYPE},
    Expected = {ok, ?LIST_DISCOVERED_RESOURCES_RESP},
    TestFun  =
        fun() ->
            erlcloud_config:list_discovered_resources(?TEST_RESOURCE_TYPE,
                Request,
                default_config())
        end,
    do_test(Request, Expected, TestFun).

do_test(Request, ExpectedResult, TestedFun) ->
    configure("test-access-key", "test-secret-key"),
    ?assertEqual(ExpectedResult, TestedFun()),
    Encoded = jsx:encode(Request),
    ?assertMatch([{_, {?EHTTPC, request, [_, post, _, Encoded, _, _]}, _}],
                 meck:history(?EHTTPC)).

do_erlcloud_httpc_request(_, post, Headers, _, _, _) ->
    Target = proplists:get_value("x-amz-target", Headers),
    ["StarlingDoveService", Operation] = string:tokens(Target, "."),
    RespBody =
        case Operation of
            "DescribeComplianceByConfigRule"      -> ?DESCRIBE_COMPLIANCE_BY_CONFIG_RULE_RESP;
            "DescribeComplianceByResource"        -> ?DESCRIBE_COMPLIANCE_BY_RESOURCE_RESP;
            "DescribeConfigRuleEvaluationStatus"  -> ?DESCRIBE_CONFIG_RULE_EVALUATION_STATUS_RESP;
            "DescribeConfigRules"                 -> ?DESCRIBE_CONFIG_RULES_RESP;
            "DescribeConfigurationRecorders"      -> ?DESCRIBE_CONFIGURATION_RECORDERS_RESP;
            "DescribeConfigurationRecorderStatus" -> ?DESCRIBE_CONFIGURATION_RECORDER_STATUS_RESP;
            "DescribeDeliveryChannels"            -> ?DESCRIBE_DELIVERY_CHANNELS_RESP;
            "DescribeDeliveryChannelStatus"       -> ?DESCRIBE_DELIVERY_CHANNEL_STATUS_RESP;
            "GetComplianceDetailsByConfigRule"    -> ?GET_COMPLIANCE_DETAILS_BY_CONFIG_RULE_RESP;
            "GetComplianceDetailsByResource"      -> ?GET_COMPLIANCE_DETAILS_BY_RESOURCE_RESP;
            "GetComplianceSummaryByConfigRule"    -> ?GET_COMPLIANCE_SUMMARY_BY_CONFIG_RULE_RESP;
            "GetComplianceSummaryByResourceType"  -> ?GET_COMPLIANCE_SUMMARY_BY_RESOURCE_TYPE_RESP;
            "GetDiscoveredResourceCounts"         -> ?GET_DISCOVERED_RESOURCE_COUNTS_RESP;
            "GetResourceConfigHistory"            -> ?GET_RESOURCE_CONFIG_HISTORY_RESP;
            "ListDiscoveredResources"             -> ?LIST_DISCOVERED_RESOURCES_RESP
        end,
    {ok, {{200, "OK"}, [], jsx:encode(RespBody)}}.

configure(AccessKeyId, SecretAccessKey) ->
    put(aws_config, #aws_config{access_key_id = AccessKeyId,
        secret_access_key = SecretAccessKey,
        retry             = fun erlcloud_retry:default_retry/1}).

default_config() -> erlcloud_aws:default_config().
