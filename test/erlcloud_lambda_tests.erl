-module(erlcloud_lambda_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlcloud_aws.hrl").
-include("erlcloud_lambda.hrl").

-define(_lambda_test(T), {?LINE, T}).
-define(_f(F), fun() -> F end).
-define(BASE_URL, "https://lambda.us-east-1.amazonaws.com:443/2015-03-31/").

operation_test_() ->
    {foreach,
        fun start/0,
        fun stop/1,
        [
            fun list_functions/1,
            fun create_alias/1,
            fun create_event_source_mapping/1,
            fun create_function/1,
            fun delete_event_source_mapping/1,
            fun get_alias/1,
            fun get_event_source_mapping/1,
            fun get_function/1,
            fun get_function_configuration/1,
            fun invoke/1,
            fun list_aliases/1,
            fun list_event_source_mappings/1,
            fun list_versions_by_function/1,
            fun publish_version/1,
            fun update_alias/1,
            fun update_event_source_mapping/1,
            fun update_function_configuration/1
        ]}.

start() ->
    meck:new(ECA = erlcloud_aws, [non_strict, passthrough]),
    meck:new(ECH = erlcloud_httpc),
    [ECA, ECH],
    ok.

stop(_) ->
    meck:unload(erlcloud_aws),
    meck:unload(erlcloud_httpc).

%%%===================================================================
%%% Output test helpers
%%%===================================================================

%% returns the mock of the erlcloud_httpc function output tests expect to be called.
-spec output_expect(string()) -> fun().
output_expect(Response) ->
    fun(_Url, _, _Headers, _Body, _Timeout, _Config) ->
        {ok, {{200, "OK"}, [], list_to_binary(Response)}}
    end.

%% output_test converts an output_test specifier into an eunit test generator
-type output_test_spec() :: {pos_integer(), {string(), term()} | {string(), string(), term()}}.
-spec output_test(fun(), output_test_spec()) -> tuple().
output_test(Fun, {Line, {Description, Response, Result}}) ->
    {Description,
        {Line,
            fun() ->
                meck:expect(erlcloud_httpc, request, output_expect(Response)),
                erlcloud_ddb2:configure(string:copies("A", 20), string:copies("a", 40)),
                Actual = Fun(),
                case Result =:= Actual of
                    true -> ok;
                    false ->
                        ?debugFmt("~nEXPECTED~n~p~nACTUAL~n~p~n", [Result, Actual])
                end,
                ?assertEqual(Result, Actual)
            end}}.

%% output_tests converts a list of output_test specifiers into an eunit test generator
-spec output_tests(fun(), [output_test_spec()]) -> [term()].
output_tests(Fun, Tests) ->
    [output_test(Fun, Test) || Test <- Tests].

list_functions(_) ->
    Tests =
        [?_lambda_test(
            {"list_functions", "{
            \"Functions\":[
            {\"CodeSha256\":\"XmLDAZXEkl5KbA8ezZpwFU+bjgTXBehUmWGOScl4F2A=\",
                \"CodeSize\":5561,
                \"Description\":\"\",
                \"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008:function:bi-assets-asset-create-host\",
                \"FunctionName\":\"bi-assets-asset-create-host\",
                \"Handler\":\"index.handler\",
                \"LastModified\":\"2015-11-27T09:55:12.973+0000\",
                \"MemorySize\":128,
                \"Role\":\"arn:aws:iam::352283894008:role/lambda_basic_execution\",
                \"Runtime\":\"nodejs\",
                \"Timeout\":3,
                \"Version\":\"$LATEST\",
                \"VpcConfig\":null},
            {\"CodeSha256\":\"XmLDAZXEkl5KbA8ezZpwFU+bjgTXBehUmWGOScl4F2A=\",
                \"CodeSize\":5561,
                \"Description\":\"\",
                \"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008:function:bi-assets-asset-create-host1\",
                \"FunctionName\":\"bi-assets-asset-create-host1\",
                \"Handler\":\"index.handler\",
                \"LastModified\":\"2015-12-01T11:20:44.464+0000\",
                \"MemorySize\":128,
                \"Role\":\"arn:aws:iam::352283894008:role/lambda_kinesis_role\",
                \"Runtime\":\"nodejs\",
                \"Timeout\":10,
                \"Version\":\"$LATEST\",
                \"VpcConfig\":null},
            {\"CodeSha256\":\"tZJ+kUZVD1vGYwMIUvAoaZmvS4I9NHVc7a/267eChYY=\",
                \"CodeSize\":132628,
                \"Description\":\"\",
                \"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008:function:bi-driver\",
                \"FunctionName\":\"bi-driver\",
                \"Handler\":\"index.handler\",
                \"LastModified\":\"2015-12-03T13:59:05.219+0000\",
                \"MemorySize\":1024,\"Role\":\"arn:aws:iam::352283894008:role/lambda_kinesis_role\",
                \"Runtime\":\"nodejs\",
                \"Timeout\":59,
                \"Version\":\"$LATEST\",
                \"VpcConfig\":null},
            {\"CodeSha256\":\"QS10seyYGXrrhAnGMbJcTi+JOa4HWLaD+9YCLYG3+VE=\",
                \"CodeSize\":121486,
                \"Description\":\"An Amazon Kinesis stream processor that logs the data being published.\",
                \"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008:function:eholland-js-router\",
                \"FunctionName\":\"eholland-js-router\",
                \"Handler\":\"index.handler\",
                \"LastModified\":\"2015-12-02T14:50:41.923+0000\",
                \"MemorySize\":1024,
                \"Role\":\"arn:aws:iam::352283894008:role/lambda_kinesis_role\",
                \"Runtime\":\"nodejs\",
                \"Timeout\":60,\"Version\":\"$LATEST\",
                \"VpcConfig\":null},
            {\"CodeSha256\":\"ey+6CSWe750XoPSdVSQditxxoHNWmPFwve/MLPNs/Do=\",
                \"CodeSize\":253,
                \"Description\":\"\",
                \"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008:function:eholland-test-aims-user\",
                \"FunctionName\":\"eholland-test-aims-user\",
                \"Handler\":\"lambda_function.lambda_handler\",
                \"LastModified\":\"2015-11-09T17:32:46.030+0000\",
                \"MemorySize\":128,\"Role\":\"arn:aws:iam::352283894008:role/lambda_basic_execution\",
                \"Runtime\":\"python2.7\",
                \"Timeout\":3,
                \"Version\":\"$LATEST\",
                \"VpcConfig\":null},
            {\"CodeSha256\":\"R9HaKKCPPAem0ikKrkMGpDtX95M8egDCDd+4Ws+Kk5c=\",
                \"CodeSize\":253,
                \"Description\":\"aims users transform function\",
                \"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008:function:eholland-test-aims-users\",
                \"FunctionName\":\"eholland-test-aims-users\",
                \"Handler\":\"lambda_function.lambda_handler\",
                \"LastModified\":\"2015-11-02T13:59:32.509+0000\",
                \"MemorySize\":128,
                \"Role\":\"arn:aws:iam::352283894008:role/lambda_basic_execution\",
                \"Runtime\":\"python2.7\",
                \"Timeout\":3,
                \"Version\":\"$LATEST\",
                \"VpcConfig\":null},
            {\"CodeSha256\":\"PGG1vCAQpc8J7e4HL1z1Pv4DGSZEmhnJaNPTGDS29kk=\",
                \"CodeSize\":32372,
                \"Description\":\"An Amazon Kinesis stream processor that logs the data being published.\",
                \"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008:function:eholland-test-router\",
                \"FunctionName\":\"eholland-test-router\",
                \"Handler\":\"ProcessKinesisRecords.lambda_handler\",
                \"LastModified\":\"2015-11-09T10:58:50.458+0000\",
                \"MemorySize\":256,
                \"Role\":\"arn:aws:iam::352283894008:role/lambda_kinesis_role\",
                \"Runtime\":\"python2.7\",
                \"Timeout\":59,
                \"Version\":\"$LATEST\",
                \"VpcConfig\":null},
            {\"CodeSha256\":\"8twXwGaAXyr8Li81rGhM6vlMh+dvXglwqgIshfaio+U=\",
                \"CodeSize\":708740,
                \"Description\":\"Lambda Router Function for ETL Service\",
                \"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008:function:us-east-1_base_eholland_master_lambda_route\",
                \"FunctionName\":\"us-east-1_base_eholland_master_lambda_route\",
                \"Handler\":\"ProcessKinesisRecords.handler\",
                \"LastModified\":\"2015-11-11T10:13:19.043+0000\",
                \"MemorySize\":512,
                \"Role\":\"arn:aws:iam::352283894008:role/lambda_kinesis_role\",
                \"Runtime\":\"nodejs\",
                \"Timeout\":59,\"Version\":\"$LATEST\",
                \"VpcConfig\":null},
            {\"CodeSha256\":\"aDwLJhljsMVHsaN+LM4jRmsSLKKBdS+eFrAhKmJ/zbQ=\",
                \"CodeSize\":253,
                \"Description\":\"\",
                \"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008:function:us-east-1_base_eholland_master_lambda_route-aims-user-create\",
                \"FunctionName\":\"us-east-1_base_eholland_master_lambda_route-aims-user-create\",
                \"Handler\":\"lambda_function.lambda_handler\",
                \"LastModified\":\"2015-11-11T09:42:10.303+0000\",
                \"MemorySize\":128,
                \"Role\":\"arn:aws:iam::352283894008:role/lambda_basic_execution\",
                \"Runtime\":\"python2.7\",
                \"Timeout\":3,
                \"Version\":\"$LATEST\",
                \"VpcConfig\":null},
            {\"CodeSha256\":\"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4IcpiPf8QM=\",
                \"CodeSize\":848,
                \"Description\":\"\",
                \"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008:function:name\",
                \"FunctionName\":\"name\",
                \"Handler\":\"index.process\",
                \"LastModified\":\"2015-12-10T13:57:48.214+0000\",
                \"MemorySize\":512,\"Role\":\"arn:aws:iam::352283894008:role/lambda_kinesis_role\",
                \"Runtime\":\"nodejs\",
                \"Timeout\":30,\"Version\":\"$LATEST\",
                \"VpcConfig\":null},
            {\"CodeSha256\":\"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4IcpiPf8QM=\",
                \"CodeSize\":848,
                \"Description\":\"\",
                \"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008:function:name2\",
                \"FunctionName\":\"name2\",
                \"Handler\":\"index.process\",
                \"LastModified\":\"2015-12-10T11:31:21.106+0000\",
                \"MemorySize\":128,
                \"Role\":\"arn:aws:iam::352283894008:role/lambda_basic_execution\",
                \"Runtime\":\"nodejs\",
                \"Timeout\":3,
                \"Version\":\"$LATEST\",
                \"VpcConfig\":null}],
            \"NextMarker\":null
            }",
        {ok, [{<<"Functions">>, [
                [{<<"CodeSha256">>,<<"XmLDAZXEkl5KbA8ezZpwFU+bjgTXBehUmWGOScl4F2A=">>},
                {<<"CodeSize">>,5561},
                {<<"Description">>,<<>>},
                {<<"FunctionArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:bi-assets-asset-create-host">>},
                {<<"FunctionName">>,<<"bi-assets-asset-create-host">>},
                {<<"Handler">>,<<"index.handler">>},
                {<<"LastModified">>,<<"2015-11-27T09:55:12.973+0000">>},
                {<<"MemorySize">>,128},
                {<<"Role">>,<<"arn:aws:iam::352283894008:role/lambda_basic_execution">>},
                {<<"Runtime">>,<<"nodejs">>},
                {<<"Timeout">>,3},{<<"Version">>,<<"$LATEST">>},
                {<<"VpcConfig">>,null}],
                [{<<"CodeSha256">>,<<"XmLDAZXEkl5KbA8ezZpwFU+bjgTXBehUmWGOScl4F2A=">>},
                    {<<"CodeSize">>,5561},
                    {<<"Description">>,<<>>},
                    {<<"FunctionArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:bi-assets-asset-create-host1">>},
                    {<<"FunctionName">>,<<"bi-assets-asset-create-host1">>},
                    {<<"Handler">>,<<"index.handler">>},
                    {<<"LastModified">>,<<"2015-12-01T11:20:44.464+0000">>},
                    {<<"MemorySize">>,128},
                    {<<"Role">>,<<"arn:aws:iam::352283894008:role/lambda_kinesis_role">>},
                    {<<"Runtime">>,<<"nodejs">>},
                    {<<"Timeout">>,10},
                    {<<"Version">>,<<"$LATEST">>},
                    {<<"VpcConfig">>,null}],
                [{<<"CodeSha256">>,<<"tZJ+kUZVD1vGYwMIUvAoaZmvS4I9NHVc7a/267eChYY=">>},
                    {<<"CodeSize">>,132628},
                    {<<"Description">>,<<>>},
                    {<<"FunctionArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:bi-driver">>},
                    {<<"FunctionName">>,<<"bi-driver">>},
                    {<<"Handler">>,<<"index.handler">>},
                    {<<"LastModified">>,<<"2015-12-03T13:59:05.219+0000">>},
                    {<<"MemorySize">>,1024},
                    {<<"Role">>,<<"arn:aws:iam::352283894008:role/lambda_kinesis_role">>},
                    {<<"Runtime">>,<<"nodejs">>},
                    {<<"Timeout">>,59},{<<"Version">>,<<"$LATEST">>},
                    {<<"VpcConfig">>,null}],
                [{<<"CodeSha256">>,<<"QS10seyYGXrrhAnGMbJcTi+JOa4HWLaD+9YCLYG3+VE=">>},
                    {<<"CodeSize">>,121486},
                    {<<"Description">>,<<"An Amazon Kinesis stream processor that logs the data being published.">>},
                    {<<"FunctionArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:eholland-js-router">>},
                    {<<"FunctionName">>,<<"eholland-js-router">>},
                    {<<"Handler">>,<<"index.handler">>},
                    {<<"LastModified">>,<<"2015-12-02T14:50:41.923+0000">>},
                    {<<"MemorySize">>,1024},
                    {<<"Role">>,<<"arn:aws:iam::352283894008:role/lambda_kinesis_role">>},
                    {<<"Runtime">>,<<"nodejs">>},
                    {<<"Timeout">>,60},
                    {<<"Version">>,<<"$LATEST">>},
                    {<<"VpcConfig">>,null}],
                [{<<"CodeSha256">>,<<"ey+6CSWe750XoPSdVSQditxxoHNWmPFwve/MLPNs/Do=">>},
                    {<<"CodeSize">>,253},
                    {<<"Description">>,<<>>},
                    {<<"FunctionArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:eholland-test-aims-user">>},
                    {<<"FunctionName">>,<<"eholland-test-aims-user">>},
                    {<<"Handler">>,<<"lambda_function.lambda_handler">>},
                    {<<"LastModified">>,<<"2015-11-09T17:32:46.030+0000">>},
                    {<<"MemorySize">>,128},
                    {<<"Role">>,<<"arn:aws:iam::352283894008:role/lambda_basic_execution">>},
                    {<<"Runtime">>,<<"python2.7">>},
                    {<<"Timeout">>,3},
                    {<<"Version">>,<<"$LATEST">>},
                    {<<"VpcConfig">>,null}],
                [{<<"CodeSha256">>,<<"R9HaKKCPPAem0ikKrkMGpDtX95M8egDCDd+4Ws+Kk5c=">>},
                    {<<"CodeSize">>,253},
                    {<<"Description">>,<<"aims users transform function">>},
                    {<<"FunctionArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:eholland-test-aims-users">>},
                    {<<"FunctionName">>,<<"eholland-test-aims-users">>},
                    {<<"Handler">>,<<"lambda_function.lambda_handler">>},
                    {<<"LastModified">>,<<"2015-11-02T13:59:32.509+0000">>},
                    {<<"MemorySize">>,128},
                    {<<"Role">>,<<"arn:aws:iam::352283894008:role/lambda_basic_execution">>},
                    {<<"Runtime">>,<<"python2.7">>},
                    {<<"Timeout">>,3},
                    {<<"Version">>,<<"$LATEST">>},
                    {<<"VpcConfig">>,null}],
                [{<<"CodeSha256">>,<<"PGG1vCAQpc8J7e4HL1z1Pv4DGSZEmhnJaNPTGDS29kk=">>},
                    {<<"CodeSize">>,32372},
                    {<<"Description">>,<<"An Amazon Kinesis stream processor that logs the data being published.">>},
                    {<<"FunctionArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:eholland-test-router">>},
                    {<<"FunctionName">>,<<"eholland-test-router">>},
                    {<<"Handler">>,<<"ProcessKinesisRecords.lambda_handler">>},
                    {<<"LastModified">>,<<"2015-11-09T10:58:50.458+0000">>},
                    {<<"MemorySize">>,256},
                    {<<"Role">>,<<"arn:aws:iam::352283894008:role/lambda_kinesis_role">>},
                    {<<"Runtime">>,<<"python2.7">>},
                    {<<"Timeout">>,59},
                    {<<"Version">>, <<"$LATEST">>},
                    {<<"VpcConfig">>,null}],
                [{<<"CodeSha256">>,<<"8twXwGaAXyr8Li81rGhM6vlMh+dvXglwqgIshfaio+U=">>},
                    {<<"CodeSize">>,708740},
                    {<<"Description">>,<<"Lambda Router Function for ETL Service">>},
                    {<<"FunctionArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:us-east-1_base_eholland_master_lambda_route">>},
                    {<<"FunctionName">>,<<"us-east-1_base_eholland_master_lambda_route">>},
                    {<<"Handler">>,<<"ProcessKinesisRecords.handler">>},
                    {<<"LastModified">>,<<"2015-11-11T10:13:19.043+0000">>},
                    {<<"MemorySize">>,512},
                    {<<"Role">>,<<"arn:aws:iam::352283894008:role/lambda_kinesis_role">>},
                    {<<"Runtime">>,<<"nodejs">>},
                    {<<"Timeout">>,59},
                    {<<"Version">>,<<"$LATEST">>},
                    {<<"VpcConfig">>,null}],
                [{<<"CodeSha256">>,<<"aDwLJhljsMVHsaN+LM4jRmsSLKKBdS+eFrAhKmJ/zbQ=">>},
                    {<<"CodeSize">>,253},
                    {<<"Description">>,<<>>},
                    {<<"FunctionArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:us-east-1_base_eholland_master_lambda_route-aims-user-create">>},
                    {<<"FunctionName">>,<<"us-east-1_base_eholland_master_lambda_route-aims-user-create">>},
                    {<<"Handler">>,<<"lambda_function.lambda_handler">>},
                    {<<"LastModified">>,<<"2015-11-11T09:42:10.303+0000">>},
                    {<<"MemorySize">>,128},
                    {<<"Role">>,<<"arn:aws:iam::352283894008:role/lambda_basic_execution">>},
                    {<<"Runtime">>,<<"python2.7">>},
                    {<<"Timeout">>,3},
                    {<<"Version">>,<<"$LATEST">>},
                    {<<"VpcConfig">>,null}],
                [{<<"CodeSha256">>,<<"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4IcpiPf8QM=">>},
                    {<<"CodeSize">>,848},
                    {<<"Description">>,<<>>},
                    {<<"FunctionArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:name">>},
                    {<<"FunctionName">>,<<"name">>},
                    {<<"Handler">>,<<"index.process">>},
                    {<<"LastModified">>,<<"2015-12-10T13:57:48.214+0000">>},
                    {<<"MemorySize">>,512},
                    {<<"Role">>,<<"arn:aws:iam::352283894008:role/lambda_kinesis_role">>},
                    {<<"Runtime">>,<<"nodejs">>},
                    {<<"Timeout">>,30},
                    {<<"Version">>,<<"$LATEST">>},
                    {<<"VpcConfig">>,null}],
                [{<<"CodeSha256">>,<<"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4IcpiPf8QM=">>},
                    {<<"CodeSize">>,848},
                    {<<"Description">>,<<>>},
                    {<<"FunctionArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:name2">>},
                    {<<"FunctionName">>,<<"name2">>},
                    {<<"Handler">>,<<"index.process">>},
                    {<<"LastModified">>,<<"2015-12-10T11:31:21.106+0000">>},
                    {<<"MemorySize">>,128},
                    {<<"Role">>,<<"arn:aws:iam::352283894008:role/lambda_basic_execution">>},
                    {<<"Runtime">>,<<"nodejs">>},
                    {<<"Timeout">>,3},
                    {<<"Version">>,<<"$LATEST">>},
                    {<<"VpcConfig">>,null}]]},
            {<<"NextMarker">>,null}]}
    })],

    output_tests(?_f(erlcloud_lambda:list_functions()), Tests).

create_alias(_) ->
    Tests =
        [?_lambda_test(
            {"create_alias", "{
                \"AliasArn\":\"arn:aws:lambda:us-east-1:352283894008:function:name:aliasName1\",
                \"Description\":\"\",
                \"FunctionVersion\":\"$LATEST\",
                \"Name\":\"aliasName1\"
            }",
        {ok,[{<<"AliasArn">>,
            <<"arn:aws:lambda:us-east-1:352283894008:function:name:aliasName1">>},
            {<<"Description">>,<<>>},
            {<<"FunctionVersion">>,<<"$LATEST">>},
            {<<"Name">>,<<"aliasName1">>}]}
    })],

    output_tests(?_f(erlcloud_lambda:create_alias(<<"name">>, <<"$LATEST">>, <<"aliasName1">>, [])), Tests).

create_event_source_mapping(_) ->
    Tests =
        [?_lambda_test(
            {"create_event_source_mapping", "{
                \"BatchSize\":100,
                \"EventSourceArn\":\"arn:aws:kinesis:us-east-1:352283894008:stream/eholland-test\",
                \"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008:function:name\",
                \"LastModified\":1449845416.123,
                \"LastProcessingResult\":\"No records processed\",
                \"State\":\"Creating\",
                \"StateTransitionReason\":\"User action\",
                \"UUID\":\"3f303f86-7395-43f3-9902-f5c80f0a5382\"
            }",
        {ok,[{<<"BatchSize">>,100},
            {<<"EventSourceArn">>,<<"arn:aws:kinesis:us-east-1:352283894008:stream/eholland-test">>},
            {<<"FunctionArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:name">>},
            {<<"LastModified">>,1449845416.123},
            {<<"LastProcessingResult">>,<<"No records processed">>},
            {<<"State">>,<<"Creating">>},
            {<<"StateTransitionReason">>,<<"User action">>},
            {<<"UUID">>,<<"3f303f86-7395-43f3-9902-f5c80f0a5382">>}]}
    })],

    output_tests(?_f(erlcloud_lambda:create_event_source_mapping(
        <<"arn:aws:kinesis:us-east-1:352283894008:stream/eholland-test">>,
        <<"name">>, <<"TRIM_HORIZON">>, [])), Tests).

create_function(_) ->
    Tests =
        [?_lambda_test(
            {"create_function", "{
                \"CodeSha256\":\"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4IcpiPf8QM=\",
                \"CodeSize\":848,
                \"Description\":\"\",
                \"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008:function:name3\",
                \"FunctionName\":\"name3\",
                \"Handler\":\"index.process\",
                \"LastModified\":\"2015-12-11T13:45:31.924+0000\",
                \"MemorySize\":128,
                \"Role\":\"arn:aws:iam::352283894008:role/lambda_basic_execution\",
                \"Runtime\":\"nodejs\",
                \"Timeout\":3,
                \"Version\":\"$LATEST\",
                \"VpcConfig\":null
            }",
        {ok,[{<<"CodeSha256">>,
            <<"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4IcpiPf8QM=">>},
            {<<"CodeSize">>,848},
            {<<"Description">>,<<>>},
            {<<"FunctionArn">>,
                <<"arn:aws:lambda:us-east-1:352283894008:function:name3">>},
            {<<"FunctionName">>,<<"name3">>},
            {<<"Handler">>,<<"index.process">>},
            {<<"LastModified">>,<<"2015-12-11T13:45:31.924+0000">>},
            {<<"MemorySize">>,128},
            {<<"Role">>,
                <<"arn:aws:iam::352283894008:role/lambda_basic_execution">>},
            {<<"Runtime">>,<<"nodejs">>},
            {<<"Timeout">>,3},
            {<<"Version">>,<<"$LATEST">>},
            {<<"VpcConfig">>,null}]}
    })],

    output_tests(?_f(erlcloud_lambda:create_function(
        #erlcloud_lambda_code{s3Bucket = <<"bi-lambda">>,
            s3Key = <<"local_transform/bi-ass"
            "ets-environment-create"
            "-environment_1-0-0_lat"
            "est.zip">>
        },
        <<"name">>, <<"index.process">>,
        <<"arn:aws:iam::352283894008:role/lambda_basic_execution">>,
        nodejs, [])), Tests).

delete_event_source_mapping(_) ->
    Tests =
        [?_lambda_test(
            {"delete_event_source_mapping", "{
                \"BatchSize\":100,
                \"EventSourceArn\":\"arn:aws:kinesis:us-east-1:352283894008:stream/eholland-test\",
                \"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008:function:name\",
                \"LastModified\":1449843960.0,\"LastProcessingResult\":\"No records processed\",
                \"State\":\"Deleting\",\"StateTransitionReason\":\"User action\",
                \"UUID\":\"a45b58ec-a539-4c47-929e-174b4dd2d963\"
            }",
        {ok, [{<<"BatchSize">>,100},
            {<<"EventSourceArn">>,<<"arn:aws:kinesis:us-east-1:352283894008:stream/eholland-test">>},
            {<<"FunctionArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:name">>},
            {<<"LastModified">>,1449843960.0},
            {<<"LastProcessingResult">>,<<"No records processed">>},
            {<<"State">>,<<"Deleting">>},
            {<<"StateTransitionReason">>,<<"User action">>},
            {<<"UUID">>,<<"a45b58ec-a539-4c47-929e-174b4dd2d963">>}]}
    })],

    output_tests(?_f(erlcloud_lambda:delete_event_source_mapping(<<"6554f300-551b-46a6-829c-41b6af6022c6">>)), Tests).

get_alias(_) ->
    Tests =
        [?_lambda_test(
            {"get_alias", "{
                \"AliasArn\":\"arn:aws:lambda:us-east-1:352283894008:function:name:aliasName\",
                \"Description\":\"\",
                \"FunctionVersion\":\"$LATEST\",
                \"Name\":\"aliasName\"
            }",
        {ok, [{<<"AliasArn">>,
            <<"arn:aws:lambda:us-east-1:352283894008:function:name:aliasName">>},
            {<<"Description">>,<<>>},
            {<<"FunctionVersion">>,<<"$LATEST">>},
            {<<"Name">>,<<"aliasName">>}]}
    })],

    output_tests(?_f(erlcloud_lambda:get_alias(<<"name">>, <<"aliasName">>)), Tests).

get_event_source_mapping(_) ->
    Tests =
        [?_lambda_test(
            {"get_event_source_mapping", "{
                \"BatchSize\":100,
                \"EventSourceArn\":\"arn:aws:kinesis:us-east-1:352283894008:stream/eholland-test\",
                \"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008:function:name\",
                \"LastModified\":1449841860.0,\"LastProcessingResult\":\"No records processed\",
                \"State\":\"Enabled\",\"StateTransitionReason\":\"User action\",
                \"UUID\":\"a45b58ec-a539-4c47-929e-174b4dd2d963\"
            }",
        {ok, [{<<"BatchSize">>,100},
            {<<"EventSourceArn">>,
                <<"arn:aws:kinesis:us-east-1:352283894008:stream/eholland-test">>},
            {<<"FunctionArn">>,
                <<"arn:aws:lambda:us-east-1:352283894008:function:name">>},
            {<<"LastModified">>,1449841860.0},
            {<<"LastProcessingResult">>,<<"No records processed">>},
            {<<"State">>,<<"Enabled">>},
            {<<"StateTransitionReason">>,<<"User action">>},
            {<<"UUID">>,<<"a45b58ec-a539-4c47-929e-174b4dd2d963">>}]}
    })],

    output_tests(?_f(erlcloud_lambda:get_event_source_mapping(<<"a45b58ec-a539-4c47-929e-174b4dd2d963">>)), Tests).

get_function(_) ->
    Tests =
        [?_lambda_test(
            {"get_function", "{
            \"Code\":{
                \"Location\":\"https://awslambda-us-east-1-"
                "tasks.s3-us-east-1.amazonaws.com/snapshots/352283894008/name-69237aec-bae9-40"
                "86-af73-a6610c2f8eb8?x-amz-security-token=AQoDYXdzENb%2F%2F%2F%2F%2F%2F%2F%2F"
                "%2F%2FwEa4APJSJNHsYWnObKlElri5ytVzNg0t%2F0zwADHn40jy%2F1ZDW9%2FYWGi8dTp1l6LWB"
                "I9TwJi0LLcgp%2FlCxJIh7hsAPftYX62J9r9lRcmgd9RnYssg1%2Fkpfyjya90epxKg2zdHm%2BuZ"
                "GukHYDcmAE1IQcHwsaQbvGAjXPCpFyxClbV6gMcFIsaBtfMxoMcbTCXG9m8l56nKgcX6Mi60vRNaB"
                "83AeNVrKMhB8EBUUbYbaB%2BG0iJg32i2HBF6VJMxamOLIEf1GJp1tWt%2FSAHfEkdTwcwtGINH3T"
                "NRv%2BY3ddsXs8pJ49eY49NCHANPC%2Bq0JzNQydbIK1shz8w1nozXYQo6%2BNh9tqOlaJNFgfFbt"
                "JkUDXv4rFqgVsfgJKJSQBeYUKmlNvIPQIoHWhRjjRzQUGmYDc3eEug7vELsNcHZixI4nNVycH%2BJ"
                "ZwaBvswy4eE7gBv3HwHi3SVlg9iXFTrfWTK%2FlCybC7mZIjAmPGiLCG5Pu8SoCgaGdHp8HmSeXXu"
                "s4VUFcVTUw1qn7E%2BSaRFg3MTpCdu1f1Nqh4pNu7GpZacyLbH%2BSocuPyTjyYGL8sk0C3rjIWZi"
                "pJcfUZsleS1cXLEGw%2FPAK5eg0RNWlVsaF1WgVimqQh3LtoRZ%2BwYCHRXsHjhCyQ8VhoguJCrsw"
                "U%3D&AWSAccessKeyId=ASIAIAUY4IMA6JRJ3FSA&Expires=1449843004&Signature=5KUmber"
                "1cOBSChlKtnLaI%2FUemU4%3D\",
                \"RepositoryType\":\"S3\"
            },
            \"Configuration\":{
                \"CodeSha256\":\"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4IcpiPf8QM=\",
                \"CodeSize\":848,
                \"Description\":\"\",
                \"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008:function:name\",
                \"FunctionName\":\"name\",\"Handler\":\"index.process\",\"LastModified\":\"2015-12-10T13:57:48.214+0000\",
                \"MemorySize\":512,
                \"Role\":\"arn:aws:iam::352283894008:role/lambda_kinesis_role\",
                \"Runtime\":\"nodejs\",
                \"Timeout\":30,
                \"Version\":\"$LATEST\",
                \"VpcConfig\":null
            }
        }",
        {ok, [{<<"Code">>,
            [{<<"Location">>, <<"https://awslambda-us-east-1-tasks.s3-us-east-1.amazonaws.com/snapshots/352283894008/name-69237aec-bae9-4086-af73-a6610c2f8eb8?x-amz-security-token=AQoDYXdzENb%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEa4APJSJNHsYWnObKlElri5ytVzNg0t%2F0zwADHn40jy%2F1ZDW9%2FYWGi8dTp1l6LWBI9TwJi0LLcgp%2FlCxJIh7hsAPftYX62J9r9lRcmgd9RnYssg1%2Fkpfyjya90epxKg2zdHm%2BuZGukHYDcmAE1IQcHwsaQbvGAjXPCpFyxClbV6gMcFIsaBtfMxoMcbTCXG9m8l56nKgcX6Mi60vRNaB83AeNVrKMhB8EBUUbYbaB%2BG0iJg32i2HBF6VJMxamOLIEf1GJp1tWt%2FSAHfEkdTwcwtGINH3TNRv%2BY3ddsXs8pJ49eY49NCHANPC%2Bq0JzNQydbIK1shz8w1nozXYQo6%2BNh9tqOlaJNFgfFbtJkUDXv4rFqgVsfgJKJSQBeYUKmlNvIPQIoHWhRjjRzQUGmYDc3eEug7vELsNcHZixI4nNVycH%2BJZwaBvswy4eE7gBv3HwHi3SVlg9iXFTrfWTK%2FlCybC7mZIjAmPGiLCG5Pu8SoCgaGdHp8HmSeXXus4VUFcVTUw1qn7E%2BSaRFg3MTpCdu1f1Nqh4pNu7GpZacyLbH%2BSocuPyTjyYGL8sk0C3rjIWZipJcfUZsleS1cXLEGw%2FPAK5eg0RNWlVsaF1WgVimqQh3LtoRZ%2BwYCHRXsHjhCyQ8VhoguJCrswU%3D&AWSAccessKeyId=ASIAIAUY4IMA6JRJ3FSA&Expires=1449843004&Signature=5KUmber1cOBSChlKtnLaI%2FUemU4%3D">>},
                {<<"RepositoryType">>,<<"S3">>}]},
            {<<"Configuration">>,
                [{<<"CodeSha256">>,<<"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4IcpiPf8QM=">>},
                    {<<"CodeSize">>,848},
                    {<<"Description">>,<<>>},
                    {<<"FunctionArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:name">>},
                    {<<"FunctionName">>,<<"name">>},
                    {<<"Handler">>,<<"index.process">>},
                    {<<"LastModified">>,<<"2015-12-10T13:57:48.214+0000">>},
                    {<<"MemorySize">>,512},
                    {<<"Role">>,<<"arn:aws:iam::352283894008:role/lambda_kinesis_role">>},
                    {<<"Runtime">>,<<"nodejs">>},
                    {<<"Timeout">>,30},
                    {<<"Version">>,<<"$LATEST">>},
                    {<<"VpcConfig">>,null}]}]}
    })],

    output_tests(?_f(erlcloud_lambda:get_function(<<"name">>)), Tests).

get_function_configuration(_) ->
    Tests =
        [?_lambda_test(
            {"get_function_configuration", "{
                \"CodeSha256\":\"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4IcpiPf8QM=\",
                \"CodeSize\":848,
                \"Description\":\"\",
                \"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008:function:name\",
                \"FunctionName\":\"name\",
                \"Handler\":\"index.process\",
                \"LastModified\":\"2015-12-10T13:57:48.214+0000\",
                \"MemorySize\":512,\"Role\":\"arn:aws:iam::352283894008:role/lambda_kinesis_role\",
                \"Runtime\":\"nodejs\",
                \"Timeout\":30,
                \"Version\":\"$LATEST\",
                \"VpcConfig\":null
            }",
        {ok, [{<<"CodeSha256">>,<<"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4IcpiPf8QM=">>},
            {<<"CodeSize">>,848},
            {<<"Description">>,<<>>},
            {<<"FunctionArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:name">>},
            {<<"FunctionName">>,<<"name">>},
            {<<"Handler">>,<<"index.process">>},
            {<<"LastModified">>,<<"2015-12-10T13:57:48.214+0000">>},
            {<<"MemorySize">>,512},
            {<<"Role">>,<<"arn:aws:iam::352283894008:role/lambda_kinesis_role">>},
            {<<"Runtime">>,<<"nodejs">>},
            {<<"Timeout">>,30},
            {<<"Version">>,<<"$LATEST">>},
            {<<"VpcConfig">>,null}]}
    })],

    output_tests(?_f(erlcloud_lambda:get_function_configuration(<<"name">>)), Tests).

invoke(_) ->
    Tests =
        [?_lambda_test(
            {"invoke", "{
                \"message\":\"Hello World!\"
            }",
        {ok, [{<<"message">>, <<"Hello World!">>}]}
    })],

    output_tests(?_f(erlcloud_lambda:invoke(<<"name">>, [])), Tests).

list_aliases(_) ->
    Tests =
        [?_lambda_test(
            {"list_aliases", "{
            \"Aliases\":[
                {
                    \"AliasArn\":\"arn:aws:lambda:us-east-1:352283894008:function:name:aliasName\",
                    \"Description\":\"\",\"FunctionVersion\":\"$LATEST\",
                    \"Name\":\"aliasName\"
                },
                {
                    \"AliasArn\":\"arn:aws:lambda:us-east-1:352283894008:function:name:aliasName1\",
                    \"Description\":\"\",\"FunctionVersion\":\"$LATEST\",
                    \"Name\":\"aliasName1\"
                }
            ],
            \"NextMarker\":null
            }",
        {ok, [{<<"Aliases">>,
            [[{<<"AliasArn">>, <<"arn:aws:lambda:us-east-1:352283894008:function:name:aliasName">>},
                {<<"Description">>,<<>>},
                {<<"FunctionVersion">>,<<"$LATEST">>},
                {<<"Name">>,<<"aliasName">>}],
                [{<<"AliasArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:name:aliasName1">>},
                    {<<"Description">>,<<>>},
                    {<<"FunctionVersion">>,<<"$LATEST">>},
                    {<<"Name">>,<<"aliasName1">>}]]},
            {<<"NextMarker">>,null}]}
        })],

    output_tests(?_f(erlcloud_lambda:list_aliases(<<"name">>)), Tests).

list_event_source_mappings(_) ->
    Tests =
        [?_lambda_test(
        {"list_event_source_mappings", "{
            \"EventSourceMappings\":[
                {\"BatchSize\":100,\"EventSourceArn\":\"arn:aws:kinesis:us-east-1:352283894008:stream/eholland-test\",
                    \"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008:function:name\",
                    \"LastModified\":1449841860.0,
                    \"LastProcessingResult\":\"No records processed\",
                    \"State\":\"Enabled\",
                    \"StateTransitionReason\":\"User action\",
                    \"UUID\":\"a45b58ec-a539-4c47-929e-174b4dd2d963\"}
            ],
            \"NextMarker\":null
            }",
        {ok, [{<<"EventSourceMappings">>,
            [
                [{<<"BatchSize">>,100},
                {<<"EventSourceArn">>,<<"arn:aws:kinesis:us-east-1:352283894008:stream/eholland-test">>},
                {<<"FunctionArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:name">>},
                {<<"LastModified">>,1449841860.0},
                {<<"LastProcessingResult">>,<<"No records processed">>},
                {<<"State">>,<<"Enabled">>},
                {<<"StateTransitionReason">>,<<"User action">>},
                {<<"UUID">>,<<"a45b58ec-a539-4c47-929e-174b4dd2d963">>}
                ]
            ]},
            {<<"NextMarker">>,null}]}
        })],

        output_tests(?_f(erlcloud_lambda:list_event_source_mappings(
                        <<"arn:aws:kinesis:us-east-1:352283894008:stream/eholland-test">>,
                        <<"name">>)),
                    Tests).

list_versions_by_function(_) ->
    Tests =
        [?_lambda_test(
            {"list_versions_by_function", "{
            \"NextMarker\":null,
            \"Versions\":[
                {\"CodeSha256\":\"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4IcpiPf8QM=\",
                    \"CodeSize\":848,
                    \"Description\":\"\",
                    \"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008:function:name:$LATEST\",
                    \"FunctionName\":\"name\",
                    \"Handler\":\"index.process\",
                    \"LastModified\":\"2015-12-10T13:57:48.214+0000\",
                    \"MemorySize\":512,
                    \"Role\":\"arn:aws:iam::352283894008:role/lambda_kinesis_role\",
                    \"Runtime\":\"nodejs\",\"Timeout\":30,
                    \"Version\":\"$LATEST\",
                    \"VpcConfig\":null
                },
                {\"CodeSha256\":\"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4IcpiPf8QM=\",
                    \"CodeSize\":848,\"Description\":\"\","
                    "\"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008:function:name:1\",
                    \"FunctionName\":\"name\",
                    \"Handler\":\"index.process\",
                    \"LastModified\":\"2015-12-10T11:36:12.776+0000\",
                    \"MemorySize\":128,\"Role\":\"arn:aws:iam::352283894008:role/lambda_kinesis_role\",
                    \"Runtime\":\"nodejs\",\"Timeout\":3,
                    \"Version\":\"1\",
                    \"VpcConfig\":null},
                {\"CodeSha256\":\"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4IcpiPf8QM=\",
                    \"CodeSize\":848,
                    \"Description\":\"\",
                    \"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008:function:name:2\",
                    \"FunctionName\":\"name\",
                    \"Handler\":\"index.process\",\"LastModified\":\"2015-12-10T13:56:43.171+0000\",
                    \"MemorySize\":128,
                    \"Role\":\"arn:aws:iam::352283894008:role/lambda_kinesis_role\",
                    \"Runtime\":\"nodejs\",
                    \"Timeout\":3,
                    \"Version\":\"2\",
                    \"VpcConfig\":null}
            ]}",
        {ok, [{<<"NextMarker">>,null},
            {<<"Versions">>,
                [[{<<"CodeSha256">>, <<"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4IcpiPf8QM=">>},
                    {<<"CodeSize">>, 848},
                    {<<"Description">>, <<>>},
                    {<<"FunctionArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:name:$LATEST">>},
                    {<<"FunctionName">>,<<"name">>},
                    {<<"Handler">>,<<"index.process">>},
                    {<<"LastModified">>,<<"2015-12-10T13:57:48.214+0000">>},
                    {<<"MemorySize">>,512},
                    {<<"Role">>,<<"arn:aws:iam::352283894008:role/lambda_kinesis_role">>},
                    {<<"Runtime">>,<<"nodejs">>},
                    {<<"Timeout">>,30},
                    {<<"Version">>,<<"$LATEST">>},
                    {<<"VpcConfig">>,null}],
                    [{<<"CodeSha256">>,<<"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4IcpiPf8QM=">>},
                        {<<"CodeSize">>,848},
                        {<<"Description">>,<<>>},
                        {<<"FunctionArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:name:1">>},
                        {<<"FunctionName">>,<<"name">>},
                        {<<"Handler">>,<<"index.process">>},
                        {<<"LastModified">>,<<"2015-12-10T11:36:12.776+0000">>},
                        {<<"MemorySize">>,128},
                        {<<"Role">>,<<"arn:aws:iam::352283894008:role/lambda_kinesis_role">>},
                        {<<"Runtime">>,<<"nodejs">>},
                        {<<"Timeout">>,3},
                        {<<"Version">>,<<"1">>},
                        {<<"VpcConfig">>,null}],
                    [{<<"CodeSha256">>,<<"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4IcpiPf8QM=">>},
                        {<<"CodeSize">>,848},
                        {<<"Description">>,<<>>},
                        {<<"FunctionArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:name:2">>},
                        {<<"FunctionName">>,<<"name">>},
                        {<<"Handler">>,<<"index.process">>},
                        {<<"LastModified">>,<<"2015-12-10T13:56:43.171+0000">>},
                        {<<"MemorySize">>,128},
                        {<<"Role">>,<<"arn:aws:iam::352283894008:role/lambda_kinesis_role">>},
                        {<<"Runtime">>,<<"nodejs">>},
                        {<<"Timeout">>,3},
                        {<<"Version">>,<<"2">>},
                        {<<"VpcConfig">>,null}]]}]}
                })],

        output_tests(?_f(erlcloud_lambda:list_versions_by_function(<<"name">>)), Tests).

publish_version(_) ->
    Tests =
        [?_lambda_test(
            {"publish_version", "{
                \"CodeSha256\":\"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4IcpiPf8QM=\",
                \"CodeSize\":848,
                \"Description\":\"\",
                \"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008:function:name:3\",
                \"FunctionName\":\"name\",\"Handler\":\"index.process\",
                \"LastModified\":\"2015-12-10T13:57:48.214+0000\",
                \"MemorySize\":512,
                \"Role\":\"arn:aws:iam::352283894008:role/lambda_kinesis_role\",
                \"Runtime\":\"nodejs\",
                \"Timeout\":30,
                \"Version\":\"3\",
                \"VpcConfig\":null
            }",
        {ok, [{<<"CodeSha256">>,<<"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4IcpiPf8QM=">>},
            {<<"CodeSize">>,848},
            {<<"Description">>,<<>>},
            {<<"FunctionArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:name:3">>},
            {<<"FunctionName">>,<<"name">>},
            {<<"Handler">>,<<"index.process">>},
            {<<"LastModified">>,<<"2015-12-10T13:57:48.214+0000">>},
            {<<"MemorySize">>,512},
            {<<"Role">>,<<"arn:aws:iam::352283894008:role/lambda_kinesis_role">>},
            {<<"Runtime">>,<<"nodejs">>},
            {<<"Timeout">>,30},
            {<<"Version">>,<<"3">>},
            {<<"VpcConfig">>,null}]}
        })],

    output_tests(?_f(erlcloud_lambda:publish_version(<<"name">>)), Tests).

update_alias(_) ->
    Tests =
        [?_lambda_test(
            {"update_alias", "{
                \"AliasArn\":\"arn:aws:lambda:us-east-1:352283894008:function:name:aliasName\",
                \"Description\":\"\",\"FunctionVersion\":\"$LATEST\",
                \"Name\":\"aliasName\"
            }",
        {ok, [{<<"AliasArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:name:aliasName">>},
            {<<"Description">>,<<>>},
            {<<"FunctionVersion">>,<<"$LATEST">>},
            {<<"Name">>,<<"aliasName">>}]}
    })],

    output_tests(?_f(erlcloud_lambda:update_alias(<<"name">>, <<"aliasName">>)), Tests).

update_event_source_mapping(_) ->
    Tests =
        [?_lambda_test(
            {"update_event_source_mapping", "{
                \"BatchSize\":100,
                \"EventSourceArn\":\"arn:aws:kinesis:us-east-1:352283894008:stream/eholland-test\",
                \"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008:function:name\",
                \"LastModified\":1449844011.991,
                \"LastProcessingResult\":\"No records processed\",
                \"State\":\"Updating\",
                \"StateTransitionReason\":\"User action\",
                \"UUID\":\"a45b58ec-a539-4c47-929e-174b4dd2d963\"
            }",
        {ok, [{<<"BatchSize">>,100},
            {<<"EventSourceArn">>,<<"arn:aws:kinesis:us-east-1:352283894008:stream/eholland-test">>},
            {<<"FunctionArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:name">>},
            {<<"LastModified">>,1449844011.991},
            {<<"LastProcessingResult">>,<<"No records processed">>},
            {<<"State">>,<<"Updating">>},
            {<<"StateTransitionReason">>,<<"User action">>},
            {<<"UUID">>,<<"a45b58ec-a539-4c47-929e-174b4dd2d963">>}]}
        })],

    output_tests(?_f(erlcloud_lambda:update_event_source_mapping(
                        <<"a45b58ec-a539-4c47-929e-174b4dd2d963">>,
                        100, undefined, undefined)),
                Tests).

update_function_configuration(_) ->
    Tests =
        [?_lambda_test(
            {"update_event_source_mapping", "{
                \"CodeSha256\":\"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4IcpiPf8QM=\",
                \"CodeSize\":848,
                \"Description\":\"\",
                \"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008:function:name\",
                \"FunctionName\":\"name\",\"Handler\":\"index.process\",
                \"LastModified\":\"2015-12-11T14:31:52.034+0000\",
                \"MemorySize\":512,
                \"Role\":\"arn:aws:iam::352283894008:role/lambda_kinesis_role\",
                \"Runtime\":\"nodejs\",
                \"Timeout\":30,
                \"Version\":\"$LATEST\",
                \"VpcConfig\":null
            }",
        {ok, [{<<"CodeSha256">>,<<"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4IcpiPf8QM=">>},
            {<<"CodeSize">>,848},
            {<<"Description">>,<<>>},
            {<<"FunctionArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:name">>},
            {<<"FunctionName">>,<<"name">>},
            {<<"Handler">>,<<"index.process">>},
            {<<"LastModified">>,<<"2015-12-11T14:31:52.034+0000">>},
            {<<"MemorySize">>,512},
            {<<"Role">>,<<"arn:aws:iam::352283894008:role/lambda_kinesis_role">>},
            {<<"Runtime">>,<<"nodejs">>},
            {<<"Timeout">>,30},
            {<<"Version">>,<<"$LATEST">>},
            {<<"VpcConfig">>,null}]}
    })],

    output_tests(?_f(erlcloud_lambda:update_function_configuration(
        <<"name">>, undefined, undefined, 512, undefined, 30)),
        Tests).