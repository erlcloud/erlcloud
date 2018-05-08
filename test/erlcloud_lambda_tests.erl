-module(erlcloud_lambda_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlcloud_aws.hrl").
-include("erlcloud_lambda.hrl").

-define(BASE_URL, "https://lambda.us-east-1.amazonaws.com:443/2015-03-31/").

route53_test_() ->
    {foreach,
     fun setup/0,
     fun meck:unload/1,
     [
      fun api_tests/1
     ]}.

mocks() ->
    [
     mocked_create_alias(),
     mocked_create_event_source_mapping(),
     mocked_create_function(),
     mocked_delete_event_source_mapping(),
     mocked_get_alias(),
     mocked_get_event_source_mapping(),
     mocked_get_function(),
     mocked_get_function_configuration(),
     mocked_invoke(),
     mocked_list_aliases(),
     mocked_list_event_source_mappings(),
     mocked_list_function(),
     mocked_list_versions_by_function(),
     mocked_publish_version(),
     mocked_update_alias(),
     mocked_update_event_source_mapping(),
     mocked_update_function_code(),
     mocked_update_function_configuration()
    ].

setup() ->
    Config = #aws_config{access_key_id="AccessId",
                         secret_access_key="secret",
                         security_token="token"},
    meck:new(ECA = erlcloud_aws, [non_strict, passthrough]),
    meck:expect(ECA, default_config, 0, Config),
    meck:expect(ECA, update_config, 1, {ok, Config}),
    meck:new(ECH = erlcloud_httpc),
    meck:expect(ECH, request, mocks()),
    [ECA, ECH].

mocked_create_alias() ->
    {
      [?BASE_URL ++ "functions/name/aliases", post, '_',
       <<"{\"FunctionVersion\":\"$LATEST\",\"Name\":\"aliasName1\"}">>, '_', '_'],
      make_response(<<"{\"AliasArn\":\"arn:aws:lambda:us-east-1:352283894008:"
"function:name:aliasName1\",\"Description\":\"\",\"FunctionVersion\":\"$LATEST"
"\",\"Name\":\"aliasName1\"}">>)
    }.

mocked_create_event_source_mapping() ->
    {
      [?BASE_URL ++ "event-source-mappings", post, '_',
       <<"{\"EventSourceArn\":\"arn:aws:kinesis:us-east-1:352283894008:stream"
         "/eholland-test\",\"FunctionName\":\"name\",\"StartingPosition\":\"TRI"
         "M_HORIZON\"}">>, '_', '_'],
      make_response(<<"{\"BatchSize\":100,\"EventSourceArn\":\"arn:aws:kinesi"
"s:us-east-1:352283894008:stream/eholland-test\",\"FunctionArn\":\"arn:aws:lam"
"bda:us-east-1:352283894008:function:name\",\"LastModified\":1449845416.123,\""
"LastProcessingResult\":\"No records processed\",\"State\":\"Creating\",\"Stat"
"eTransitionReason\":\"User action\",\"UUID\":\"3f303f86-7395-43f3-9902-f5c80f"
"0a5382\"}">>)
    }.

mocked_create_function() ->
    {
      [?BASE_URL ++ "functions", post, '_',
       <<"{\"Code\":{\"S3Bucket\":\"bi-lambda\",\"S3Key\":\"local_transform/bi-a"
"ssets-environment-create-environment_1-0-0_latest.zip\"},\"FunctionName\":\"name"
"\",\"Handler\":\"index.process\",\"Role\":\"arn:aws:iam::352283894008:role/lambd"
"a_basic_execution\",\"Runtime\":\"nodejs\"}">> , '_', '_'],
      make_response(<<"{\"CodeSha256\":\"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4Ic"
"piPf8QM=\",\"CodeSize\":848,\"Description\":\"\",\"FunctionArn\":\"arn:aws:la"
"mbda:us-east-1:352283894008:function:name3\",\"FunctionName\":\"name3\",\"Han"
"dler\":\"index.process\",\"LastModified\":\"2015-12-11T13:45:31.924+0000\",\""
"MemorySize\":128,\"Role\":\"arn:aws:iam::352283894008:role/lambda_basic_execu"
"tion\",\"Runtime\":\"nodejs\",\"Timeout\":3,\"Version\":\"$LATEST\",\"VpcConf"
"ig\":null}">>)
    }.
mocked_delete_event_source_mapping() ->
    {
      [?BASE_URL ++ "event-source-mappings/6554f300-551b-46a6-829c-41b6af6022c6?",
       delete, '_', <<>>, '_', '_'],
      make_response(<<"{\"BatchSize\":100,\"EventSourceArn\":\"arn:aws:kinesi"
"s:us-east-1:352283894008:stream/eholland-test\",\"FunctionArn\":\"arn:aws:lam"
"bda:us-east-1:352283894008:function:name\",\"LastModified\":1449843960.0,\"La"
"stProcessingResult\":\"No records processed\",\"State\":\"Deleting\",\"StateT"
"ransitionReason\":\"User action\",\"UUID\":\"a45b58ec-a539-4c47-929e-174b4dd2"
"d963\"}">>)
    }.
mocked_get_alias() ->
        {
      [?BASE_URL ++ "functions/name/aliases/aliasName?",
       get, '_', <<>>, '_', '_'],
      make_response(<<"{\"AliasArn\":\"arn:aws:lambda:us-east-1:352283894008:"
"function:name:aliasName\",\"Description\":\"\",\"FunctionVersion\":\"$LATEST\"
"",\"Name\":\"aliasName\"}">>)
    }.
mocked_get_event_source_mapping() ->
        {
      [?BASE_URL ++ "event-source-mappings/a45b58ec-a539-4c47-929e-174b4dd2d963?",
       get, '_', <<>>, '_', '_'],
      make_response(<<"{\"BatchSize\":100,\"EventSourceArn\":\"arn:aws:kinesi"
"s:us-east-1:352283894008:stream/eholland-test\",\"FunctionArn\":\"arn:aws:lam"
"bda:us-east-1:352283894008:function:name\",\"LastModified\":1449841860.0,\"La"
"stProcessingResult\":\"No records processed\",\"State\":\"Enabled\",\"StateTr"
"ansitionReason\":\"User action\",\"UUID\":\"a45b58ec-a539-4c47-929e-174b4dd2d"
"963\"}">>)
    }.
mocked_get_function() ->
    {
      [?BASE_URL ++ "functions/name?",
       get, '_', <<>>, '_', '_'],
      make_response(<<"{\"Code\":{\"Location\":\"https://awslambda-us-east-1-"
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
"1cOBSChlKtnLaI%2FUemU4%3D\",\"RepositoryType\":\"S3\"},\"Configuration\":{\"C"
"odeSha256\":\"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4IcpiPf8QM=\",\"CodeSize\":848"
",\"Description\":\"\",\"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008"
":function:name\",\"FunctionName\":\"name\",\"Handler\":\"index.process\",\"La"
"stModified\":\"2015-12-10T13:57:48.214+0000\",\"MemorySize\":512,\"Role\":\"a"
"rn:aws:iam::352283894008:role/lambda_kinesis_role\",\"Runtime\":\"nodejs\",\""
"Timeout\":30,\"Version\":\"$LATEST\",\"VpcConfig\":null}}">>)
    }.
mocked_get_function_configuration() ->
    {
      [?BASE_URL ++ "functions/name/configuration?", get, '_', <<>>, '_', '_'],
      make_response(<<"{\"CodeSha256\":\"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4Ic"
"piPf8QM=\",\"CodeSize\":848,\"Description\":\"\",\"FunctionArn\":\"arn:aws:la"
"mbda:us-east-1:352283894008:function:name\",\"FunctionName\":\"name\",\"Handl"
"er\":\"index.process\",\"LastModified\":\"2015-12-10T13:57:48.214+0000\",\"Me"
"morySize\":512,\"Role\":\"arn:aws:iam::352283894008:role/lambda_kinesis_role\"
"",\"Runtime\":\"nodejs\",\"Timeout\":30,\"Version\":\"$LATEST\",\"VpcConfig\""
":null}">>)
    }.

mocked_invoke() ->
  {
    [?BASE_URL ++ "functions/name/invocations", post, '_', <<"{}">>, '_', '_'],
    make_response(<<"{\"message\":\"Hello World!\"}">>)
  }.


mocked_list_aliases() ->
    {
      [?BASE_URL ++ "functions/name/aliases?", get, '_', <<>>, '_', '_'],
      make_response(<<"{\"Aliases\":[{\"AliasArn\":\"arn:aws:lambda:us-east-1"
":352283894008:function:name:aliasName\",\"Description\":\"\",\"FunctionVersio"
"n\":\"$LATEST\",\"Name\":\"aliasName\"},{\"AliasArn\":\"arn:aws:lambda:us-eas"
"t-1:352283894008:function:name:aliasName1\",\"Description\":\"\",\"FunctionVe"
"rsion\":\"$LATEST\",\"Name\":\"aliasName1\"}],\"NextMarker\":null}">>)
    }.

mocked_list_event_source_mappings() ->
    {
      [?BASE_URL ++ "event-source-mappings/?EventSourceArn=arn%3Aaws%3Akinesis%"
       "3Aus-east-1%3A352283894008%3Astream%2Feholland-test&FunctionName=name",
       get, '_', <<>>, '_', '_'],
      make_response(<<"{\"EventSourceMappings\":[{\"BatchSize\":100,\"EventSo"
"urceArn\":\"arn:aws:kinesis:us-east-1:352283894008:stream/eholland-test\",\"F"
"unctionArn\":\"arn:aws:lambda:us-east-1:352283894008:function:name\",\"LastMo"
"dified\":1449841860.0,\"LastProcessingResult\":\"No records processed\",\"Sta"
"te\":\"Enabled\",\"StateTransitionReason\":\"User action\",\"UUID\":\"a45b58e"
"c-a539-4c47-929e-174b4dd2d963\"}],\"NextMarker\":null}">>)
    }.

mocked_list_function() ->
    {
      [?BASE_URL ++ "functions/?",
       get, '_', <<>>, '_', '_'],
      make_response(<<"{\"Functions\":[{\"CodeSha256\":\"XmLDAZXEkl5KbA8ezZpw"
"FU+bjgTXBehUmWGOScl4F2A=\",\"CodeSize\":5561,\"Description\":\"\",\"FunctionA"
"rn\":\"arn:aws:lambda:us-east-1:352283894008:function:bi-assets-asset-create-"
"host\",\"FunctionName\":\"bi-assets-asset-create-host\",\"Handler\":\"index.h"
"andler\",\"LastModified\":\"2015-11-27T09:55:12.973+0000\",\"MemorySize\":128"
",\"Role\":\"arn:aws:iam::352283894008:role/lambda_basic_execution\",\"Runtime"
"\":\"nodejs\",\"Timeout\":3,\"Version\":\"$LATEST\",\"VpcConfig\":null},{\"Co"
"deSha256\":\"XmLDAZXEkl5KbA8ezZpwFU+bjgTXBehUmWGOScl4F2A=\",\"CodeSize\":5561"
",\"Description\":\"\",\"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008"
":function:bi-assets-asset-create-host1\",\"FunctionName\":\"bi-assets-asset-c"
"reate-host1\",\"Handler\":\"index.handler\",\"LastModified\":\"2015-12-01T11:"
"20:44.464+0000\",\"MemorySize\":128,\"Role\":\"arn:aws:iam::352283894008:role"
"/lambda_kinesis_role\",\"Runtime\":\"nodejs\",\"Timeout\":10,\"Version\":\"$L"
"ATEST\",\"VpcConfig\":null},{\"CodeSha256\":\"tZJ+kUZVD1vGYwMIUvAoaZmvS4I9NHV"
"c7a/267eChYY=\",\"CodeSize\":132628,\"Description\":\"\",\"FunctionArn\":\"ar"
"n:aws:lambda:us-east-1:352283894008:function:bi-driver\",\"FunctionName\":\"b"
"i-driver\",\"Handler\":\"index.handler\",\"LastModified\":\"2015-12-03T13:59:"
"05.219+0000\",\"MemorySize\":1024,\"Role\":\"arn:aws:iam::352283894008:role/l"
"ambda_kinesis_role\",\"Runtime\":\"nodejs\",\"Timeout\":59,\"Version\":\"$LAT"
"EST\",\"VpcConfig\":null},{\"CodeSha256\":\"QS10seyYGXrrhAnGMbJcTi+JOa4HWLaD+"
"9YCLYG3+VE=\",\"CodeSize\":121486,\"Description\":\"An Amazon Kinesis stream "
"processor that logs the data being published.\",\"FunctionArn\":\"arn:aws:lam"
"bda:us-east-1:352283894008:function:eholland-js-router\",\"FunctionName\":\"e"
"holland-js-router\",\"Handler\":\"index.handler\",\"LastModified\":\"2015-12-"
"02T14:50:41.923+0000\",\"MemorySize\":1024,\"Role\":\"arn:aws:iam::3522838940"
"08:role/lambda_kinesis_role\",\"Runtime\":\"nodejs\",\"Timeout\":60,\"Version"
"\":\"$LATEST\",\"VpcConfig\":null},{\"CodeSha256\":\"ey+6CSWe750XoPSdVSQditxx"
"oHNWmPFwve/MLPNs/Do=\",\"CodeSize\":253,\"Description\":\"\",\"FunctionArn\":"
"\"arn:aws:lambda:us-east-1:352283894008:function:eholland-test-aims-user\",\""
"FunctionName\":\"eholland-test-aims-user\",\"Handler\":\"lambda_function.lamb"
"da_handler\",\"LastModified\":\"2015-11-09T17:32:46.030+0000\",\"MemorySize\""
":128,\"Role\":\"arn:aws:iam::352283894008:role/lambda_basic_execution\",\"Run"
"time\":\"python2.7\",\"Timeout\":3,\"Version\":\"$LATEST\",\"VpcConfig\":null"
"},{\"CodeSha256\":\"R9HaKKCPPAem0ikKrkMGpDtX95M8egDCDd+4Ws+Kk5c=\",\"CodeSize"
"\":253,\"Description\":\"aims users transform function\",\"FunctionArn\":\"ar"
"n:aws:lambda:us-east-1:352283894008:function:eholland-test-aims-users\",\"Fun"
"ctionName\":\"eholland-test-aims-users\",\"Handler\":\"lambda_function.lambda"
"_handler\",\"LastModified\":\"2015-11-02T13:59:32.509+0000\",\"MemorySize\":1"
"28,\"Role\":\"arn:aws:iam::352283894008:role/lambda_basic_execution\",\"Runti"
"me\":\"python2.7\",\"Timeout\":3,\"Version\":\"$LATEST\",\"VpcConfig\":null},"
"{\"CodeSha256\":\"PGG1vCAQpc8J7e4HL1z1Pv4DGSZEmhnJaNPTGDS29kk=\",\"CodeSize\""
":32372,\"Description\":\"An Amazon Kinesis stream processor that logs the dat"
"a being published.\",\"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008:"
"function:eholland-test-router\",\"FunctionName\":\"eholland-test-router\",\"H"
"andler\":\"ProcessKinesisRecords.lambda_handler\",\"LastModified\":\"2015-11-"
"09T10:58:50.458+0000\",\"MemorySize\":256,\"Role\":\"arn:aws:iam::35228389400"
"8:role/lambda_kinesis_role\",\"Runtime\":\"python2.7\",\"Timeout\":59,\"Versi"
"on\":\"$LATEST\",\"VpcConfig\":null},{\"CodeSha256\":\"8twXwGaAXyr8Li81rGhM6v"
"lMh+dvXglwqgIshfaio+U=\",\"CodeSize\":708740,\"Description\":\"Lambda Router "
"Function for ETL Service\",\"FunctionArn\":\"arn:aws:lambda:us-east-1:3522838"
"94008:function:us-east-1_base_eholland_master_lambda_route\",\"FunctionName\""
":\"us-east-1_base_eholland_master_lambda_route\",\"Handler\":\"ProcessKinesis"
"Records.handler\",\"LastModified\":\"2015-11-11T10:13:19.043+0000\",\"MemoryS"
"ize\":512,\"Role\":\"arn:aws:iam::352283894008:role/lambda_kinesis_role\",\"R"
"untime\":\"nodejs\",\"Timeout\":59,\"Version\":\"$LATEST\",\"VpcConfig\":null"
"},{\"CodeSha256\":\"aDwLJhljsMVHsaN+LM4jRmsSLKKBdS+eFrAhKmJ/zbQ=\",\"CodeSize"
"\":253,\"Description\":\"\",\"FunctionArn\":\"arn:aws:lambda:us-east-1:352283"
"894008:function:us-east-1_base_eholland_master_lambda_route-aims-user-create\""
",\"FunctionName\":\"us-east-1_base_eholland_master_lambda_route-aims-user-cr"
"eate\",\"Handler\":\"lambda_function.lambda_handler\",\"LastModified\":\"2015"
"-11-11T09:42:10.303+0000\",\"MemorySize\":128,\"Role\":\"arn:aws:iam::3522838"
"94008:role/lambda_basic_execution\",\"Runtime\":\"python2.7\",\"Timeout\":3,"
"\"Version\":\"$LATEST\",\"VpcConfig\":null},{\"CodeSha256\":\"zeoBX1hIWJBHk1mu"
"Je1iFyS1CcAmsT0Ct4IcpiPf8QM=\",\"CodeSize\":848,\"Description\":\"\",\"Functi"
"onArn\":\"arn:aws:lambda:us-east-1:352283894008:function:name\",\"FunctionNam"
"e\":\"name\",\"Handler\":\"index.process\",\"LastModified\":\"2015-12-10T13:5"
"7:48.214+0000\",\"MemorySize\":512,\"Role\":\"arn:aws:iam::352283894008:role/"
"lambda_kinesis_role\",\"Runtime\":\"nodejs\",\"Timeout\":30,\"Version\":\"$LA"
"TEST\",\"VpcConfig\":null},{\"CodeSha256\":\"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0C"
"t4IcpiPf8QM=\",\"CodeSize\":848,\"Description\":\"\",\"FunctionArn\":\"arn:aw"
"s:lambda:us-east-1:352283894008:function:name2\",\"FunctionName\":\"name2\","
"\"Handler\":\"index.process\",\"LastModified\":\"2015-12-10T11:31:21.106+0000"
"\",\"MemorySize\":128,\"Role\":\"arn:aws:iam::352283894008:role/lambda_basic_e"
"xecution\",\"Runtime\":\"nodejs\",\"Timeout\":3,\"Version\":\"$LATEST\",\"Vpc"
"Config\":null}],\"NextMarker\":null}">>)
    }.

mocked_list_versions_by_function() ->
    {
      [?BASE_URL ++ "functions/name/versions?", get, '_', <<>>, '_', '_'],
      make_response(<<"{\"NextMarker\":null,\"Versions\":[{\"CodeSha256\":\"z"
"eoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4IcpiPf8QM=\",\"CodeSize\":848,\"Description"
"\":\"\",\"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008:function:name:"
"$LATEST\",\"FunctionName\":\"name\",\"Handler\":\"index.process\",\"LastModif"
"ied\":\"2015-12-10T13:57:48.214+0000\",\"MemorySize\":512,\"Role\":\"arn:aws:"
"iam::352283894008:role/lambda_kinesis_role\",\"Runtime\":\"nodejs\",\"Timeout"
"\":30,\"Version\":\"$LATEST\",\"VpcConfig\":null},{\"CodeSha256\":\"zeoBX1hIW"
"JBHk1muJe1iFyS1CcAmsT0Ct4IcpiPf8QM=\",\"CodeSize\":848,\"Description\":\"\","
"\"FunctionArn\":\"arn:aws:lambda:us-east-1:352283894008:function:name:1\",\"Fu"
"nctionName\":\"name\",\"Handler\":\"index.process\",\"LastModified\":\"2015-1"
"2-10T11:36:12.776+0000\",\"MemorySize\":128,\"Role\":\"arn:aws:iam::352283894"
"008:role/lambda_kinesis_role\",\"Runtime\":\"nodejs\",\"Timeout\":3,\"Version"
"\":\"1\",\"VpcConfig\":null},{\"CodeSha256\":\"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT"
"0Ct4IcpiPf8QM=\",\"CodeSize\":848,\"Description\":\"\",\"FunctionArn\":\"arn:"
"aws:lambda:us-east-1:352283894008:function:name:2\",\"FunctionName\":\"name\""
",\"Handler\":\"index.process\",\"LastModified\":\"2015-12-10T13:56:43.171+000"
"0\",\"MemorySize\":128,\"Role\":\"arn:aws:iam::352283894008:role/lambda_kines"
"is_role\",\"Runtime\":\"nodejs\",\"Timeout\":3,\"Version\":\"2\",\"VpcConfig"
"\":null}]}">>)
    }.

mocked_publish_version() ->
    {
      [?BASE_URL ++ "functions/name/versions", post, '_', <<"{}">>, '_', '_'],
      make_response(<<"{\"CodeSha256\":\"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4Ic"
"piPf8QM=\",\"CodeSize\":848,\"Description\":\"\",\"FunctionArn\":\"arn:aws:la"
"mbda:us-east-1:352283894008:function:name:3\",\"FunctionName\":\"name\",\"Han"
"dler\":\"index.process\",\"LastModified\":\"2015-12-10T13:57:48.214+0000\",\""
"MemorySize\":512,\"Role\":\"arn:aws:iam::352283894008:role/lambda_kinesis_rol"
"e\",\"Runtime\":\"nodejs\",\"Timeout\":30,\"Version\":\"3\",\"VpcConfig\":nul"
"l}">>)
    }.

mocked_update_alias() ->
    {
      [?BASE_URL ++ "functions/name/aliases/aliasName", put,
       '_', <<"{}">>, '_', '_'],
      make_response(<<"{\"AliasArn\":\"arn:aws:lambda:us-east-1:352283894008:"
"function:name:aliasName\",\"Description\":\"\",\"FunctionVersion\":\"$LATEST"
"\",\"Name\":\"aliasName\"}">>)
    }.
mocked_update_event_source_mapping() ->
    {
      [?BASE_URL ++ "event-source-mappings/a45b58ec-a539-4c47-929e-174b4dd2d963",
       put, '_', <<"{\"BatchSize\":100}">>, '_', '_'],
      make_response(<<"{\"BatchSize\":100,\"EventSourceArn\":\"arn:aws:kinesi"
"s:us-east-1:352283894008:stream/eholland-test\",\"FunctionArn\":\"arn:aws:lam"
"bda:us-east-1:352283894008:function:name\",\"LastModified\":1449844011.991,\""
"LastProcessingResult\":\"No records processed\",\"State\":\"Updating\",\"Stat"
"eTransitionReason\":\"User action\",\"UUID\":\"a45b58ec-a539-4c47-929e-174b4d"
"d2d963\"}">>)
    }.

mocked_update_function_code() ->
    {
      [?BASE_URL ++ "functions/name/code", put, '_',
       <<"{\"Publish\":true,\"S3Bucket\":\"bi-lambda\",\"S3Key\":\"local_transf"
         "orm/bi-assets-environment-create-environment_1-0-0_latest.zip\"}">>,
       '_', '_'],
      make_response(<<"{\"CodeSha256\":\"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4Ic"
"piPf8QM=\",\"CodeSize\":848,\"Description\":\"\",\"FunctionArn\":\"arn:aws:la"
"mbda:us-east-1:352283894008:function:name:4\",\"FunctionName\":\"name\",\"Han"
"dler\":\"index.process\",\"LastModified\":\"2015-12-11T14:29:17.023+0000\",\""
"MemorySize\":512,\"Role\":\"arn:aws:iam::352283894008:role/lambda_kinesis_rol"
"e\",\"Runtime\":\"nodejs\",\"Timeout\":30,\"Version\":\"4\",\"VpcConfig\":nul"
"l}">>)
    }.
mocked_update_function_configuration() ->
    {
      [?BASE_URL ++ "functions/name/configuration", put, '_',
       <<"{\"MemorySize\":512,\"Timeout\":30}">>, '_', '_'],
      make_response(<<"{\"CodeSha256\":\"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4Ic"
"piPf8QM=\",\"CodeSize\":848,\"Description\":\"\",\"FunctionArn\":\"arn:aws:la"
"mbda:us-east-1:352283894008:function:name\",\"FunctionName\":\"name\",\"Handl"
"er\":\"index.process\",\"LastModified\":\"2015-12-11T14:31:52.034+0000\",\"Me"
"morySize\":512,\"Role\":\"arn:aws:iam::352283894008:role/lambda_kinesis_role\"
"",\"Runtime\":\"nodejs\",\"Timeout\":30,\"Version\":\"$LATEST\",\"VpcConfig\""
":null}">>)
    }.

api_tests(_) ->
    [
     fun() ->
             Result   = erlcloud_lambda:list_functions(),
             Expected =
                 {ok,
                  [{<<"Functions">>,
                    [[{<<"CodeSha256">>,<<"XmLDAZXEkl5KbA8ezZpwFU+bjgTXBehUmWGOScl4F2A=">>},
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
                   {<<"NextMarker">>,null}]},
             ?assertEqual(Expected, Result)
     end,
     fun() ->
             Result = erlcloud_lambda:create_alias(<<"name">>, <<"$LATEST">>,
                                                   <<"aliasName1">>, []),
             Expected = {ok,[{<<"AliasArn">>,
                              <<"arn:aws:lambda:us-east-1:352283894008:function:name:aliasName1">>},
                             {<<"Description">>,<<>>},
                             {<<"FunctionVersion">>,<<"$LATEST">>},
                             {<<"Name">>,<<"aliasName1">>}]},
             ?assertEqual(Expected, Result)
     end,
     fun() ->
             Result = erlcloud_lambda:create_event_source_mapping(
                        <<"arn:aws:kinesis:us-east-1:352283894008:stream/eholland-test">>,
                        <<"name">>, <<"TRIM_HORIZON">>, []),
             Expected = {ok,
                         [{<<"BatchSize">>,100},
                          {<<"EventSourceArn">>,<<"arn:aws:kinesis:us-east-1:352283894008:stream/eholland-test">>},
                          {<<"FunctionArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:name">>},
                          {<<"LastModified">>,1449845416.123},
                          {<<"LastProcessingResult">>,<<"No records processed">>},
                          {<<"State">>,<<"Creating">>},
                          {<<"StateTransitionReason">>,<<"User action">>},
                          {<<"UUID">>,<<"3f303f86-7395-43f3-9902-f5c80f0a5382">>}]},
             ?assertEqual(Expected, Result)
     end,
     fun() ->
             Result = erlcloud_lambda:create_function(
                        #erlcloud_lambda_code{s3Bucket = <<"bi-lambda">>,
                                              s3Key = <<"local_transform/bi-ass"
                                                        "ets-environment-create"
                                                        "-environment_1-0-0_lat"
                                                        "est.zip">>
                                             },
                        <<"name">>, <<"index.process">>,
                        <<"arn:aws:iam::352283894008:role/lambda_basic_execution">>,
                        nodejs, []),
             Expected = {ok,[{<<"CodeSha256">>,
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
                             {<<"VpcConfig">>,null}]},
             ?assertEqual(Expected, Result)
     end,
     fun() ->
             Result = erlcloud_lambda:delete_event_source_mapping(
                        <<"6554f300-551b-46a6-829c-41b6af6022c6">>),
             Expected = {ok,
                         [{<<"BatchSize">>,100},
                          {<<"EventSourceArn">>,<<"arn:aws:kinesis:us-east-1:352283894008:stream/eholland-test">>},
                          {<<"FunctionArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:name">>},
                          {<<"LastModified">>,1449843960.0},
                          {<<"LastProcessingResult">>,<<"No records processed">>},
                          {<<"State">>,<<"Deleting">>},
                          {<<"StateTransitionReason">>,<<"User action">>},
                          {<<"UUID">>,<<"a45b58ec-a539-4c47-929e-174b4dd2d963">>}]},
             ?assertEqual(Expected, Result)
     end,
     fun() ->
             Result = erlcloud_lambda:get_alias(<<"name">>, <<"aliasName">>),
             Expected = {ok, [{<<"AliasArn">>,
                               <<"arn:aws:lambda:us-east-1:352283894008:function:name:aliasName">>},
                              {<<"Description">>,<<>>},
                              {<<"FunctionVersion">>,<<"$LATEST">>},
                              {<<"Name">>,<<"aliasName">>}]},
             ?assertEqual(Expected, Result)
     end,
     fun() ->
             Result = erlcloud_lambda:get_event_source_mapping(
                        <<"a45b58ec-a539-4c47-929e-174b4dd2d963">>),
             Expected = {ok, [{<<"BatchSize">>,100},
                              {<<"EventSourceArn">>,
                               <<"arn:aws:kinesis:us-east-1:352283894008:stream/eholland-test">>},
                              {<<"FunctionArn">>,
                               <<"arn:aws:lambda:us-east-1:352283894008:function:name">>},
                              {<<"LastModified">>,1449841860.0},
                              {<<"LastProcessingResult">>,<<"No records processed">>},
                              {<<"State">>,<<"Enabled">>},
                              {<<"StateTransitionReason">>,<<"User action">>},
                              {<<"UUID">>,<<"a45b58ec-a539-4c47-929e-174b4dd2d963">>}]},
             ?assertEqual(Expected, Result)
     end,
     fun() ->
             Result = erlcloud_lambda:get_function(<<"name">>),
             Expected = {ok, [{<<"Code">>,
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
                                {<<"VpcConfig">>,null}]}]},
             ?assertEqual(Expected, Result)
     end,
     fun() ->
             Result = erlcloud_lambda:get_function_configuration(<<"name">>),
             Expected = {ok, [{<<"CodeSha256">>,<<"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4IcpiPf8QM=">>},
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
                              {<<"VpcConfig">>,null}]},
             ?assertEqual(Expected, Result)
     end,
     fun() ->
             Result = erlcloud_lambda:invoke(<<"name">>, []),
             Expected = {ok, [{<<"message">>, <<"Hello World!">>}]},
             ?assertEqual(Expected, Result)
     end,
     fun() ->
             Result = erlcloud_lambda:invoke(<<"name">>, [], [{show_headers, true}], #aws_config{}),
             Expected = {ok, [], [{<<"message">>, <<"Hello World!">>}]},
             ?assertEqual(Expected, Result)
     end,
     fun() ->
             Result = erlcloud_lambda:list_aliases(<<"name">>),
             Expected = {ok, [{<<"Aliases">>,
                               [[{<<"AliasArn">>, <<"arn:aws:lambda:us-east-1:352283894008:function:name:aliasName">>},
                                 {<<"Description">>,<<>>},
                                 {<<"FunctionVersion">>,<<"$LATEST">>},
                                 {<<"Name">>,<<"aliasName">>}],
                                [{<<"AliasArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:name:aliasName1">>},
                                 {<<"Description">>,<<>>},
                                 {<<"FunctionVersion">>,<<"$LATEST">>},
                                 {<<"Name">>,<<"aliasName1">>}]]},
                              {<<"NextMarker">>,null}]},
             ?assertEqual(Expected, Result)
     end,
     fun() ->
             Result = erlcloud_lambda:list_event_source_mappings(
                        <<"arn:aws:kinesis:us-east-1:352283894008:stream/eholland-test">>,
                        <<"name">>),
             Expected = {ok, [{<<"EventSourceMappings">>,
                               [[{<<"BatchSize">>,100},
                                 {<<"EventSourceArn">>,<<"arn:aws:kinesis:us-east-1:352283894008:stream/eholland-test">>},
                                 {<<"FunctionArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:name">>},
                                 {<<"LastModified">>,1449841860.0},
                                 {<<"LastProcessingResult">>,<<"No records processed">>},
                                 {<<"State">>,<<"Enabled">>},
                                 {<<"StateTransitionReason">>,<<"User action">>},
                                 {<<"UUID">>,<<"a45b58ec-a539-4c47-929e-174b4dd2d963">>}]]},
                              {<<"NextMarker">>,null}]},
             ?assertEqual(Expected, Result)
     end,
     fun() ->
             Result = erlcloud_lambda:list_versions_by_function(<<"name">>),
             Expected = {ok, [{<<"NextMarker">>,null},
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
                                 {<<"VpcConfig">>,null}]]}]},
             ?assertEqual(Expected, Result)
     end,
     fun() ->
             Result = erlcloud_lambda:publish_version(<<"name">>),
             Expected = {ok, [{<<"CodeSha256">>,<<"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4IcpiPf8QM=">>},
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
                              {<<"VpcConfig">>,null}]},
             ?assertEqual(Expected, Result)
     end,
     fun() ->
             Result = erlcloud_lambda:update_alias(<<"name">>, <<"aliasName">>),
             Expected = {ok, [{<<"AliasArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:name:aliasName">>},
                              {<<"Description">>,<<>>},
                              {<<"FunctionVersion">>,<<"$LATEST">>},
                              {<<"Name">>,<<"aliasName">>}]},
             ?assertEqual(Expected, Result)
     end,
     fun() ->
             Result = erlcloud_lambda:update_event_source_mapping(
                        <<"a45b58ec-a539-4c47-929e-174b4dd2d963">>,
                        100, undefined, undefined),
             Expected = {ok,
                         [{<<"BatchSize">>,100},
                          {<<"EventSourceArn">>,<<"arn:aws:kinesis:us-east-1:352283894008:stream/eholland-test">>},
                          {<<"FunctionArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:name">>},
                          {<<"LastModified">>,1449844011.991},
                          {<<"LastProcessingResult">>,<<"No records processed">>},
                          {<<"State">>,<<"Updating">>},
                          {<<"StateTransitionReason">>,<<"User action">>},
                          {<<"UUID">>,<<"a45b58ec-a539-4c47-929e-174b4dd2d963">>}]},
             ?assertEqual(Expected, Result)
     end,
     fun() ->
             Result = erlcloud_lambda:update_function_code(
                        <<"name">>, true, #erlcloud_lambda_code{
                                             s3Bucket = <<"bi-lambda">>,
                                             s3Key = <<"local_transform/bi-assets-environment-create-environment_1-0-0_latest.zip">>
                                            }),
             Expected = {ok,
                         [{<<"CodeSha256">>,<<"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4IcpiPf8QM=">>},
                          {<<"CodeSize">>,848},
                          {<<"Description">>,<<>>},
                          {<<"FunctionArn">>,<<"arn:aws:lambda:us-east-1:352283894008:function:name:4">>},
                          {<<"FunctionName">>,<<"name">>},
                          {<<"Handler">>,<<"index.process">>},
                          {<<"LastModified">>,<<"2015-12-11T14:29:17.023+0000">>},
                          {<<"MemorySize">>,512},
                          {<<"Role">>,<<"arn:aws:iam::352283894008:role/lambda_kinesis_role">>},
                          {<<"Runtime">>,<<"nodejs">>},
                          {<<"Timeout">>,30},
                          {<<"Version">>,<<"4">>},
                          {<<"VpcConfig">>,null}]},
             ?assertEqual(Expected, Result)
     end,
     fun() ->
             Result = erlcloud_lambda:update_function_configuration(
                        <<"name">>, undefined, undefined, 512, undefined, 30),
             Expected = {ok, [{<<"CodeSha256">>,<<"zeoBX1hIWJBHk1muJe1iFyS1CcAmsT0Ct4IcpiPf8QM=">>},
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
                              {<<"VpcConfig">>,null}]},
             ?assertEqual(Expected, Result)
     end
    ].

make_response(Value) ->
    {ok, {{200, <<"OK">>}, [], Value}}.
