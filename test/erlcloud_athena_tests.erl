-module(erlcloud_athena_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlcloud_aws.hrl").

%% API
-export([]).

-define(EHTTPC, erlcloud_httpc).

-define(DB_NAME_1,    <<"db_1">>).
-define(DB_NAME_2,    <<"db_2">>).
-define(QUERY_ID_1,   <<"59b6affa-8898-4a98-a5c6-afa9ee2250cc">>).
-define(QUERY_ID_2,   <<"1500090a-e458-4620-95eb-708d913ad32c">>).
-define(QUERY_NAME_1, <<"query_1">>).
-define(QUERY_NAME_2, <<"query_2">>).
-define(QUERY_DESC_1, <<"desc_1">>).
-define(QUERY_DESC_2, <<"desc_2">>).
-define(QUERY_STR_1,  <<"select * from tbl_1;">>).
-define(QUERY_STR_2,  <<"select * from tbl_2;">>).
-define(LOCATION_1,   <<"s3://1/1.csv">>).
-define(LOCATION_2,   <<"s3://2/2.csv">>).
-define(CLIENT_TOKEN, <<"some-token-uuid">>).

-define(BATCH_GET_NAMED_QUERY_RESP,
    #{<<"NamedQueries">> =>
      [#{<<"Database">>     => ?DB_NAME_1,
         <<"Description">>  => ?QUERY_DESC_1,
         <<"Name">>         => ?QUERY_NAME_1,
         <<"NamedQueryId">> => ?QUERY_ID_1,
         <<"QueryString">>  => ?QUERY_STR_1},
       #{<<"Database">>     => ?DB_NAME_2,
         <<"Description">>  => ?QUERY_DESC_2,
         <<"Name">>         => ?QUERY_NAME_2,
         <<"NamedQueryId">> => ?QUERY_ID_2,
         <<"QueryString">>  => ?QUERY_STR_2}],
      <<"UnprocessedNamedQueryIds">> => []}
).

-define(BATCH_GET_QUERY_EXECUTION_RESP,
    #{<<"QueryExecutions">> =>
      [#{<<"Query">>                 => ?QUERY_STR_1,
         <<"QueryExecutionContext">> => #{<<"Database">> => ?DB_NAME_1},
         <<"QueryExecutionId">>      => ?QUERY_ID_1,
         <<"ResultConfiguration">>   => #{<<"OutputLocation">> => ?LOCATION_1},
         <<"Statistics">> => #{<<"DataScannedInBytes">>          => 2524,
                               <<"EngineExecutionTimeInMillis">> => 1970},
         <<"Status">> => #{<<"CompletionDateTime">> => 1506094499.507,
                           <<"State">>              => <<"SUCCEEDED">>,
                           <<"SubmissionDateTime">> => 1506094497.16}},
       #{<<"Query">>                 => ?QUERY_STR_2,
         <<"QueryExecutionContext">> => #{<<"Database">> => ?DB_NAME_2},
         <<"QueryExecutionId">>      => ?QUERY_ID_2,
         <<"ResultConfiguration">>   => #{<<"OutputLocation">> => ?LOCATION_2},
         <<"Statistics">> => #{<<"DataScannedInBytes">>          => 2524,
                               <<"EngineExecutionTimeInMillis">> => 1970},
         <<"Status">> => #{<<"CompletionDateTime">> => 1506094499.507,
                           <<"State">>              => <<"SUCCEEDED">>,
                           <<"SubmissionDateTime">> => 1506094497.16}}],
      <<"UnprocessedQueryExecutionIds">> => []}
).

-define(CREATE_NAMED_QUERY_RESP,
    [{<<"NamedQueryId">>, ?QUERY_ID_1}]).

-define(GET_NAMED_QUERY_RESP,
    #{<<"NamedQuery">> =>
      #{<<"Database">>     => ?DB_NAME_1,
        <<"Description">>  => ?QUERY_DESC_1,
        <<"Name">>         => ?QUERY_NAME_1,
        <<"NamedQueryId">> => ?QUERY_ID_1,
        <<"QueryString">>  => ?QUERY_STR_1}}
).

-define(GET_QUERY_EXECUTION_RESP,
    #{<<"QueryExecutions">> =>
      #{<<"Query">>                 => ?QUERY_STR_1,
        <<"QueryExecutionContext">> => #{<<"Database">> => ?DB_NAME_1},
        <<"QueryExecutionId">>      => ?QUERY_ID_1,
        <<"ResultConfiguration">>   => #{<<"OutputLocation">> => ?LOCATION_1},
        <<"Statistics">> => #{<<"DataScannedInBytes">>          => 2524,
                              <<"EngineExecutionTimeInMillis">> => 1970},
        <<"Status">> => #{<<"CompletionDateTime">> => 1506094499.507,
                          <<"State">>              => <<"SUCCEEDED">>,
                          <<"SubmissionDateTime">> => 1506094497.16}}}
).

-define(GET_QUERY_RESULTS_RESP,
    #{<<"NextToken">> => ?CLIENT_TOKEN,
      <<"ResultSet">> =>
      #{<<"ColumnInfos">> =>
        [#{<<"CaseSensitive">> => true,
           <<"CatalogName">>   => <<"hive">>,
           <<"Label">>         => <<"id">>,
           <<"Name">>          => <<"id">>,
           <<"Nullable">>      => <<"UNKNOWN">>,
           <<"Precision">>     => 1073741824,
           <<"Scale">>         => 0,
           <<"SchemaName">>    => <<>>,
           <<"TableName">>     => <<>>,
           <<"Type">>          => <<"varchar">>}],
        <<"ResultRows">> => [#{<<"Data">> => [<<"id">>]}],
        <<"ResultSetMetadata">> =>
        #{<<"ColumnInfo">> =>
          [#{<<"CaseSensitive">> => true,
             <<"CatalogName">>   => <<"hive">>,
             <<"Label">>         => <<"id">>,
             <<"Name">>          => <<"id">>,
             <<"Nullable">>      => <<"UNKNOWN">>,
             <<"Precision">>     => 1073741824,
             <<"Scale">>         => 0,
             <<"SchemaName">>    => <<>>,
             <<"TableName">>     => <<>>,
             <<"Type">>          => <<"varchar">>}]},
        <<"Rows">> => [#{<<"Data">> => [#{<<"VarCharValue">> =><<"id">>}]}]},
      <<"UpdateCount">> => 0,
      <<"UpdateType">>  => <<>>}
).

-define(LIST_NAMED_QUERIES_RESP,
    #{<<"NamedQueryIds">> => [?QUERY_ID_1, ?QUERY_ID_2],
      <<"NextToken">>     => ?CLIENT_TOKEN}).

-define(LIST_QUERY_EXECUTIONS_RESP,
    #{<<"QueryExecutionIds">> => [?QUERY_ID_1, ?QUERY_ID_2],
      <<"NextToken">>         => ?CLIENT_TOKEN}).

-define(START_QUERY_EXECUTION_RESP,
    #{<<"QueryExecutionId">> => ?QUERY_ID_1}
).

setup() ->
    erlcloud_athena:configure("test-access-key", "test-secret-key"),
    meck:new(?EHTTPC, [passthrough]),
    meck:expect(?EHTTPC, request, 6, fun do_erlcloud_httpc_request/6),
    [?EHTTPC].

erlcloud_athena_test_() ->
    {
        foreach,
        fun setup/0,
        fun meck:unload/1,
        [
            fun test_batch_get_named_query/0,
            fun test_batch_get_query_execution/0,
            fun test_create_named_query/0,
            fun test_delete_named_query/0,
            fun test_get_named_query/0,
            fun test_get_query_execution/0,
            fun test_get_query_results/0,
            fun test_list_named_queries/0,
            fun test_list_query_executions/0,
            fun test_start_query_execution/0,
            fun test_start_query_execution_with_encryption/0,
            fun test_start_query_execution_without_kms_key/0,
            fun test_start_query_execution_without_encrypt_option/0,
            fun test_stop_query_execution/0,
            fun test_error_no_retry/0,
            fun test_error_retry/0
        ]
    }.

test_batch_get_named_query() ->
    Request  = #{<<"NamedQueryIds">> => [?QUERY_ID_1, ?QUERY_ID_2]},
    Expected = {ok, ?BATCH_GET_NAMED_QUERY_RESP},
    TestFun  = fun() ->
        erlcloud_athena:batch_get_named_query([?QUERY_ID_1, ?QUERY_ID_2])
               end,
    do_test(Request, Expected, TestFun).

test_batch_get_query_execution() ->
    Request  = #{<<"QueryExecutionIds">> => [?QUERY_ID_1, ?QUERY_ID_2]},
    Expected = {ok, ?BATCH_GET_QUERY_EXECUTION_RESP},
    TestFun  = fun() ->
        erlcloud_athena:batch_get_query_execution([?QUERY_ID_1, ?QUERY_ID_2])
               end,
    do_test(Request, Expected, TestFun).

test_create_named_query() ->
    Request = #{<<"ClientRequestToken">> => ?CLIENT_TOKEN,
                <<"Database">>           => ?DB_NAME_1,
                <<"Name">>               => ?QUERY_NAME_1,
                <<"QueryString">>        => ?QUERY_STR_1,
                <<"Description">>        => ?QUERY_DESC_1},
    Expected = {ok, ?QUERY_ID_1},
    TestFun  = fun() ->
        erlcloud_athena:create_named_query(?CLIENT_TOKEN, ?DB_NAME_1,
                                           ?QUERY_NAME_1, ?QUERY_STR_1,
                                           ?QUERY_DESC_1)
               end,
    do_test(Request, Expected, TestFun).

test_delete_named_query() ->
    Request  = #{<<"NamedQueryId">> => ?QUERY_ID_1},
    Expected = ok,
    TestFun  = fun() -> erlcloud_athena:delete_named_query(?QUERY_ID_1) end,
    do_test(Request, Expected, TestFun).

test_get_named_query() ->
    Request  = #{<<"NamedQueryId">> => ?QUERY_ID_1},
    Expected = {ok, ?GET_NAMED_QUERY_RESP},
    TestFun  = fun() -> erlcloud_athena:get_named_query(?QUERY_ID_1) end,
    do_test(Request, Expected, TestFun).

test_get_query_execution() ->
    Request  = #{<<"QueryExecutionId">> => ?QUERY_ID_1},
    Expected = {ok, ?GET_QUERY_EXECUTION_RESP},
    TestFun  = fun() -> erlcloud_athena:get_query_execution(?QUERY_ID_1) end,
    do_test(Request, Expected, TestFun).

test_get_query_results() ->
    Options  = #{<<"MaxResults">> => 1,
                 <<"NextToken">>  => ?CLIENT_TOKEN},
    Request  = Options#{<<"QueryExecutionId">> => ?QUERY_ID_1},
    Expected = {ok, ?GET_QUERY_RESULTS_RESP},
    TestFun  = fun() ->
        erlcloud_athena:get_query_results(?QUERY_ID_1, Options)
               end,
    do_test(Request, Expected, TestFun).

test_list_named_queries() ->
    Expected = {ok, ?LIST_NAMED_QUERIES_RESP},
    TestFun  = fun() -> erlcloud_athena:list_named_queries() end,
    do_test(#{}, Expected, TestFun).

test_list_query_executions() ->
    Request  = #{<<"MaxResults">> => 1,
                 <<"NextToken">>  => ?CLIENT_TOKEN},
    Expected = {ok, ?LIST_QUERY_EXECUTIONS_RESP},
    TestFun  = fun() -> erlcloud_athena:list_query_executions(Request) end,
    do_test(Request, Expected, TestFun).

test_start_query_execution() ->
    Request  = get_start_query_execution_req(#{}),
    Expected = {ok, ?QUERY_ID_1},
    TestFun  = fun() ->
        erlcloud_athena:start_query_execution(?CLIENT_TOKEN, ?DB_NAME_1,
                                              ?QUERY_STR_1, ?LOCATION_1)
               end,
    do_test(Request, Expected, TestFun).

test_start_query_execution_with_encryption() ->
    EncryptConfig = #{<<"EncryptionConfiguration">> =>
                      #{<<"EncryptionOption">> => <<"SSE_KMS">>,
                        <<"KmsKey">>           => <<"some-key">>}},
    Request  = get_start_query_execution_req(EncryptConfig),
    Expected = {ok, ?QUERY_ID_1},
    TestFun  = fun() ->
        erlcloud_athena:start_query_execution(?CLIENT_TOKEN, ?DB_NAME_1,
                                              ?QUERY_STR_1, ?LOCATION_1,
                                              <<"SSE_KMS">>, <<"some-key">>)
               end,
    do_test(Request, Expected, TestFun).

test_start_query_execution_without_kms_key() ->
    EncryptConfig = #{<<"EncryptionConfiguration">> =>
                      #{<<"EncryptionOption">> => <<"SSE_KMS">>}},
    Request  = get_start_query_execution_req(EncryptConfig),
    Expected = {ok, ?QUERY_ID_1},
    TestFun  = fun() ->
        erlcloud_athena:start_query_execution(?CLIENT_TOKEN, ?DB_NAME_1,
                                              ?QUERY_STR_1, ?LOCATION_1,
                                              <<"SSE_KMS">>, undefined)
               end,
    do_test(Request, Expected, TestFun).

test_start_query_execution_without_encrypt_option() ->
    Request  = get_start_query_execution_req(#{}),
    Expected = {ok, ?QUERY_ID_1},
    TestFun  = fun() ->
        erlcloud_athena:start_query_execution(?CLIENT_TOKEN, ?DB_NAME_1,
                                              ?QUERY_STR_1, ?LOCATION_1,
                                              undefined, <<"some-key">>)
               end,
    do_test(Request, Expected, TestFun).

test_stop_query_execution() ->
    Request  = #{<<"QueryExecutionId">> => ?QUERY_ID_1},
    Expected = ok,
    TestFun  = fun() -> erlcloud_athena:stop_query_execution(?QUERY_ID_1) end,
    do_test(Request, Expected, TestFun).

test_error_no_retry() ->
    erlcloud_athena:configure("test-access-key", "test-secret-key"),
    ErrCode = 400,
    Status  = "Bad Request",
    ErrMsg  = <<"Message">>,
    meck:expect(?EHTTPC, request, 6, {ok, {{ErrCode, Status}, [], ErrMsg}}),
    ?assertEqual(
        {error, {http_error, ErrCode, Status, ErrMsg, []}},
        erlcloud_athena:stop_query_execution(?QUERY_ID_1)
    ).

test_error_retry() ->
    erlcloud_athena:configure("test-access-key", "test-secret-key"),
    ErrCode1 = 500,
    ErrCode2 = 400,
    Status1  = "Internal Server Error",
    Status2  = "Bad Request",
    ErrMsg1 = <<"Message-1">>,
    ErrMsg2 = <<"Message-2">>,
    meck:sequence(?EHTTPC, request, 6,
                  [{ok, {{ErrCode1, Status1}, [], ErrMsg1}},
                   {ok, {{ErrCode2, Status2}, [], ErrMsg2}}]),
    ?assertEqual(
        {error, {http_error, ErrCode2, Status2, ErrMsg2, []}},
        erlcloud_athena:stop_query_execution(?QUERY_ID_1)
    ).

do_test(Request, ExpectedResult, TestedFun) ->
    erlcloud_athena:configure("test-access-key", "test-secret-key"),
    ?assertEqual(ExpectedResult, TestedFun()),
    Encoded = jsx:encode(Request),
    ?assertMatch([{_, {?EHTTPC, request, [_, post, _, Encoded, _, _]}, _}],
                 meck:history(?EHTTPC)).

do_erlcloud_httpc_request(_, post, Headers, _, _, _) ->
    Target = proplists:get_value("x-amz-target", Headers),
    ["AmazonAthena", Operation] = string:tokens(Target, "."),
    RespBody =
        case Operation of
            "BatchGetNamedQuery"     -> ?BATCH_GET_NAMED_QUERY_RESP;
            "BatchGetQueryExecution" -> ?BATCH_GET_QUERY_EXECUTION_RESP;
            "CreateNamedQuery"       -> ?CREATE_NAMED_QUERY_RESP;
            "DeleteNamedQuery"       -> #{};
            "GetNamedQuery"          -> ?GET_NAMED_QUERY_RESP;
            "GetQueryExecution"      -> ?GET_QUERY_EXECUTION_RESP;
            "GetQueryResults"        -> ?GET_QUERY_RESULTS_RESP;
            "ListNamedQueries"       -> ?LIST_NAMED_QUERIES_RESP;
            "ListQueryExecutions"    -> ?LIST_QUERY_EXECUTIONS_RESP;
            "StartQueryExecution"    -> ?START_QUERY_EXECUTION_RESP;
            "StopQueryExecution"     -> #{}
        end,
    {ok, {{200, "OK"}, [], jsx:encode(RespBody)}}.

get_start_query_execution_req(EncryptConfig) ->
    ResultConfig = EncryptConfig#{<<"OutputLocation">> => ?LOCATION_1},
    #{<<"ClientRequestToken">>    => ?CLIENT_TOKEN,
      <<"QueryExecutionContext">> => #{<<"Database">> => ?DB_NAME_1},
      <<"QueryString">>           => ?QUERY_STR_1,
      <<"ResultConfiguration">>   => ResultConfig}.
