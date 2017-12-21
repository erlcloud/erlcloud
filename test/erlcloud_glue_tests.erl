-module(erlcloud_glue_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlcloud_aws.hrl").

%% API
-export([]).

-define(EHTTPC, erlcloud_httpc).

-define(DB_NAME,      <<"db-1">>).
-define(TBL_NAME,     <<"test_tbl">>).
-define(JOB_NAME,     <<"job-name">>).
-define(CRAWLER_NAME, <<"crawler-name">>).
-define(LOCATION,     <<"s3://tst/">>).
-define(CATALOG_ID,   <<"catalog-1">>).
-define(JOB_RUN_ID,   <<"job-run-1">>).

-define(TBL_INPUT,
    #{<<"CreateTime">> => 1506697886.0,
      <<"CreatedBy">>  => <<"arn:aws:iam::948063967832:user/anovak">>,
      <<"Name">>       => ?TBL_NAME,
      <<"Parameters">> => #{<<"EXTERNAL">> => <<"TRUE">>},
      <<"PartitionKeys">> =>
        [#{<<"Name">> => <<"dt">>, <<"Type">> => <<"string">>}],
      <<"Retention">> => 0,
      <<"StorageDescriptor">> =>
        #{<<"Columns">> =>
            [#{<<"Name">> => <<"id">>,  <<"Type">> => <<"string">>},
             #{<<"Name">> => <<"env">>, <<"Type">> => <<"string">>}],
          <<"Compressed">>      => false,
          <<"Location">>        => ?LOCATION,
          <<"NumberOfBuckets">> => 0,
          <<"SortColumns">>     => [],
          <<"StoredAsSubDirectories">> => false},
      <<"TableType">>  => <<"EXTERNAL_TABLE">>,
      <<"UpdateTime">> => 1506697886.0}
).

-define(DB_INPUT,
    #{<<"Name">>        => ?DB_NAME,
      <<"Description">> => <<"Database description">>}
).

-define(JOB_INPUT,
    #{<<"AllocatedCapacity">> => 10,
      <<"Connections">>       => #{},
      <<"DefaultArguments">>  => #{},
      <<"ExecutionProperty">> => #{<<"MaxConcurrentRuns">> => 1},
      <<"MaxRetries">>        => 0,
      <<"Name">>              => ?JOB_NAME,
      <<"Role">>              => <<"Glue-Test-Role">>,
      <<"Command">>           => #{<<"Name">>           => <<"glueetl">>,
                                   <<"ScriptLocation">> => ?LOCATION}}
).

-define(CRAWLER_INPUT,
    #{<<"DatabaseName">> => ?DB_NAME,
      <<"Name">>         => ?CRAWLER_NAME,
      <<"Role">>         => <<"some-iam-role">>,
      <<"Targets">>      =>
          #{<<"S3Targets">> => [
              #{<<"Path">>       => ?LOCATION,
                <<"Exclusions">> => [<<"**.json">>]}
          ]}
    }
).

-define(CRAWLER_METRIC,
    #{<<"CrawlerName">>          => ?CRAWLER_NAME,
      <<"LastRuntimeSeconds">>   => 10,
      <<"MedianRuntimeSeconds">> => 8,
      <<"StillEstimating">>      => false,
      <<"TablesCreated">>        => 2,
      <<"TablesDeleted">>        => 2,
      <<"TablesUpdated">>        => 2}
).

-define(JOB_RUN,
    #{<<"AllocatedCapacity">> => 10,
      <<"Arguments">>         => #{},
      <<"Attempt">>           => 0,
      <<"ErrorMessage">>      => <<"NameError">>,
      <<"Id">>                => ?JOB_RUN_ID,
      <<"JobName">>           => ?JOB_NAME,
      <<"JobRunState">>       => <<"FAILED">>,
      <<"PredecessorRuns">>   => []}).

-define(GET_TABLE,     #{<<"Table">>     => ?TBL_INPUT}).
-define(GET_TABLES,    #{<<"TableList">> => [?TBL_INPUT]}).
-define(GET_CRAWLER,   #{<<"Crawler">>   => ?CRAWLER_INPUT}).
-define(GET_CRAWLERS,  #{<<"Crawlers">>  => [?CRAWLER_INPUT]}).
-define(GET_JOB,       #{<<"Job">>       => ?JOB_INPUT}).
-define(GET_JOBS,      #{<<"Jobs">>      => [?JOB_INPUT]}).
-define(GET_JOB_RUN,   #{<<"JobRun">>    => ?JOB_RUN}).
-define(GET_JOB_RUNS,  #{<<"JobRuns">>   => [?JOB_RUN]}).
-define(START_JOB_RUN, #{<<"JobRunId">>  => ?JOB_RUN_ID}).
-define(UPDATE_JOB,    #{<<"JobName">>   => ?JOB_NAME}).
-define(CREATE_JOB,    #{<<"Name">>      => ?JOB_NAME}).

-define(GET_CRAWLER_METRICS, #{<<"CrawlerMetricsList">> => [?CRAWLER_METRIC]}).

setup() ->
    erlcloud_glue:configure("test-access-key", "test-secret-key"),
    meck:new(?EHTTPC, [passthrough]),
    meck:expect(?EHTTPC, request, 6, fun do_erlcloud_httpc_request/6),
    [?EHTTPC].

erlcloud_glue_test_() ->
    {
        foreach,
        fun setup/0,
        fun meck:unload/1,
        [
            fun test_create_crawler/0,
            fun test_create_database/0,
            fun test_create_job/0,
            fun test_create_table/0,
            fun test_delete_crawler/0,
            fun test_delete_job/0,
            fun test_delete_table/0,
            fun test_get_crawler/0,
            fun test_get_crawler_metrics/0,
            fun test_get_crawlers/0,
            fun test_get_job/0,
            fun test_get_jobs/0,
            fun test_get_jobs_pagination/0,
            fun test_get_job_run/0,
            fun test_get_job_run_default_predecessor/0,
            fun test_get_job_runs/0,
            fun test_get_table/0,
            fun test_get_tables_no_options/0,
            fun test_get_tables_with_options/0,
            fun test_reset_job_bookmark/0,
            fun test_start_crawler/0,
            fun test_start_crawler_schedule/0,
            fun test_start_job_run/0,
            fun test_start_job_run_with_run_id/0,
            fun test_start_job_run_with_alloc_capacity/0,
            fun test_start_job_run_with_arguments/0,
            fun test_stop_crawler/0,
            fun test_stop_crawler_schedule/0,
            fun test_update_crawler/0,
            fun test_update_crawler_schedule/0,
            fun test_update_job/0,
            fun test_update_table/0,
            fun test_error_no_retry/0,
            fun test_error_retry/0
        ]
    }.

test_create_crawler() ->
    TestFun = fun() -> erlcloud_glue:create_crawler(?CRAWLER_INPUT) end,
    do_test(?CRAWLER_INPUT, ok, TestFun).

test_create_database() ->
    TestFun = fun() -> erlcloud_glue:create_database(?DB_INPUT) end,
    do_test(#{<<"DatabaseInput">> => ?DB_INPUT}, ok, TestFun).

test_create_job() ->
    TestFun  = fun() -> erlcloud_glue:create_job(?JOB_INPUT) end,
    Expected = {ok, ?CREATE_JOB},
    do_test(?JOB_INPUT, Expected, TestFun).

test_create_table() ->
    Request = #{<<"CatalogId">>    => ?CATALOG_ID,
                <<"DatabaseName">> => ?DB_NAME,
                <<"TableInput">>   => ?TBL_INPUT},
    TestFun = fun() ->
                  erlcloud_glue:create_table(?DB_NAME, ?TBL_INPUT, ?CATALOG_ID)
              end,
    do_test(Request, ok, TestFun).

test_delete_crawler() ->
    Request = #{<<"Name">> => ?CRAWLER_NAME},
    TestFun = fun() -> erlcloud_glue:delete_crawler(?CRAWLER_NAME) end,
    do_test(Request, ok, TestFun).

test_delete_job() ->
    Request = #{<<"JobName">> => ?JOB_NAME},
    TestFun = fun() -> erlcloud_glue:delete_job(?JOB_NAME) end,
    do_test(Request, ok, TestFun).

test_delete_table() ->
    Request = #{<<"CatalogId">>    => ?CATALOG_ID,
                <<"DatabaseName">> => ?DB_NAME,
                <<"Name">>         => ?TBL_NAME},
    TestFun = fun() ->
                  erlcloud_glue:delete_table(?DB_NAME, ?TBL_NAME, ?CATALOG_ID)
              end,
    do_test(Request, ok, TestFun).

test_get_crawler() ->
    Request  = #{<<"Name">> => ?CRAWLER_NAME},
    Expected = {ok, ?GET_CRAWLER},
    TestFun  = fun() -> erlcloud_glue:get_crawler(?CRAWLER_NAME) end,
    do_test(Request, Expected, TestFun).

test_get_crawler_metrics() ->
    Request  = #{<<"CrawlerNameList">> => [?CRAWLER_NAME]},
    Expected = {ok, ?GET_CRAWLER_METRICS},
    TestFun  = fun() -> erlcloud_glue:get_crawler_metrics([?CRAWLER_NAME]) end,
    do_test(Request, Expected, TestFun).

test_get_crawlers() ->
    Request  = #{},
    Expected = {ok, ?GET_CRAWLERS},
    TestFun  = fun() -> erlcloud_glue:get_crawlers() end,
    do_test(Request, Expected, TestFun).

test_get_job() ->
    Request  = #{<<"JobName">> => ?JOB_NAME},
    Expected = {ok, ?GET_JOB},
    TestFun  = fun() -> erlcloud_glue:get_job(?JOB_NAME) end,
    do_test(Request, Expected, TestFun).

test_get_jobs() ->
    Request  = #{},
    Expected = {ok, ?GET_JOBS},
    TestFun  = fun() -> erlcloud_glue:get_jobs() end,
    do_test(Request, Expected, TestFun).

test_get_jobs_pagination() ->
    PaginationMap = #{<<"MaxResults">> => 1,
                      <<"NextToken">>  => <<"some-token">>},
    Expected = {ok, ?GET_JOBS},
    TestFun  = fun() -> erlcloud_glue:get_jobs(PaginationMap) end,
    do_test(PaginationMap, Expected, TestFun).

test_get_job_run() ->
    Request  = #{<<"JobName">>              => ?JOB_NAME,
                 <<"RunId">>                => ?JOB_RUN_ID,
                 <<"PredecessorsIncluded">> => true},
    Expected = {ok, ?GET_JOB_RUN},
    TestFun  = fun() ->
                   erlcloud_glue:get_job_run(?JOB_NAME, ?JOB_RUN_ID, true)
               end,
    do_test(Request, Expected, TestFun).

test_get_job_run_default_predecessor() ->
    Request  = #{<<"JobName">>              => ?JOB_NAME,
                 <<"RunId">>                => ?JOB_RUN_ID,
                 <<"PredecessorsIncluded">> => false},
    Expected = {ok, ?GET_JOB_RUN},
    TestFun  = fun() -> erlcloud_glue:get_job_run(?JOB_NAME, ?JOB_RUN_ID) end,
    do_test(Request, Expected, TestFun).

test_get_job_runs() ->
    Request  = #{<<"JobName">> => ?JOB_NAME},
    Expected = {ok, ?GET_JOB_RUNS},
    TestFun  = fun() -> erlcloud_glue:get_job_runs(?JOB_NAME) end,
    do_test(Request, Expected, TestFun).

test_get_table() ->
    Request = #{<<"CatalogId">>    => ?CATALOG_ID,
                <<"DatabaseName">> => ?DB_NAME,
                <<"Name">>         => ?TBL_NAME},
    Expected = {ok, ?GET_TABLE},
    TestFun  = fun() ->
                   erlcloud_glue:get_table(?DB_NAME, ?TBL_NAME, ?CATALOG_ID)
               end,
    do_test(Request, Expected, TestFun).

test_get_tables_no_options() ->
    Request = #{<<"CatalogId">>    => ?CATALOG_ID,
                <<"DatabaseName">> => ?DB_NAME},
    Expected = {ok, ?GET_TABLES},
    TestFun  = fun() ->
                   erlcloud_glue:get_tables(?DB_NAME, ?CATALOG_ID, #{})
               end,
    do_test(Request, Expected, TestFun).

test_get_tables_with_options() ->
    Options  = #{<<"Expression">> => <<"some-regex-pattern">>,
                 <<"MaxResults">> => 1,
                 <<"NextToken">>  => <<"some-token">>},
    Request  = Options#{<<"CatalogId">>    => ?CATALOG_ID,
                        <<"DatabaseName">> => ?DB_NAME},
    Expected = {ok, ?GET_TABLES},
    TestFun  = fun() ->
                   erlcloud_glue:get_tables(?DB_NAME, ?CATALOG_ID, Options)
               end,
    do_test(Request, Expected, TestFun).

test_reset_job_bookmark() ->
    Request = #{<<"JobName">> => ?JOB_NAME},
    TestFun = fun() -> erlcloud_glue:reset_job_bookmark(?JOB_NAME) end,
    do_test(Request, ok, TestFun).

test_start_crawler() ->
    Request = #{<<"Name">> => ?CRAWLER_NAME},
    TestFun = fun() -> erlcloud_glue:start_crawler(?CRAWLER_NAME) end,
    do_test(Request, ok, TestFun).

test_start_crawler_schedule() ->
    Request = #{<<"CrawlerName">> => ?CRAWLER_NAME},
    TestFun = fun() -> erlcloud_glue:start_crawler_schedule(?CRAWLER_NAME) end,
    do_test(Request, ok, TestFun).

test_start_job_run() ->
    Request  = #{<<"JobName">>   => ?JOB_NAME,
                 <<"Arguments">> => #{}},
    Expected = {ok, ?START_JOB_RUN},
    TestFun  = fun() -> erlcloud_glue:start_job_run(?JOB_NAME) end,
    do_test(Request, Expected, TestFun).

test_start_job_run_with_run_id() ->
    Request  = #{<<"JobName">>   => ?JOB_NAME,
                 <<"JobRunId">>  => ?JOB_RUN_ID,
                 <<"Arguments">> => #{}},
    Expected = {ok, ?START_JOB_RUN},
    TestFun  = fun() -> erlcloud_glue:start_job_run(?JOB_NAME, ?JOB_RUN_ID) end,
    do_test(Request, Expected, TestFun).

test_start_job_run_with_alloc_capacity() ->
    Request  = #{<<"JobName">>           => ?JOB_NAME,
                 <<"AllocatedCapacity">> => 5,
                 <<"Arguments">>         => #{}},
    Expected = {ok, ?START_JOB_RUN},
    TestFun  = fun() -> erlcloud_glue:start_job_run(?JOB_NAME, 5) end,
    do_test(Request, Expected, TestFun).

test_start_job_run_with_arguments() ->
    Args     = #{<<"Arg-1">> => <<"Value-1">>},
    Request  = #{<<"JobName">>   => ?JOB_NAME,
                 <<"Arguments">> => Args},
    Expected = {ok, ?START_JOB_RUN},
    TestFun  = fun() -> erlcloud_glue:start_job_run(?JOB_NAME, Args) end,
    do_test(Request, Expected, TestFun).

test_stop_crawler() ->
    Request = #{<<"Name">> => ?CRAWLER_NAME},
    TestFun = fun() -> erlcloud_glue:stop_crawler(?CRAWLER_NAME) end,
    do_test(Request, ok, TestFun).

test_stop_crawler_schedule() ->
    Request = #{<<"CrawlerName">> => ?CRAWLER_NAME},
    TestFun = fun() -> erlcloud_glue:stop_crawler_schedule(?CRAWLER_NAME) end,
    do_test(Request, ok, TestFun).

test_update_crawler() ->
    TestFun = fun() -> erlcloud_glue:update_crawler(?CRAWLER_INPUT) end,
    do_test(?CRAWLER_INPUT, ok, TestFun).

test_update_crawler_schedule() ->
    Schedule = <<"cron(15 12 * * ? *)">>,
    Request  = #{<<"CrawlerName">> => ?CRAWLER_NAME,
                 <<"Schedule">>    => Schedule},
    TestFun = fun() ->
                  erlcloud_glue:update_crawler_schedule(?CRAWLER_NAME, Schedule)
              end,
    do_test(Request, ok, TestFun).

test_update_job() ->
    Request  = #{<<"JobName">>   => ?JOB_NAME,
                 <<"JobUpdate">> => ?JOB_INPUT},
    Expected = {ok, ?UPDATE_JOB},
    TestFun  = fun() -> erlcloud_glue:update_job(?JOB_NAME, ?JOB_INPUT) end,
    do_test(Request, Expected, TestFun).

test_update_table() ->
    Request = #{<<"DatabaseName">> => ?DB_NAME,
                <<"TableInput">>   => ?TBL_INPUT},
    TestFun = fun() ->
                  erlcloud_glue:update_table(?DB_NAME, ?TBL_INPUT)
              end,
    do_test(Request, ok, TestFun).

test_error_no_retry() ->
    erlcloud_glue:configure("test-access-key", "test-secret-key"),
    ErrCode = 400,
    Status  = "Bad Request",
    ErrMsg  = <<"Message">>,
    meck:expect(?EHTTPC, request, 6, {ok, {{ErrCode, Status}, [], ErrMsg}}),
    ?assertEqual(
        {error, {http_error, ErrCode, Status, ErrMsg, []}},
        erlcloud_glue:get_tables(?DB_NAME)
    ).

test_error_retry() ->
    erlcloud_glue:configure("test-access-key", "test-secret-key"),
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
        erlcloud_glue:get_tables(?DB_NAME)
    ).

do_test(Request, ExpectedResult, TestedFun) ->
    erlcloud_glue:configure("test-access-key", "test-secret-key"),
    ?assertEqual(ExpectedResult, TestedFun()),
    Encoded = jsx:encode(Request),
    ?assertMatch([{_, {?EHTTPC, request, [_, post, _, Encoded, _, _]}, _}],
                 meck:history(?EHTTPC)).

do_erlcloud_httpc_request(_, post, Headers, _, _, _) ->
    Target = proplists:get_value("x-amz-target", Headers),
    ["AWSGlue", Operation] = string:tokens(Target, "."),
    RespBody =
        case Operation of
            "CreateCrawler"         -> #{};
            "CreateDatabase"        -> #{};
            "CreateJob"             -> ?CREATE_JOB;
            "CreateTable"           -> #{};
            "DeleteCrawler"         -> #{};
            "DeleteJob"             -> #{};
            "DeleteTable"           -> #{};
            "GetCrawler"            -> ?GET_CRAWLER;
            "GetCrawlerMetrics"     -> ?GET_CRAWLER_METRICS;
            "GetCrawlers"           -> ?GET_CRAWLERS;
            "GetJob"                -> ?GET_JOB;
            "GetJobs"               -> ?GET_JOBS;
            "GetJobRun"             -> ?GET_JOB_RUN;
            "GetJobRuns"            -> ?GET_JOB_RUNS;
            "GetTable"              -> ?GET_TABLE;
            "GetTables"             -> ?GET_TABLES;
            "ResetJobBookmark"      -> #{};
            "StartCrawler"          -> #{};
            "StartCrawlerSchedule"  -> #{};
            "StartJobRun"           -> ?START_JOB_RUN;
            "StopCrawler"           -> #{};
            "StopCrawlerSchedule"   -> #{};
            "UpdateCrawler"         -> #{};
            "UpdateCrawlerSchedule" -> #{};
            "UpdateJob"             -> ?UPDATE_JOB;
            "UpdateTable"           -> #{}
        end,
    {ok, {{200, "OK"}, [], jsx:encode(RespBody)}}.
