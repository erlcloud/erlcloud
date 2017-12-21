-module(erlcloud_glue).

-include("erlcloud_aws.hrl").

%% API
-export([configure/2, configure/3, configure/4, configure/5,
         new/2, new/3, new/4, new/5]).

-export([
    create_crawler/1,
    create_crawler/2,

    create_database/1,
    create_database/2,
    create_database/3,

    create_job/1,
    create_job/2,

    create_table/2,
    create_table/3,
    create_table/4,

    delete_crawler/1,
    delete_crawler/2,

    delete_job/1,
    delete_job/2,

    delete_table/2,
    delete_table/3,
    delete_table/4,

    get_crawler/1,
    get_crawler/2,

    get_crawler_metrics/0,
    get_crawler_metrics/1,
    get_crawler_metrics/2,
    get_crawler_metrics/3,

    get_crawlers/0,
    get_crawlers/1,
    get_crawlers/2,

    get_job/1,
    get_job/2,

    get_jobs/0,
    get_jobs/1,
    get_jobs/2,

    get_job_run/2,
    get_job_run/3,
    get_job_run/4,

    get_job_runs/1,
    get_job_runs/2,
    get_job_runs/3,

    get_table/2,
    get_table/3,
    get_table/4,

    get_tables/1,
    get_tables/2,
    get_tables/3,
    get_tables/4,

    reset_job_bookmark/1,
    reset_job_bookmark/2,

    start_crawler/1,
    start_crawler/2,

    start_crawler_schedule/1,
    start_crawler_schedule/2,

    start_job_run/1,
    start_job_run/2,
    start_job_run/5,

    stop_crawler/1,
    stop_crawler/2,

    stop_crawler_schedule/1,
    stop_crawler_schedule/2,

    update_crawler/1,
    update_crawler/2,

    update_crawler_schedule/1,
    update_crawler_schedule/2,
    update_crawler_schedule/3,

    update_job/2,
    update_job/3,

    update_table/2,
    update_table/3,
    update_table/4
]).

-spec new(string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id     = AccessKeyID,
                secret_access_key = SecretAccessKey,
                retry             = fun erlcloud_retry:default_retry/1}.

-spec new(string(), string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id     = AccessKeyID,
                secret_access_key = SecretAccessKey,
                glue_host         = Host,
                retry             = fun erlcloud_retry:default_retry/1}.

-spec new(string(), string(), string(), non_neg_integer()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host, Port) ->
    #aws_config{access_key_id     = AccessKeyID,
                secret_access_key = SecretAccessKey,
                glue_host         = Host,
                glue_port         = Port,
                retry             = fun erlcloud_retry:default_retry/1}.

-spec new(string(), string(), string(), non_neg_integer(), string()) ->
    aws_config().
new(AccessKeyID, SecretAccessKey, Host, Port, Scheme) ->
    #aws_config{access_key_id     = AccessKeyID,
                secret_access_key = SecretAccessKey,
                glue_host         = Host,
                glue_port         = Port,
                glue_scheme       = Scheme,
                retry             = fun erlcloud_retry:default_retry/1}.

-spec configure(string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey)),
    ok.

-spec configure(string(), string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host)),
    ok.

-spec configure(string(), string(), string(), non_neg_integer()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host, Port) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host, Port)),
    ok.

-spec configure(string(), string(), string(), non_neg_integer(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host, Port, Scheme) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host, Port, Scheme)),
    ok.

%% @doc
%% Glue API:
%% https://docs.aws.amazon.com/glue/latest/webapi/API_CreateCrawler.html
%%
%% `
%% erlcloud_glue:create_crawler(#{<<"Classifiers">>   => [],
%%                                <<"Configuration">> => <<"some-config">>},
%%                                <<"DatabaseName">>  => <<"db-name">>,
%%                                <<"Name">>          => <<"crawler-name">>,
%%                                ...
%%                                <<"Targets">>       => {}}).
%% '
%%
-spec create_crawler(map()) -> ok | {error, any()}.
create_crawler(CrawlerInput) ->
    create_crawler(CrawlerInput, default_config()).

-spec create_crawler(map(), aws_config()) -> ok | {error, any()}.
create_crawler(CrawlerInput, Config) ->
    case request(Config, "CreateCrawler", CrawlerInput) of
        {ok, _} -> ok;
        Error   -> Error
    end.

%% @doc
%% Glue API:
%% https://docs.aws.amazon.com/glue/latest/webapi/API_CreateDatabase.html
%%
%% `
%% erlcloud_glue:create_database(#{<<"Description">> => <<"some-desc">>,
%%                                 <<"Name">>        => <<"db-name">>},
%%                               <<"catalog-id">>).
%% '
%%
-spec create_database(map()) -> ok | {error, any()}.
create_database(DatabaseInput) ->
    create_database(DatabaseInput, undefined, default_config()).

-spec create_database(map(), binary() | aws_config()) ->
    ok | {error, any()}.
create_database(DatabaseInput, CatalogId) when is_binary(CatalogId) ->
    create_database(DatabaseInput, CatalogId, default_config());
create_database(DatabaseInput, Config) when is_record(Config, aws_config) ->
    create_database(DatabaseInput, undefined, Config).

-spec create_database(map(), binary() | undefined, aws_config()) ->
    ok | {error, any()}.
create_database(DatabaseInput, CatalogId, Config) ->
    Request0 = #{<<"DatabaseInput">> => DatabaseInput},
    Request1 = update_catalog_id(CatalogId, Request0),
    case request(Config, "CreateDatabase", Request1) of
        {ok, _} -> ok;
        Error   -> Error
    end.

%% @doc
%% Glue API:
%% https://docs.aws.amazon.com/glue/latest/webapi/API_CreateJob.html
%%
%% `
%% erlcloud_glue:create_job(#{<<"Name">>        => <<"job-name">>,
%%                            <<"Description">> => <<"some-desc">>,
%%                            <<"Connections">> =>
%%                                #{<<"Connections">> => [<<"conn1">>]}
%%                            ...
%%                            <<"LogUri">>      => <<"some-uri">>}).
%% '
%%
-spec create_job(map()) -> {ok, map()} | {error, any()}.
create_job(JobInput) ->
    create_job(JobInput, default_config()).

-spec create_job(map(), aws_config()) -> {ok, map()} | {error, any()}.
create_job(JobInput, Config) ->
    request(Config, "CreateJob", JobInput).

%% @doc
%% Glue API:
%% https://docs.aws.amazon.com/glue/latest/webapi/API_CreateTable.html
%%
%% `
%% erlcloud_glue:create_table(<<"db-name">>,
%%                            #{<<"Description">> => <<"some-desc">>,
%%                              <<"Name">>        => <<"tbl-name">>,
%%                              ...
%%                              <<"TableType">>   => <<"some-type">>},
%%                            <<"catalog-id">>).
%% '
%%
-spec create_table(binary(), map()) -> ok | {error, any()}.
create_table(DbName, TableInput) ->
    create_table(DbName, TableInput, undefined, default_config()).

-spec create_table(binary(),
                   map(),
                   binary() | aws_config()) ->
    ok | {error, any()}.
create_table(DbName, TableInput, CatalogId) when is_binary(CatalogId) ->
    create_table(DbName, TableInput, CatalogId, default_config());
create_table(DbName, TableInput, Config) when is_record(Config, aws_config) ->
    create_table(DbName, TableInput, undefined, Config).

-spec create_table(binary(), map(), binary() | undefined, aws_config()) ->
    ok | {error, any()}.
create_table(DbName, TableInput, CatalogId, Config) ->
    Request0 = #{<<"DatabaseName">> => DbName,
                 <<"TableInput">>   => TableInput},
    Request1 = update_catalog_id(CatalogId, Request0),
    case request(Config, "CreateTable", Request1) of
        {ok, _} -> ok;
        Error   -> Error
    end.

%% @doc
%% Glue API:
%% https://docs.aws.amazon.com/glue/latest/webapi/API_DeleteCrawler.html
%%
-spec delete_crawler(binary()) -> ok | {error, any()}.
delete_crawler(CrawlerName) ->
    delete_crawler(CrawlerName, default_config()).

-spec delete_crawler(binary(), aws_config()) -> ok | {error, any()}.
delete_crawler(CrawlerName, Config) ->
    Request = #{<<"Name">> => CrawlerName},
    case request(Config, "DeleteCrawler", Request) of
        {ok, _} -> ok;
        Error   -> Error
    end.

%% @doc
%% Glue API:
%% https://docs.aws.amazon.com/glue/latest/webapi/API_DeleteJob.html
%%
-spec delete_job(binary()) -> ok | {error, any()}.
delete_job(JobName) ->
    delete_job(JobName, default_config()).

-spec delete_job(binary(), aws_config()) -> ok | {error, any()}.
delete_job(JobName, Config) ->
    case request(Config, "DeleteJob", #{<<"JobName">> => JobName}) of
        {ok, _} -> ok;
        Error   -> Error
    end.

%% @doc
%% Glue API:
%% https://docs.aws.amazon.com/glue/latest/webapi/API_DeleteTable.html
%%
-spec delete_table(binary(), binary()) -> ok | {error, any()}.
delete_table(DbName, TableName) ->
    delete_table(DbName, TableName, undefined, default_config()).

-spec delete_table(binary(),
                   binary(),
                   binary() | aws_config()) ->
    ok | {error, any()}.
delete_table(DbName, TableName, CatalogId) when is_binary(CatalogId) ->
    delete_table(DbName, TableName, CatalogId, default_config());
delete_table(DbName, TableName, Config) when is_record(Config, aws_config) ->
    delete_table(DbName, TableName, undefined, Config).

-spec delete_table(binary(), binary(), binary() | undefined, aws_config()) ->
    ok | {error, any()}.
delete_table(DbName, TableName, CatalogId, Config) ->
    Request0 = #{<<"DatabaseName">> => DbName,
                 <<"Name">>         => TableName},
    Request1 = update_catalog_id(CatalogId, Request0),
    case request(Config, "DeleteTable", Request1) of
        {ok, _} -> ok;
        Error   -> Error
    end.

%% @doc
%% Glue API:
%% https://docs.aws.amazon.com/glue/latest/webapi/API_GetCrawler.html
%%
-spec get_crawler(binary()) -> {ok, map()} | {error, any()}.
get_crawler(CrawlerName) ->
    get_crawler(CrawlerName, default_config()).

-spec get_crawler(binary(), aws_config()) -> {ok, map()} | {error, any()}.
get_crawler(CrawlerName, Config) ->
    request(Config, "GetCrawler", #{<<"Name">> => CrawlerName}).

%% @doc
%% Glue API:
%% https://docs.aws.amazon.com/glue/latest/webapi/API_GetCrawlerMetrics.html
%% `
%% erlcloud_glue:get_crawler_metrics(
%%                        [<<"crawler-name-1">>, <<"crawl}er-name-2">>],
%%                        #{<<"MaxResults">> => 1,
%%                          <<"NextToken">>  => <<"some-token">>}
%% ).
%% '
%%
-spec get_crawler_metrics() -> {ok, map()} | {error, any()}.
get_crawler_metrics() ->
    get_crawler_metrics(default_config()).

-spec get_crawler_metrics(list() | map() | aws_config()) ->
    {ok, map()} | {error, any()}.
get_crawler_metrics(NameList) when is_list(NameList) ->
    get_crawler_metrics(NameList, #{}, default_config());
get_crawler_metrics(PaginationMap) when is_map(PaginationMap) ->
    get_crawler_metrics([], PaginationMap, default_config());
get_crawler_metrics(Config) when is_record(Config, aws_config) ->
    get_crawler_metrics([], #{}, Config).

-spec get_crawler_metrics(list() | map(), map() | aws_config()) ->
    {ok, map()} | {error, any()}.
get_crawler_metrics(NameList, PaginationMap)
  when is_list(NameList), is_map(PaginationMap) ->
    get_crawler_metrics(NameList, PaginationMap, default_config());
get_crawler_metrics(NameList, Config)
  when is_list(NameList), is_record(Config, aws_config) ->
    get_crawler_metrics(NameList, #{}, default_config());
get_crawler_metrics(PaginationMap, Config)
  when is_map(PaginationMap), is_record(Config, aws_config) ->
    get_crawler_metrics([], PaginationMap, default_config()).

-spec get_crawler_metrics(list(), map(), aws_config()) ->
    {ok, map()} | {error, any()}.
get_crawler_metrics(NameList, PaginationMap, Config) ->
    Request = PaginationMap#{<<"CrawlerNameList">> => NameList},
    request(Config, "GetCrawlerMetrics", Request).

%% @doc
%% Glue API:
%% https://docs.aws.amazon.com/glue/latest/webapi/API_GetCrawlers.html
%%
%% `
%% erlcloud_glue:get_crawlers(#{<<"MaxResults">> => 1,
%%                              <<"NextToken">>  => <<"some-token">>}).
%% '
%%
-spec get_crawlers() -> {ok, map()} | {error, any()}.
get_crawlers() ->
    get_crawlers(default_config()).

-spec get_crawlers(map() | aws_config()) -> {ok, map()} | {error, any()}.
get_crawlers(PaginationMap) when is_map(PaginationMap) ->
    get_crawlers(PaginationMap, default_config());
get_crawlers(Config) when is_record(Config, aws_config) ->
    get_crawlers(#{}, Config).

-spec get_crawlers(map(), aws_config()) -> {ok, map()} | {error, any()}.
get_crawlers(PaginationMap, Config) ->
    request(Config, "GetCrawlers", PaginationMap).

%% @doc
%% Glue API:
%% https://docs.aws.amazon.com/glue/latest/webapi/API_GetJob.html
%%
-spec get_job(binary()) -> {ok, map()} | {error, any()}.
get_job(JobName) ->
    get_job(JobName, default_config()).

-spec get_job(binary(), aws_config()) -> {ok, map()} | {error, any()}.
get_job(JobName, Config) ->
    request(Config, "GetJob", #{<<"JobName">> => JobName}).

%% @doc
%% Glue API:
%% https://docs.aws.amazon.com/glue/latest/webapi/API_GetJobs.html
%%
%% `
%% erlcloud_glue:get_jobs(#{<<"MaxResults">> => 1,
%%                          <<"NextToken">>  => <<"some-token">>}).
%% '
%%
-spec get_jobs() -> {ok, map()} | {error, any()}.
get_jobs() ->
    get_jobs(default_config()).

-spec get_jobs(map() | aws_config()) -> {ok, map()} | {error, any()}.
get_jobs(PaginationMap) when is_map(PaginationMap) ->
    get_jobs(PaginationMap, default_config());
get_jobs(Config) when is_record(Config, aws_config) ->
    get_jobs(#{}, Config).

-spec get_jobs(map(), aws_config()) -> {ok, map()} | {error, any()}.
get_jobs(PaginationMap, Config) ->
    request(Config, "GetJobs", PaginationMap).

%% @doc
%% Glue API:
%% https://docs.aws.amazon.com/glue/latest/webapi/API_GetJobRun.html
%%
-spec get_job_run(binary(), binary()) -> {ok, map()} | {error, any()}.
get_job_run(JobName, RunId) ->
    get_job_run(JobName, RunId, false, default_config()).

-spec get_job_run(binary(), binary(), boolean() | aws_config()) ->
    {ok, map()} | {error, any()}.
get_job_run(JobName, RunId, PredecessorsIncluded)
  when is_boolean(PredecessorsIncluded) ->
    get_job_run(JobName, RunId, PredecessorsIncluded, default_config());
get_job_run(JobName, RunId, Config) when is_record(Config, aws_config) ->
    get_job_run(JobName, RunId, false, Config).

-spec get_job_run(binary(), binary(), boolean(), aws_config()) ->
    {ok, map()} | {error, any()}.
get_job_run(JobName, RunId, PredecessorsIncluded, Config) ->
    Request = #{<<"JobName">>              => JobName,
                <<"RunId">>                => RunId,
                <<"PredecessorsIncluded">> => PredecessorsIncluded},
    request(Config, "GetJobRun", Request).

%% @doc
%% Glue API:
%% https://docs.aws.amazon.com/glue/latest/webapi/API_GetJobRuns.html
%%
%% `
%% erlcloud_glue:get_job_runs(<<"job-name">>,
%%                            #{<<"MaxResults">> => 1,
%%                              <<"NextToken">>  => <<"some-token">>}).
%% '
%%
-spec get_job_runs(binary()) -> {ok, map()} | {error, any()}.
get_job_runs(JobName) ->
    get_job_runs(JobName, #{}, default_config()).

-spec get_job_runs(binary(), map() | aws_config()) ->
    {ok, map()} | {error, any()}.
get_job_runs(JobName, PaginationMap) when is_map(PaginationMap) ->
    get_job_runs(JobName, PaginationMap, default_config());
get_job_runs(JobName, Config) when is_record(Config, aws_config) ->
    get_job_runs(JobName, #{}, Config).

-spec get_job_runs(binary(), map(), aws_config()) ->
    {ok, map()} | {error, any()}.
get_job_runs(JobName, PaginationMap, Config) ->
    Request = PaginationMap#{<<"JobName">> => JobName},
    request(Config, "GetJobRuns", Request).

%% @doc
%% Glue API:
%% https://docs.aws.amazon.com/glue/latest/webapi/API_GetTable.html
%%
-spec get_table(binary(), binary()) -> {ok, map()} | {error, any()}.
get_table(DbName, TableName) ->
    get_table(DbName, TableName, undefined, default_config()).

-spec get_table(binary(),
                binary(),
                binary() | aws_config()) ->
    {ok, map()} | {error, any()}.
get_table(DbName, TableName, CatalogId) when is_binary(CatalogId) ->
    get_table(DbName, TableName, CatalogId, default_config());
get_table(DbName, TableName, Config) when is_record(Config, aws_config) ->
    get_table(DbName, TableName, undefined, Config).

-spec get_table(binary(), binary(), binary() | undefined, aws_config()) ->
    {ok, map()} | {error, any()}.
get_table(DbName, TableName, CatalogId, Config) ->
    Request0 = #{<<"DatabaseName">> => DbName,
                 <<"Name">>         => TableName},
    Request1 = update_catalog_id(CatalogId, Request0),
    request(Config, "GetTable", Request1).

%% @doc
%% Glue API:
%% https://docs.aws.amazon.com/glue/latest/webapi/API_GetTables.html
%%
%% `
%% erlcloud_glue:get_tables(<<"db-name">>,
%%                          <<"catalog-id">>,
%%                          #{<<"Expression">> => <<"some-regex-pattern">>,
%%                            <<"MaxResults">> => 1,
%%                            <<"NextToken">>  => <<"some-token">>}).
%% '
%%
-spec get_tables(binary()) -> {ok, map()} | {error, any()}.
get_tables(DbName) ->
    get_tables(DbName, undefined, #{}, default_config()).

-spec get_tables(binary(),
                 binary() | map() | aws_config()) ->
    {ok, map()} | {error, any()}.
get_tables(DbName, CatalogId) when is_binary(CatalogId) ->
    get_tables(DbName, CatalogId, #{}, default_config());
get_tables(DbName, Options) when is_map(Options) ->
    get_tables(DbName, undefined, Options, default_config());
get_tables(DbName, Config) when is_record(Config, aws_config) ->
    get_tables(DbName, undefined, #{}, Config).

-spec get_tables(binary(),
                 binary() | map(),
                 map() | aws_config()) ->
    {ok, map()} | {error, any()}.
get_tables(DbName, CatalogId, Options)
  when is_binary(CatalogId), is_map(Options) ->
    get_tables(DbName, CatalogId, Options, default_config());
get_tables(DbName, CatalogId, Config)
  when is_binary(CatalogId), is_record(Config, aws_config) ->
    get_tables(DbName, CatalogId, #{}, Config);
get_tables(DbName, Options, Config)
  when is_map(Options), is_record(Config, aws_config) ->
    get_tables(DbName, undefined, Options, Config).

-spec get_tables(binary(), binary() | undefined, map(), aws_config()) ->
    {ok, map()} | {error, any()}.
get_tables(DbName, CatalogId, Options, Config) ->
    Request0 = Options#{<<"DatabaseName">> => DbName},
    Request1 = update_catalog_id(CatalogId, Request0),
    request(Config, "GetTables", Request1).

%% @doc
%% Glue API:
%% https://docs.aws.amazon.com/glue/latest/webapi/API_ResetJobBookmark.html
%%
-spec reset_job_bookmark(binary()) -> ok | {error, any()}.
reset_job_bookmark(JobName) ->
    reset_job_bookmark(JobName, default_config()).

-spec reset_job_bookmark(binary(), aws_config()) -> ok | {error, any()}.
reset_job_bookmark(JobName, Config) ->
    case request(Config, "ResetJobBookmark", #{<<"JobName">> => JobName}) of
        {ok, _} -> ok;
        Error   -> Error
    end.

%% @doc
%% Glue API:
%% https://docs.aws.amazon.com/glue/latest/webapi/API_StartCrawler.html
%%
-spec start_crawler(binary()) -> ok | {error, any()}.
start_crawler(CrawlerName) ->
    start_crawler(CrawlerName, default_config()).

-spec start_crawler(binary(), aws_config()) -> ok | {error, any()}.
start_crawler(CrawlerName, Config) ->
    case request(Config, "StartCrawler", #{<<"Name">> => CrawlerName}) of
        {ok, _} -> ok;
        Error   -> Error
    end.

%% @doc
%% Glue API:
%% https://docs.aws.amazon.com/glue/latest/webapi/API_StartCrawlerSchedule.html
%%
-spec start_crawler_schedule(binary()) -> ok | {error, any()}.
start_crawler_schedule(CrawlerName) ->
    start_crawler_schedule(CrawlerName, default_config()).

-spec start_crawler_schedule(binary(), aws_config()) -> ok | {error, any()}.
start_crawler_schedule(CrawlerName, Config) ->
    Request = #{<<"CrawlerName">> => CrawlerName},
    case request(Config, "StartCrawlerSchedule", Request) of
        {ok, _} -> ok;
        Error   -> Error
    end.

%% @doc
%% Glue API:
%% https://docs.aws.amazon.com/glue/latest/webapi/API_StartJobRun.html
%%
%% `
%% erlcloud_glue:start_job_run(<<"job-name">>,
%%                             <<"job-run-id">>,
%%                             5,
%%                             #{<<"Key-1">> => <<"Value-1">>,
%%                               <<"Key-2">> => <<"Value-2">>},
%%                             AwsConfig).
%% '
%%
-spec start_job_run(binary()) -> {ok, map()} | {error, any()}.
start_job_run(JobName) ->
    start_job_run(JobName, undefined, undefined, #{}, default_config()).

-spec start_job_run(binary(), binary() | integer() | map()) ->
    {ok, map()} | {error, any()}.
start_job_run(JobName, JobRunId) when is_binary(JobRunId) ->
    start_job_run(JobName, JobRunId, undefined, #{}, default_config());
start_job_run(JobName, AllocatedCapacity) when is_integer(AllocatedCapacity) ->
    start_job_run(JobName, undefined, AllocatedCapacity, #{}, default_config());
start_job_run(JobName, Arguments) when is_map(Arguments) ->
    start_job_run(JobName, undefined, undefined, Arguments, default_config());
start_job_run(JobName, Config) when is_record(Config, aws_config) ->
    start_job_run(JobName, undefined, undefined, #{}, Config).

-spec start_job_run(binary(),
                    binary() | undefined,
                    integer() | undefined,
                    map(),
                    aws_config()) ->
    {ok, map()} | {error, any()}.
start_job_run(JobName, JobRunId, AllocatedCapacity, Arguments, Config) ->
    Request0 = #{<<"JobName">>   => JobName,
                 <<"Arguments">> => Arguments},
    Request1 = update_map([{<<"JobRunId">>,          JobRunId},
                           {<<"AllocatedCapacity">>, AllocatedCapacity}],
                          Request0),
    request(Config, "StartJobRun", Request1).

%% @doc
%% Glue API:
%% https://docs.aws.amazon.com/glue/latest/webapi/API_StopCrawler.html
%%
-spec stop_crawler(binary()) -> ok | {error, any()}.
stop_crawler(CrawlerName) ->
    stop_crawler(CrawlerName, default_config()).

-spec stop_crawler(binary(), aws_config()) -> ok | {error, any()}.
stop_crawler(CrawlerName, Config) ->
    case request(Config, "StopCrawler", #{<<"Name">> => CrawlerName}) of
        {ok, _} -> ok;
        Error   -> Error
    end.

%% @doc
%% Glue API:
%% https://docs.aws.amazon.com/glue/latest/webapi/API_StopCrawlerSchedule.html
%%
-spec stop_crawler_schedule(binary()) -> ok | {error, any()}.
stop_crawler_schedule(CrawlerName) ->
    stop_crawler_schedule(CrawlerName, default_config()).

-spec stop_crawler_schedule(binary(), aws_config()) -> ok | {error, any()}.
stop_crawler_schedule(CrawlerName, Config) ->
    Request = #{<<"CrawlerName">> => CrawlerName},
    case request(Config, "StopCrawlerSchedule", Request) of
        {ok, _} -> ok;
        Error   -> Error
    end.

%% @doc
%% Glue API:
%% https://docs.aws.amazon.com/glue/latest/webapi/API_UpdateCrawler.html
%%
%% `
%% erlcloud_glue:update_crawler(#{<<"Classifiers">>   => [],
%%                                <<"Configuration">> => <<"some-config">>},
%%                                <<"DatabaseName">>  => <<"db-name">>,
%%                                <<"Name">>          => <<"crawler-name">>,
%%                                ...
%%                                <<"Targets">>       => {}}).
%% '
%%
-spec update_crawler(map()) -> ok | {error, any()}.
update_crawler(CrawlerInput) ->
    update_crawler(CrawlerInput, default_config()).

-spec update_crawler(map(), aws_config()) -> ok | {error, any()}.
update_crawler(CrawlerInput, Config) ->
    case request(Config, "UpdateCrawler", CrawlerInput) of
        {ok, _} -> ok;
        Error   -> Error
    end.

%% @doc
%% Glue API:
%% https://docs.aws.amazon.com/glue/latest/webapi/API_UpdateCrawlerSchedule.html
%%
%% `
%% erlcloud_glue:update_crawler_schedule(<<"crawler-name-1">>,
%%                                       <<"cron(15 12 * * ? *)">>).
%% '
%%
-spec update_crawler_schedule(binary()) -> ok | {error, any()}.
update_crawler_schedule(CrawlerName) ->
    update_crawler_schedule(CrawlerName, undefined, default_config()).

-spec update_crawler_schedule(binary(), binary() | undefined) ->
    ok | {error, any()}.
update_crawler_schedule(CrawlerName, Schedule)
  when is_binary(CrawlerName), is_binary(Schedule) ->
    update_crawler_schedule(CrawlerName, Schedule, default_config()).

-spec update_crawler_schedule(binary(), binary() | undefined, aws_config()) ->
    ok | {error, any()}.
update_crawler_schedule(CrawlerName, Schedule, Config) ->
    Request0 = #{<<"CrawlerName">> => CrawlerName},
    Request1 = update_map({<<"Schedule">>, Schedule}, Request0),
    case request(Config, "UpdateCrawlerSchedule", Request1) of
        {ok, _} -> ok;
        Error   -> Error
    end.

%% @doc
%% Glue API:
%% https://docs.aws.amazon.com/glue/latest/webapi/API_UpdateJob.html
%%
%% `
%% erlcloud_glue:update_job(<<"job-name">>,
%%                          #{<<"AllocatedCapacity">> => 5,
%%                            <<"Command">> =>
%%                                #{<<"Name">>           => <<"some-name">>,
%%                                  <<"ScriptLocation">> => <<"location">>},
%%                            ...
%%                            <<"Role">> => <<"some-role">>}).
%% '
%%
-spec update_job(binary(), map()) -> {ok, map()} | {error, any()}.
update_job(JobName, JobInput) ->
    update_job(JobName, JobInput, default_config()).

-spec update_job(binary(), map(), aws_config()) -> {ok, map()} | {error, any()}.
update_job(JobName, JobInput, Config) ->
    Request = #{<<"JobName">>   => JobName,
                <<"JobUpdate">> => JobInput},
    request(Config, "UpdateJob", Request).

%% @doc
%% Glue API:
%% https://docs.aws.amazon.com/glue/latest/webapi/API_UpdateTable.html
%%
%% erlcloud_glue:update_table(<<"db-name">>,
%%                            #{<<"Description">> => <<"some-desc">>,
%%                              <<"Name">>        => <<"tbl-name">>,
%%                              ...
%%                              <<"TableType">>   => <<"some-type">>},
%%                            <<"catalog-id">>).
%% '
%%
-spec update_table(binary(), map()) -> ok | {error, any()}.
update_table(DbName, TableInput) ->
    update_table(DbName, TableInput, undefined, default_config()).

-spec update_table(binary(),
                   map(),
                   binary() | aws_config()) ->
    ok | {error, any()}.
update_table(DbName, TableInput, CatalogId) when is_binary(CatalogId) ->
    update_table(DbName, TableInput, CatalogId, default_config());
update_table(DbName, TableInput, Config) when is_record(Config, aws_config) ->
    update_table(DbName, TableInput, undefined, Config).

-spec update_table(binary(), map(), binary() | undefined, aws_config()) ->
    ok | {error, any()}.
update_table(DbName, TableInput, CatalogId, Config) ->
    Request0 = #{<<"DatabaseName">> => DbName,
                 <<"TableInput">>   => TableInput},
    Request1 = update_catalog_id(CatalogId, Request0),
    case request(Config, "UpdateTable", Request1) of
        {ok, _} -> ok;
        Error   -> Error
    end.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
update_map(KeyValues, Map) when is_list(KeyValues) ->
    lists:foldl(fun update_map/2, Map, KeyValues);
update_map({_, undefined}, Map) ->
    Map;
update_map({Key, Value}, Map) ->
    maps:put(Key, Value, Map).

update_catalog_id(CatalogId, Request) ->
    update_map({<<"CatalogId">>, CatalogId}, Request).

default_config() -> erlcloud_aws:default_config().

request(Config0, OperationName, Request) ->
    case erlcloud_aws:update_config(Config0) of
        {ok, Config} ->
            Body       = jsx:encode(Request),
            Operation  = "AWSGlue." ++ OperationName,
            Headers    = get_headers(Config, Operation, Body),
            AwsRequest = #aws_request{service         = glue,
                                      uri             = get_url(Config),
                                      method          = post,
                                      request_headers = Headers,
                                      request_body    = Body},
            request(Config, AwsRequest);
        {error, Reason} ->
            {error, Reason}
    end.

request(Config, Request) ->
    Result = erlcloud_retry:request(Config, Request, fun handle_result/1),
    case erlcloud_aws:request_to_return(Result) of
        {ok, {_, <<>>}}     -> {ok, #{}};
        {ok, {_, RespBody}} -> {ok, jsx:decode(RespBody, [return_maps])};
        {error, _} = Error  -> Error
    end.

handle_result(#aws_request{response_type = ok} = Request) ->
    Request;
handle_result(#aws_request{response_type    = error,
                           error_type      = aws,
                           response_status = Status} = Request)
  when Status >= 500 ->
    Request#aws_request{should_retry = true};
handle_result(#aws_request{response_type = error,
                           error_type    = aws} = Request) ->
    Request#aws_request{should_retry = false}.

get_headers(#aws_config{glue_host = Host} = Config, Operation, Body) ->
    Headers = [{"host",         Host},
               {"x-amz-target", Operation},
               {"content-type", "application/x-amz-json-1.1"}],
    Region = erlcloud_aws:aws_region_from_host(Host),
    erlcloud_aws:sign_v4_headers(Config, Headers, Body, Region, "glue").

get_url(#aws_config{glue_scheme = Scheme,
                    glue_host   = Host,
                    glue_port   = Port}) ->
    Scheme ++ Host ++ ":" ++ integer_to_list(Port).
