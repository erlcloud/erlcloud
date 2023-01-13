-module(erlcloud_athena).

-include("erlcloud_aws.hrl").

%% API
-export([configure/2, configure/3, configure/4, configure/5,
         new/2, new/3, new/4, new/5]).

-export([
    batch_get_named_query/1,
    batch_get_named_query/2,

    batch_get_prepared_statement/2,
    batch_get_prepared_statement/3,

    batch_get_query_execution/1,
    batch_get_query_execution/2,

    create_named_query/4,
    create_named_query/5,
    create_named_query/6,
    create_named_query/7,

    create_prepared_statement/3,
    create_prepared_statement/4,
    create_prepared_statement/5,

    delete_named_query/1,
    delete_named_query/2,

    delete_prepared_statement/2,
    delete_prepared_statement/3,

    get_named_query/1,
    get_named_query/2,

    get_prepared_statement/2,
    get_prepared_statement/3,

    get_query_execution/1,
    get_query_execution/2,

    get_query_results/1,
    get_query_results/2,
    get_query_results/3,

    list_named_queries/0,
    list_named_queries/1,
    list_named_queries/2,
    list_named_queries/3,

    list_prepared_statements/1,
    list_prepared_statements/2,
    list_prepared_statements/3,

    list_query_executions/0,
    list_query_executions/1,
    list_query_executions/2,
    list_query_executions/3,

    start_query_execution/4,
    start_query_execution/5,
    start_query_execution/6,
    start_query_execution/7,
    start_query_execution/8,

    stop_query_execution/1,
    stop_query_execution/2,

    update_prepared_statement/3,
    update_prepared_statement/4,
    update_prepared_statement/5
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
                athena_host       = Host,
                retry             = fun erlcloud_retry:default_retry/1}.

-spec new(string(), string(), string(), non_neg_integer()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host, Port) ->
    #aws_config{access_key_id     = AccessKeyID,
                secret_access_key = SecretAccessKey,
                athena_host       = Host,
                athena_port       = Port,
                retry             = fun erlcloud_retry:default_retry/1}.

-spec new(string(), string(), string(), non_neg_integer(), string()) ->
    aws_config().
new(AccessKeyID, SecretAccessKey, Host, Port, Scheme) ->
    #aws_config{access_key_id     = AccessKeyID,
                secret_access_key = SecretAccessKey,
                athena_host       = Host,
                athena_port       = Port,
                athena_scheme     = Scheme,
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
%% Athena API:
%% http://docs.aws.amazon.com/athena/latest/APIReference/API_BatchGetNamedQuery.html
%%
-spec batch_get_named_query([binary()]) -> {ok, map()} | {error, any()}.
batch_get_named_query(NamedQueryIds) ->
    batch_get_named_query(NamedQueryIds, default_config()).

-spec batch_get_named_query([binary()], aws_config()) ->
    {ok, map()} | {error, any()}.
batch_get_named_query(NamedQueryIds, Config) ->
    Request = #{<<"NamedQueryIds">> => NamedQueryIds},
    request(Config, "BatchGetNamedQuery", Request).

%% @doc
%% Athena API:
%% https://docs.aws.amazon.com/athena/latest/APIReference/API_BatchGetPreparedStatement.html
%%
-spec batch_get_prepared_statement(binary(), list(binary())) -> {ok, map()} | {error, any()}.
batch_get_prepared_statement(WorkGroup, PreparedStatementNames) ->
    batch_get_prepared_statement(WorkGroup, PreparedStatementNames, default_config()).

-spec batch_get_prepared_statement(binary(), list(binary()), aws_config()) -> {ok, map()} | {error, any()}.
batch_get_prepared_statement(WorkGroup, PreparedStatementNames, Config) ->
    Request = #{<<"WorkGroup">> => WorkGroup, <<"PreparedStatementNames">> => PreparedStatementNames},
    request(Config, "BatchGetPreparedStatement", Request).

%% @doc
%% Athena API:
%% http://docs.aws.amazon.com/athena/latest/APIReference/API_BatchGetQueryExecution.html
%%
-spec batch_get_query_execution([binary()]) -> {ok, map()} | {error, any()}.
batch_get_query_execution(QueryExecutionIds) ->
    batch_get_query_execution(QueryExecutionIds, default_config()).

-spec batch_get_query_execution([binary()], aws_config()) ->
    {ok, map()} | {error, any()}.
batch_get_query_execution(QueryExecutionIds, Config) ->
    Request = #{<<"QueryExecutionIds">> => QueryExecutionIds},
    request(Config, "BatchGetQueryExecution", Request).

%% @doc
%% Athena API:
%% http://docs.aws.amazon.com/athena/latest/APIReference/API_CreateNamedQuery.html
%%
%% `
%% erlcloud_athena:create_named_query(
%%                            <<"some-client-request-token">>,
%%                            <<"db-name">>,
%%                            <<"query-name">>,
%%                            <<"select * from some-tbl">>,
%%                            <<"optional-query-description">>,
%%                            <<"optional-some-workgroup">>
%% ).
%% '
%%
-type create_named_query_opts() :: [create_named_query_opt()].
-type create_named_query_opt()  :: {workgroup, binary()}.

-spec create_named_query(binary(), binary(), binary(), binary()) ->
    {ok, binary()} | {error, any()}.
create_named_query(ClientReqToken, Db, Name, Query) ->
    Config = default_config(),
    create_named_query(ClientReqToken, Db, Name, Query, undefined, [], Config).

-spec create_named_query(binary(), binary(), binary(), binary(),
                         aws_config() | binary()) ->
    {ok, binary()} | {error, any()}.
create_named_query(ClientReqToken, Db, Name, Query, Config)
  when is_record(Config, aws_config) ->
    create_named_query(ClientReqToken, Db, Name, Query, undefined, [], Config);
create_named_query(ClientReqToken, Db, Name, Query, Description)
  when is_binary(Description) ->
    Config = default_config(),
    create_named_query(ClientReqToken, Db, Name, Query, Description, [], Config).

-spec create_named_query(binary(), binary(), binary(), binary(),
                         binary(), aws_config() | create_named_query_opts()) ->
    {ok, binary()} | {error, any()}.
create_named_query(ClientReqToken, Db, Name, Query, Description, Config)
  when is_record(Config, aws_config) ->
    create_named_query(ClientReqToken, Db, Name, Query, Description, [], Config);
create_named_query(ClientReqToken, Db, Name, Query, Description, Options)
  when is_list(Options) ->
    create_named_query(ClientReqToken, Db, Name, Query, Description, Options, default_config()).

-spec create_named_query(binary(), binary(), binary(), binary(),
                         binary() | undefined,
                         create_named_query_opts(),
                         aws_config()) ->
    {ok, binary()} | {error, any()}.
create_named_query(ClientReqToken, Db, Name, Query, Description, Options, Config) ->
    Params  = encode_params([{description, Description} | Options]),
    Request = Params#{<<"ClientRequestToken">> => ClientReqToken,
                      <<"Database">>           => Db,
                      <<"Name">>               => Name,
                      <<"QueryString">>        => Query},
    case request(Config, "CreateNamedQuery", Request) of
        {ok, Res} -> {ok, maps:get(<<"NamedQueryId">>, Res)};
        Error     -> Error
    end.

%% @doc
%% Athena API:
%% https://docs.aws.amazon.com/athena/latest/APIReference/API_CreatePreparedStatement.html
%%
-spec create_prepared_statement(binary(), binary(), binary()) -> ok | {error, any()}.
create_prepared_statement(WorkGroup, StatementName, QueryStatement) ->
    create_prepared_statement(WorkGroup, StatementName, QueryStatement, undefined, default_config()).

-spec create_prepared_statement(binary(), binary(), binary(), binary() | aws_config()) -> ok | {error, any()}.
create_prepared_statement(WorkGroup, StatementName, QueryStatement, Config) when is_record(Config, aws_config) ->
    create_prepared_statement(WorkGroup, StatementName, QueryStatement, undefined, Config);
create_prepared_statement(WorkGroup, StatementName, QueryStatement, Description) when is_binary(Description) ->
    create_prepared_statement(WorkGroup, StatementName, QueryStatement, Description, default_config()).

-spec create_prepared_statement(binary(), binary(), binary(), binary() | undefined, aws_config()) -> ok | {error, any()}.
create_prepared_statement(WorkGroup, StatementName, QueryStatement, Description, Config) ->
    Params  = encode_params([{description, Description}]),
    Request = Params#{<<"WorkGroup">>      => WorkGroup,
                      <<"StatementName">>  => StatementName,
                      <<"QueryStatement">> => QueryStatement},
    case request(Config, "CreatePreparedStatement", Request) of
        {ok, _} -> ok;
        Error   -> Error
    end.


%% @doc
%% Athena API:
%% http://docs.aws.amazon.com/athena/latest/APIReference/API_DeleteNamedQuery.html
%%
-spec delete_named_query(binary()) -> ok | {error, any()}.
delete_named_query(NamedQueryId) ->
    delete_named_query(NamedQueryId, default_config()).

-spec delete_named_query(binary(), aws_config()) -> ok | {error, any()}.
delete_named_query(NamedQueryId, Config) ->
    Request = #{<<"NamedQueryId">> => NamedQueryId},
    case request(Config, "DeleteNamedQuery", Request) of
        {ok, _} -> ok;
        Error   -> Error
    end.
%% @doc
%% Athena API:
%% https://docs.aws.amazon.com/athena/latest/APIReference/API_DeletePreparedStatement.html
%%

-spec delete_prepared_statement(binary(), binary()) -> ok | {error | any}.
delete_prepared_statement(WorkGroup, StatementName) ->
    delete_prepared_statement(WorkGroup, StatementName, default_config()).

-spec delete_prepared_statement(binary(), binary(), aws_config()) -> ok | {error | any}.
delete_prepared_statement(WorkGroup, StatementName, Config) ->
    Request = #{<<"WorkGroup">> => WorkGroup, <<"StatementName">> => StatementName},
    case request(Config, "DeletePreparedStatement", Request) of
        {ok, _} -> ok;
        Error   -> Error
    end.

%% @doc
%% Athena API:
%% http://docs.aws.amazon.com/athena/latest/APIReference/API_GetNamedQuery.html
%%
-spec get_named_query(binary()) -> {ok, map()} | {error, any()}.
get_named_query(NamedQueryId) ->
    get_named_query(NamedQueryId, default_config()).

-spec get_named_query(binary(), aws_config()) -> {ok, map()} | {error, any()}.
get_named_query(NamedQueryId, Config) ->
    Request = #{<<"NamedQueryId">> => NamedQueryId},
    request(Config, "GetNamedQuery", Request).

%% @doc
%% Athena API:
%% https://docs.aws.amazon.com/athena/latest/APIReference/API_GetPreparedStatement.html
%%
-spec get_prepared_statement(binary(), binary()) -> {ok, map()} | {error, any()}.
get_prepared_statement(WorkGroup, StatementName) ->
    get_prepared_statement(WorkGroup, StatementName, default_config()).

-spec get_prepared_statement(binary(), binary(), aws_config()) -> {ok, map()} | {error, any()}.
get_prepared_statement(WorkGroup, StatementName, Config) ->
    Request = #{<<"WorkGroup">> => WorkGroup, <<"StatementName">> => StatementName},
    request(Config, "GetPreparedStatement", Request).

%% @doc
%% Athena API:
%% http://docs.aws.amazon.com/athena/latest/APIReference/API_GetQueryExecution.html
%%
-spec get_query_execution(binary()) -> {ok, map()} | {error, any()}.
get_query_execution(QueryExecutionId) ->
    get_query_execution(QueryExecutionId, default_config()).

-spec get_query_execution(binary(), aws_config()) ->
    {ok, map()} | {error, any()}.
get_query_execution(QueryExecutionId, Config) ->
    Request = #{<<"QueryExecutionId">> => QueryExecutionId},
    request(Config, "GetQueryExecution", Request).

%% @doc
%% Athena API:
%% http://docs.aws.amazon.com/athena/latest/APIReference/API_GetQueryResults.html
%%
%% `
%% erlcloud_athena:get_query_results(<<"some-query-uuid">>,
%%                                   #{<<"MaxResults">> => 1,
%%                                     <<"NextToken">>  => <<"some-token">>}).
%% '
%%
-spec get_query_results(binary()) -> {ok, map()} | {error, any()}.
get_query_results(QueryExecutionId) ->
    get_query_results(QueryExecutionId, #{}, default_config()).

-spec get_query_results(binary(), map() | aws_config()) ->
    {ok, map()} | {error, any()}.
get_query_results(QueryExecutionId, Config)
  when is_record(Config, aws_config) ->
    get_query_results(QueryExecutionId, #{}, Config);
get_query_results(QueryExecutionId, PaginationMap)
  when is_map(PaginationMap) ->
    get_query_results(QueryExecutionId, PaginationMap, default_config()).

-spec get_query_results(binary(), map(), aws_config()) ->
    {ok, map()} | {error, any()}.
get_query_results(QueryExecutionId, PaginationMap, Config) ->
    Request = PaginationMap#{<<"QueryExecutionId">> => QueryExecutionId},
    request(Config, "GetQueryResults", Request).

%% @doc
%% Athena API:
%% http://docs.aws.amazon.com/athena/latest/APIReference/API_ListNamedQueries.html
%%
%% `
%% erlcloud_athena:list_named_queries(#{<<"MaxResults">> => 1,
%%                                      <<"NextToken">>  => <<"some-token">>,
%%                                      <<"WorkGroup">>  => <<"some-workgroup">>}).
%% '
%%
-type list_named_queries_opts() :: [list_named_queries_opt()].
-type list_named_queries_opt()  :: {workgroup, binary()}.

-spec list_named_queries() -> {ok, map()} | {error, any()}.
list_named_queries() ->
    list_named_queries(#{}, [], default_config()).

-spec list_named_queries(map() | aws_config()) ->
    {ok, map()} | {error, any()}.
list_named_queries(Config) when is_record(Config, aws_config) ->
    list_named_queries(#{}, [], Config);
list_named_queries(PaginationMap) when is_map(PaginationMap) ->
    list_named_queries(PaginationMap, [], default_config()).

-spec list_named_queries(map(), aws_config() | list_named_queries_opts()) ->
    {ok, map} | {error, any()}.
list_named_queries(PaginationMap, Config) when is_record(Config, aws_config) ->
    list_named_queries(PaginationMap, [], Config);
list_named_queries(PaginationMap, Options) when is_list(Options) ->
    list_named_queries(PaginationMap, Options, default_config()).

-spec list_named_queries(map(), list_named_queries_opts(), aws_config()) ->
    {ok, map} | {error, any()}.
list_named_queries(PaginationMap, Options, Config) ->
    Params = encode_params(Options),
    request(Config, "ListNamedQueries", maps:merge(PaginationMap, Params)).

%% @doc
%% Athena API:
%% https://docs.aws.amazon.com/athena/latest/APIReference/API_ListPreparedStatements.html
%%
%% `
%% erlcloud_athena:list_prepared_statements(<<"some-work-group">>,
%%                                          #{<<"MaxResults">> => 1,
%%                                            <<"NextToken">>  => <<"some-token">>}).
%% '
%%
- spec list_prepared_statements(binary()) -> {ok, map()} | {error, any()}.
list_prepared_statements(WorkGroup) ->
    list_prepared_statements(WorkGroup, #{}, default_config()).

- spec list_prepared_statements(binary(), map() | aws_config()) -> {ok, map()} | {error, any()}.
list_prepared_statements(WorkGroup, Config) when is_record(Config, aws_config) ->
    list_prepared_statements(WorkGroup, #{}, Config);
list_prepared_statements(WorkGroup, PaginationMap) when is_map(PaginationMap) ->
    list_prepared_statements(WorkGroup, PaginationMap, default_config()).

-spec list_prepared_statements(binary(), map(), aws_config()) -> {ok, map()} | {error, any()}.
list_prepared_statements(WorkGroup, PaginationMap, Config) ->
    Request = PaginationMap#{<<"WorkGroup">> => WorkGroup},
    request(Config, "ListPreparedStatements", Request).

%% @doc
%% Athena API:
%% http://docs.aws.amazon.com/athena/latest/APIReference/API_ListQueryExecutions.html
%%
%% `
%% erlcloud_athena:list_query_executions(#{<<"MaxResults">> => 1,
%%                                         <<"NextToken">>  => <<"some-token">>}).
%% '
%%
-type list_query_executions_opts() :: [list_query_executions_opt()].
-type list_query_executions_opt() :: {workgroup, binary()}.

-spec list_query_executions() -> {ok, map()} | {error, any()}.
list_query_executions() ->
    list_query_executions(#{}, [], default_config()).

-spec list_query_executions(map() | aws_config()) ->
    {ok, map()} | {error, any()}.
list_query_executions(Config) when is_record(Config, aws_config) ->
    list_query_executions(#{}, [], Config);
list_query_executions(PaginationMap) when is_map(PaginationMap) ->
    list_query_executions(PaginationMap, [], default_config()).

-spec list_query_executions(map(), aws_config() | list_query_executions_opts()) ->
    {ok, map()} | {error, any()}.
list_query_executions(PaginationMap, Config) when is_record(Config, aws_config) ->
    list_query_executions(PaginationMap, [], Config);
list_query_executions(PaginationMap, Options) when is_list(Options) ->
    list_query_executions(PaginationMap, Options, default_config()).

-spec list_query_executions(map(), list_query_executions_opts(), aws_config()) ->
    {ok, map()} | {error, any()}.
list_query_executions(PaginationMap, Options, Config) ->
    Params = encode_params(Options),
    request(Config, "ListQueryExecutions", maps:merge(PaginationMap, Params)).

%% @doc
%% Athena API:
%% http://docs.aws.amazon.com/athena/latest/APIReference/API_StartQueryExecution.html
%%
%% `
%% erlcloud_athena:start_query_execution(
%%                              <<"some-client-request-token">>,
%%                              <<"db-name">>,
%%                              <<"select * from some-tbl">>,
%%                              <<"s3://some-bucket">>,
%%                              <<"SSE_KMS">>,
%%                              <<"some-kms-key-id">>,
%%                              <<"optional-some-workgroup">>}]
%% ).
%% '
%%
-type start_query_execution_opts() :: [start_query_execution_opt()].
-type start_query_execution_opt()  :: {workgroup, binary()}.

-spec start_query_execution(binary(), binary(), binary(), binary()) ->
    {ok, binary()} | {error, any()}.
start_query_execution(ClientReqToken, Db, Query, OutputLocation) ->
    start_query_execution(ClientReqToken, Db, Query, OutputLocation, undefined,
                          undefined, [], default_config()).

-spec start_query_execution(binary(), binary(), binary(), binary(),
                            aws_config()) ->
    {ok, binary()} | {error, any()}.
start_query_execution(ClientReqToken, Db, Query, OutputLocation, Config)
  when is_record(Config, aws_config) ->
    start_query_execution(ClientReqToken, Db, Query, OutputLocation, undefined,
                          undefined, [], Config).

-spec start_query_execution(binary(), binary(), binary(), binary(),
                            binary() | undefined,
                            binary() | undefined) ->
    {ok, binary()} | {error, any()}.
start_query_execution(ClientReqToken, Db, Query, OutputLocation,
                      EncryptionOption, KmsKey) ->
    start_query_execution(ClientReqToken, Db, Query, OutputLocation,
                          EncryptionOption, KmsKey, [], default_config()).

-spec start_query_execution(binary(), binary(), binary(), binary(),
                            binary() | undefined,
                            binary() | undefined,
                            aws_config() | start_query_execution_opts()) ->
    {ok, binary()} | {error, any()}.
start_query_execution(ClientReqToken, Db, Query, OutputLocation,
                      EncryptionOption, KmsKey, Config)
  when is_record(Config, aws_config)  ->
    start_query_execution(ClientReqToken, Db, Query, OutputLocation,
                          EncryptionOption, KmsKey, [], Config);
start_query_execution(ClientReqToken, Db, Query, OutputLocation,
                      EncryptionOption, KmsKey, Opts)
  when is_list(Opts) ->
    start_query_execution(ClientReqToken, Db, Query, OutputLocation,
                          EncryptionOption, KmsKey, Opts, default_config()).

-spec start_query_execution(binary(), binary(), binary(), binary(),
                            binary() | undefined,
                            binary() | undefined,
                            start_query_execution_opts(),
                            aws_config()) ->
    {ok, binary()} | {error, any()}.
start_query_execution(ClientReqToken, Db, Query, OutputLocation,
                      EncryptionOption, KmsKey, Options, Config) ->
    Params        = encode_params(Options),
    EncryptConfig = get_encrypt_config(EncryptionOption, KmsKey),
    ResultConfig  = EncryptConfig#{<<"OutputLocation">> => OutputLocation},
    QueryExecCtxt = #{<<"Database">> => Db},
    Request       = Params#{<<"ClientRequestToken">>    => ClientReqToken,
                            <<"QueryExecutionContext">> => QueryExecCtxt,
                            <<"QueryString">>           => Query,
                            <<"ResultConfiguration">>   => ResultConfig},
    case request(Config, "StartQueryExecution", Request) of
        {ok, Res} -> {ok, maps:get(<<"QueryExecutionId">>, Res)};
        Error     -> Error
    end.

get_encrypt_config(undefined, _) ->
    #{};
get_encrypt_config(EncryptOption, undefined) ->
    #{<<"EncryptionConfiguration">> =>
      #{<<"EncryptionOption">> => EncryptOption}};
get_encrypt_config(EncryptOption, KmsKey) ->
    #{<<"EncryptionConfiguration">> =>
      #{<<"EncryptionOption">> => EncryptOption,
        <<"KmsKey">>           => KmsKey}}.

%% @doc
%% Athena API:
%% https://docs.aws.amazon.com/athena/latest/APIReference/API_UpdatePreparedStatement.html
%%
-spec update_prepared_statement(binary(), binary(), binary()) -> ok | {error, any()}.
update_prepared_statement(WorkGroup, StatementName, QueryStatement) ->
    update_prepared_statement(WorkGroup, StatementName, QueryStatement, undefined, default_config()).

-spec update_prepared_statement(binary(), binary(), binary(), binary() | aws_config()) -> ok | {error, any()}.
update_prepared_statement(WorkGroup, StatementName, QueryStatement, Config) when is_record(Config, aws_config) ->
    update_prepared_statement(WorkGroup, StatementName, QueryStatement, undefined, Config);
update_prepared_statement(WorkGroup, StatementName, QueryStatement, Description) when is_binary(Description) ->
    update_prepared_statement(WorkGroup, StatementName, QueryStatement, Description, default_config()).

-spec update_prepared_statement(binary(), binary(), binary(), binary() | undefined, aws_config()) -> ok | {error, any()}.
update_prepared_statement(WorkGroup, StatementName, QueryStatement, Description, Config) ->
    Params = encode_params([{description, Description}]),
    Request = Params#{<<"WorkGroup">>      => WorkGroup,
                      <<"StatementName">>  => StatementName,
                      <<"QueryStatement">> => QueryStatement},
    case request(Config, "UpdatePreparedStatement", Request) of
        {ok, _} -> ok;
        Error   -> Error
    end.

%% @doc
%% Athena API:
%% http://docs.aws.amazon.com/athena/latest/APIReference/API_StopQueryExecution.html
%%
-spec stop_query_execution(binary()) -> ok | {error, any()}.
stop_query_execution(QueryExecutionId) ->
    stop_query_execution(QueryExecutionId, default_config()).

-spec stop_query_execution(binary(), aws_config()) -> ok | {error, any()}.
stop_query_execution(QueryExecutionId, Config) ->
    Request = #{<<"QueryExecutionId">> => QueryExecutionId},
    case request(Config, "StopQueryExecution", Request) of
        {ok, _} -> ok;
        Error   -> Error
    end.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
default_config() -> erlcloud_aws:default_config().

request(Config0, OperationName, Request) ->
    case erlcloud_aws:update_config(Config0) of
        {ok, Config} ->
            Body       = jsx:encode(Request),
            Operation  = "AmazonAthena." ++ OperationName,
            Headers    = get_headers(Config, Operation, Body),
            AwsRequest = #aws_request{service         = athena,
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

get_headers(#aws_config{athena_host = Host} = Config, Operation, Body) ->
    Headers = [{"host",         Host},
               {"x-amz-target", Operation},
               {"content-type", "application/x-amz-json-1.1"}],
    Region = erlcloud_aws:aws_region_from_host(Host),
    erlcloud_aws:sign_v4_headers(Config, Headers, Body, Region, "athena").

get_url(#aws_config{athena_scheme = Scheme,
                    athena_host   = Host,
                    athena_port   = Port}) ->
    Scheme ++ Host ++ ":" ++ integer_to_list(Port).

encode_params(Params) ->
  encode_params(Params, []).

encode_params([], Acc) ->
  maps:from_list(Acc);
encode_params([{_, undefined} | T], Acc) ->
  encode_params(T, Acc);
encode_params([{description, Description} | T], Acc) when is_binary(Description) ->
  encode_params(T, [{<<"Description">>, Description} | Acc]);
encode_params([{workgroup, WorkGroup} | T], Acc) when is_binary(WorkGroup) ->
  encode_params(T, [{<<"WorkGroup">>, WorkGroup} | Acc]);
encode_params([Option | _], _Acc) ->
  error({erlcloud_athena, {invalid_parameter, Option}}).
