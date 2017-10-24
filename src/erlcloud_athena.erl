-module(erlcloud_athena).

-include("erlcloud_aws.hrl").

%% API
-export([configure/2, configure/3, configure/4, configure/5,
         new/2, new/3, new/4, new/5]).

-export([
    batch_get_named_query/1,
    batch_get_named_query/2,

    batch_get_query_execution/1,
    batch_get_query_execution/2,

    create_named_query/4,
    create_named_query/5,
    create_named_query/6,
    delete_named_query/1,
    delete_named_query/2,

    get_named_query/1,
    get_named_query/2,

    get_query_execution/1,
    get_query_execution/2,

    get_query_results/1,
    get_query_results/2,
    get_query_results/3,

    list_named_queries/0,
    list_named_queries/1,
    list_named_queries/2,

    list_query_executions/0,
    list_query_executions/1,
    list_query_executions/2,

    start_query_execution/4,
    start_query_execution/5,
    start_query_execution/6,
    start_query_execution/7,

    stop_query_execution/1,
    stop_query_execution/2
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
%%                            <<"optional-query-description">>
%% ).
%% '
%%
-spec create_named_query(binary(), binary(), binary(), binary()) ->
    {ok, binary()} | {error, any()}.
create_named_query(ClientReqToken, Db, Name, Query) ->
    Config = default_config(),
    create_named_query(ClientReqToken, Db, Name, Query, undefined, Config).

-spec create_named_query(binary(), binary(), binary(), binary(),
                         aws_config() | binary()) ->
    {ok, binary()} | {error, any()}.
create_named_query(ClientReqToken, Db, Name, Query, Config)
  when is_record(Config, aws_config) ->
    create_named_query(ClientReqToken, Db, Name, Query, undefined, Config);
create_named_query(ClientReqToken, Db, Name, Query, Description)
  when is_binary(Description) ->
    Config = default_config(),
    create_named_query(ClientReqToken, Db, Name, Query, Description, Config).

-spec create_named_query(binary(), binary(), binary(), binary(),
                         binary() | undefined,
                         aws_config()) ->
    {ok, binary()} | {error, any()}.
create_named_query(ClientReqToken, Db, Name, Query, Description, Config) ->
    Request0 = #{<<"ClientRequestToken">> => ClientReqToken,
                  <<"Database">>           => Db,
                  <<"Name">>               => Name,
                  <<"QueryString">>        => Query},
    Request1 = update_query_description(Request0, Description),
    case request(Config, "CreateNamedQuery", Request1) of
        {ok, Res} -> {ok, maps:get(<<"NamedQueryId">>, Res)};
        Error     -> Error
    end.

update_query_description(Request, undefined)   -> Request;
update_query_description(Request, Description) ->
    maps:put(<<"Description">>, Description, Request).

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
%%                                      <<"NextToken">>  => <<"some-token">>}).
%% '
%%
-spec list_named_queries() -> {ok, map()} | {error, any()}.
list_named_queries() ->
    list_named_queries(#{}, default_config()).

-spec list_named_queries(map() | aws_config()) -> {ok, map()} | {error, any()}.
list_named_queries(Config) when is_record(Config, aws_config) ->
    list_named_queries(#{}, Config);
list_named_queries(PaginationMap) when is_map(PaginationMap) ->
    list_named_queries(PaginationMap, default_config()).

-spec list_named_queries(map(), aws_config()) -> {ok, map} | {error, any()}.
list_named_queries(PaginationMap, Config) ->
    request(Config, "ListNamedQueries", PaginationMap).

%% @doc
%% Athena API:
%% http://docs.aws.amazon.com/athena/latest/APIReference/API_ListQueryExecutions.html
%%
%% `
%% erlcloud_athena:list_query_executions(#{<<"MaxResults">> => 1,
%%                                         <<"NextToken">>  => <<"some-token">>}).
%% '
%%
-spec list_query_executions() -> {ok, map()} | {error, any()}.
list_query_executions() ->
    list_query_executions(#{}, default_config()).

-spec list_query_executions(map() | aws_config()) ->
    {ok, map()} | {error, any()}.
list_query_executions(Config) when is_record(Config, aws_config) ->
    list_query_executions(#{}, Config);
list_query_executions(PaginationMap) when is_map(PaginationMap) ->
    list_query_executions(PaginationMap, default_config()).

-spec list_query_executions(map(), aws_config()) ->
    {ok, map()} | {error, any()}.
list_query_executions(PaginationMap, Config) ->
    request(Config, "ListQueryExecutions", PaginationMap).

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
%%                              <<"some-kms-key-id">>}]
%% ).
%% '
%%
-spec start_query_execution(binary(), binary(), binary(), binary()) ->
    {ok, binary()} | {error, any()}.
start_query_execution(ClientReqToken, Db, Query, OutputLocation) ->
    start_query_execution(ClientReqToken, Db, Query, OutputLocation, undefined,
                          undefined, default_config()).

-spec start_query_execution(binary(), binary(), binary(), binary(),
                            aws_config()) ->
    {ok, binary()} | {error, any()}.
start_query_execution(ClientReqToken, Db, Query, OutputLocation, Config)
  when is_record(Config, aws_config) ->
    start_query_execution(ClientReqToken, Db, Query, OutputLocation, undefined,
                          undefined, Config).

-spec start_query_execution(binary(), binary(), binary(), binary(),
                            binary() | undefined,
                            binary() | undefined) ->
    {ok, binary()} | {error, any()}.
start_query_execution(ClientReqToken, Db, Query, OutputLocation,
                      EncryptionOption, KmsKey) ->
    start_query_execution(ClientReqToken, Db, Query, OutputLocation,
                          EncryptionOption, KmsKey, default_config()).

-spec start_query_execution(binary(), binary(), binary(), binary(),
                            binary() | undefined,
                            binary() | undefined,
                            aws_config()) ->
    {ok, binary()} | {error, any()}.
start_query_execution(ClientReqToken, Db, Query, OutputLocation,
                      EncryptionOption, KmsKey, Config) ->
    EncryptConfig = get_encrypt_config(EncryptionOption, KmsKey),
    ResultConfig  = EncryptConfig#{<<"OutputLocation">> => OutputLocation},
    QueryExecCtxt = #{<<"Database">> => Db},
    Request       = #{<<"ClientRequestToken">>    => ClientReqToken,
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
