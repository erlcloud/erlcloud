-module(erlcloud_lambda).
-author('eholland@alertlogic.com').

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").
-include("erlcloud_lambda.hrl").

-define(API_VERSION, "2015-03-31").

%%% Library initialization.
-export([configure/2, configure/3, configure/4,
         new/2, new/3, new/4]).
-export([query/6]).
-export([create_alias/4, create_alias/5,
         create_event_source_mapping/4, create_event_source_mapping/5,
         create_function/6, create_function/7,
         delete_event_source_mapping/1, delete_event_source_mapping/2,
         delete_function/1, delete_function/2, delete_function/3,
         get_alias/2, get_alias/3,
         get_event_source_mapping/1, get_event_source_mapping/2,
         get_function/1, get_function/2, get_function/3,
         get_function_configuration/1, get_function_configuration/2, get_function_configuration/3,
         invoke/1, invoke/2, invoke/3, invoke/4, invoke/5,
         list_aliases/1, list_aliases/2, list_aliases/4, list_aliases/5,
         list_event_source_mappings/2, list_event_source_mappings/4, list_event_source_mappings/5,
         list_functions/0, list_functions/2, list_functions/3,
         list_versions_by_function/1, list_versions_by_function/3, list_versions_by_function/4,
         publish_version/1, publish_version/3, publish_version/4,
         update_alias/2, update_alias/4, update_alias/5,
         update_event_source_mapping/4, update_event_source_mapping/5,
         update_function_code/3, update_function_code/4,
         update_function_configuration/3, update_function_configuration/6, update_function_configuration/7]).

-type(runtime()     :: 'nodejs' | 'nodejs4.3' | 'nodejs6.10' | 'nodejs8.10' | 'java8' |
    'python2.7' | 'python3.6' | 'dotnetcore1.0' | 'dotnetcore2.0' | 'dotnetcore2.1' | 'nodejs4.3-edge' | 'go1.x').
-type(return_val()  :: any()).

-import(erlcloud_util, [filter_undef/1]).

%%------------------------------------------------------------------------------
%% Library initialization.
%%------------------------------------------------------------------------------

-spec new(string(), string()) -> aws_config().

new(AccessKeyID, SecretAccessKey) ->
    #aws_config{
       access_key_id=AccessKeyID,
       secret_access_key=SecretAccessKey
      }.

-spec new(string(), string(), string()) -> aws_config().

new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{
       access_key_id=AccessKeyID,
       secret_access_key=SecretAccessKey,
       lambda_host=Host
      }.

-spec new(string(), string(), string(), non_neg_integer()) -> aws_config().

new(AccessKeyID, SecretAccessKey, Host, Port) ->
    #aws_config{
       access_key_id=AccessKeyID,
       secret_access_key=SecretAccessKey,
       lambda_host=Host,
       lambda_port=Port
      }.

-spec configure(string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey) ->
    erlcloud_config:configure(AccessKeyID, SecretAccessKey, fun new/2).

-spec configure(string(), string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host) ->
    erlcloud_config:configure(AccessKeyID, SecretAccessKey, Host, fun new/3).

-spec configure(string(), string(), string(), non_neg_integer()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host, Port) ->
    erlcloud_config:configure(AccessKeyID, SecretAccessKey, Host, Port, fun new/4).

default_config() -> erlcloud_aws:default_config().


query(Config, Method, Path, Body, QParams, Options) ->
  ApiVersion = proplists:get_value(version, Options, ?API_VERSION),
  Options2 = proplists:delete(version, Options),
  BasePath = "/" ++ ApiVersion ++ "/",
  case lambda_request(Config, Method, BasePath ++ Path, Body, QParams, [{raw_response_body, true} | Options2]) of
    {ok, Bin} ->
      {ok, jiffy:decode(Bin, [return_maps, copy_strings])};
    Error -> Error
  end.

%%------------------------------------------------------------------------------
%% CreateAlias
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Lambda API:
%% [http://docs.aws.amazon.com/lambda/latest/dg/API_CreateAlias.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec create_alias(FunctionName    :: binary(),
                         FunctionVersion :: binary(),
                         AliasName       :: binary(),
                         Options         :: proplist()) -> return_val().

create_alias(FunctionName, FunctionVersion, AliasName, Options) ->
create_alias(FunctionName, FunctionVersion,
                 AliasName, Options, default_config()).

-spec create_alias(FunctionName    :: binary(),
                         FunctionVersion :: binary(),
                         AliasName       :: binary(),
                         Options         :: proplist(),
                         Config          :: aws_config()) -> return_val().
create_alias(FunctionName, FunctionVersion, AliasName, Options, Config) ->
    Path = base_path() ++ "functions/" ++  url_parameter(FunctionName) ++ "/aliases",
    Json = [{<<"FunctionVersion">>, FunctionVersion},
            {<<"Name">>, AliasName}
            | Options],
    lambda_request(Config, post, Path, Json).

%%------------------------------------------------------------------------------
%% CreateEventSourceMapping
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Lambda API:
%% [http://docs.aws.amazon.com/lambda/latest/dg/API_CreateEventSourceMapping.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec create_event_source_mapping(EventSourceArn   :: binary(),
                                        FunctionName     :: binary(),
                                        StartingPosition :: binary(),
                                        Options          :: proplist())
                                       -> return_val().
create_event_source_mapping(EventSourceArn, FunctionName,
                            StartingPosition, Options) ->
    create_event_source_mapping(EventSourceArn, FunctionName,
                                StartingPosition, Options, default_config()).

-spec create_event_source_mapping(EventSourceArn   :: binary(),
                                        FunctionName     :: binary(),
                                        StartingPosition :: binary(),
                                        Options          :: proplist(),
                                        Config           :: aws_config())
                                       -> return_val().
create_event_source_mapping(EventSourceArn, FunctionName,
                            StartingPosition, Options, Config) ->
    Path = base_path() ++ "event-source-mappings",
    Json = [{<<"EventSourceArn">>, EventSourceArn},
            {<<"FunctionName">>, FunctionName},
            {<<"StartingPosition">>, StartingPosition}
            | Options],
    lambda_request(Config, post, Path, Json).


%%------------------------------------------------------------------------------
%% CreateFunction
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Lambda API:
%% [http://docs.aws.amazon.com/lambda/latest/dg/API_CreateFunction.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec create_function(Code         :: erlcloud_lambda_code(),
                            FunctionName :: binary(),
                            Handler      :: binary(),
                            Role         :: binary(),
                            Runtime      :: runtime(),
                            Options      :: proplist()) -> return_val().
create_function(#erlcloud_lambda_code{} = Code,
                FunctionName, Handler, Role, Runtime, Options) ->
    create_function(Code, FunctionName, Handler, Role,
                    Runtime, Options, default_config()).

-spec create_function(Code         :: erlcloud_lambda_code(),
                            FunctionName :: binary(),
                            Handler      :: binary(),
                            Role         :: binary(),
                            Runtime      :: runtime(),
                            Options      :: proplist(),
                            Config       :: aws_config()) -> return_val().
create_function(#erlcloud_lambda_code{} = Code,
                FunctionName, Handler, Role, Runtime, Options, Config) ->
    Json = [{<<"Code">>, from_record(Code)},
            {<<"FunctionName">>, FunctionName},
            {<<"Handler">>, Handler},
            {<<"Role">>, Role},
            {<<"Runtime">>, Runtime}
            | Options],
    Path = base_path() ++ "functions",
    lambda_request(Config, post, Path, Json).

%%------------------------------------------------------------------------------
%% DeleteEventSourceMapping
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Lambda API:
%% [http://docs.aws.amazon.com/lambda/latest/dg/API_DeleteEventSourceMapping.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------

-spec delete_event_source_mapping(Uuid :: binary()) -> return_val().
delete_event_source_mapping(Uuid) ->
    delete_event_source_mapping(Uuid, default_config()).

-spec delete_event_source_mapping(Uuid   :: binary(),
                                        Config :: aws_config()) -> return_val().
delete_event_source_mapping(Uuid, Config) ->
    Path = base_path() ++ "event-source-mappings/" ++ url_parameter(Uuid),
    lambda_request(Config, delete, Path, undefined).

%%------------------------------------------------------------------------------
%% DeleteFunction
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Lambda API:
%% [https://docs.aws.amazon.com/lambda/latest/dg/API_DeleteFunction.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec delete_function(FunctionName :: binary()) -> return_val().
delete_function(FunctionName) ->
    delete_function(FunctionName, default_config()).

-spec delete_function(FunctionName :: binary(),
    Config       :: aws_config()) -> return_val().
delete_function(FunctionName, Config) ->
    delete_function(FunctionName, undefined, Config).

-spec delete_function(FunctionName :: binary(),
    Qualifier    :: undefined | binary(),
    Config       :: aws_config()) -> return_val().
delete_function(FunctionName, Qualifier, Config) ->
    Path = base_path() ++ "functions/" ++ url_parameter(FunctionName),
    QParams = filter_undef([{"Qualifier", Qualifier}]),
    lambda_request(Config, delete, Path, undefined, QParams).

%%------------------------------------------------------------------------------
%% GetAlias
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Lambda API:
%% [http://docs.aws.amazon.com/lambda/latest/dg/API_GetAlias.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------

-spec get_alias(FunctionName :: binary(),
                Name         :: binary()) -> return_val().
get_alias(FunctionName, Name) ->
    get_alias(FunctionName, Name, default_config()).

-spec get_alias(FunctionName :: binary(),
                Name         :: binary(),
                Config       :: aws_config()) -> return_val().
get_alias(FunctionName, Name, Config) ->
    Path = base_path() ++ "functions/"
        ++ url_parameter(FunctionName) ++ "/aliases/" ++ url_parameter(Name),
    lambda_request(Config, get, Path, undefined).

%%------------------------------------------------------------------------------
%% GetEventSourceMapping
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Lambda API:
%% [http://docs.aws.amazon.com/lambda/latest/dg/API_GetEventSourceMapping.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------

-spec get_event_source_mapping(Uuid :: binary()) -> return_val().
get_event_source_mapping(Uuid) ->
    get_event_source_mapping(Uuid, default_config()).

-spec get_event_source_mapping(Uuid   :: binary(),
                               Config :: aws_config()) -> return_val().
get_event_source_mapping(Uuid, Config) ->
    Path = base_path() ++ "event-source-mappings/" ++ url_parameter(Uuid),
    lambda_request(Config, get, Path, undefined).

%%------------------------------------------------------------------------------
%% GetFunction
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Lambda API:
%% [http://docs.aws.amazon.com/lambda/latest/dg/API_GetFunction.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------

-spec get_function(FunctionName :: binary()) -> return_val().
get_function(FunctionName) ->
    get_function(FunctionName, default_config()).

-spec get_function(FunctionName :: binary(),
                   Config       :: aws_config()) -> return_val().
get_function(FunctionName, Config) ->
    get_function(FunctionName, undefined, Config).

-spec get_function(FunctionName :: binary(),
                   Qualifier    :: undefined | binary(),
                   Config       :: aws_config()) -> return_val().
get_function(FunctionName, Qualifier, Config) ->
    Path = base_path() ++ "functions/" ++ url_parameter(FunctionName),
    QParams = filter_undef([{"Qualifier", Qualifier}]),
    lambda_request(Config, get, Path, undefined, QParams).

%%------------------------------------------------------------------------------
%% GetFunctionConfiguration
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Lambda API:
%% [http://docs.aws.amazon.com/lambda/latest/dg/API_GetFunctionConfiguration.html]
%%
%% ===Example===
%%
%%-----------------------------------------------------------------------------
-spec get_function_configuration(FunctionName :: binary()) -> return_val().
get_function_configuration(FunctionName) ->
    get_function_configuration(FunctionName, default_config()).

-spec get_function_configuration(FunctionName :: binary(),
                                 Config       :: aws_config()) -> return_val().
get_function_configuration(FunctionName, Config) ->
    get_function_configuration(FunctionName, undefined, Config).

get_function_configuration(Function, Qualifier, Config) ->
    Path = base_path() ++ "functions/" ++ url_parameter(Function) ++ "/configuration",
    QParams = filter_undef([{"Qualifier", Qualifier}]),
    lambda_request(Config, get, Path, undefined, QParams).


%%------------------------------------------------------------------------------
%% @doc
%% Lambda API:
%% [http://docs.aws.amazon.com/lambda/latest/dg/API_Invoke.html]
%%
%% option {show_headers, true|false} may be used for invoking Lambdas
%% this option changes returned spec of successful Lambda invocation
%%      false(default): output spec is {ok, Data}
%%      true: output spec is {ok, Headers, Data} where additional headers of
%%            Lambda invocation is returned
%%
%%
%% ===Examples===
%% Async invoke with no logs and empty event
%% erlcloud_lambda:invoke(<<"my_lambda">>, [],
%%    [{"X-Amz-Invocation-Type", "Event"}, {"X-Amz-Log-Type", "None"}], AwsCfg).
%% Sync invoke returned invocation headers (contains logs)
%% erlcloud_lambda:invoke(<<"my_lambda">>, [],
%%    [{show_headers, true}, {"X-Amz-Log-Type", "Tail"},
%%    {"X-Amz-Invocation-Type", "RequestResponse"}], AwsCfg).
%%
%%
%%-----------------------------------------------------------------------------
-spec invoke(FunctionName :: binary()) -> return_val().
invoke(FunctionName) ->
  invoke(FunctionName, default_config()).

-spec invoke(FunctionName :: binary(),
             Config :: aws_config() | list()) -> return_val().
invoke(FunctionName, Config = #aws_config{}) ->
    invoke(FunctionName, [], Config);
invoke(FunctionName, Payload) when is_list(Payload)->
    invoke(FunctionName, Payload, default_config()).

-spec invoke(FunctionName :: binary(),
             Payload :: list() | binary(),
             Config  :: aws_config() | binary()) -> return_val().
invoke(FunctionName, Payload, ConfigOrQualifier) when is_list(Payload)->
    invoke(FunctionName, Payload, [], ConfigOrQualifier).

-spec invoke(FunctionName :: binary(),
             Payload :: list() | binary(),
             Options :: list(),
             Config  :: aws_config() | binary()) -> return_val().
invoke(FunctionName, Payload, Options, Config = #aws_config{}) ->
    invoke(FunctionName, Payload, Options, undefined, Config);
invoke(FunctionName, Payload, Options, Qualifier) when is_binary(Qualifier) ->
    invoke(FunctionName, Payload, Options, Qualifier, default_config()).

-spec invoke(FunctionName :: binary(),
             Payload   :: list() | binary(),
             Options   :: list(),
             Qualifier :: binary()| undefined,
             Config    :: aws_config()) -> return_val().
invoke(FunctionName, Payload, Options, Qualifier, Config = #aws_config{}) ->
    Path = base_path() ++ "functions/" ++ url_parameter(FunctionName) ++ "/invocations",
    QParams = filter_undef([{"Qualifier", Qualifier}]),
    lambda_request(Config, post, Path, Payload, QParams, Options).

%%------------------------------------------------------------------------------
%% ListAliases
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Lambda API:
%% [http://docs.aws.amazon.com/lambda/latest/dg/API_ListAliases.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------

-spec list_aliases(FunctionName    :: binary()) -> return_val().
list_aliases(FunctionName) ->
    list_aliases(FunctionName, undefined, undefined, undefined).

-spec list_aliases(FunctionName    :: binary(),
                   FunctionVersion :: binary() | undefined) -> return_val().
list_aliases(FunctionName, FunctionVersion) ->
    list_aliases(FunctionName, FunctionVersion, undefined, undefined).

-spec list_aliases(FunctionName    :: binary(),
                   FunctionVersion :: binary() | undefined,
                   Marker          :: binary() | undefined,
                   MaxItems        :: integer() | undefined) -> return_val().
list_aliases(FunctionName, FunctionVersion, Marker, MaxItems) ->
    list_aliases(FunctionName, FunctionVersion,
                 Marker, MaxItems, default_config()).

-spec list_aliases(FunctionName    :: binary(),
                   FunctionVersion :: binary() | undefined,
                   Marker          :: binary() | undefined,
                   MaxItems        :: integer() | undefined,
                   Config          :: aws_config()) -> return_val().
list_aliases(FunctionName, FunctionVersion, Marker, MaxItems, Config) ->
    Path = base_path() ++ "functions/"
        ++ url_parameter(FunctionName) ++ "/aliases",
    QParams = filter_undef([{"Marker", Marker},
                            {"MaxItems", MaxItems},
                            {"FunctionVersion", FunctionVersion}]),
    lambda_request(Config, get, Path, undefined, QParams).

%%------------------------------------------------------------------------------
%% ListEventSourceMappings
%%------------------------------------------------------------------------------
%%------------------------------------------------------------------------------
%% @doc
%% Lambda API:
%% [http://docs.aws.amazon.com/lambda/latest/dg/API_ListEventSourceMappings.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------

-spec list_event_source_mappings(EventSourceArn :: binary(),
                                 FunctionName   :: binary()) -> return_val().
list_event_source_mappings(EventSourceArn, FunctionName) ->
    list_event_source_mappings(EventSourceArn, FunctionName, undefined, undefined).

-spec list_event_source_mappings(EventSourceArn :: binary(),
                                 FunctionName   :: binary(),
                                 Marker         :: binary() | undefined,
                                 MaxItems       :: integer() | undefined) -> return_val().
list_event_source_mappings(EventSourceArn, FunctionName, Marker, MaxItems) ->
    list_event_source_mappings(EventSourceArn, FunctionName,
                               Marker, MaxItems, default_config()).

-spec list_event_source_mappings(EventSourceArn :: binary(),
                                 FunctionName   :: binary(),
                                 Marker         :: binary() | undefined,
                                 MaxItems       :: integer() | undefined,
                                 Config         :: aws_config()) -> return_val().
list_event_source_mappings(EventSourceArn, FunctionName, Marker, MaxItems, Config) ->
    Path = base_path() ++ "event-source-mappings/",
    QParams = filter_undef([{"Marker", Marker},
                            {"MaxItems", MaxItems},
                            {"EventSourceArn", EventSourceArn},
                            {"FunctionName", FunctionName}]),
    lambda_request(Config, get, Path, undefined, QParams).

%%------------------------------------------------------------------------------
%% ListFunctions
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Lambda API:
%% [http://docs.aws.amazon.com/lambda/latest/dg/API_ListFunctions.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------

-spec list_functions() -> return_val().
list_functions() ->
    list_functions(undefined, undefined, default_config()).

-spec list_functions(Marker   :: binary(),
                     MaxItems :: integer()) -> return_val().
list_functions(Marker, MaxItems) ->
    list_functions(Marker, MaxItems, default_config()).

-spec list_functions(Marker   :: undefined | binary(),
                     MaxItems :: undefined | integer(),
                     Config   :: aws_config()) -> return_val().
list_functions(Marker, MaxItems, Config) ->
    Path = base_path() ++ "functions/",
    QParams = filter_undef([{"Marker", Marker},
                            {"MaxItems", MaxItems}]),
    lambda_request(Config, get, Path, undefined, QParams).

%%------------------------------------------------------------------------------
%% ListVersionsByFunction
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Lambda API:
%% [http://docs.aws.amazon.com/lambda/latest/dg/API_ListVersionsByFunction.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec list_versions_by_function(FunctionName :: binary()) -> return_val().
list_versions_by_function(Function) ->
    list_versions_by_function(Function, undefined, undefined).

-spec list_versions_by_function(FunctionName :: binary(),
                                Marker       :: binary() | undefined,
                                MaxItems     :: integer() | undefined) -> return_val().
list_versions_by_function(Function, Marker, MaxItems) ->
    list_versions_by_function(Function, Marker, MaxItems, default_config()).

-spec list_versions_by_function(FunctionName :: binary(),
                                Marker       :: binary() | undefined,
                                MaxItems     :: integer() | undefined,
                                Config       :: aws_config()) -> return_val().
list_versions_by_function(Function, Marker, MaxItems, Config) ->
    Path = base_path() ++ "functions/" ++ url_parameter(Function) ++ "/versions",
    QParams = filter_undef([{"Marker", Marker},
                            {"MaxItems", MaxItems}]),
    lambda_request(Config, get, Path, undefined, QParams).


%%------------------------------------------------------------------------------
%% PublishVersion
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Lambda API:
%% [http://docs.aws.amazon.com/lambda/latest/dg/API_PublishVersion.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec publish_version(FunctionName :: binary()) -> return_val().
publish_version(FunctionName) ->
    publish_version(FunctionName, undefined, undefined).

-spec publish_version(FunctionName :: binary(),
                      CodeSha      :: binary() | undefined,
                      Description  :: binary() | undefined) -> return_val().
publish_version(FunctionName, CodeSha, Description) ->
    publish_version(FunctionName, CodeSha, Description, default_config()).

-spec publish_version(FunctionName :: binary(),
                      CodeSha      :: binary() | undefined,
                      Description  :: binary() | undefined,
                      Config       :: aws_config()) -> return_val().
publish_version(FunctionName, CodeSha, Description, Config) ->
    Path = base_path() ++ "functions/" ++ url_parameter(FunctionName) ++ "/versions",
    Json = filter_undef([{<<"CodeSha">>, CodeSha},
                         {<<"Description">>, Description}]),
    lambda_request(Config, post, Path, Json).

%%------------------------------------------------------------------------------
%% UpdateAlias
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Lambda API:
%% [http://docs.aws.amazon.com/lambda/latest/dg/API_UpdateAlias.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec update_alias(FunctionName    :: binary(),
                   AliasName       :: binary()) -> return_val().
update_alias(FunctionName, AliasName) ->
    update_alias(FunctionName, AliasName, undefined, undefined).

-spec update_alias(FunctionName    :: binary(),
                   AliasName       :: binary(),
                   Description     :: binary() | undefined,
                   FunctionVersion :: binary() | undefined) -> return_val().
update_alias(FunctionName, AliasName, Description, FunctionVersion) ->
    update_alias(FunctionName, AliasName,
                 Description, FunctionVersion, default_config()).

-spec update_alias(FunctionName    :: binary(),
                   AliasName       :: binary(),
                   Description     :: binary() | undefined,
                   FunctionVersion :: binary() | undefined,
                   Config          :: aws_config()) -> return_val().
update_alias(FunctionName, AliasName, Description, FunctionVersion, Config) ->
    Path = base_path() ++ "functions/"
        ++ url_parameter(FunctionName) ++ "/aliases/" ++ url_parameter(AliasName),
    Json = filter_undef([{"Description", Description},
                         {"FunctionVersion", FunctionVersion}]),
    lambda_request(Config, put, Path, Json).

%%------------------------------------------------------------------------------
%% UpdateEventSourceMapping
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Lambda API:
%% [http://docs.aws.amazon.com/lambda/latest/dg/API_UpdateEventSourceMapping.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------

-spec update_event_source_mapping(Uuid         :: binary(),
                                  BatchSize    :: integer() | undefined,
                                  Enabled      :: boolean() | undefined,
                                  FunctionName :: binary() | undefined) -> return_val().
update_event_source_mapping(Uuid, BatchSize, Enabled, FunctionName) ->
    update_event_source_mapping(Uuid, BatchSize, Enabled,
                                FunctionName, default_config()).

-spec update_event_source_mapping(Uuid         :: binary(),
                                  BatchSize    :: integer() | undefined,
                                  Enabled      :: boolean() | undefined,
                                  FunctionName :: binary() | undefined,
                                  Config       :: aws_config()) -> return_val().
update_event_source_mapping(Uuid, BatchSize, Enabled, FunctionName, Config) ->
    Path = base_path() ++ "event-source-mappings/" ++ url_parameter(Uuid),
    Json = filter_undef([{<<"BatchSize">>, BatchSize},
                         {<<"Enabled">>, Enabled},
                         {<<"FunctionName">>, FunctionName}]),
    lambda_request(Config, put, Path, Json).

%%------------------------------------------------------------------------------
%% UpdateFunctionCode
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Lambda API:
%% [http://docs.aws.amazon.com/lambda/latest/dg/API_UpdateFunctionCode.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------

-spec update_function_code(FunctionName :: binary(),
                           Publish      :: boolean(),
                           Code         :: erlcloud_lambda_code()) -> return_val().
update_function_code(FunctionName, Publish, Code) ->
    update_function_code(FunctionName, Publish, Code, default_config()).

-spec update_function_code(FunctionName :: binary(),
                           Publish      :: boolean(),
                           Code         :: erlcloud_lambda_code(),
                           Config       :: aws_config()) -> return_val().
update_function_code(FunctionName, Publish, Code, Config) ->
    Path = base_path() ++ "functions/" ++ url_parameter(FunctionName) ++ "/code",
    Json = [{<<"Publish">>, Publish} | from_record(Code)],
    lambda_request(Config, put, Path, Json).

%%------------------------------------------------------------------------------
%% UpdateFunctionConfiguration
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Lambda API:
%% [http://docs.aws.amazon.com/lambda/latest/dg/API_UpdateFunctionConfiguration.html]
%%
%% ===Example===
%%
%%------------------------------------------------------------------------------
-spec update_function_configuration(FunctionName :: binary(),
                                    Description  :: binary() | undefined,
                                    Handler      :: binary() | undefined,
                                    MemorySize   :: integer() | undefined,
                                    Role         :: binary() | undefined,
                                    Timeout      :: 1..300 | undefined) -> return_val().
update_function_configuration(FunctionName, Description,
                              Handler, MemorySize, Role, Timeout) ->
    update_function_configuration(FunctionName, Description, Handler,
                                  MemorySize, Role, Timeout, default_config()).


-spec update_function_configuration(FunctionName :: binary(),
                                    Description  :: binary() | undefined,
                                    Handler      :: binary() | undefined,
                                    MemorySize   :: integer() | undefined,
                                    Role         :: binary() | undefined,
                                    Timeout      :: 1..300 | undefined,
                                    Config       :: aws_config()) -> return_val().
update_function_configuration(FunctionName, Description, Handler,
                              MemorySize, Role, Timeout, Config) ->
    Configuration = [{<<"Description">>, Description},
                     {<<"Handler">>, Handler},
                     {<<"MemorySize">>, MemorySize},
                     {<<"Role">>, Role},
                     {<<"Timeout">>, Timeout}],
    update_function_configuration(FunctionName, Configuration, Config).

-spec update_function_configuration(FunctionName :: binary(),
    Configuration :: list(tuple()), % JSX json object
    Config       :: aws_config()) -> return_val().
update_function_configuration(FunctionName, Configuration, Config) when is_list(Configuration) ->
    Path = base_path() ++ "functions/" ++ url_parameter(FunctionName) ++ "/configuration",
    Json = filter_undef(Configuration),
    lambda_request(Config, put, Path, Json).

%%------------------------------------------------------------------------------
%% Utility Functions
%%------------------------------------------------------------------------------

from_record(#erlcloud_lambda_code{s3Bucket        = S3Bucket,
                                  s3Key           = S3Key,
                                  s3ObjectVersion = S3ObjectVersion,
                                  zipFile         = ZipFile}) ->
    List = [{<<"S3Bucket">>, S3Bucket},
            {<<"S3Key">>, S3Key},
            {<<"S3ObjectVersion">>, S3ObjectVersion},
            {<<"ZipFile">>, ZipFile}],
    filter_undef(List).

url_parameter(Param) ->
    erlcloud_http:url_encode(binary_to_list(Param)).

base_path() ->
    "/" ++ ?API_VERSION ++ "/".

lambda_request(Config, Method, Path, Body) ->
    lambda_request(Config, Method, Path, Body, []).

lambda_request(Config, Method, Path, Body, QParams) ->
    lambda_request(Config, Method, Path, Body, QParams, []).

lambda_request(Config, Method, Path, Body, QParams, Options) ->
    case erlcloud_aws:update_config(Config) of
        {ok, Config1} ->
            lambda_request_no_update(Config1, Method, Path, Options, Body, QParams);
        {error, Reason} ->
            {error, Reason}
    end.

lambda_request_no_update(Config, Method, Path, Options, Body0, QParams) ->
    ShowRespHeaders = proplists:get_value(show_headers, Options, false),
    RawBody = proplists:get_value(raw_response_body, Options, false),
    Hdrs0 = proplists:delete(show_headers, Options),
    Hdrs = proplists:delete(raw_response_body, Hdrs0),
    Body = encode_body(Body0),
    Headers = headers(Method, Path, Hdrs, Config, Body, QParams),
    QueryString = erlcloud_http:make_query_string(QParams),
    case erlcloud_aws:do_aws_request_form_raw(
        Method, Config#aws_config.lambda_scheme, Config#aws_config.lambda_host,
        Config#aws_config.lambda_port, Path, Body, Headers, QueryString, Config, ShowRespHeaders) of
        {ok, RespHeaders, RespBody} ->
            {ok, RespHeaders, decode_body(RespBody, RawBody)};
        {ok, RespBody} ->
            {ok, decode_body(RespBody, RawBody)};
        E ->
            E
    end.

decode_body(Body, true) ->
    Body;
decode_body(<<>>, _RawBody) ->
    [];
decode_body(BinData, _RawBody) ->
    jsx:decode(BinData, [{return_maps, false}]).

encode_body(Bin) when is_binary(Bin) ->
    Bin;
encode_body(undefined) ->
    <<>>;
encode_body([]) ->
    <<"{}">>;
encode_body(Body) ->
    jsx:encode(Body).

headers(Method, Uri, Hdrs, Config, Body, QParams) ->
    Headers = [{"host", Config#aws_config.lambda_host},
               {"content-type", "application/json"} | Hdrs],
    Region = erlcloud_aws:aws_region_from_host(Config#aws_config.lambda_host),
    erlcloud_aws:sign_v4(Method, erlcloud_http:url_encode_loose(Uri), Config,
                         Headers, Body, Region, "lambda", QParams).
