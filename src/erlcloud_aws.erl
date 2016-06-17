-module(erlcloud_aws).

-export([aws_request/5, aws_request/6, aws_request/7, aws_request/8,
         aws_request_xml/5, aws_request_xml/6, aws_request_xml/7, aws_request_xml/8,
         aws_request2/7,
         aws_request_xml2/5, aws_request_xml2/7,
         aws_request4/8,
         aws_request_xml4/6, aws_request_xml4/8,
         aws_region_from_host/1,
         aws_request_form/8,
         param_list/2, default_config/0, update_config/1,
         service_config/3,
         configure/1, format_timestamp/1,
         http_headers_body/1,
         request_to_return/1,
         sign_v4_headers/5,
         sign_v4/8,
         get_service_status/1,
         get_timeout/1,
         profile/0, profile/1, profile/2
]).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

-define(ERLCLOUD_RETRY_TIMEOUT, 10000).

-record(metadata_credentials,
        {access_key_id :: string(),
         secret_access_key :: string(),
         security_token=undefined :: string(),
         expiration_gregorian_seconds :: integer()
        }).

-record(profile_options, {
          session_name :: string(),
          session_secs :: 900..3600,
          external_id :: string()
}).


aws_request_xml(Method, Host, Path, Params, #aws_config{} = Config) ->
    Body = aws_request(Method, Host, Path, Params, Config),
    element(1, xmerl_scan:string(binary_to_list(Body))).
aws_request_xml(Method, Host, Path, Params, AccessKeyID, SecretAccessKey) ->
    Body = aws_request(Method, Host, Path, Params, AccessKeyID, SecretAccessKey),
    element(1, xmerl_scan:string(binary_to_list(Body))).
aws_request_xml(Method, Protocol, Host, Port, Path, Params, #aws_config{} = Config) ->
    Body = aws_request(Method, Protocol, Host, Port, Path, Params, Config),
    element(1, xmerl_scan:string(binary_to_list(Body))).
aws_request_xml(Method, Protocol, Host, Port, Path, Params, AccessKeyID, SecretAccessKey) ->
    Body = aws_request(Method, Protocol, Host, Port, Path, Params, AccessKeyID, SecretAccessKey),
    element(1, xmerl_scan:string(binary_to_list(Body))).

aws_request_xml2(Method, Host, Path, Params, #aws_config{} = Config) ->
    aws_request_xml2(Method, undefined, Host, undefined, Path, Params, Config).
aws_request_xml2(Method, Protocol, Host, Port, Path, Params, #aws_config{} = Config) ->
    case aws_request2(Method, Protocol, Host, Port, Path, Params, Config) of
        {ok, Body} ->
            {ok, element(1, xmerl_scan:string(binary_to_list(Body)))};
        {error, Reason} ->
            {error, Reason}
    end.

aws_request_xml4(Method, Host, Path, Params, Service, #aws_config{} = Config) ->
    aws_request_xml4(Method, undefined, Host, undefined, Path, Params, Service, Config).
aws_request_xml4(Method, Protocol, Host, Port, Path, Params, Service, #aws_config{} = Config) ->
    case aws_request4(Method, Protocol, Host, Port, Path, Params, Service, Config) of
        {ok, Body} ->
            {ok, element(1, xmerl_scan:string(binary_to_list(Body)))};
        {error, Reason} ->
            {error, Reason}
    end.

aws_request(Method, Host, Path, Params, #aws_config{} = Config) ->
    aws_request(Method, undefined, Host, undefined, Path, Params, Config).
aws_request(Method, Host, Path, Params, AccessKeyID, SecretAccessKey) ->
    aws_request(Method, undefined, Host, undefined, Path, Params, AccessKeyID, SecretAccessKey).
aws_request(Method, Protocol, Host, Port, Path, Params, #aws_config{} = Config) ->
    case aws_request2(Method, Protocol, Host, Port, Path, Params, Config) of
        {ok, Body} ->
            Body;
        {error, Reason} ->
            erlang:error({aws_error, Reason})
    end.
aws_request(Method, Protocol, Host, Port, Path, Params, AccessKeyID, SecretAccessKey) ->
    aws_request(Method, Protocol, Host, Port, Path, Params,
                #aws_config{access_key_id = AccessKeyID, secret_access_key = SecretAccessKey}).

%% aws_request2 returns {ok, Body} or {error, Reason} instead of throwing as aws_request does
%% This is the preferred pattern for new APIs
aws_request2(Method, Protocol, Host, Port, Path, Params, Config) ->
    case update_config(Config) of
        {ok, Config1} ->
            aws_request2_no_update(Method, Protocol, Host, Port, Path, Params, Config1);
        {error, Reason} ->
            {error, Reason}
    end.

aws_request2_no_update(Method, Protocol, Host, Port, Path, Params, #aws_config{} = Config) ->
    Timestamp = format_timestamp(erlang:universaltime()),
    QParams = lists:sort(
                [{"Timestamp", Timestamp},
                 {"SignatureVersion", "2"},
                 {"SignatureMethod", "HmacSHA1"},
                 {"AWSAccessKeyId", Config#aws_config.access_key_id}|Params] ++
                    case Config#aws_config.security_token of
                        undefined -> [];
                        Token -> [{"SecurityToken", Token}]
                    end),

    QueryToSign = erlcloud_http:make_query_string(QParams),
    RequestToSign = [string:to_upper(atom_to_list(Method)), $\n,
                     string:to_lower(Host), $\n, Path, $\n, QueryToSign],
    Signature = base64:encode(erlcloud_util:sha_mac(Config#aws_config.secret_access_key, RequestToSign)),

    Query = [QueryToSign, "&Signature=", erlcloud_http:url_encode(Signature)],

    aws_request_form(Method, Protocol, Host, Port, Path, Query, [], Config).

aws_region_from_host(Host) ->
    case string:tokens(Host, ".") of
        %% the aws endpoint can vary depending on the region
        %% we need to account for that:
        %%  us-west-2: s3.us-west-2.amazonaws.com
        %%  cn-north-1 (AWS China): s3.cn-north-1.amazonaws.com.cn
        %% it's assumed that the first element is the aws service (s3, ec2, etc),
        %% the second is the region identifier, the rest is ignored
        %% the exception (of course) is the dynamodb streams which follows a different
        %% format
        ["streams", "dynamodb", Value | _Rest] ->
            Value;
        [_, Value, _, _ | _Rest] ->
            Value;
        _ ->
            "us-east-1"
    end.

aws_request4(Method, Protocol, Host, Port, Path, Params, Service, Config) ->
    case update_config(Config) of
        {ok, Config1} ->
            aws_request4_no_update(Method, Protocol, Host, Port, Path, Params, Service, Config1);
        {error, Reason} ->
            {error, Reason}
    end.

aws_request4_no_update(put, Protocol, Host, Port, Path, Params, Service, #aws_config{} = Config) ->
    Region = aws_region_from_host(Host),
    SignedHeaders = sign_v4(put, Path, Config, [{"host", Host}], Params, Region, Service, []),
    aws_request_form(put, Protocol, Host, Port, Path, Params, SignedHeaders, Config);

aws_request4_no_update(Method, Protocol, Host, Port, Path, Params, Service, #aws_config{} = Config) ->
    Query = erlcloud_http:make_query_string(Params),
    Region = aws_region_from_host(Host),

    SignedHeaders = case Method of
                        post ->
                            sign_v4(Method, Path, Config,
                                    [{"host", Host}], list_to_binary(Query),
                                    Region, Service, []);
                        get ->
                            sign_v4(Method, Path, Config, [{"host", Host}],
                                    <<>>, Region, Service, Params)
                    end,

    aws_request_form(Method, Protocol, Host, Port, Path, Query, SignedHeaders, Config).


-spec aws_request_form(Method :: atom(), Protocol :: undefined | string(), Host :: string(),
                        Port :: undefined | integer() | string(), Path :: string(), Form :: iodata(),
                        Headers :: list(), Config :: aws_config()) -> {ok, binary()} | {error, tuple()}.
aws_request_form(Method, Protocol, Host, Port, Path, Form, Headers, Config) ->
    UProtocol = case Protocol of
        undefined -> "https://";
        _ -> [Protocol, "://"]
    end,

    URL = case Port of
        undefined -> [UProtocol, Host, Path];
        _ -> [UProtocol, Host, $:, port_to_str(Port), Path]
    end,

    %% Note: httpc MUST be used with {timeout, timeout()} option
    %%       Many timeout related failures is observed at prod env
    %%       when library is used in 24/7 manner
    Response =
        case Method of
            get ->
                Req = lists:flatten([URL, $?, Form]),
                erlcloud_httpc:request(
                  Req, get, Headers, <<>>, get_timeout(Config), Config);
            _ ->
                erlcloud_httpc:request(
                  lists:flatten(URL), Method,
                  [{<<"content-type">>, <<"application/x-www-form-urlencoded; charset=utf-8">>} | Headers],
                  if is_binary(Form) -> Form; true -> list_to_binary(Form) end, get_timeout(Config), Config)
        end,

    http_body(Response).

param_list([], _Key) -> [];
param_list(Values, Key) when is_tuple(Key) ->
    Seq = lists:seq(1, size(Key)),
    lists:flatten(
      [[{lists:append([element(J, Key), ".", integer_to_list(I)]),
         element(J, Value)} || J <- Seq] ||
          {I, Value} <- lists:zip(lists:seq(1, length(Values)), Values)]
     );
param_list([[{_, _}|_]|_] = Values, Key) ->
    lists:flatten(
      [[{lists:flatten([Key, $., integer_to_list(I), $., SubKey]),
         value_to_string(Value)} || {SubKey, Value} <- SValues] ||
          {I, SValues} <- lists:zip(lists:seq(1, length(Values)), Values)]
     );
param_list(Values, Key) ->
    [{lists:flatten([Key, $., integer_to_list(I)]), Value} ||
        {I, Value} <- lists:zip(lists:seq(1, length(Values)), Values)].

value_to_string(Integer) when is_integer(Integer) -> integer_to_list(Integer);
value_to_string(Atom) when is_atom(Atom) -> atom_to_list(Atom);
value_to_string(Binary) when is_binary(Binary) -> Binary;
value_to_string(String) when is_list(String) -> String;
value_to_string({{_Yr, _Mo, _Da}, {_Hr, _Min, _Sec}} = Timestamp) -> format_timestamp(Timestamp).

format_timestamp({{Yr, Mo, Da}, {H, M, S}}) ->
    lists:flatten(
      io_lib:format("~4.10.0b-~2.10.0b-~2.10.0bT~2.10.0b:~2.10.0b:~2.10.0bZ",
                    [Yr, Mo, Da, H, M, S])).

default_config() ->
    case get(aws_config) of
        undefined ->
            AccessKeyId = case os:getenv("AWS_ACCESS_KEY_ID") of
                              false -> undefined;
                              AKI -> AKI
                          end,
            SecretAccessKey = case os:getenv("AWS_SECRET_ACCESS_KEY") of
                                  false -> undefined;
                                  SAC -> SAC
                              end,
            #aws_config{access_key_id = AccessKeyId,
                        secret_access_key = SecretAccessKey};
        Config ->
            Config
    end.

-spec update_config(aws_config()) -> {ok, aws_config()} | {error, term()}.
update_config(#aws_config{access_key_id = KeyId} = Config)
  when is_list(KeyId), KeyId /= [] ->
    %% In order to support caching of the aws_config, we could store the expiration_time
    %% and check it here. If it is about to expire (within 5 minutes is what boto uses)
    %% then we should get the new config.
    {ok, Config};
update_config(#aws_config{} = Config) ->
    %% AccessKey is not set. Try to read from role metadata.
    case get_metadata_credentials(Config) of
        {error, Reason} ->
            {error, Reason};
        {ok, Credentials} ->
            {ok, Config#aws_config {
                   access_key_id = Credentials#metadata_credentials.access_key_id,
                   secret_access_key = Credentials#metadata_credentials.secret_access_key,
                   security_token = Credentials#metadata_credentials.security_token}}
    end.


%%%---------------------------------------------------------------------------
-spec service_config( Service :: atom() | string() | binary(),
                      Region :: string() | binary(),
                      Config :: #aws_config{} ) -> #aws_config{}.
%%%---------------------------------------------------------------------------
%% @doc Generate config updated to work with specified AWS service & region
%%
%% This function will generate a new configuration, based on the one
%% provided, and configured to access the specified AWS service in the
%% specified region.  If called for a service without a region based
%% endpoint, the config provided will be returned unaltered.
%%
%% If an invalid service name is provided, then this will throw an error,
%% presuming that this is just a coding error.  This behavior allows the
%% chaining of calls to this interface to allow concise configuraiton of a
%% config for multiple services.
%%
service_config( Service, Region, Config ) when is_atom(Service) ->
    service_config( atom_to_binary(Service, latin1), Region, Config );
service_config( Service, Region, Config ) when is_list(Service) ->
    service_config( list_to_binary(Service), Region, Config );
service_config( Service, Region, Config ) when is_list(Region) ->
    service_config( Service, list_to_binary(Region), Config );
service_config( <<"autoscaling">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ as_host = Host };
service_config( <<"cloudformation">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ cloudformation_host = Host };
service_config( <<"cfn">>, Region, Config ) ->
    service_config( <<"cloudformation">>, Region, Config );
service_config( <<"cloudtrail">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ cloudtrail_host = Host };
service_config( <<"dynamodb">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ ddb_host = Host };
service_config( <<"ddb">>, Region, Config ) ->
    service_config( <<"dynamodb">>, Region, Config );
service_config( <<"streams.dynamodb">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ ddb_streams_host = Host };
service_config( <<"ec2">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ ec2_host = Host };
service_config( <<"elasticloadbalancing">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ elb_host = Host };
service_config( <<"elb">>, Region, Config ) ->
    service_config( <<"elasticloadbalancing">>, Region, Config );
service_config( <<"iam">> = Service, <<"cn-north-1">> = Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ iam_host = Host };
service_config( <<"iam">>, _Region, Config ) -> Config;
service_config( <<"kinesis">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ kinesis_host = Host };
service_config( <<"mechanicalturk">>, _Region, Config ) -> Config;
service_config( <<"rds">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ rds_host = Host };
service_config( <<"s3">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ s3_host = Host };
service_config( <<"sdb">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ sdb_host = Host };
service_config( <<"ses">>, Region, Config ) ->
    Host = service_host( <<"email">>, Region ),
    Config#aws_config{ ses_host = Host };
service_config( <<"sns">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ sns_host = Host };
service_config( <<"sqs">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ sqs_host = Host };
service_config( <<"sts">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ sts_host = Host };
service_config( <<"waf">>, _Region, Config ) -> Config.

    
%%%---------------------------------------------------------------------------
-spec service_host( Service :: binary(),
                    Region :: binary() ) -> string().
%%%---------------------------------------------------------------------------
%% @hidden
%% @doc Host name for the specified AWS service & region
%%
%% This function handles the special and general cases of service host
%% names.
%%
service_host( <<"s3">>, <<"us-east-1">> ) -> "s3-external-1.amazonaws.com";
service_host( <<"s3">>, <<"cn-north-1">> ) -> "s3.cn-north-1.amazonaws.com.cn";
service_host( <<"s3">>, <<"us-gov-west-1">> ) ->
    "s3-fips-us-gov-west-1.amazonaws.com";
service_host( <<"s3">>, Region ) ->
    binary_to_list( <<"s3-", Region/binary, ".amazonaws.com">> );
service_host( <<"sdb">>, <<"us-east-1">> ) -> "sdb.amazonaws.com";
service_host( Service, Region ) when is_binary(Service) ->
    binary_to_list( <<Service/binary, $., Region/binary, ".amazonaws.com">> ).



-spec configure(aws_config()) -> {ok, aws_config()}.

configure(#aws_config{} = Config) ->
    put(aws_config, Config),
    {ok, default_config()}.

-spec get_metadata_credentials(aws_config()) -> {ok, #metadata_credentials{}} | {error, term()}.
get_metadata_credentials(Config) ->
    %% See if we have cached credentials
    case application:get_env(erlcloud, metadata_credentials) of
        {ok, #metadata_credentials{expiration_gregorian_seconds = Expiration} = Credentials} ->
            Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
            %% Get new credentials if these will expire in less than 5 minutes
            case Expiration - Now < 300 of
                true -> get_credentials_from_metadata(Config);
                false -> {ok, Credentials}
            end;
        undefined ->
            get_credentials_from_metadata(Config)
    end.

timestamp_to_gregorian_seconds(Timestamp) ->
    {ok, [Yr, Mo, Da, H, M, S], []} = io_lib:fread("~d-~d-~dT~d:~d:~dZ", binary_to_list(Timestamp)),
    calendar:datetime_to_gregorian_seconds({{Yr, Mo, Da}, {H, M, S}}).

-spec get_credentials_from_metadata(aws_config())
                                   -> {ok, #metadata_credentials{}} | {error, term()}.
get_credentials_from_metadata(Config) ->
    %% TODO this function should retry on errors getting credentials
    %% First get the list of roles
    case http_body(
           erlcloud_httpc:request(
             "http://169.254.169.254/latest/meta-data/iam/security-credentials/",
             get, [], <<>>, get_timeout(Config), Config)) of
        {error, Reason} ->
            {error, Reason};
        {ok, Body} ->
            %% Always use the first role
            [Role | _] = binary:split(Body, <<$\n>>),
            case http_body(
                   erlcloud_httpc:request(
                     "http://169.254.169.254/latest/meta-data/iam/security-credentials/" ++
                         binary_to_list(Role),
                     get, [], <<>>, get_timeout(Config), Config)) of
                {error, Reason} ->
                    {error, Reason};
                {ok, Json} ->
                    Creds = jsx:decode(Json),
                    Record = #metadata_credentials
                        {access_key_id = binary_to_list(proplists:get_value(<<"AccessKeyId">>, Creds)),
                         secret_access_key = binary_to_list(proplists:get_value(<<"SecretAccessKey">>, Creds)),
                         security_token = binary_to_list(proplists:get_value(<<"Token">>, Creds)),
                         expiration_gregorian_seconds = timestamp_to_gregorian_seconds(
                                                          proplists:get_value(<<"Expiration">>, Creds))},
                    application:set_env(erlcloud, metadata_credentials, Record),
                    {ok, Record}
            end
    end.

port_to_str(Port) when is_integer(Port) ->
    integer_to_list(Port);
port_to_str(Port) when is_list(Port) ->
    Port.

-spec http_body({ok, tuple()} | {error, term()})
               -> {ok, binary()} | {error, tuple()}.
%% Extract the body and do error handling on the return of a httpc:request call.
http_body(Return) ->
    case http_headers_body(Return) of
        {ok, {_, Body}} ->
            {ok, Body};
        {error, Reason} ->
            {error, Reason}
    end.

-type headers() :: [{string(), string()}].
-spec http_headers_body({ok, tuple()} | {error, term()})
                       -> {ok, {headers(), binary()}} | {error, tuple()}.
%% Extract the headers and body and do error handling on the return of a httpc:request call.
http_headers_body({ok, {{OKStatus, _StatusLine}, Headers, Body}})
  when OKStatus >= 200, OKStatus =< 299 ->
    {ok, {Headers, Body}};
http_headers_body({ok, {{Status, StatusLine}, _Headers, Body}}) ->
    {error, {http_error, Status, StatusLine, Body}};
http_headers_body({error, Reason}) ->
    {error, {socket_error, Reason}}.

get_timeout(#aws_config{timeout = undefined}) ->
    ?ERLCLOUD_RETRY_TIMEOUT;
get_timeout(#aws_config{timeout = Timeout}) ->
    Timeout.

%% Convert an aws_request record to return value as returned by http_headers_body
request_to_return(#aws_request{response_type = ok,
                               response_headers = Headers,
                               response_body = Body}) ->
    {ok, {[ {string:to_lower(H), V} || {H, V} <- Headers ], Body}};
request_to_return(#aws_request{response_type = error,
                               error_type = httpc,
                               httpc_error_reason = Reason}) ->
    {error, {socket_error, Reason}};
request_to_return(#aws_request{response_type = error,
                               error_type = aws,
                               response_status = Status,
                               response_status_line = StatusLine,
                               response_body = Body}) ->
    {error, {http_error, Status, StatusLine, Body}}.

%% http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html
-spec sign_v4_headers(aws_config(), headers(), binary(), string(), string()) -> headers().
sign_v4_headers(Config, Headers, Payload, Region, Service) ->
    sign_v4(post, "/", Config, Headers, Payload, Region, Service, []).

-spec sign_v4(atom(), list(), aws_config(), headers(), binary(), string(), string(), list()) -> headers().
sign_v4(Method, Uri, Config, Headers, Payload, Region, Service, QueryParams) ->
    Date = iso_8601_basic_time(),
    PayloadHash = hash_encode(Payload),
    Headers1 = [{"x-amz-content-sha256", PayloadHash}, {"x-amz-date", Date} | Headers],
    Headers2 = case Config#aws_config.security_token of
                   undefined -> Headers1;
                   Token -> [{"x-amz-security-token", Token} | Headers1]
               end,
    {Request, SignedHeaders} = canonical_request(Method, Uri, QueryParams, Headers2, PayloadHash),
    CredentialScope = credential_scope(Date, Region, Service),
    ToSign = to_sign(Date, CredentialScope, Request),
    SigningKey = signing_key(Config, Date, Region, Service),
    Signature = base16(erlcloud_util:sha256_mac( SigningKey, ToSign)),
    Authorization = authorization(Config, CredentialScope, SignedHeaders, Signature),
    [{"Authorization", lists:flatten(Authorization)} | Headers2].

iso_8601_basic_time() ->
    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_universal_time(os:timestamp()),
    lists:flatten(io_lib:format(
                    "~4.10.0B~2.10.0B~2.10.0BT~2.10.0B~2.10.0B~2.10.0BZ",
                    [Year, Month, Day, Hour, Min, Sec])).

canonical_request(Method, CanonicalURI, QParams, Headers, PayloadHash) ->
    {CanonicalHeaders, SignedHeaders} = canonical_headers(Headers),
    CanonicalQueryString = canonical_query_string(QParams),
    {[string:to_upper(atom_to_list(Method)), $\n,
      CanonicalURI, $\n,
      CanonicalQueryString, $\n,
      CanonicalHeaders, $\n,
      SignedHeaders, $\n,
      PayloadHash],
     SignedHeaders}.

canonical_headers(Headers) ->
    Normalized = [{string:to_lower(Name), trimall(Value)} || {Name, Value} <- Headers],
    Sorted = lists:keysort(1, Normalized),
    Canonical = [[Name, $:, Value, $\n] || {Name, Value} <- Sorted],
    Signed = string:join([Name || {Name, _} <- Sorted], ";"),
    {Canonical, Signed}.

%% @doc calculate canonical query string out of query params and according to v4 documentation
canonical_query_string([]) ->
    "";
canonical_query_string(Params) ->
    Normalized = [{erlcloud_http:url_encode(Name), erlcloud_http:url_encode(erlcloud_http:value_to_string(Value))} || {Name, Value} <- Params],
    Sorted = lists:keysort(1, Normalized),
    string:join([case Value of
                     [] -> [Key, "="];
                     _ -> [Key, "=", Value]
                 end
                 || {Key, Value} <- Sorted, Value =/= none, Value =/= undefined], "&").

trimall(Value) ->
    %% TODO - remove excess internal whitespace in header values
    re:replace(Value, "(^\\s+)|(\\s+$)", "", [global]).

hash_encode(Data) ->
    Hash = erlcloud_util:sha256( Data),
    base16(Hash).

base16(Data) ->
    io_lib:format("~64.16.0b", [binary:decode_unsigned(Data)]).

credential_scope(Date, Region, Service) ->
    DateOnly = string:left(Date, 8),
    [DateOnly, $/, Region, $/, Service, "/aws4_request"].

to_sign(Date, CredentialScope, Request) ->
    ["AWS4-HMAC-SHA256\n",
     Date, $\n,
     CredentialScope, $\n,
     hash_encode(Request)].

signing_key(Config, Date, Region, Service) ->
    %% TODO cache the signing key so we don't have to recompute for every request
    DateOnly = string:left(Date, 8),
    KDate = erlcloud_util:sha256_mac( "AWS4" ++ Config#aws_config.secret_access_key, DateOnly),
    KRegion = erlcloud_util:sha256_mac( KDate, Region),
    KService = erlcloud_util:sha256_mac( KRegion, Service),
    erlcloud_util:sha256_mac( KService, "aws4_request").

authorization(Config, CredentialScope, SignedHeaders, Signature) ->
    ["AWS4-HMAC-SHA256"
     " Credential=", Config#aws_config.access_key_id, $/, CredentialScope, $,,
     " SignedHeaders=", SignedHeaders, $,,
     " Signature=", Signature].

%% This function fetches http://status.aws.amazon.com/data.json
%% and examine "current" section for on going AWS issues/failures.
%% Example of a return status:
%% [{<<"service_name">>,
%%   <<"Amazon Elastic Compute Cloud (Frankfurt)">>},
%%  {<<"summary">>,<<"[RESOLVED] Internet Connectivity ">>},
%%  {<<"date">>,<<"1436972949">>},
%%  {<<"status">>,1},
%%  {<<"details">>,<<>>},
%%  {<<"description">>,
%%   <<"<div><span class=\"yellowfg\"> 8:22 AM PDT</span>&nbsp;Between 7:55 AM PDT and 8:05 AM PDT we experienced Internet connectivity issues for some instances in the EU-CENTRAL-1 Region. The issue has been resolved and the service is operating normally.</div>">>},
%%  {<<"service">>,<<"ec2-eu-central-1">>}]
%%
%% <<"status">> field values are the following:
%%  0 - service is operating normally;
%%  1 - performance issues;
%%  2 - service disruption.
-spec get_service_status(list(string())) -> ok | list().
get_service_status(ServiceNames) when is_list(ServiceNames) ->
    {ok, Json} = aws_request_form(get, "http", "status.aws.amazon.com", undefined, 
        "/data.json", "", [], default_config()),

    case get_filtered_statuses(ServiceNames, 
            proplists:get_value(<<"current">>, jsx:decode(Json)))
    of
        [] -> ok;
        ReturnStatuses -> ReturnStatuses
    end.

get_filtered_statuses(ServiceNames, Statuses) ->
    lists:filter(
        fun(S)->
            lists:any(
                fun(InputService)->
                    ServiceNameBin = list_to_binary(InputService),
                    ServiceNameLen = byte_size(ServiceNameBin),
                    case proplists:get_value(<<"service">>, S) of
                        <<ServiceNameBin:ServiceNameLen/binary, _/binary>> -> true;
                        _ -> false
                    end
                end,
                ServiceNames)
        end,
        Statuses).


%%%---------------------------------------------------------------------------
-spec profile() -> {ok, aws_config()} | {error, string()}.
%%%---------------------------------------------------------------------------
%% @doc Retrieve a config based on default credentials
%%
%% This function will retrieve the credentials for the <em>default</em>
%% profile, in the same way as the AWS CLI tools, and construct a {@link
%% #aws_config{}. config record} to be used accessing services.
%%
%% @see profile/2
%%
profile() ->
    profile( default, [] ).


%%%---------------------------------------------------------------------------
-spec profile( Name :: atom() ) -> {ok, aws_config()} | {error, string()}.
%%%---------------------------------------------------------------------------
%% @doc Retrieve a config based on named credentials profile
%%
%% This function will retrieve the credentials for the named profile, in the
%% same way as the AWS CLI tools, and construct a {@link
%% #aws_config{}. config record} to be used accessing services.
%%
%% @see profile/2
%%
profile( Name ) ->
    profile( Name, [] ).


-type profile_option() :: {role_session_name, string()}
                          | {role_session_secs, 900..3600}.

%%%---------------------------------------------------------------------------
-spec profile( Name :: atom(), Options :: [profile_option()] ) ->
                     {ok, aws_config()} | {error, string()}.
%%%---------------------------------------------------------------------------
%% @doc Retrieve a config based on named credentials profile
%%
%% This function will read the <code>$HOME/.aws/credentials</code> file used
%% by the AWS CLI tools and construct a {@link #aws_config{}. config record}
%% to be used accessing services for the named profile.  This supports both
%% direct credentials that appear in the profile:
%%
%% <code><pre>
%%  [default]
%%  aws_access_key_id = XXXXXXXXXXXXXXXXXXX2
%%  aws_secret_access_key = yyyyyyyyyyyyyyyyyyyyyyyyyy+yyyy/yyyyyyyy2
%% </pre></code>
%%
%% as well as indirection using <em>source_profile</em>:
%%
%% <code><pre>
%%  [foo]
%%  source_profile = default
%% </pre></code>
%%
%% and finally, will supports the <em>role_arn</em> specification, and will
%% assume the role indicated using the credentials current when interpreting
%% the profile in which they it is declared:
%%
%% <code><pre>
%%  [foo]
%%  role_arn=arn:aws:iam::892406118791:role/centralized-users
%%  source_profile = default
%% </pre></code>
%%
%% When using the the <em>role_arn</em> specification, you may supply the
%% following two options to control the way in which the assume_role request
%% is made via AWS STS service:
%%
%% <ul>
%%  <li><code>'role_session_name'</code>
%%    <p>The name that should be used
%%    for the <code>RoleSessionName</code> parameter.  If this option is not
%%    specified, then it will default to "erlcloud"</p>
%%  </li>
%%  <li><code>'role_duration_secs'</code>
%%    <p>The number of seconds that
%%    should be used for the <code>DurationSeconds</code> parameter.  If
%%    this option is not specified, then it will default to 900 seconds.</p>
%%  </li>
%%  <li><code>'external_id'</code>
%%    <p>The identifier that is used in the <code>ExternalId</code>
%%    parameter.  If this option is not specified, then it will default to
%%    `undefined`, which will work for normal in-account roles, but will
%%    need to be specified for roles in external accounts.</p>
%%  </li>
%% </ul>
%%
profile( Name, Options ) ->
    try
        OptionsRec = profile_options( Options ),
        Profiles = profiles_read(),
        profiles_resolve( Name, Profiles, OptionsRec )
    catch
        throw:Error -> Error
    end.

profile_options( Options ) ->
    SessionName = proplists:get_value( role_session_name, Options, "erlcloud" ),
    SessionDuration = proplists:get_value( role_duration_secs, Options, 900 ),
    ExternalId = proplists:get_value( external_id, Options ),
    #profile_options{ session_name = SessionName, external_id = ExternalId,
                      session_secs = SessionDuration }.

profiles_read() ->
    HOME = profiles_home(),
    Path = HOME ++ "/.aws/credentials",
    case file:read_file( Path ) of
        {ok, Contents} ->
            profiles_parse( Contents );
        Error ->
            error_msg( "could not read credentials file ~s, because ~p",
                   [Path, Error] )
    end.
    
profiles_home() ->
    case os:getenv("HOME") of
        false ->
            error_msg( "HOME environment variable is not set" );
        HOME -> HOME
    end.
        
profiles_parse( Content ) ->
    case eini:parse( Content ) of
        {ok, Profiles} -> Profiles;
        Error ->
            error_msg( "failed to parse credentials, because: ~p", [Error] )
    end.
            
profiles_resolve( Name, Profiles, Options ) ->
    profiles_resolve( Name, Profiles, undefined, Options ).

profiles_resolve( BinName, Profiles, Role, Options ) when is_binary(BinName) ->
    try binary_to_existing_atom( BinName, latin1 ) of
        Name -> profiles_resolve( Name, Profiles, Role, Options )
    catch
        error:badarg -> 
            error_msg( "invalid source_profile reference to ~s", [BinName] )
    end;
profiles_resolve( Name, Profiles, BinRole, Options ) when is_binary(BinRole) ->
    Role = binary_to_list( BinRole ),
    profiles_resolve( Name, Profiles, Role, Options );
profiles_resolve( Name, Profiles, Role, Options ) ->
    case proplists:get_value( Name, Profiles ) of
        undefined ->
            error_msg( "profile ~s does not exist", [Name] );
        Keys ->
            profiles_recurse( Keys, Profiles, Role, Options )
    end.

profiles_recurse( Keys, Profiles, Role, Options ) ->
    case profiles_credentials( Keys ) of
        {ok, Id, Secret} ->
            profiles_assume( Id, Secret, Role, Options );
        {cont, ProfileName, BinRole} ->
            profiles_resolve( ProfileName, Profiles, BinRole, Options )
    end.

profiles_credentials( Keys ) ->
    Names = [aws_access_key_id, aws_secret_access_key, source_profile],
    BinRole = proplists:get_value( role_arn, Keys ),
    case [proplists:get_value( K, Keys ) || K <- Names] of
        [Id, Secret, undefined] when Id =/= undefined, Secret =/= undefined ->
            {ok, binary_to_list(Id), binary_to_list(Secret)};
        [undefined, undefined, BinProfile] when BinProfile =/= undefined ->
            {cont, BinProfile, BinRole}
    end.

profiles_assume( Id, Secret, undefined, _Options ) ->
    Config = #aws_config{ access_key_id = Id, secret_access_key = Secret },
    {ok, Config};
profiles_assume( Id, Secret, Role,
                 #profile_options{ session_name = Name, external_id = ExtId,
                                   session_secs = Duration } ) ->
    Config = #aws_config{ access_key_id = Id, secret_access_key = Secret },
    {AssumedConfig, _Creds} =
        erlcloud_sts:assume_role( Config, Role, Name, Duration, ExtId ),
    {ok, AssumedConfig}.
    

error_msg( Message ) ->
    Error = iolist_to_binary( Message ),
    throw( {error, Error} ).

error_msg( Format, Values ) ->
    Error = iolist_to_binary( io_lib:format( Format, Values ) ),
    throw( {error, Error} ).
