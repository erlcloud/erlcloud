-module(erlcloud_aws).
-export([aws_request/5, aws_request/6, aws_request/7, aws_request/8,
         aws_request_xml/5, aws_request_xml/6, aws_request_xml/7, aws_request_xml/8,
         aws_request2/7,
         aws_request_xml2/5, aws_request_xml2/7,
         param_list/2, default_config/0, update_config/1, format_timestamp/1]).

-include_lib("erlcloud/include/erlcloud_aws.hrl").

aws_request_xml(Method, Host, Path, Params, #aws_config{} = Config) ->
    Body = aws_request(Method, Host, Path, Params, Config),
    element(1, xmerl_scan:string(Body)).
aws_request_xml(Method, Host, Path, Params, AccessKeyID, SecretAccessKey) ->
    Body = aws_request(Method, Host, Path, Params, AccessKeyID, SecretAccessKey),
    element(1, xmerl_scan:string(Body)).
aws_request_xml(Method, Protocol, Host, Port, Path, Params, #aws_config{} = Config) ->
    Body = aws_request(Method, Protocol, Host, Port, Path, Params, Config),
    element(1, xmerl_scan:string(Body)).
aws_request_xml(Method, Protocol, Host, Port, Path, Params, AccessKeyID, SecretAccessKey) ->
    Body = aws_request(Method, Protocol, Host, Port, Path, Params, AccessKeyID, SecretAccessKey),
    element(1, xmerl_scan:string(Body)).

aws_request_xml2(Method, Host, Path, Params, #aws_config{} = Config) ->
    aws_request_xml2(Method, undefined, Host, undefined, Path, Params, Config).
aws_request_xml2(Method, Protocol, Host, Port, Path, Params, #aws_config{} = Config) ->
    case aws_request2(Method, Protocol, Host, Port, Path, Params, Config) of
        {ok, Body} ->
            {ok, element(1, xmerl_scan:string(Body))};
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
aws_request2(Method, Protocol, Host, Port, Path, Params, #aws_config{} = Config0) ->
    Config = update_config(Config0),
    Timestamp = format_timestamp(erlang:universaltime()),
    QParams = lists:sort([{"Timestamp", Timestamp},
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
    Signature = base64:encode(crypto:sha_mac(Config#aws_config.secret_access_key, RequestToSign)),

    Query = [QueryToSign, "&Signature=", erlcloud_http:url_encode(Signature)],

    case Protocol of
        undefined -> UProtocol = "https://";
        _ -> UProtocol = [Protocol, "://"]
    end,

    case Port of
        undefined -> URL = [UProtocol, Host, Path];
        _ -> URL = [UProtocol, Host, $:, port_to_str(Port), Path]
    end,

    Response =
        case Method of
            get ->
                Req = lists:flatten([URL, $?, Query]),
                httpc:request(Req);
            _ ->
                httpc:request(Method,
                              {lists:flatten(URL), [], "application/x-www-form-urlencoded",
                               list_to_binary(Query)}, [], [])
        end,

    case Response of
        {ok, {{_HTTPVer, 200, _StatusLine}, _Headers, Body}} ->
            {ok, Body};
        {ok, {{_HTTPVer, Status, StatusLine}, _Headers, Body}} ->
            {error, {http_error, Status, StatusLine, Body}};
        {error, Error} ->
            {error, {socket_error, Error}}
    end.

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
            #aws_config{access_key_id=os:getenv("AWS_ACCESS_KEY_ID"),
                        secret_access_key=os:getenv("AWS_SECRET_ACCESS_KEY")};
        Config ->
            Config
    end.

-spec update_config(aws_config()) -> aws_config().
update_config(#aws_config{access_key_id = KeyId} = Config)
  when is_list(KeyId) ->
    %% In order to support caching of the aws_config, we could store the expiration_time
    %% and check it here. If it is about to expire (within 5 minutes is what boto uses)
    %% then we should get the new config.
    Config;
update_config(#aws_config{} = Config) ->
    %% AccessKey is not set. Try to read from role metadata.
    %% First get the list of roles
    {ok, {{_, 200, _}, _, Body}} =
        httpc:request("http://169.254.169.254/latest/meta-data/iam/security-credentials/"),
    %% Always use the first role
    Role = string:sub_word(Body, 1, $\n),
    {ok, {{_, 200, _}, _, Json}} =
        httpc:request("http://169.254.169.254/latest/meta-data/iam/security-credentials/" ++ Role),
    Credentials = jsx:decode(list_to_binary(Json)),
    Config#aws_config{
      access_key_id = binary_to_list(proplists:get_value(<<"AccessKeyId">>, Credentials)),
      secret_access_key = binary_to_list(proplists:get_value(<<"SecretAccessKey">>, Credentials)),
      security_token = binary_to_list(proplists:get_value(<<"Token">>, Credentials))}.

port_to_str(Port) when is_integer(Port) ->
    integer_to_list(Port);
port_to_str(Port) when is_list(Port) ->
    Port.
