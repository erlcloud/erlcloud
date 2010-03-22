-module(erlcloud_aws).
-export([aws_request/6, aws_request_xml/6,
         param_list/2, default_config/0]).

-include("erlcloud_aws.hrl").

aws_request_xml(Method, Host, Path, Params, AccessKeyID, SecretAccessKey) ->
    Body = aws_request(Method, Host, Path, Params, AccessKeyID, SecretAccessKey),
    element(1, xmerl_scan:string(Body)).

aws_request(Method, Host, Path, Params, AccessKeyID, SecretAccessKey) ->
    Timestamp = format_timestamp(erlang:universaltime()),
    QParams = lists:sort([
        {"Timestamp", Timestamp}, {"SignatureVersion", "2"},
        {"SignatureMethod", "HmacSHA1"},
        {"AWSAccessKeyId", AccessKeyID}|Params]),

    QueryToSign = erlcloud_http:make_query_string(QParams),
    RequestToSign = [string:to_upper(atom_to_list(Method)), $\n,
                     Host, $\n, Path, $\n, QueryToSign],
    Signature = base64:encode(crypto:sha_mac(SecretAccessKey, RequestToSign)),
    
    Query = [QueryToSign, "&Signature=", erlcloud_http:url_encode(Signature)],
    
    URL = ["https://", Host, Path],

    Response = case Method of
        get ->
            http:request(lists:flatten([URL, $?, Query]));
        _ ->
            http:request(Method,
                         {lists:flatten(URL), [], "application/x-www-form-urlencoded",
                          list_to_binary(Query)}, [], [])
    end,
    
    case Response of
        {ok, {{_HTTPVer, 200, _StatusLine}, _Headers, Body}} ->
            Body;
        {ok, {{_HTTPVer, Status, _StatusLine}, _Headers, _Body}} ->
            erlang:error({aws_error, {http_error, Status, _StatusLine, _Body}});
        {error, Error} ->
            erlang:error({aws_error, {socket_error, Error}})
    end.

param_list([], _Key) -> [];
param_list([[{_, _}|_]|_] = Values, Key) ->
    lists:flatten(
        [[{lists:flatten([Key, $., integer_to_list(I), $., SubKey]),
           value_to_string(Value)} || {SubKey, Value} <- SValues] ||
         {I, SValues} <- lists:zip(lists:seq(0, length(Values) - 1), Values)]
    );
param_list(Values, Key) ->
    [{lists:flatten([Key, $., integer_to_list(I)]), Value} ||
     {I, Value} <- lists:zip(lists:seq(0, length(Values) - 1), Values)].

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
            #aws_config{access_key_id=os:getenv("AMAZON_ACCESS_KEY_ID"),
                        secret_access_key=os:getenv("AMAZON_SECRET_ACCESS_KEY")};
        Config ->
            Config
    end.
