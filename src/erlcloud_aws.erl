-module(erlcloud_aws).
-export([aws_request/6, aws_request_xml/6]).

aws_request_xml(Method, Host, Path, Params, AccessKeyID, SecretAccessKey) ->
    Body = aws_request(Method, Host, Path, Params, AccessKeyID, SecretAccessKey),
    element(1, xmerl_scan:string(Body)).

aws_request(Method, Host, Path, Params, AccessKeyID, SecretAccessKey) ->
    Timestamp = format_timestamp(erlang:universaltime()),
    FParams = [{Key, value_to_string(Value)} || {Key, Value} <- Params, Value =/= none, Value =/= undefined],
    QParams = lists:sort([
        {"Timestamp", Timestamp}, {"SignatureVersion", "2"},
        {"SignatureMethod", "HmacSHA1"},
        {"AWSAccessKeyId", AccessKeyID}|FParams]),

    QueryToSign = string:join([[Key, "=", url_encode(Value)] || {Key, Value} <- QParams], "&"),
    RequestToSign = [string:to_upper(atom_to_list(Method)), $\n,
                     Host, $\n, Path, $\n, QueryToSign],
    Signature = base64:encode(crypto:sha_mac(SecretAccessKey, RequestToSign)),
    
    Query = [QueryToSign, "&Signature=", url_encode(Signature)],
    
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
            erlang:error({ec2_error, {http_error, Status, _StatusLine, _Body}});
        {error, Error} ->
            erlang:error({ec2_error, {socket_error, Error}})
    end.

value_to_string(Integer) when is_integer(Integer) -> integer_to_list(Integer);
value_to_string(Atom) when is_atom(Atom) -> atom_to_list(Atom);
value_to_string(Binary) when is_binary(Binary) -> Binary;
value_to_string(String) when is_list(String) -> String;
value_to_string({{_Yr, _Mo, _Da}, {_Hr, _Min, _Sec}} = Timestamp) -> format_timestamp(Timestamp).

format_timestamp({{Yr, Mo, Da}, {H, M, S}}) ->
    lists:flatten(
        io_lib:format("~4.10.0b-~2.10.0b-~2.10.0bT~2.10.0b:~2.10.0b:~2.10.0bZ",
                      [Yr, Mo, Da, H, M, S])).

url_encode(String) ->
    url_encode(String, []).
url_encode([], Accum) ->
    lists:reverse(Accum);
url_encode([Char|String], Accum)
  when Char >= $A, Char =< $Z; 
       Char >= $a, Char =< $z;
       Char >= $0, Char =< $9;
       Char =:= $-; Char =:= $_;
       Char =:= $.; Char =:= $~ ->
    url_encode(String, [Char|Accum]);
url_encode([Char|String], Accum)
  when Char >=0, Char =< 255 ->
    url_encode(String, [hex_char(Char rem 16), hex_char(Char div 16),$%|Accum]);
url_encode(<<>>, Accum) ->
    lists:reverse(Accum);
url_encode(<<Char, String/binary>>, Accum)
  when Char >= $A, Char =< $Z; 
       Char >= $a, Char =< $z;
       Char >= $0, Char =< $9;
       Char =:= $-; Char =:= $_;
       Char =:= $.; Char =:= $~ ->
    url_encode(String, [Char|Accum]);
url_encode(<<Char, String/binary>>, Accum) ->
    url_encode(String, [hex_char(Char rem 16), hex_char(Char div 16),$%|Accum]).

hex_char(C) when C >= 0, C =< 9 -> $0 + C;
hex_char(C) when C >= 10, C =< 15 -> $A + C - 10.
