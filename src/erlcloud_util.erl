-module(erlcloud_util).

-export([
    sha_mac/2,
    sha256_mac/2,
    md5/1,
    sha256/1,
    rand_uniform/1,
    is_dns_compliant_name/1,
    query_all/4, query_all/5,
    query_all_token/4,
    make_response/2,
    get_items/2,
    to_string/1,
    encode_list/2,
    encode_object/2,
    encode_object_list/2,
    next_token/2,
    filter_undef/1,
    uri_parse/1,
    http_uri_decode/1,
    http_uri_encode/1,
    proplists_to_map/1, proplists_to_map/2
]).

-define(MAX_ITEMS, 1000).

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 23).
sha_mac(K, S) ->
    crypto:mac(hmac, sha, K, S).
-else.
sha_mac(K, S) ->
    crypto:hmac(sha, K, S).
-endif.
-else.
sha_mac(K, S) ->
    crypto:hmac(sha, K, S).
-endif.


-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 23).
sha256_mac(K, S) ->
    crypto:mac(hmac, sha256, K, S).
-else.
sha256_mac(K, S) ->
    crypto:hmac(sha256, K, S).
-endif.
-else.
sha256_mac(K, S) ->
    crypto:hmac(sha256, K, S).
-endif.

sha256(V) ->
    crypto:hash(sha256, V).

md5(V) ->
    crypto:hash(md5, V).

-ifndef(ERLANG_OTP_VERSION_19).
rand_uniform(N) ->
    random:uniform(N).
-else.
rand_uniform(N) ->
    rand:uniform(N).
-endif.

-spec is_dns_compliant_name(string()) -> boolean().
is_dns_compliant_name(Name) ->
    RegExp = "^(([a-z0-9]|[a-z0-9][a-z0-9\\-]*[a-z0-9])\\.)*([a-z0-9]|[a-z0-9][a-z0-9\\-]*[a-z0-9])$",
    case re:run(Name, RegExp) of
        nomatch ->
            false;
        _ ->
            true
    end.


query_all_token(QueryFun, Config, Action, Params) ->
    query_all_token(QueryFun, Config, Action, Params, undefined, []).

query_all_token(QueryFun, Config, Action, Params, Token, Acc) ->
    NewParams = case Token of
                    undefined ->
                        Params;
                    _ ->
                        [{"NextToken", Token} | Params]
                end,
    case QueryFun(Config, Action, NewParams) of
        {ok, Doc} ->
            NewToken = next_token("/*/*/NextToken", Doc),
            Queried = [Doc | Acc],
            case NewToken of
                ok ->
                    {ok, lists:reverse(Queried)};
                {paged, NewTokenValue} ->
                    query_all_token(QueryFun, Config, Action, Params,
                              NewTokenValue, Queried)
            end;
        Error ->
            Error
    end.


query_all(QueryFun, Config, Action, Params) ->
    query_all(QueryFun, Config, Action, Params, ?MAX_ITEMS, undefined, []).

query_all(QueryFun, Config, Action, Params, MaxItems) ->
    query_all(QueryFun, Config, Action, Params, MaxItems, undefined, []).

query_all(QueryFun, Config, Action, Params, MaxItems, Marker, Acc) ->
    MarkerParams = case Marker of
                    undefined ->
                        Params;
                    _ ->
                        [{"Marker", Marker} | Params]
                end,
    NewParams = [{"MaxItems", MaxItems} | MarkerParams],
    case QueryFun(Config, Action, NewParams) of
        {ok, Doc} ->
            IsTruncated = erlcloud_xml:get_bool("/*/*/IsTruncated", Doc),
            NewMarker = erlcloud_xml:get_text("/*/*/Marker", Doc),
            Queried = [Doc | Acc],
            case IsTruncated of
                true ->
                    query_all(QueryFun, Config, Action, Params,
                              MaxItems, NewMarker, Queried);
                false ->
                    {ok, lists:reverse(Queried)}
            end;
        Error ->
            Error
    end.

-spec encode_list(string(), [term()]) ->
    proplists:proplist().
encode_list(ElementName, Elements) ->
    Numbered = lists:zip(lists:seq(1, length(Elements)), Elements),
    [{ElementName ++ ".member." ++ integer_to_list(N), Element} ||
        {N, Element} <- Numbered].

-spec encode_object(string(), proplists:proplist()) ->
    proplists:proplist().
encode_object(ElementName, ElementParameters) ->
    lists:map(
        fun({Key, Value}) ->
            {ElementName ++ "." ++ Key, Value}
        end,
        ElementParameters
    ).

-spec encode_object_list(string(), [proplists:proplist()]) ->
    proplists:proplist().
encode_object_list(Prefix, ElementParameterList) ->
    lists:flatten(lists:foldl(
        fun(ElementMap, Acc) ->
            [lists:map(
                fun({Key, Value}) ->
                    {Prefix ++ ".member." ++ integer_to_list(length(Acc)+1) ++ "." ++ Key, Value}
                end,
                ElementMap
            ) | Acc]
        end,
        [],
        ElementParameterList
    )).

make_response(Xml, Result) ->
    IsTruncated = erlcloud_xml:get_bool("/*/*/IsTruncated", Xml),
    Marker = erlcloud_xml:get_text("/*/*/Marker", Xml),
    case IsTruncated of
        false ->
            {ok, Result};
        true ->
            {ok, Result, Marker}
    end.


get_items(ItemPath, Xmls) when is_list(Xmls) ->
    lists:append([get_items(ItemPath, Xml) || Xml <- Xmls]);
get_items(ItemPath, Xml) ->
    xmerl_xpath:string(ItemPath, Xml).

-spec to_string(string() | integer()) -> string().
to_string(X) when is_list(X)              -> X;
to_string(X) when is_integer(X) -> integer_to_list(X).

-spec next_token(string(), term()) -> ok | {paged, string()}.
next_token(Path, XML) ->
    case xmerl_xpath:string(Path, XML) of
        [Next] ->
            {paged, erlcloud_xml:get_text(Next)};
        _ ->
            ok
    end.

-spec filter_undef(proplists:proplist()) -> proplists:proplist().
filter_undef(List) ->
    lists:filter(fun({_Name, Value}) -> Value =/= undefined end, List).

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 23).
uri_parse(Uri) ->
    URIMap = uri_string:parse(Uri),
    DefaultScheme = "https",
    DefaultPort = 443,
    Scheme = list_to_atom(maps:get(scheme, URIMap, DefaultScheme)),
    UserInfo = maps:get(userinfo, URIMap, ""),
    Host = maps:get(host, URIMap, ""),
    Port = maps:get(port, URIMap, DefaultPort),
    Path = maps:get(path, URIMap, ""),
    Query = maps:get(query, URIMap, ""),
    {ok, {Scheme, UserInfo, Host, Port, Path, Query}}.
-else.
uri_parse(Uri) ->
    http_uri:parse(Uri).
-endif.
-else.
uri_parse(Uri) ->
    http_uri:parse(Uri).
-endif.

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 23).
http_uri_decode(HexEncodedURI) ->
    [{URI, true}] = uri_string:dissect_query(HexEncodedURI),
    URI.
-else.
http_uri_decode(HexEncodedURI) ->
    http_uri:decode(HexEncodedURI).
-endif.
-else.
http_uri_decode(HexEncodedURI) ->
    http_uri:decode(HexEncodedURI).
-endif.

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 23).
http_uri_encode(URI) ->
    uri_string:compose_query([{URI, true}]).
-else.
http_uri_encode(URI) ->
    http_uri:encode(URI).
-endif.
-else.
http_uri_encode(URI) ->
    http_uri:encode(URI).
-endif.

-spec proplists_to_map(proplists:proplist() | any()) -> map() | any().
proplists_to_map([]) -> [];
proplists_to_map([{}]) -> #{};
proplists_to_map([{_,_} | _] = Proplist) ->
    proplists_to_map(Proplist, #{});
proplists_to_map([Head | _Tail] = List) when is_list(Head) ->
    [proplists_to_map(E) || E <- List];
proplists_to_map(V) -> V.

-spec proplists_to_map(proplists:proplist(), map()) -> map().
proplists_to_map([], Acc) ->
    Acc;
proplists_to_map([{Key, Val} | Tail], Acc) when is_list(Val) ->
    proplists_to_map(Tail, Acc#{Key => proplists_to_map(Val)});
proplists_to_map([{Key, Val} | Tail], Acc) ->
    proplists_to_map(Tail, Acc#{Key => Val}).

