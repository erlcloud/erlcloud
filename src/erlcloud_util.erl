-module(erlcloud_util).
-export([sha_mac/2, sha256_mac/2, md5/1, sha256/1,rand_uniform/1,
         is_dns_compliant_name/1,
         query_all/4, query_all/5, query_all_token/4, make_response/2, 
         get_items/2, to_string/1, encode_list/2, next_token/2]).

-define(MAX_ITEMS, 1000).

sha_mac(K, S) ->
    crypto:hmac(sha, K, S).


sha256_mac(K, S) ->
    crypto:hmac(sha256, K, S).

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

encode_list(ElementName, Elements) ->
    encode_list(ElementName, Elements, 1, []).
encode_list(_, [], _, Acc) ->
    Acc;
encode_list(ElementName, [{Key, Val} | Elements], N, Acc) ->
    ElementName2 = ElementName ++ "." ++ Key,
    Acc2 = Acc ++ encode_element(ElementName2, Val),
    encode_list(ElementName, Elements, N, Acc2);
encode_list(ElementName, [Element | Elements], N, Acc) ->
    ElementName2 = ElementName ++ ".member." ++ integer_to_list(N),
    Acc2 = Acc ++ encode_element(ElementName2, Element),
    encode_list(ElementName, Elements, N+1, Acc2).

encode_element(ElementName, Element) when is_boolean(Element) or
                                          is_integer(Element) ->
    [{ElementName, Element}];
encode_element(ElementName, Element) when is_list(Element) ->
    case io_lib:printable_list(Element) of
        true ->
            [{ElementName, Element}];
        false -> 
            encode_list(ElementName, Element)
    end.

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

