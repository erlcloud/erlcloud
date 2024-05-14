-module(erlcloud_xml).
-export([decode/2, decode/3, get_bool/2, get_float/2, get_integer/2,
         get_integer/3, get_list/2, get_text/1, get_text/2,
         get_text/3, get_time/2, xml_to_map/1]).

-include_lib("xmerl/include/xmerl.hrl").

decode(Values, Node) ->
    lists:reverse(
      lists:foldl(
        fun ({Name, XPath, Type}, Output) ->
                case get_value(XPath, Type, Node) of
                    undefined -> Output;
                    Value ->
                        [{Name, Value}|Output]
                end
        end, [], Values)
     ).

decode(Values, Node, Record) ->
    lists:foldl(
      fun ({Index, XPath, Type}, Output) ->
              case get_value(XPath, Type, Node) of
                  undefined -> Output;
                  Value -> setelement(Index, Output, Value)
              end
      end, Record, Values
     ).

get_value(XPath, Type, Node) ->
    case Type of
        text -> get_text(XPath, Node);
        optional_text -> get_text(XPath, Node, undefined);
        integer -> get_integer(XPath, Node);
        optional_integer ->
            case get_text(XPath, Node, undefined) of
                undefined -> undefined;
                Text -> list_to_integer(Text)
            end;
        float -> get_float(XPath, Node);
        time -> get_time(XPath, Node);
        list -> get_list(XPath, Node);
        boolean -> get_bool(XPath, Node);
        optional_boolean ->
            case get_text(XPath, Node, undefined) of
                undefined -> undefined;
                "true" -> true;
                _ -> false
            end;
        present -> xmerl_xpath:string(XPath, Node) =/= [];
        xml -> Node;
        Fun when is_function(Fun, 1) ->
            Fun(xmerl_xpath:string(XPath, Node));
        {single, Fun} when is_function(Fun, 1) ->
            case xmerl_xpath:string(XPath, Node) of
                [] -> undefined;
                [SubNode] -> Fun(SubNode)
            end;
        {map, Fun} when is_function(Fun, 1) ->
            lists:map(Fun, xmerl_xpath:string(XPath, Node));
        {optional_map, Fun} when is_function(Fun, 1) ->
            case xmerl_xpath:string(XPath, Node) of
                [] -> undefined;
                List  -> lists:map(Fun, List)
            end;
        {single, List} when is_list(List) ->
            case xmerl_xpath:string(XPath, Node) of
                [] -> undefined;
                [SubNode] -> decode(List, SubNode)
            end;
        {value, Fun} when is_function(Fun, 1) ->
            Fun(get_text(XPath, Node));
        List when is_list(List) ->
            [decode(List, SubNode) || SubNode <- xmerl_xpath:string(XPath, Node)]
    end.

get_float(XPath, Node) ->
    list_to_float(get_text(XPath, Node)).

get_text(#xmlText{value=Value}) -> Value;
get_text(#xmlElement{content=Content}) ->
    lists:flatmap(fun get_text/1, Content).

get_text(XPath, Doc) -> get_text(XPath, Doc, "").
get_text({XPath, AttrName}, Doc, Default) ->
    case xmerl_xpath:string(XPath ++ "/@" ++ AttrName, Doc) of
        [] -> Default;
        [#xmlAttribute{value=Value}|_] -> Value
    end;
get_text(XPath, Doc, Default) ->
    case xmerl_xpath:string(XPath ++ "/text()", Doc) of
        [] -> Default;
        TextNodes ->
            lists:flatten([Node#xmlText.value || Node <- TextNodes])
    end.

get_list(XPath, Doc) ->
    [get_text(Node) || Node <- xmerl_xpath:string(XPath, Doc)].

get_integer(XPath, Doc) -> get_integer(XPath, Doc, 0).
get_integer(XPath, Doc, Default) ->
    case get_text(XPath, Doc) of
        "" -> Default;
        Text -> list_to_integer(Text)
    end.

get_bool(XPath, Doc) ->
    case get_text(XPath, Doc, "false") of
        "1" -> true;
        "true" -> true;
        _ -> false
    end.

get_time(XPath, Doc) ->
    case get_text(XPath, Doc, undefined) of
        undefined -> undefined;
        Time -> parse_time(Time)
    end.

parse_time(String) ->
    case re:run(String, "^(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})(?:\\.\\d+)?Z", [{capture, all_but_first, list}]) of
        {match, [Yr, Mo, Da, H, M, S]} ->
            {{list_to_integer(Yr), list_to_integer(Mo), list_to_integer(Da)},
             {list_to_integer(H), list_to_integer(M), list_to_integer(S)}};
        nomatch ->
            error
    end.

xml_to_map(Doc) ->
    {ok, T} = do_xml_to_map(Doc),
    {ok, maps:from_list([T])}.


do_xml_to_map(#xmlElement{name = Name, content = []}) ->
    Name2 = lists:reverse(atom_to_list(Name)),

    case Name2 of
        %% consider element names with suffix "*Set" to be a list type
        [$t, $e, $S | _] ->
            {ok, {Name, []}};
        _ ->
        %% consider everything else to be an empty string
        %% we may make a statement in the docs that an empty list maybe returned
        %% as an empty binary, to make client aware of possible variations in the
        %% response

            {ok, {Name, <<>>}}
    end;

do_xml_to_map(#xmlElement{name = Name, content = [#xmlText{value = Text}]}) ->
    {ok, {Name, normalize_text(Text)}};
do_xml_to_map(#xmlElement{name = Name, content = Content}) ->
    L = [ V ||
        E <- Content,
        {ok, V} <- [do_xml_to_map(E)]
    ],
    Value =
        case L of
            [{item, _} | _] ->
                %% raise an exception if the list contains anything else than "item"
                [ begin {item, V} = T, V end || T <- L ];
            [{member, _} | _] ->
                [ begin {member, V} = T, V end || T <- L ];
            _ ->
                maps:from_list(L)
        end,

    {ok, {Name, Value}};

%% multiple text items?
%% text mixed with elements?
do_xml_to_map(#xmlText{}) ->
    ignore.


normalize_text("true") -> true;
normalize_text("false") -> false;
normalize_text(Text) -> iolist_to_binary(Text).
