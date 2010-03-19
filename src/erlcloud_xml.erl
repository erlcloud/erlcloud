-module(erlcloud_xml).
-export([decode/2, get_bool/2, get_float/2, get_integer/2, get_list/2,
         get_text/1, get_text/2, get_time/2]).

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

get_value(XPath, Type, Node) ->
    case Type of
        text -> get_text(XPath, Node);
        integer -> get_integer(XPath, Node);
        float -> get_float(XPath, Node);
        time -> get_time(XPath, Node);
        list -> get_list(XPath, Node)
    end.

get_float(XPath, Node) ->
    list_to_float(get_text(XPath, Node)).

get_text(#xmlText{value=Value}) -> Value;
get_text(#xmlElement{content=Content}) ->
    lists:flatten([get_text(Node) || Node <- Content]).

get_text(XPath, Doc) -> get_text(XPath, Doc, "").
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
        "true" -> true;
        _ -> false
    end.

get_time(XPath, Doc) ->
    % XXX: Unimplemented.
    get_text(XPath, Doc, none).
