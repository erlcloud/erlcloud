-module(erlcloud_json).

-export([
    decode_bin/1, decode/2, decode/3,
    decode_bin_with_maps/1,
    encode/1
]).

-include("erlcloud.hrl").

-type decode_return() :: [{Name :: atom(), Value :: string() | integer()}].
-type decode_value_type() :: optional_string | optional_integer | optional_boolean | {optional_map, fun(([{Key :: binary(), Value :: string() | integer()}]) -> decode_return())}.
-type decode_value() :: {atom(), JsonField :: binary(), Type :: decode_value_type()}.
-type decode_value_r() :: {pos_integer(), JsonField :: binary(), Type :: atom()}.

-spec decode_bin(binary()) -> jsx:json_term().
decode_bin(Binary) ->
    jiffy_to_jsx(jiffy:decode(Binary, [copy_strings])).

-spec decode_bin_with_maps(binary()) -> jiffy:json_value().
decode_bin_with_maps(Binary) ->
    jiffy:decode(Binary, [return_maps, copy_strings]).

-spec encode(jsx:json_term()) -> binary().
encode(Term) ->
    iolist_to_binary(jiffy:encode(jsx_to_jiffy(Term))).

jiffy_to_jsx(List) when is_list(List) ->
    [ jiffy_to_jsx(X) || X <- List ];
jiffy_to_jsx({[]}) ->
    [{}];
jiffy_to_jsx({List}) ->
    [ {K, jiffy_to_jsx(V)} || {K, V} <- List ];
jiffy_to_jsx(V)
  when is_binary(V); is_number(V); is_boolean(V); V =:= null ->
    V.

jsx_to_jiffy([{}]) -> {[]};
jsx_to_jiffy([T|_] = Obj) when is_tuple(T) ->
    {[ {K, jsx_to_jiffy(V)} || {K, V} <- Obj ]};
jsx_to_jiffy(List) when is_list(List) ->
    [ jsx_to_jiffy(V) || V <- List ];
jsx_to_jiffy(V) ->
    V.

-spec decode([decode_value()], proplist()) -> decode_return().
decode(Values, Json) ->
    lists:foldr(
        fun ({Name, JsonField, Type}, Output) ->
            case get_value(JsonField, Type, Json) of
                undefined -> Output;
                Value ->
                    [{Name, Value} | Output]
            end
        end, [], Values).

-spec decode([decode_value_r()], proplist(), tuple()) -> tuple().
decode(Values, Json, Record) ->
    lists:foldr(
        fun ({Index, JsonField, Type}, Output) ->
            case get_value(JsonField, Type, Json) of
                undefined -> Output;
                Value -> setelement(Index, Output, Value)
            end
        end, Record, Values).

get_value(JsonField, Type, Json) ->
    case Type of
        optional_string -> proplists:get_value(JsonField, Json, undefined);
        optional_integer -> proplists:get_value(JsonField, Json, undefined);
        optional_boolean -> proplists:get_value(JsonField, Json, undefined);
        string -> proplists:get_value(JsonField, Json, "");
        integer -> proplists:get_value(JsonField, Json, 0);
        Fun when is_function(Fun, 1) ->
            Fun(proplists:get_value(JsonField, Json));
        {optional_map, Fun} when is_function(Fun, 1) ->
            case proplists:get_value(JsonField, Json, []) of
                [] -> undefined;
                List when is_list(List) -> lists:map(Fun, List)
            end;
       {map, Fun} when is_function(Fun, 1) ->
            lists:map(Fun, proplists:get_value(JsonField, Json, []))
    end.
