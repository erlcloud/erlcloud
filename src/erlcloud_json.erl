-module(erlcloud_json).
-export([decode/2, decode/3]).

-include("erlcloud.hrl").

-type decode_value() :: {atom(), JsonField :: binary(), Type :: atom()}.
-type decode_value_r() :: {pos_integer(), JsonField :: binary(), Type :: atom()}.

-spec decode([decode_value()], proplist()) -> proplist().
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