-module(erlcloud_base64).

%% API
-export([
    encode/1,
    decode/1
]).

-spec encode(binary()) -> binary().
-spec decode(binary()) -> binary().

-ifdef(ENABLE_NIF_DECODERS).

encode(Value) ->
    b64fast:encode64(Value).

decode(Value) ->
    b64fast:decode64(Value).

-else.

encode(Value) ->
    base64:encode(Value).

decode(Value) ->
    base64:decode(Value).

-endif.

