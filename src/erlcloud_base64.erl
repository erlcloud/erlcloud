-module(erlcloud_base64).

%% API
-export([
    encode/1,
    decode/1
]).

-spec encode(binary()) -> binary().
encode(Value) ->
    b64fast:encode64(Value).

-spec decode(binary()) -> binary().
decode(Value) ->
    b64fast:decode64(Value).