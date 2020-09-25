-module(erlcloud_test_util).

-export([
    normalize_json/1,
    validate_body/2
]).

-include_lib("eunit/include/eunit.hrl").

normalize_json(Expected) when is_binary(Expected) ->
    jiffy:decode(Expected, [return_maps]);
normalize_json(Term) ->
    normalize_json(erlcloud_json:encode(Term)).

validate_body(Body, Expected) ->
    ?assertEqual(
        normalize_json(Expected),
        normalize_json(Body)
    ).
