-module(erlcloud_aws_tests).
-include_lib("eunit/include/eunit.hrl").

request_test() ->
    meck:new(httpc, [unstick]),
    meck:expect(httpc, request, fun(_) -> {ok, {{0, 200, 0}, 0, ok}} end),
    ok = erlcloud_aws:aws_request(get, "host", "/", [], "id", "key"),
    [{_, {httpc, request, [Url]}, _}] = meck:history(httpc),
    test_url(https, "host", 443, "/", Url),
    meck:unload(httpc).


request2_test() ->
    meck:new(httpc, [unstick]),
    meck:expect(httpc, request, fun(_) -> {ok, {{0, 200, 0}, 0, ok}} end),
    ok = erlcloud_aws:aws_request(get, "http", "host1", "9999", "/path1", [], "id", "key"),
    [{_, {httpc, request, [Url]}, _}] = meck:history(httpc),
    test_url(http, "host1", 9999, "/path1", Url),
    meck:unload(httpc).

test_url(ExpScheme, ExpHost, ExpPort, ExpPath, Url) ->
    {Scheme, _UserInfo, Host, Port, Path, _Query} = http_uri:parse(Url),
    ?assertEqual(ExpScheme, Scheme),
    ?assertEqual(ExpHost, Host),
    ?assertEqual(ExpPort, Port),
    ?assertEqual(ExpPath, Path).
