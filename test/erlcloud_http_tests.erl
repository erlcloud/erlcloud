%% -*- coding: utf-8 -*-
-module(erlcloud_http_tests).
-include_lib("eunit/include/eunit.hrl").

url_encode_test_() ->
  [
    ?_assertMatch("foo%20bar%C2%A1~%21", erlcloud_http:url_encode("foo bar¡~!")),
    ?_assertMatch("%E2%80%98single%20quotes%E2%80%99", erlcloud_http:url_encode("‘single quotes’")),
    ?_assertMatch("%C2%A2", erlcloud_http:url_encode("¢")),
    ?_assertMatch("%F0%A1%81%AF%20%F0%A1%81%B6", erlcloud_http:url_encode("𡁯 𡁶"))
  ].
