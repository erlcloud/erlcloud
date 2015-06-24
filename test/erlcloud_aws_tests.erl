-module(erlcloud_aws_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlcloud_aws.hrl").

request_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun request_default_test/1,
      fun request_prot_host_port_str_test/1,
      fun request_prot_host_port_int_test/1
     ]}.

lhttpc_options_test_() ->
    {foreach,
     fun start_lhttpc/0,
     fun stop_lhttpc/1,
     [fun lhttpc_options_are_empty_list_by_default/1,
      fun lhttpc_options_get_passed_down/1 
     ]}.

start() ->
    meck:new(erlcloud_httpc),
    meck:expect(erlcloud_httpc, request, fun(_,_,_,_,_,_) -> {ok, {{200, "OK"}, [], ok}} end),
    ok.

stop(_) ->
    meck:unload(erlcloud_httpc).


start_lhttpc() ->
    meck:new(lhttpc),
    meck:expect(lhttpc, request, fun(_,_,_,_,_,_) -> {ok, {{200, "OK"}, [], ok}} end),
    ok.

stop_lhttpc(_) ->
    meck:unload(lhttpc).

request_default_test(_) ->
    ok = erlcloud_aws:aws_request(get, "host", "/", [], "id", "key"),
    Url = get_url_from_history(meck:history(erlcloud_httpc)),
    test_url(https, "host", 443, "/", Url).

request_prot_host_port_str_test(_) ->
    ok = erlcloud_aws:aws_request(get, "http", "host1", "9999", "/path1", [], "id", "key"),
    Url = get_url_from_history(meck:history(erlcloud_httpc)),
    test_url(http, "host1", 9999, "/path1", Url).

request_prot_host_port_int_test(_) ->
    ok = erlcloud_aws:aws_request(get, "http", "host1", 9999, "/path1", [], "id", "key"),
    Url = get_url_from_history(meck:history(erlcloud_httpc)),
    test_url(http, "host1", 9999, "/path1", Url).


lhttpc_options_are_empty_list_by_default(_) ->
    ok = erlcloud_aws:aws_request(get, "http", "host1", 9999, "/path1", [], "id", "key"),
    LHTTPCOptions = get_config_from_history(meck:history(lhttpc)),
    ?_assertEqual([], LHTTPCOptions).

lhttpc_options_get_passed_down(_) ->
    Config = #aws_config{lhttpc_options = [{foo, bar}], 
			 access_key_id="id",
			 secret_access_key="key"},
    ok = erlcloud_aws:aws_request(get, "http", "example.com", 8008, "/", [], Config),
    LHTTPCOptions = get_config_from_history(meck:history(lhttpc)),
    ?_assertEqual([{foo, bar}], LHTTPCOptions).

% ==================
% Internal functions
% ==================

get_url_from_history([{_, {erlcloud_httpc, request, [Url, _, _, _, _, _]}, _}]) ->
    Url.
get_config_from_history([{_, {lhttpc, request, [_, _, _, _, _, Config]}, _}]) ->
    Config.

test_url(ExpScheme, ExpHost, ExpPort, ExpPath, Url) ->
    {ok, {Scheme, _UserInfo, Host, Port, Path, _Query}} = http_uri:parse(Url),
    [?_assertEqual(ExpScheme, Scheme),
     ?_assertEqual(ExpHost, Host),
     ?_assertEqual(ExpPort, Port),
     ?_assertEqual(ExpPath, Path)].
