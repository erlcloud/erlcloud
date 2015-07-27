-module(erlcloud_aws_tests).
-include_lib("eunit/include/eunit.hrl").

request_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun request_default_test/1,
      fun request_prot_host_port_str_test/1,
      fun request_prot_host_port_int_test/1,
      fun get_service_status_test/1]}.

start() ->
    meck:new(erlcloud_httpc),
    meck:expect(erlcloud_httpc, request, fun(_,_,_,_,_,_) -> {ok, {{200, "OK"}, [], ok}} end),
    ok.

stop(_) ->
    meck:unload(erlcloud_httpc).

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

get_service_status_test(_) ->
    StatusJsonS3 = jsx:encode(
        [{<<"archive">>,
            [[{<<"service_name">>,
               <<"Amazon Simple Storage Service (US Standard)">>},
              {<<"summary">>,
               <<"[RESOLVED] Elevated errors for bucket operations in US-STANDARD ">>},
              {<<"date">>,<<"1408497982">>},
              {<<"status">>,1},
              {<<"details">>,<<>>},
              {<<"description">>,
               <<"<div><span class=\"yellowfg\"> 6:46 PM PDT</span>&nbsp;We are investigatin">>},
              {<<"service">>,<<"s3-us-standard">>}]
            ]},
         {<<"current">>,
            [[{<<"service_name">>,
               <<"Amazon Simple Storage Service (US Standard)">>},
              {<<"summary">>,
               <<"[RESOLVED] Elevated errors for bucket operations in US-STANDARD ">>},
              {<<"date">>,<<"1408497982">>},
              {<<"status">>,0},
              {<<"details">>,<<>>},
              {<<"description">>,
               <<"<div><span class=\"yellowfg\"> 6:46 PM PDT</span>&nbsp;We are investigatin">>},
              {<<"service">>,<<"s3-eu-central-1">>}]
            ]}
        ]
    ),
    OKStatusJson = jsx:encode(
        [{<<"archive">>,
            [[{<<"service_name">>,
               <<"Amazon Simple Storage Service (US Standard)">>},
              {<<"summary">>,
               <<"[RESOLVED] Elevated errors for bucket operations in US-STANDARD ">>},
              {<<"date">>,<<"1408497982">>},
              {<<"status">>,1},
              {<<"details">>,<<>>},
              {<<"description">>,
               <<"<div><span class=\"yellowfg\"> 6:46 PM PDT</span>&nbsp;We are investigatin">>},
              {<<"service">>,<<"s3-us-standard">>}]
            ]},
         {<<"current">>,[]}
        ]
    ),

    meck:expect(erlcloud_httpc, request, fun(_,_,_,_,_,_) -> {ok, {{200, "OK"}, [], StatusJsonS3}} end),
    [S3Status] = erlcloud_aws:get_service_status("s3"),
    meck:expect(erlcloud_httpc, request, fun(_,_,_,_,_,_) -> {ok, {{200, "OK"}, [], OKStatusJson}} end),
    OKStatus = erlcloud_aws:get_service_status("sqs"),
    meck:expect(erlcloud_httpc, request, fun(_,_,_,_,_,_) -> {ok, {{200, "OK"}, [], StatusJsonS3}} end),
    OKStatusEC2 = erlcloud_aws:get_service_status("ec2"),
    
    ?debugFmt("OKStatusEC2 = ~p~n", [OKStatusEC2]),
    [?_assertEqual(proplists:get_value(<<"status">>, S3Status), 0),
     ?_assertEqual(proplists:get_value(<<"service">>, S3Status), <<"s3-eu-central-1">>),
     ?_assertEqual(OKStatus, ok),
     ?_assertEqual(OKStatusEC2, ok)
     ].

% ==================
% Internal functions
% ==================

get_url_from_history([{_, {erlcloud_httpc, request, [Url, _, _, _, _, _]}, _}]) ->
    Url.

test_url(ExpScheme, ExpHost, ExpPort, ExpPath, Url) ->
    {ok, {Scheme, _UserInfo, Host, Port, Path, _Query}} = http_uri:parse(Url),
    [?_assertEqual(ExpScheme, Scheme),
     ?_assertEqual(ExpHost, Host),
     ?_assertEqual(ExpPort, Port),
     ?_assertEqual(ExpPath, Path)].
