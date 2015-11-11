-module(erlcloud_aws_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

request_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun request_default_test/1,
      fun request_retry_test/1,
      fun request_prot_host_port_str_test/1,
      fun request_prot_host_port_int_test/1,
      fun get_service_status_test/1]}.

start() ->
    meck:new(erlcloud_httpc),
    meck:expect(erlcloud_httpc, request, fun(_,_,_,_,_,_) -> {ok, {{200, "OK"}, [], ok}} end),
    ok.

stop(_) ->
    meck:unload(erlcloud_httpc).

config() ->
    #aws_config{access_key_id = "id",
                secret_access_key = "key",
                retry = fun erlcloud_retry:default_retry/1,
                retry_num = 3}.

request_default_test(_) ->
    ok = erlcloud_aws:aws_request(get, "host", "/", [], "id", "key"),
    Url = get_url_from_history(meck:history(erlcloud_httpc)),
    test_url(https, "host", 443, "/", Url).

request_retry_test(_) ->
    Response400 = {ok, {{400, "Bad Request"}, [],
        <<"<ErrorResponse xmlns=\"http://elasticloadbalancing.amazonaws.com/doc/2012-06-01/\">\n"
          "  <Error>\n"
          "    <Type>Sender</Type>\n"
          "    <Code>Throttling</Code>\n"
          "    <Message>Rate exceeded</Message>\n"
          "  </Error>\n"
          "  <RequestId>d4af1389-87fc-11e5-b540-37851aabdff0</RequestId>\n"
          "</ErrorResponse>\n">>}},
    Response500 = {ok, {{500, "Internal Server Error"}, [],
        <<"<?xml version=\"1.0\"?><ErrorResponse xmlns=\"http://queue.amazonaws.com/doc/2012-11-05/\">"
            "<Error>"
                "<Type>Receiver</Type>"
                "<Code>InternalError</Code>"
                "<Message>We encountered an internal error. Please try again.</Message>"
                "<Detail/>"
            "</Error>
          <RequestId>87503803-73c7-5e4d-8619-76be476a7915</RequestId></ErrorResponse>">>}},
    Response200 = {ok, {{200, "OK"}, [], <<"OkBody">>}},
    meck:sequence(erlcloud_httpc, request, 6, [Response400, Response200]),
    Result1 = erlcloud_aws:aws_request(get, "host", "/", [], config()),
    ?_assertEqual(<<"OkBody">>, Result1),

    meck:sequence(erlcloud_httpc, request, 6, [Response400, Response500, Response200]),
    Result2 = erlcloud_aws:aws_request(get, "host", "/", [], config()),
    ?_assertEqual(<<"OkBody">>, Result2),
    
    meck:sequence(erlcloud_httpc, request, 6, [Response400, Response500, Response400, Response200]),
    Result3 = erlcloud_aws:aws_request_xml4(get, "host", "/", [], "any", config()),
    ?_assertMatch({error, {http_error, 400, "Bad Request", _ErrorMsg}}, Result3).

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
              {<<"service">>,<<"s3-eu-central-1">>}],
             [{<<"service_name">>,
               <<"Amazon Simple Storage Service (US Standard)">>},
              {<<"summary">>,
               <<"[RESOLVED] Elevated errors for bucket operations in US-STANDARD ">>},
              {<<"date">>,<<"1408497982">>},
              {<<"status">>,2},
              {<<"details">>,<<>>},
              {<<"description">>,
               <<"<div><span class=\"yellowfg\"> 6:46 PM PDT</span>&nbsp;We are investigatin">>},
              {<<"service">>,<<"ec2-us-west-2">>}]
            ]}
        ]
    ),
    OKStatusEmptyJson = jsx:encode(
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
    [S3Status, EC2Status] = erlcloud_aws:get_service_status(["s3", "ec2", "sns"]),
    meck:expect(erlcloud_httpc, request, fun(_,_,_,_,_,_) -> {ok, {{200, "OK"}, [], OKStatusEmptyJson}} end),
    OKStatusEmpty = erlcloud_aws:get_service_status(["sqs", "sns"]),
    meck:expect(erlcloud_httpc, request, fun(_,_,_,_,_,_) -> {ok, {{200, "OK"}, [], StatusJsonS3}} end),
    OKStatus = erlcloud_aws:get_service_status(["cloudformation", "sns", "vpc"]),
    
    [?_assertEqual(proplists:get_value(<<"status">>, S3Status), 0),
     ?_assertEqual(proplists:get_value(<<"service">>, S3Status), <<"s3-eu-central-1">>),
     ?_assertEqual(proplists:get_value(<<"status">>, EC2Status), 2),
     ?_assertEqual(proplists:get_value(<<"service">>, EC2Status), <<"ec2-us-west-2">>),
     ?_assertEqual(OKStatusEmpty, ok),
     ?_assertEqual(OKStatus, ok)
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
