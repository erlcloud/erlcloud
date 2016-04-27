-module(erlcloud_aws_tests).

-include_lib("erlcloud/include/erlcloud_aws.hrl").
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


profile_default_test_() ->
    {setup, fun profiles_test_setup/0, fun profiles_test_cleanup/1,
     ?_test(
        ?assertMatch(
           {ok, #aws_config{
            access_key_id = "XXXXXXXXXXXXXXXXXXX2",
            secret_access_key = "yyyyyyyyyyyyyyyyyyyyyyyyyy+yyyy/yyyyyyyy2" }},
           erlcloud_aws:profile() )
       )
    }.


profile_direct_test_() ->
    {setup, fun profiles_test_setup/0, fun profiles_test_cleanup/1,
     ?_test(
        ?assertMatch(
           {ok, #aws_config{
            access_key_id = "XXXXXXXXXXXXXXXXXXX1",
            secret_access_key = "yyyyyyyyyyyyyyyyyyyyyyyyyy+yyyy/yyyyyyyy1" }},
           erlcloud_aws:profile( bar ) )
       )
    }.

profile_indirect_test_() ->
    {setup, fun profiles_test_setup/0, fun profiles_test_cleanup/1,
     ?_test(
        ?assertMatch(
           {ok, #aws_config{
            access_key_id = "XXXXXXXXXXXXXXXXXXX1",
            secret_access_key = "yyyyyyyyyyyyyyyyyyyyyyyyyy+yyyy/yyyyyyyy1" }},
           erlcloud_aws:profile( blah ) )
       )
    }.
    
profile_indirect_role_test_() ->
    {setup, fun profiles_assume_setup/0, fun profiles_assume_cleanup/1,
     ?_test(
        ?assertMatch(
           {ok, #aws_config{
            access_key_id = "XXXXXXXXXXXXXXXXXXX3",
            secret_access_key = "yyyyyyyyyyyyyyyyyyyyyyyyyy+yyyy/yyyyyyyy3",
            security_token = "WHOOOOOOOO:12345" }},
           erlcloud_aws:profile( flooga ) )
       )
    }.

profile_indirect_role_options_test_() ->
    Options = [{role_session_name, "wonder"},
               {role_duration_secs, 3600}],
    {setup, fun profiles_assume_setup/0, fun profiles_assume_cleanup/1,
     ?_test(
        ?assertMatch(
           {ok, #aws_config{
            access_key_id = "XXXXXXXXXXXXXXXXXXX3",
            secret_access_key = "yyyyyyyyyyyyyyyyyyyyyyyyyy+yyyy/yyyyyyyy3",
            security_token = "WHOOOOOOOO:54321" }},
           erlcloud_aws:profile( flooga, Options ) )
       )
    }.

profile_indirect_role_options_external_id_test_() ->
    Options = [{role_session_name, "wonder"},
               {role_duration_secs, 3600},
               {external_id, "HOOPDIE"}],
    {setup, fun profiles_assume_setup/0, fun profiles_assume_cleanup/1,
     ?_test(
        ?assertMatch(
           {ok, #aws_config{
            access_key_id = "XXXXXXXXXXXXXXXXXXX3",
            secret_access_key = "yyyyyyyyyyyyyyyyyyyyyyyyyy+yyyy/yyyyyyyy3",
            security_token = "WHOOOOOOOO:54321" }},
           erlcloud_aws:profile( flooga, Options ) )
       )
    }.

profile_undefined_profile_test_() ->
    {setup, fun profiles_test_setup/0, fun profiles_test_cleanup/1,
     ?_test(
        ?assertMatch( {error, _}, erlcloud_aws:profile( what ) )
       )
    }.
    
profile_undefined_indirect_profile_test_() ->
    {setup, fun profiles_test_setup/0, fun profiles_test_cleanup/1,
     ?_test(
        ?assertMatch( {error, _}, erlcloud_aws:profile( whoa ) )
       )
    }.
    

profiles_test_setup() ->
    Profile = <<"
[bar]
aws_access_key_id = XXXXXXXXXXXXXXXXXXX1
aws_secret_access_key = yyyyyyyyyyyyyyyyyyyyyyyyyy+yyyy/yyyyyyyy1

[baz]
aws_access_key_id = XXXXXXXXXXXXXXXXXXX3
aws_secret_access_key = yyyyyyyyyyyyyyyyyyyyyyyyyy+yyyy/yyyyyyyy3

[flooga]
role_arn=arn:aws:iam::892406118791:role/centralized-users
source_profile=baz

[default]
aws_access_key_id = XXXXXXXXXXXXXXXXXXX2
aws_secret_access_key = yyyyyyyyyyyyyyyyyyyyyyyyyy+yyyy/yyyyyyyy2

[blah]
source_profile=bar

[whoa]
source_profile=cowboy
">>,
    meck:new( file, [unstick, passthrough] ),
    meck:expect( file, read_file, fun( _ ) -> {ok, Profile} end ).

profiles_test_cleanup(_) ->
    meck:unload( file ).

profiles_assume_setup() ->
    profiles_test_setup(),
    meck:new( erlcloud_sts ),
    meck:expect( erlcloud_sts, assume_role,
                 fun( Config, _, "erlcloud", 900, _ ) ->
                      {Config#aws_config{ security_token = "WHOOOOOOOO:12345" },
                       []};
                    ( Config, _, "wonder", 3600, _ ) ->
                      {Config#aws_config{ security_token = "WHOOOOOOOO:54321" },
                       []};
                    ( Config, _, "external", 3600, "HOOPDIE" ) ->
                      {Config#aws_config{ security_token = "WHOOOOOOOO:99999" },
                       []}
                 end ).

profiles_assume_cleanup(P) ->
    profiles_test_cleanup(P),
    meck:unload( erlcloud_sts ).


