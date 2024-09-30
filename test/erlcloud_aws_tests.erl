-module(erlcloud_aws_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

request_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun test_request_default/1,
      fun test_request_retry/1,
      fun test_request_prot_host_port_str/1,
      fun test_request_prot_host_port_int/1,
      fun test_get_service_status/1,
      fun test_auto_config_with_env/1,
      fun validate_map_params/0
    ]}.

start() ->
    meck:new(erlcloud_httpc),
    meck:expect(erlcloud_httpc, request, fun(_,_,_,_,_,_) -> {ok, {{200, "OK"}, [], <<"OkBody">>}} end),
    ok.

stop(_) ->
    meck:unload(erlcloud_httpc).

config() ->
    #aws_config{access_key_id = "id",
                secret_access_key = "key",
                retry = fun erlcloud_retry:default_retry/1,
                retry_num = 3}.

test_request_default(_) ->
    <<"OkBody">> = erlcloud_aws:aws_request(get, "host", "/", [], "id", "key"),
    Url = get_url_from_history(meck:history(erlcloud_httpc)),
    test_url(https, "host", 443, "/", Url).

test_request_retry(_) ->
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
    Response429 = {ok, {{429, "Too Many Requests"}, [],
        <<"{"
             "\"Reason\":\"ReservedFunctionConcurrentInvocationLimitExceeded\","
             "\"Type\":\"User\","
             "\"message\":\"Rate Exceeded.\""
          "}">>}},
    Response200 = {ok, {{200, "OK"}, [], <<"OkBody">>}},
    MeckAndRequest =
        fun
          ({ResponseSeq, xml4}) ->
              meck:sequence(erlcloud_httpc, request, 6, ResponseSeq),
              erlcloud_aws:aws_request_xml4(get, "host", "/", [], "any", config());
          (ResponseSeq) ->
              meck:sequence(erlcloud_httpc, request, 6, ResponseSeq),
              <<"OkBody">> = erlcloud_aws:aws_request(get, "host", "/", [], config())
        end,
    [?_assertMatch(<<"OkBody">>, MeckAndRequest([Response400, Response200])),
     ?_assertMatch(<<"OkBody">>, MeckAndRequest([Response400, Response500, Response200])),
     ?_assertMatch(<<"OkBody">>, MeckAndRequest([Response429, Response200])),
     ?_assertMatch({error, {http_error, 400, "Bad Request", _ErrorMsg}},
                   MeckAndRequest({[Response400, Response500, Response400, Response200], xml4}))
     ].


test_request_prot_host_port_str(_) ->
    <<"OkBody">> = erlcloud_aws:aws_request(get, "http", "host1", "9999", "/path1", [], "id", "key"),
    Url = get_url_from_history(meck:history(erlcloud_httpc)),
    test_url(http, "host1", 9999, "/path1", Url).

test_request_prot_host_port_int(_) ->
    <<"OkBody">> = erlcloud_aws:aws_request(get, "http", "host1", 9999, "/path1", [], "id", "key"),
    Url = get_url_from_history(meck:history(erlcloud_httpc)),
    test_url(http, "host1", 9999, "/path1", Url).

test_get_service_status(_) ->
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


test_auto_config_with_env(_) ->
    % Note: meck do not support os module
    X_AWS_ACCESS  = os:getenv("AWS_ACCESS_KEY_ID"),
    X_AWS_SECRET  = os:getenv("AWS_SECRET_ACCESS_KEY"),
    X_AWS_SESSION = os:getenv("AWS_SESSION_TOKEN"),

    os:putenv("AWS_ACCESS_KEY_ID", "access"),
    os:putenv("AWS_SECRET_ACCESS_KEY", "secret"),
    os:putenv("AWS_SESSION_TOKEN", "token"),
    Config = erlcloud_aws:auto_config(),
    os_rstenv("AWS_ACCESS_KEY_ID", X_AWS_ACCESS),
    os_rstenv("AWS_SECRET_ACCESS_KEY", X_AWS_SECRET),
    os_rstenv("AWS_SESSION_TOKEN", X_AWS_SESSION),

    [?_assertMatch("access", element(#aws_config.access_key_id, element(2, Config))),
     ?_assertMatch("secret", element(#aws_config.secret_access_key, element(2, Config))),
     ?_assertMatch("token", element(#aws_config.security_token, element(2, Config)))
    ].

os_rstenv(Var, false) ->
  os:unsetenv(Var);
os_rstenv(Var, Value) ->
  os:putenv(Var, Value).

% ==================
% Internal functions
% ==================

get_url_from_history([{_, {erlcloud_httpc, request, [Url, _, _, _, _, _]}, _}]) ->
    Url.

test_url(ExpScheme, ExpHost, ExpPort, ExpPath, Url) ->
    {ok, {Scheme, _UserInfo, Host, Port, Path, _Query}} = erlcloud_util:uri_parse(Url),
    [?_assertEqual(ExpScheme, Scheme),
     ?_assertEqual(ExpHost, Host),
     ?_assertEqual(ExpPort, Port),
     ?_assertEqual(ExpPath, Path)].


-define(DEFAULT_ACCESS_ID, "XXXXXXXXXXXXXXXXXXX2").
-define(DEFAULT_ACCESS_KEY, "yyyyyyyyyyyyyyyyyyyyyyyyyy+yyyy/yyyyyyyy2").
-define(BAR_ACCESS_ID, "XXXXXXXXXXXXXXXXXXX1").
-define(BAR_ACCESS_KEY, "yyyyyyyyyyyyyyyyyyyyyyyyyy+yyyy/yyyyyyyy1").
-define(BAZ_ACCESS_ID, "XXXXXXXXXXXXXXXXXXX3").
-define(BAZ_ACCESS_KEY, "yyyyyyyyyyyyyyyyyyyyyyyyyy+yyyy/yyyyyyyy3").


profile_default_test_() ->
    {setup, fun profiles_test_setup/0, fun profiles_test_cleanup/1,
     ?_test(
        ?assertMatch(
           {ok, #aws_config{
            access_key_id = ?DEFAULT_ACCESS_ID,
            secret_access_key = ?DEFAULT_ACCESS_KEY }},
           erlcloud_aws:profile() )
       )
    }.


profile_direct_test_() ->
    {setup, fun profiles_test_setup/0, fun profiles_test_cleanup/1,
     ?_test(
        ?assertMatch(
           {ok, #aws_config{
            access_key_id = ?BAR_ACCESS_ID,
            secret_access_key = ?BAR_ACCESS_KEY }},
           erlcloud_aws:profile( bar ) )
       )
    }.

profile_indirect_test_() ->
    {setup, fun profiles_test_setup/0, fun profiles_test_cleanup/1,
     ?_test(
        ?assertMatch(
           {ok, #aws_config{
            access_key_id = ?BAR_ACCESS_ID,
            secret_access_key = ?BAR_ACCESS_KEY }},
           erlcloud_aws:profile( blah ) )
       )
    }.

profile_indirect_role_test_() ->
    {setup, fun profiles_assume_setup/0, fun profiles_assume_cleanup/1,
     ?_test(
        ?assertMatch(
           {ok, #aws_config{
            access_key_id = ?BAZ_ACCESS_ID,
            secret_access_key = ?BAZ_ACCESS_KEY,
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
            access_key_id = ?BAZ_ACCESS_ID,
            secret_access_key = ?BAZ_ACCESS_KEY,
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
            access_key_id = ?BAZ_ACCESS_ID,
            secret_access_key = ?BAZ_ACCESS_KEY,
            security_token = "WHOOOOOOOO:54321" }},
           erlcloud_aws:profile( flooga, Options ) )
       )
    }.

profile_external_id_role_test_() ->
    {setup, fun profiles_assume_setup/0, fun profiles_assume_cleanup/1,
     ?_test(
        ?assertMatch(
           {ok, #aws_config{
            access_key_id = ?BAZ_ACCESS_ID,
            secret_access_key = ?BAZ_ACCESS_KEY,
            security_token = "WHOOOOOOOO:12345:12349321" }},
           erlcloud_aws:profile( eid ) )
       )
    }.

profile_double_external_id_role_test_() ->
    {setup, fun profiles_assume_setup/0, fun profiles_assume_cleanup/1,
     ?_test(
        ?assertMatch(
           {ok, #aws_config{
            access_key_id = ?BAZ_ACCESS_ID,
            secret_access_key = ?BAZ_ACCESS_KEY,
            security_token = "WHOOOOOOOO:12345:fubar" }},
           erlcloud_aws:profile( eidrecurse ) )
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
[eid]
role_arn=arn:aws:iam::100406118791:role/centralized-users
external_id=12349321
source_profile=baz
[eidrecurse]
role_arn=arn:aws:iam::000406118791:role/centralized-users
external_id=fubar
source_profile=eid
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
                 fun( Config, _, _, 900, undefined ) ->
                      {Config#aws_config{ security_token = "WHOOOOOOOO:12345" },
                       []};
                    ( Config, _, "wonder", 3600, _ ) ->
                      {Config#aws_config{ security_token = "WHOOOOOOOO:54321" },
                       []};
                    ( Config, _, "external", 3600, "HOOPDIE" ) ->
                      {Config#aws_config{ security_token = "WHOOOOOOOO:99999" },
                       []};
                    ( Config, _, _, _, ExtId ) ->
                         Token = "WHOOOOOOOO:12345:" ++ ExtId,
                         {Config#aws_config{ security_token = Token }, []}
                 end ).

profiles_assume_cleanup(P) ->
    profiles_test_cleanup(P),
    meck:unload( erlcloud_sts ).


%% Tests that if application environment variable "erlcloud.aws_config" contains
%% overrides for default `#aws_config{}', those overrides are indeed applied.
default_config_override_test() ->
    %% in case if any of previous tests have used `erlcloud_aws:configure/1'
    _ = erase(aws_config),

    %% everything still works, when the overrides are not provided
    ?assertEqual(undefined, application:get_env(erlcloud, aws_config)),
    ?assert(
        is_record(erlcloud_aws:default_config(), aws_config)
    ),

    Keys = record_info(fields, aws_config),
    KeysLen = length(Keys),
    Nums = lists:seq(1, KeysLen),

    %% reshuffling keys just to make sure that order of arguments
    %% doesn't matter
    {LeftKVs, RightKVs} = lists:split(
        erlang:round(KeysLen/2), lists:zip(Keys, Nums)
    ),

    %% assigning each record's field an unique number
    ok = application:set_env(erlcloud, aws_config, RightKVs ++ LeftKVs),

    %% checking that each field was changed and has appropriate number
    ?assertEqual(
        [aws_config | Nums], tuple_to_list(erlcloud_aws:default_config())
    ).


default_config_region_sunny_test() ->
    Region = <<"ca-central-1">>,
    ?assertMatch(#aws_config{},
        erlcloud_aws:default_config_region(#aws_config{}, Region)).

service_config_autoscaling_test() ->
    Service = <<"autoscaling">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>,
               <<"cn-north-1">>, <<"cn-northwest-1">>],
    Expected = ["autoscaling.us-east-1.amazonaws.com",
                "autoscaling.us-west-1.amazonaws.com",
                "autoscaling.us-west-2.amazonaws.com",
                "autoscaling.eu-west-1.amazonaws.com",
                "autoscaling.eu-central-1.amazonaws.com",
                "autoscaling.ap-northeast-1.amazonaws.com",
                "autoscaling.ap-northeast-2.amazonaws.com",
                "autoscaling.ap-southeast-1.amazonaws.com",
                "autoscaling.ap-southeast-2.amazonaws.com",
                "autoscaling.sa-east-1.amazonaws.com",
                "autoscaling.cn-north-1.amazonaws.com.cn",
                "autoscaling.cn-northwest-1.amazonaws.com.cn"],
    ?assertEqual( Expected,
                  [H || #aws_config{ as_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_cloudformation_test() ->
    Service = <<"cloudformation">>,
    ServiceAlt = <<"cfn">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>,
               <<"cn-north-1">>, <<"cn-northwest-1">>],
    Expected = ["cloudformation.us-east-1.amazonaws.com",
                "cloudformation.us-west-1.amazonaws.com",
                "cloudformation.us-west-2.amazonaws.com",
                "cloudformation.eu-west-1.amazonaws.com",
                "cloudformation.eu-central-1.amazonaws.com",
                "cloudformation.ap-northeast-1.amazonaws.com",
                "cloudformation.ap-northeast-2.amazonaws.com",
                "cloudformation.ap-southeast-1.amazonaws.com",
                "cloudformation.ap-southeast-2.amazonaws.com",
                "cloudformation.sa-east-1.amazonaws.com",
                "cloudformation.cn-north-1.amazonaws.com.cn",
                "cloudformation.cn-northwest-1.amazonaws.com.cn"],
    ?assertEqual( Expected,
                  [H || #aws_config{ cloudformation_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ),
    ?assertEqual( Expected,
                  [H || #aws_config{ cloudformation_host = H } <-
                            [erlcloud_aws:service_config(
                               ServiceAlt, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_cloudtrail_test() ->
    Service = <<"cloudtrail">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>,
               <<"cn-north-1">>, <<"cn-northwest-1">>],
    Expected = ["cloudtrail.us-east-1.amazonaws.com",
                "cloudtrail.us-west-1.amazonaws.com",
                "cloudtrail.us-west-2.amazonaws.com",
                "cloudtrail.eu-west-1.amazonaws.com",
                "cloudtrail.eu-central-1.amazonaws.com",
                "cloudtrail.ap-northeast-1.amazonaws.com",
                "cloudtrail.ap-northeast-2.amazonaws.com",
                "cloudtrail.ap-southeast-1.amazonaws.com",
                "cloudtrail.ap-southeast-2.amazonaws.com",
                "cloudtrail.sa-east-1.amazonaws.com",
                "cloudtrail.cn-north-1.amazonaws.com.cn",
                "cloudtrail.cn-northwest-1.amazonaws.com.cn"],
    ?assertEqual( Expected,
                  [H || #aws_config{ cloudtrail_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_dynamodb_test() ->
    Service = <<"dynamodb">>,
    ServiceAlt = <<"ddb">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>,
               <<"cn-north-1">>, <<"cn-northwest-1">>],
    Expected = ["dynamodb.us-east-1.amazonaws.com",
                "dynamodb.us-west-1.amazonaws.com",
                "dynamodb.us-west-2.amazonaws.com",
                "dynamodb.eu-west-1.amazonaws.com",
                "dynamodb.eu-central-1.amazonaws.com",
                "dynamodb.ap-northeast-1.amazonaws.com",
                "dynamodb.ap-northeast-2.amazonaws.com",
                "dynamodb.ap-southeast-1.amazonaws.com",
                "dynamodb.ap-southeast-2.amazonaws.com",
                "dynamodb.sa-east-1.amazonaws.com",
                "dynamodb.cn-north-1.amazonaws.com.cn",
                "dynamodb.cn-northwest-1.amazonaws.com.cn"],
    ?assertEqual( Expected,
                  [H || #aws_config{ ddb_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ),
    ?assertEqual( Expected,
                  [H || #aws_config{ ddb_host = H } <-
                            [erlcloud_aws:service_config(
                               ServiceAlt, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_dynamodb_streams_test() ->
    Service = <<"streams.dynamodb">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>,
               <<"cn-north-1">>, <<"cn-northwest-1">>],
    Expected = ["streams.dynamodb.us-east-1.amazonaws.com",
                "streams.dynamodb.us-west-1.amazonaws.com",
                "streams.dynamodb.us-west-2.amazonaws.com",
                "streams.dynamodb.eu-west-1.amazonaws.com",
                "streams.dynamodb.eu-central-1.amazonaws.com",
                "streams.dynamodb.ap-northeast-1.amazonaws.com",
                "streams.dynamodb.ap-northeast-2.amazonaws.com",
                "streams.dynamodb.ap-southeast-1.amazonaws.com",
                "streams.dynamodb.ap-southeast-2.amazonaws.com",
                "streams.dynamodb.sa-east-1.amazonaws.com",
                "streams.dynamodb.cn-north-1.amazonaws.com.cn",
                "streams.dynamodb.cn-northwest-1.amazonaws.com.cn"],
    ?assertEqual( Expected,
                  [H || #aws_config{ ddb_streams_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_ec2_test() ->
    Service = <<"ec2">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>,
               <<"cn-north-1">>, <<"cn-northwest-1">>],
    Expected = ["ec2.us-east-1.amazonaws.com",
                "ec2.us-west-1.amazonaws.com",
                "ec2.us-west-2.amazonaws.com",
                "ec2.eu-west-1.amazonaws.com",
                "ec2.eu-central-1.amazonaws.com",
                "ec2.ap-northeast-1.amazonaws.com",
                "ec2.ap-northeast-2.amazonaws.com",
                "ec2.ap-southeast-1.amazonaws.com",
                "ec2.ap-southeast-2.amazonaws.com",
                "ec2.sa-east-1.amazonaws.com",
                "ec2.cn-north-1.amazonaws.com.cn",
                "ec2.cn-northwest-1.amazonaws.com.cn"],
    ?assertEqual( Expected,
                  [H || #aws_config{ ec2_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_elasticloadbalancing_test() ->
    Service = <<"elasticloadbalancing">>,
    ServiceAlt = <<"elb">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>,
               <<"cn-north-1">>, <<"cn-northwest-1">>],
    Expected = ["elasticloadbalancing.us-east-1.amazonaws.com",
                "elasticloadbalancing.us-west-1.amazonaws.com",
                "elasticloadbalancing.us-west-2.amazonaws.com",
                "elasticloadbalancing.eu-west-1.amazonaws.com",
                "elasticloadbalancing.eu-central-1.amazonaws.com",
                "elasticloadbalancing.ap-northeast-1.amazonaws.com",
                "elasticloadbalancing.ap-northeast-2.amazonaws.com",
                "elasticloadbalancing.ap-southeast-1.amazonaws.com",
                "elasticloadbalancing.ap-southeast-2.amazonaws.com",
                "elasticloadbalancing.sa-east-1.amazonaws.com",
                "elasticloadbalancing.cn-north-1.amazonaws.com.cn",
                "elasticloadbalancing.cn-northwest-1.amazonaws.com.cn"],
    ?assertEqual( Expected,
                  [H || #aws_config{ elb_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ),
    ?assertEqual( Expected,
                  [H || #aws_config{ elb_host = H } <-
                            [erlcloud_aws:service_config(
                               ServiceAlt, Region, #aws_config{} )
                             || Region <- Regions]] ).

% current as of 2017-01-27, list from
% http://docs.aws.amazon.com/general/latest/gr/rande.html#emr_region
service_config_elasticmapreduce_test() ->
    Service = <<"elasticmapreduce">>,
    ServiceAlt = <<"emr">>,
    Regions = [<<"us-east-1">>, <<"us-east-2">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"ca-central-1">>, <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>,
               <<"cn-north-1">>, <<"cn-northwest-1">>],
    Expected = ["elasticmapreduce.us-east-1.amazonaws.com",
                "elasticmapreduce.us-east-2.amazonaws.com",
                "elasticmapreduce.us-west-1.amazonaws.com",
                "elasticmapreduce.us-west-2.amazonaws.com",
                "elasticmapreduce.ca-central-1.amazonaws.com",
                "elasticmapreduce.eu-west-1.amazonaws.com",
                "elasticmapreduce.eu-central-1.amazonaws.com",
                "elasticmapreduce.ap-northeast-1.amazonaws.com",
                "elasticmapreduce.ap-northeast-2.amazonaws.com",
                "elasticmapreduce.ap-southeast-1.amazonaws.com",
                "elasticmapreduce.ap-southeast-2.amazonaws.com",
                "elasticmapreduce.sa-east-1.amazonaws.com",
                "elasticmapreduce.cn-north-1.amazonaws.com.cn",
                "elasticmapreduce.cn-northwest-1.amazonaws.com.cn"],
    ?assertEqual( Expected,
                  [H || #aws_config{ emr_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ),
    ?assertEqual( Expected,
                  [H || #aws_config{ emr_host = H } <-
                            [erlcloud_aws:service_config(
                               ServiceAlt, Region, #aws_config{} )
                             || Region <- Regions]] ).


service_config_iam_test() ->
    Service = <<"iam">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>],
    Expected = lists:duplicate( length(Regions), "iam.amazonaws.com" ),
    ?assertEqual( Expected,
                  [H || #aws_config{ iam_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_china_iam_test() ->
    Service = <<"iam">>,
    Regions = [<<"cn-north-1">>, <<"cn-northwest-1">>],
    Expected = lists:duplicate( length(Regions), "iam.amazonaws.com.cn" ),
    ?assertEqual( Expected,
                  [H || #aws_config{ iam_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_kinesis_test() ->
    Service = <<"kinesis">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>,
               <<"cn-north-1">>, <<"cn-northwest-1">>],
    Expected = ["kinesis.us-east-1.amazonaws.com",
                "kinesis.us-west-1.amazonaws.com",
                "kinesis.us-west-2.amazonaws.com",
                "kinesis.eu-west-1.amazonaws.com",
                "kinesis.eu-central-1.amazonaws.com",
                "kinesis.ap-northeast-1.amazonaws.com",
                "kinesis.ap-northeast-2.amazonaws.com",
                "kinesis.ap-southeast-1.amazonaws.com",
                "kinesis.ap-southeast-2.amazonaws.com",
                "kinesis.sa-east-1.amazonaws.com",
                "kinesis.cn-north-1.amazonaws.com.cn",
                "kinesis.cn-northwest-1.amazonaws.com.cn"],
    ?assertEqual( Expected,
                  [H || #aws_config{ kinesis_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_mechanicalturk_test() ->
    Service = <<"mechanicalturk">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>],
    Expected = lists:duplicate( length(Regions),
                                "mechanicalturk.amazonaws.com" ),
    ?assertEqual( Expected,
                  [H || #aws_config{ mturk_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_rds_test() ->
    Service = <<"rds">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>,
               <<"cn-north-1">>, <<"cn-northwest-1">>],
    Expected = ["rds.us-east-1.amazonaws.com",
                "rds.us-west-1.amazonaws.com",
                "rds.us-west-2.amazonaws.com",
                "rds.eu-west-1.amazonaws.com",
                "rds.eu-central-1.amazonaws.com",
                "rds.ap-northeast-1.amazonaws.com",
                "rds.ap-northeast-2.amazonaws.com",
                "rds.ap-southeast-1.amazonaws.com",
                "rds.ap-southeast-2.amazonaws.com",
                "rds.sa-east-1.amazonaws.com",
                "rds.cn-north-1.amazonaws.com.cn",
                "rds.cn-northwest-1.amazonaws.com.cn"],
    ?assertEqual( Expected,
                  [H || #aws_config{ rds_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_s3_test() ->
    Service = <<"s3">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>, <<"us-gov-west-1">>,
               <<"cn-north-1">>, <<"cn-northwest-1">>],
    Expected = ["s3-external-1.amazonaws.com",
                "s3.us-west-1.amazonaws.com",
                "s3.us-west-2.amazonaws.com",
                "s3.eu-west-1.amazonaws.com",
                "s3.eu-central-1.amazonaws.com",
                "s3.ap-northeast-1.amazonaws.com",
                "s3.ap-northeast-2.amazonaws.com",
                "s3.ap-southeast-1.amazonaws.com",
                "s3.ap-southeast-2.amazonaws.com",
                "s3.sa-east-1.amazonaws.com",
                "s3-fips-us-gov-west-1.amazonaws.com",
                "s3.cn-north-1.amazonaws.com.cn",
                "s3.cn-northwest-1.amazonaws.com.cn"],
    ?assertEqual( Expected,
                  [H || #aws_config{ s3_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_sdb_test() ->
    Service = <<"sdb">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>],
    Expected = ["sdb.amazonaws.com",
                "sdb.us-west-1.amazonaws.com",
                "sdb.us-west-2.amazonaws.com",
                "sdb.eu-west-1.amazonaws.com",
                "sdb.eu-central-1.amazonaws.com",
                "sdb.ap-northeast-1.amazonaws.com",
                "sdb.ap-southeast-1.amazonaws.com",
                "sdb.ap-southeast-2.amazonaws.com",
                "sdb.sa-east-1.amazonaws.com"],
    ?assertEqual( Expected,
                  [H || #aws_config{ sdb_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_ses_test() ->
    Service = <<"ses">>,
    Regions = [<<"us-east-1">>, <<"us-west-2">>,
               <<"eu-west-1">>],
    Expected = ["email.us-east-1.amazonaws.com",
                "email.us-west-2.amazonaws.com",
                "email.eu-west-1.amazonaws.com"],
    ?assertEqual( Expected,
                  [H || #aws_config{ ses_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_sns_test() ->
    Service = <<"sns">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>,
               <<"us-gov-west-1">>,
               <<"cn-north-1">>, <<"cn-northwest-1">>],
    Expected = ["sns.us-east-1.amazonaws.com",
                "sns.us-west-1.amazonaws.com",
                "sns.us-west-2.amazonaws.com",
                "sns.eu-west-1.amazonaws.com",
                "sns.eu-central-1.amazonaws.com",
                "sns.ap-northeast-1.amazonaws.com",
                "sns.ap-northeast-2.amazonaws.com",
                "sns.ap-southeast-1.amazonaws.com",
                "sns.ap-southeast-2.amazonaws.com",
                "sns.sa-east-1.amazonaws.com",
                "sns.us-gov-west-1.amazonaws.com",
                "sns.cn-north-1.amazonaws.com.cn",
                "sns.cn-northwest-1.amazonaws.com.cn"],
    ?assertEqual( Expected,
                  [H || #aws_config{ sns_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_sqs_test() ->
    Service = <<"sqs">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"us-gov-west-1">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>,
               <<"cn-north-1">>, <<"cn-northwest-1">>],
    Expected = ["sqs.us-east-1.amazonaws.com",
                "sqs.us-west-1.amazonaws.com",
                "sqs.us-west-2.amazonaws.com",
                "sqs.us-gov-west-1.amazonaws.com",
                "sqs.eu-west-1.amazonaws.com",
                "sqs.eu-central-1.amazonaws.com",
                "sqs.ap-northeast-1.amazonaws.com",
                "sqs.ap-northeast-2.amazonaws.com",
                "sqs.ap-southeast-1.amazonaws.com",
                "sqs.ap-southeast-2.amazonaws.com",
                "sqs.sa-east-1.amazonaws.com",
                "sqs.cn-north-1.amazonaws.com.cn",
                "sqs.cn-northwest-1.amazonaws.com.cn"],
    ?assertEqual( Expected,
                  [H || #aws_config{ sqs_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_sts_test() ->
    Service = <<"sts">>,
    ServiceAlt = sts,
    ServiceAlt2 = "sts",
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"us-gov-west-1">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>,
               <<"cn-north-1">>, <<"cn-northwest-1">>],
    RegionsAlt = ["us-east-1", "us-west-1", "us-west-2",
                  "us-gov-west-1",
                  "eu-west-1", "eu-central-1",
                  "ap-northeast-1", "ap-northeast-2",
                  "ap-southeast-1", "ap-southeast-2",
                  "sa-east-1",
                  "cn-north-1", "cn-northwest-1"],
    Expected = ["sts.us-east-1.amazonaws.com",
                "sts.us-west-1.amazonaws.com",
                "sts.us-west-2.amazonaws.com",
                "sts.us-gov-west-1.amazonaws.com",
                "sts.eu-west-1.amazonaws.com",
                "sts.eu-central-1.amazonaws.com",
                "sts.ap-northeast-1.amazonaws.com",
                "sts.ap-northeast-2.amazonaws.com",
                "sts.ap-southeast-1.amazonaws.com",
                "sts.ap-southeast-2.amazonaws.com",
                "sts.sa-east-1.amazonaws.com",
                "sts.cn-north-1.amazonaws.com.cn",
                "sts.cn-northwest-1.amazonaws.com.cn"],
    ?assertEqual( Expected,
                  [H || #aws_config{ sts_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ),
    ?assertEqual( Expected,
                  [H || #aws_config{ sts_host = H } <-
                            [erlcloud_aws:service_config(
                               ServiceAlt, Region, #aws_config{} )
                             || Region <- RegionsAlt]] ),
    ?assertEqual( Expected,
                  [H || #aws_config{ sts_host = H } <-
                            [erlcloud_aws:service_config(
                               ServiceAlt2, Region, #aws_config{} )
                             || Region <- RegionsAlt]] ).

service_config_waf_test() ->
    Service = <<"waf">>,
    Regions = [<<"us-east-1">>],
    Expected = lists:duplicate( length(Regions), "waf.amazonaws.com" ),
    ?assertEqual( Expected,
                  [H || #aws_config{ waf_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

get_host_vpc_endpoint_setup_fun() ->
    meck:new(erlcloud_ec2_meta),
    meck:expect(erlcloud_ec2_meta, generate_session_token,
                fun(60, #aws_config{}) ->
                    {ok, <<"60_seconds_imdsv2_token">>}
                end),
    meck:expect(erlcloud_ec2_meta, get_instance_metadata,
                fun("placement/availability-zone", #aws_config{}, <<"60_seconds_imdsv2_token">>) ->
                    {ok, <<"us-east-1a">>}
                end).

get_host_vpc_endpoint_teardown(_) ->
    application:unset_env(erlcloud, availability_zone),
    application:unset_env(erlcloud, services_vpc_endpoints),
    meck:unload(erlcloud_ec2_meta).


get_host_vpc_endpoint_test_() ->
    {foreach,
     fun get_host_vpc_endpoint_setup_fun/0,
     fun get_host_vpc_endpoint_teardown/1,
     [fun test_get_host_vpc_endpoint_no_settings/0,
      fun test_get_host_vpc_endpoint_no_os_env/0,
      fun test_get_host_vpc_endpoint_invalid_os_env/0,
      fun test_get_host_vpc_endpoint_valid_os_env/0
     ]}.

test_get_host_vpc_endpoint_no_settings() ->
    Default = <<"kinesis.us-east-1.amazonaws.com">>,
    ?assertEqual(Default,
                 erlcloud_aws:get_host_vpc_endpoint(<<"kinesis">>, Default)),
    ?assertEqual(0, meck:num_calls(erlcloud_ec2_meta, get_instance_metadata, '_')).


test_get_host_vpc_endpoint_no_os_env() ->
    Service = <<"kinesis">>,
    EnvVar = "KINESIS_VPC_ENDPOINTS",
    Default = <<"kinesis.us-east-1.amazonaws.com">>,
    application:set_env(erlcloud, services_vpc_endpoints, [{Service, {env, EnvVar}}]),
    ?assertEqual(Default,
                 erlcloud_aws:get_host_vpc_endpoint(<<"kinesis">>, Default)),
    ?assertEqual(0, meck:num_calls(erlcloud_ec2_meta, get_instance_metadata, '_')).

test_get_host_vpc_endpoint_invalid_os_env() ->
    Service = <<"kinesis">>,
    EnvVar = "KINESIS_VPC_ENDPOINTS",
    EnvSetting = "vpc-xyz123-us-east-foobar.kinesis.us-east-1.vpce.amazonaws.com",
    true = os:putenv("KINESIS_VPC_ENDPOINTS", EnvSetting),
    Default = <<"kinesis.us-east-1.amazonaws.com">>,
    application:set_env(erlcloud, services_vpc_endpoints, [{Service, {env, EnvVar}}]),
    try
        ?assertEqual(Default,
                     erlcloud_aws:get_host_vpc_endpoint(<<"kinesis">>, Default)),
        ?assertEqual(1, meck:num_calls(erlcloud_ec2_meta, get_instance_metadata, '_'))
    after
        true = os:unsetenv(EnvVar)
    end.

test_get_host_vpc_endpoint_valid_os_env() ->
    Service = <<"kinesis">>,
    EnvVar = "KINESIS_VPC_ENDPOINTS",
    EnvSetting = "ABC:vpc-xyz123-us-east-1a.kinesis.us-east-1.vpce.amazonaws.com,"
                 "DEF:vpc-xyz123-us-east-1b.kinesis.us-east-1.vpce.amazonaws.com",
    true = os:putenv("KINESIS_VPC_ENDPOINTS", EnvSetting),
    Default = <<"kinesis.us-east-1.amazonaws.com">>,
    application:set_env(erlcloud, services_vpc_endpoints, [{Service, {env, EnvVar}}]),
    try
        ?assertEqual(undefined,
                     application:get_env(erlcloud, availability_zone)),
        ?assertEqual(<<"vpc-xyz123-us-east-1a.kinesis.us-east-1.vpce.amazonaws.com">>,
                     erlcloud_aws:get_host_vpc_endpoint(<<"kinesis">>, Default)),
        ?assertEqual({ok, <<"us-east-1a">>},
                     application:get_env(erlcloud, availability_zone)),
        ?assertEqual(<<"vpc-xyz123-us-east-1a.kinesis.us-east-1.vpce.amazonaws.com">>,
                     erlcloud_aws:get_host_vpc_endpoint(<<"kinesis">>, Default)),
        ?assertEqual(1, meck:num_calls(erlcloud_ec2_meta, get_instance_metadata, '_'))
    after
        true = os:unsetenv(EnvVar)
    end.

validate_map_params() -> 
    Params = #{
        <<"key1">> => <<"val1">>,
        <<"key2">> => [<<"val21">>, <<"val22">>],
        <<"key3">> => #{
            <<"skey1">> => <<"val31">>,
            <<"skey2">> => [<<"a">>,<<"b">>],
            <<"skey3">> => #{<<"nkey1">> => <<"nested">>}
                    },
        <<"key4">> => [#{<<"skey1">> => 10}, #{<<"skey1">>=> 20},#{<<"skey1">>=> 30}]
    },
    DesiredResult = [
        {<<"key1">>,<<"val1">>},
        {<<"key2.1">>,<<"val21">>},
        {<<"key2.2">>,<<"val22">>},
        {<<"key3.skey1">>,<<"val31">>},
        {<<"key3.skey2.1">>,<<"a">>},
        {<<"key3.skey2.2">>,<<"b">>},
        {<<"key3.skey3.nkey1">>,<<"nested">>},
        {<<"key4.1.skey1">>,10},
        {<<"key4.2.skey1">>,20},
        {<<"key4.3.skey1">>,30}
    ],
    ?assertEqual(DesiredResult, erlcloud_aws:process_params(Params)).
