-module(erlcloud_util_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").

request_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun test_proplists_to_map/1]}.

start() ->
    ok.

stop(_) ->
    ok.

test_proplists_to_map(_) ->
    Proplist = [{<<"Directories">>,
      [[{<<"Alias">>,<<"d-93671bd1a1">>},
        {<<"DnsIpAddresses">>,[<<"172.16.1.31">>,<<"172.16.0.42">>]},
        {<<"SelfservicePermissions">>,[
          {<<"SwitchRunningMode">>,<<"ENABLED">>}]},
        {<<"State">>,<<"REGISTERED">>},
        {<<"SubnetIds">>,
         [<<"subnet-02d62b601c121d0ca">>,
          <<"subnet-08b608fe3a2ecc501">>]},
        {<<"Tenancy">>,<<"SHARED">>},
        {<<"WorkspaceAccessProperties">>,
         [{<<"DeviceTypeAndroid">>,<<"ALLOW">>},
          {<<"DeviceTypeZeroClient">>,<<"ALLOW">>}]},
        {<<"WorkspaceCreationProperties">>,
         [{<<"EnableInternetAccess">>,true},
          {<<"UserEnabledAsLocalAdministrator">>,true}]},
        {<<"WorkspaceSecurityGroupId">>,
         <<"sg-064d3dbf40db978bd">>}]]}],
    Expected = #{<<"Directories">> =>
          [#{<<"Alias">> => <<"d-93671bd1a1">>,
             <<"DnsIpAddresses">> =>
                 [<<"172.16.1.31">>,<<"172.16.0.42">>],
             <<"SelfservicePermissions">> =>
                 #{<<"SwitchRunningMode">> => <<"ENABLED">>},
             <<"State">> => <<"REGISTERED">>,
             <<"SubnetIds">> =>
                 [<<"subnet-02d62b601c121d0ca">>,
                  <<"subnet-08b608fe3a2ecc501">>],
             <<"Tenancy">> => <<"SHARED">>,
             <<"WorkspaceAccessProperties">> =>
                 #{<<"DeviceTypeAndroid">> => <<"ALLOW">>,
                   <<"DeviceTypeZeroClient">> => <<"ALLOW">>},
             <<"WorkspaceCreationProperties">> =>
                 #{<<"EnableInternetAccess">> => true,
                   <<"UserEnabledAsLocalAdministrator">> => true},
             <<"WorkspaceSecurityGroupId">> =>
                 <<"sg-064d3dbf40db978bd">>}]},
    [?_assertEqual(Expected, erlcloud_util:proplists_to_map(Proplist)),
     ?_assertEqual(#{}, erlcloud_util:proplists_to_map([{}])),
     ?_assertEqual([], erlcloud_util:proplists_to_map([])),
     ?_assertEqual([<<"a">>, <<"b">>], erlcloud_util:proplists_to_map([<<"a">>, <<"b">>]))
     ].


