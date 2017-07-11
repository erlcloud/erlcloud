%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_ec2_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").
-include("erlcloud_ec2.hrl").

%% Unit tests for ec2.
%% These tests work by using meck to mock erlcloud_httpc. There are two classes of test: input and output.
%%
%% Input tests verify that different function args produce the desired query parameters.
%% An input test list provides a list of funs and the parameters that are expected to result.
%%
%% Output tests verify that the http response produces the correct return from the fun.
%% An output test lists provides a list of response bodies and the expected return.

%% The _ec2_test macro provides line number annotation to a test, similar to _test, but doesn't wrap in a fun
-define(_ec2_test(T), {?LINE, T}).
%% The _f macro is a terse way to wrap code in a fun. Similar to _test but doesn't annotate with a line number
-define(_f(F), fun() -> F end).

%% helper funs
-export([
    generate_one_instance/1, generate_instances_response/3,
    generate_one_snapshot/1, generate_snapshots_response/3,
    generate_one_tag/1, generate_tags_response/3,
    generate_one_spot_price/1, generate_spot_price_history_response/3,
    generate_one_instance_status/1, generate_instance_status_response/3,
    generate_one_reserved_instance_offering/1, generate_reserved_instances_offerings_response/3,
    test_pagination/3
]).

%%%===================================================================
%%% Test entry points
%%%===================================================================

describe_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [
      fun describe_tags_input_tests/1,
      fun describe_tags_output_tests/1,
      fun request_spot_fleet_input_tests/1,
      fun request_spot_fleet_output_tests/1,
      fun cancel_spot_fleet_requests_input_tests/1,
      fun cancel_spot_fleet_requests_output_tests/1,
      fun modify_spot_fleet_request_input_tests/1,
      fun modify_spot_fleet_request_output_tests/1,
      fun describe_spot_fleet_request_input_tests/1,
      fun describe_spot_fleet_request_output_tests/1,
      fun describe_images_tests/1,
      fun describe_vpn_gateways_tests/1,
      fun describe_customer_gateways_tests/1,
      fun describe_vpn_connections_tests/1,
      fun create_flow_logs_input_tests/1,
      fun create_flow_logs_output_tests/1,
      fun delete_flow_logs_input_tests/1,
      fun delete_flow_logs_output_tests/1,
      fun describe_flow_logs_input_tests/1,
      fun describe_flow_logs_output_tests/1
     ]}.

start() ->
    meck:new(erlcloud_httpc),
    ok.

stop(_) ->
    meck:unload(erlcloud_httpc).

%%%===================================================================
%%% Input test helpers
%%%===================================================================

%% common_params returns the list of parameters that are not validated by these tests.
%% They should be checked by lower level unit tests.
-spec common_params() -> [string()].
common_params() ->
    ["AWSAccessKeyId",
     "SignatureMethod",
     "SignatureVersion",
     "Timestamp",
     "Version",
     "Signature"].

%% validate_param checks that the query parameter is either a common param or expected
%% by the test case. If expected, returns expected with the param deleted to be used in
%% subsequent calls.
-type expected_param() :: {string(), string()}.
-spec validate_param(string(), [expected_param()]) -> [expected_param()].
validate_param(Param, Expected) ->
    {Key, Value} = case string:tokens(Param, "=") of
        [K, V] -> {K, V};
        [K] -> {K, ""}
    end,
    case lists:member(Key, common_params()) of
        true ->
            Expected;
        false ->
            Expected1 = lists:delete({Key, Value}, Expected),
            %?debugFmt("EXPECTED ~p~nEXPECTED1 ~p", [Expected, Expected1]),
            case length(Expected) - 1 =:= length(Expected1) of
                true -> ok;
                false ->
                    ?debugFmt("Parameter not expected: ~p", [{Key, Value}])
            end,
            ?assertEqual(length(Expected) - 1, length(Expected1)),
            Expected1
    end.

%% verifies that the parameters in the body match the expected parameters
-spec validate_params(binary(), [expected_param()]) -> ok.
validate_params(Body, Expected) ->
    ParamList = string:tokens(binary_to_list(Body), "&"),
    Remain = lists:foldl(fun validate_param/2, Expected, ParamList),
    ?assertEqual([], Remain).

%% returns the mock of the erlcloud_httpc function input tests expect to be called.
%% Validates the query body and responds with the provided response.
-spec input_expect(string(), [expected_param()]) -> fun().
input_expect(Response, Expected) ->
    fun(_Url, post, _Headers, Body, _Timeout, _Config) ->
            validate_params(Body, Expected),
            {ok, {{200, "OK"}, [], list_to_binary(Response)}}
    end.

%% input_test converts an input_test specifier into an eunit test generator
-type input_test_spec() :: {pos_integer(), {fun(), [expected_param()]} | {string(), fun(), [expected_param()]}}.
-spec input_test(string(), input_test_spec()) -> tuple().
input_test(Response, {Line, {Description, Fun, Params}}) when
      is_list(Description) ->
    {Description,
     {Line,
      fun() ->
              meck:expect(erlcloud_httpc, request, input_expect(Response, Params)),
              %% Configure to make sure there is a key. Would like to do this in start, but
              %% that isn't called in the same process
              erlcloud_ec2:configure(string:copies("A", 20), string:copies("a", 40)),
              Fun()
      end}}.
%% input_test(Response, {Line, {Fun, Params}}) ->
%%     input_test(Response, {Line, {"", Fun, Params}}).

%% input_tests converts a list of input_test specifiers into an eunit test generator
-spec input_tests(string(), [input_test_spec()]) -> [tuple()].
input_tests(Response, Tests) ->
    [input_test(Response, Test) || Test <- Tests].

%%%===================================================================
%%% Output test helpers
%%%===================================================================

%% returns the mock of the erlcloud_httpc function output tests expect to be called.
-spec output_expect(string()) -> fun().
output_expect(Response) ->
    fun(_Url, post, _Headers, _Body, _Timeout, _Config) ->
            {ok, {{200, "OK"}, [], list_to_binary(Response)}}
    end.

%% output_test converts an output_test specifier into an eunit test generator
-type output_test_spec() :: {pos_integer(), {string(), term()} | {string(), string(), term()}}.
-spec output_test(fun(), output_test_spec()) -> tuple().
output_test(Fun, {Line, {Description, Response, Result}}) ->
    {Description,
     {Line,
      fun() ->
              meck:expect(erlcloud_httpc, request, output_expect(Response)),
              erlcloud_ec2:configure(string:copies("A", 20), string:copies("a", 40)),
              Actual = Fun(),
              ?assertEqual(Result, Actual)
      end}}.
%% output_test(Fun, {Line, {Response, Result}}) ->
%%     output_test(Fun, {Line, {"", Response, Result}}).

%% output_tests converts a list of output_test specifiers into an eunit test generator
-spec output_tests(fun(), [output_test_spec()]) -> [term()].
output_tests(Fun, Tests) ->
    [output_test(Fun, Test) || Test <- Tests].


%%%===================================================================
%%% Actual test specifiers
%%%===================================================================


%% DescribeTags test based on the API examples:
%% http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeTags.html
describe_tags_input_tests(_) ->
    Tests =
        [?_ec2_test(
            {"This example describes all the tags in your account.",
             ?_f(erlcloud_ec2:describe_tags()),
             [{"Action", "DescribeTags"}]}),
         ?_ec2_test(
            {"This example describes only the tags for the AMI with ID ami-1a2b3c4d.",
             ?_f(erlcloud_ec2:describe_tags([{resource_id, ["ami-1a2b3c4d"]}])),
             [{"Action", "DescribeTags"},
              {"Filter.1.Name", "resource-id"},
              {"Filter.1.Value.1", "ami-1a2b3c4d"}]}),
         ?_ec2_test(
            {"This example describes the tags for all your instances.",
             ?_f(erlcloud_ec2:describe_tags([{resource_type, ["instance"]}])),
             [{"Action", "DescribeTags"},
              {"Filter.1.Name", "resource-type"},
              {"Filter.1.Value.1", "instance"}]}),
         ?_ec2_test(
            {"This example describes the tags for all your instances tagged with the key webserver.",
             ?_f(erlcloud_ec2:describe_tags([{key, ["webserver"]}])),
             [{"Action", "DescribeTags"},
              {"Filter.1.Name", "key"},
              {"Filter.1.Value.1", "webserver"}]}),
         ?_ec2_test(
            {"This example describes the tags for all your instances tagged with either stack=Test or stack=Production.",
             ?_f(erlcloud_ec2:describe_tags([{resource_type, ["instance"]}, {key, ["stack"]}, {value, ["Test", "Production"]}])),
             [{"Action", "DescribeTags"},
              {"Filter.1.Name", "resource-type"},
              {"Filter.1.Value.1", "instance"},
              {"Filter.2.Name", "key"},
              {"Filter.2.Value.1", "stack"},
              {"Filter.3.Name", "value"},
              {"Filter.3.Value.1", "Test"},
              {"Filter.3.Value.2", "Production"}]}),
         ?_ec2_test(
            {"This example describes the tags for all your instances tagged with Purpose=[empty string].",
             ?_f(erlcloud_ec2:describe_tags([{resource_type, ["instance"]}, {key, ["Purpose"]}, {value, [""]}])),
             [{"Action", "DescribeTags"},
              {"Filter.1.Name", "resource-type"},
              {"Filter.1.Value.1", "instance"},
              {"Filter.2.Name", "key"},
              {"Filter.2.Value.1", "Purpose"},
              {"Filter.3.Name", "value"},
              {"Filter.3.Value.1", ""}]})],

    Response = "
<DescribeTagsResponse xmlns=\"http://ec2.amazonaws.com/doc/2012-12-01/\">
   <requestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</requestId>
   <tagSet/>
</DescribeTagsResponse>",
    input_tests(Response, Tests).

describe_tags_output_tests(_) ->
    Tests =
        [?_ec2_test(
            {"This example describes all the tags in your account.", "
<DescribeTagsResponse xmlns=\"http://ec2.amazonaws.com/doc/2012-12-01/\">
   <requestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</requestId>
   <tagSet>
      <item>
         <resourceId>ami-1a2b3c4d</resourceId>
         <resourceType>image</resourceType>
         <key>webserver</key>
         <value/>
      </item>
       <item>
         <resourceId>ami-1a2b3c4d</resourceId>
         <resourceType>image</resourceType>
         <key>stack</key>
         <value>Production</value>
      </item>
      <item>
         <resourceId>i-5f4e3d2a</resourceId>
         <resourceType>instance</resourceType>
         <key>webserver</key>
         <value/>
      </item>
       <item>
         <resourceId>i-5f4e3d2a</resourceId>
         <resourceType>instance</resourceType>
         <key>stack</key>
         <value>Production</value>
      </item>
      <item>
         <resourceId>i-12345678</resourceId>
         <resourceType>instance</resourceType>
         <key>database_server</key>
         <value/>
      </item>
       <item>
         <resourceId>i-12345678</resourceId>
         <resourceType>instance</resourceType>
         <key>stack</key>
         <value>Test</value>
      </item>
    </tagSet>
</DescribeTagsResponse>",
             {ok, [#ec2_tag{resource_id="ami-1a2b3c4d", resource_type="image", key="webserver", value=""},
                   #ec2_tag{resource_id="ami-1a2b3c4d", resource_type="image", key="stack", value="Production"},
                   #ec2_tag{resource_id="i-5f4e3d2a", resource_type="instance", key="webserver", value=""},
                   #ec2_tag{resource_id="i-5f4e3d2a", resource_type="instance", key="stack", value="Production"},
                   #ec2_tag{resource_id="i-12345678", resource_type="instance", key="database_server", value=""},
                   #ec2_tag{resource_id="i-12345678", resource_type="instance", key="stack", value="Test"}]}}),
         ?_ec2_test(
            {"This example describes only the tags for the AMI with ID ami-1a2b3c4d.", "
<DescribeTagsResponse xmlns=\"http://ec2.amazonaws.com/doc/2012-12-01/\">
   <requestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</requestId>
   <tagSet>
      <item>
         <resourceId>ami-1a2b3c4d</resourceId>
         <resourceType>image</resourceType>
         <key>webserver</key>
         <value/>
      </item>
      <item>
         <resourceId>ami-1a2b3c4d</resourceId>
         <resourceType>image</resourceType>
         <key>stack</key>
         <value>Production</value>
      </item>
    </tagSet>
</DescribeTagsResponse>",
             {ok, [#ec2_tag{resource_id="ami-1a2b3c4d", resource_type="image", key="webserver", value=""},
                   #ec2_tag{resource_id="ami-1a2b3c4d", resource_type="image", key="stack", value="Production"}]}})],

    %% Remaining AWS API examples return subsets of the same data
    output_tests(?_f(erlcloud_ec2:describe_tags()), Tests).

describe_vpn_gateways_tests(_) ->
    Tests = 
        [?_ec2_test(
            {"This example describes VPN gateways.", "
<DescribeVpnGatewaysResponse xmlns=\"http://ec2.amazonaws.com/doc/2015-04-15/\">
  <requestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</requestId>
  <vpnGatewaySet>   
    <item>
      <vpnGatewayId>vgw-8db04f81</vpnGatewayId>
      <state>available</state>
      <type>ipsec.1</type>
      <availabilityZone>us-east-1a</availabilityZone> 
      <attachments>
        <item>
          <vpcId>vpc-1a2b3c4d</vpcId>
          <state>attached</state>
        </item>
      </attachments>
      <tagSet/>
    </item>
    <item>
      <vpnGatewayId>vgw-8db04f82</vpnGatewayId>
      <state>available</state>
      <type>ipsec.1</type>
      <availabilityZone>us-east-1a</availabilityZone> 
      <attachments>
        <item>
          <vpcId>vpc-1a2b3c4d</vpcId>
          <state>attached</state>
        </item>
      </attachments>
      <tagSet/>
    </item>
  </vpnGatewaySet>
</DescribeVpnGatewaysResponse>",
             {ok, [[{vpn_gateway_id, "vgw-8db04f81"},
                    {vpn_gateway_type, "ipsec.1"},
                    {vpn_gateway_state, "available"},
                    {vpn_az, "us-east-1a"},
                    {vpc_attachment_set, [[{vpc_id, "vpc-1a2b3c4d"}, {state, "attached"}]]},
                    {tag_set, []}],
                   [{vpn_gateway_id, "vgw-8db04f82"},
                    {vpn_gateway_type, "ipsec.1"},
                    {vpn_gateway_state, "available"},
                    {vpn_az, "us-east-1a"},
                    {vpc_attachment_set, [[{vpc_id, "vpc-1a2b3c4d"}, {state, "attached"}]]},
                    {tag_set, []}]]}})],
    
    %% Remaining AWS API examples return subsets of the same data
    output_tests(?_f(erlcloud_ec2:describe_vpn_gateways()), Tests).

describe_customer_gateways_tests(_) ->
    Tests = 
        [?_ec2_test(
            {"This example describes customer gateways.", "
<DescribeCustomerGatewaysResponse xmlns=\"http://ec2.amazonaws.com/doc/2015-04-15/\">
  <requestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</requestId>
  <customerGatewaySet>
    <item>
       <customerGatewayId>cgw-b4dc3961</customerGatewayId>
       <state>available</state>
       <type>ipsec.1</type>
       <ipAddress>12.1.2.3</ipAddress> 
       <bgpAsn>65534</bgpAsn>   
       <tagSet/>
    </item>
    <item>
       <customerGatewayId>cgw-b4dc3962</customerGatewayId>
       <state>available</state>
       <type>ipsec.1</type>
       <ipAddress>12.1.2.3</ipAddress> 
       <bgpAsn>65534</bgpAsn>   
       <tagSet/>
    </item>
  </customerGatewaySet>
</DescribeCustomerGatewaysResponse>",
             {ok, [[{customer_gateway_id, "cgw-b4dc3961"},
                    {customer_gateway_state, "available"},
                    {customer_gateway_type, "ipsec.1"},
                    {customer_gateway_ip, "12.1.2.3"},
                    {customer_gateway_bgpasn, "65534"},
                    {tag_set, []}],
                   [{customer_gateway_id, "cgw-b4dc3962"},
                    {customer_gateway_state, "available"},
                    {customer_gateway_type, "ipsec.1"},
                    {customer_gateway_ip, "12.1.2.3"},
                    {customer_gateway_bgpasn, "65534"},
                    {tag_set, []}]]}})],
    
    %% Remaining AWS API examples return subsets of the same data
    output_tests(?_f(erlcloud_ec2:describe_customer_gateways()), Tests).

describe_vpn_connections_tests(_) ->
    Tests = 
        [?_ec2_test(
            {"This example describes VPN connections.", "
<DescribeVpnConnectionsResponse xmlns=\"http://ec2.amazonaws.com/doc/2015-04-15/\">
  <requestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</requestId>
  <vpnConnectionSet>
    <item>
      <vpnConnectionId>vpn-44a8938f</vpnConnectionId>
      <state>available</state>
      <customerGatewayConfiguration>config1</customerGatewayConfiguration>     
      <type>ipsec.1</type>
      <customerGatewayId>cgw-b4dc3961</customerGatewayId>
      <vpnGatewayId>vgw-8db04f81</vpnGatewayId>
      <tagSet/>
    </item>
    <item>
      <vpnConnectionId>vpn-54a8938f</vpnConnectionId>
      <state>available</state>
      <customerGatewayConfiguration>config2</customerGatewayConfiguration>     
      <type>ipsec.1</type>
      <customerGatewayId>cgw-b4dc3962</customerGatewayId>
      <vpnGatewayId>vgw-8db04f82</vpnGatewayId>
      <tagSet/>
    </item>
  </vpnConnectionSet>
</DescribeVpnConnectionsResponse>",
             {ok, [[{vpn_connection_id, "vpn-44a8938f"},
                    {vpn_connection_state, "available"},
                    {customer_gateway_configuration, "config1"},
                    {vpn_connection_type, "ipsec.1"},
                    {customer_gateway_id, "cgw-b4dc3961"},
                    {vpn_gateway_id, "vgw-8db04f81"},
                    {tag_set, []}],
                   [{vpn_connection_id, "vpn-54a8938f"},
                    {vpn_connection_state, "available"},
                    {customer_gateway_configuration, "config2"},
                    {vpn_connection_type, "ipsec.1"},
                    {customer_gateway_id, "cgw-b4dc3962"},
                    {vpn_gateway_id, "vgw-8db04f82"},
                    {tag_set, []}]]}})],
    
    %% Remaining AWS API examples return subsets of the same data
    output_tests(?_f(erlcloud_ec2:describe_vpn_connections()), Tests).

%% RequestSpotFleet test based on the API examples:
%% http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestSpotFleet.html
request_spot_fleet_input_tests(_) ->
    Tests =
        [?_ec2_test(
            {"This example creates a Spot fleet request with 2 launch specifications.",
             ?_f(erlcloud_ec2:request_spot_fleet(
                #ec2_spot_fleet_request{
                  spot_fleet_request_config = #spot_fleet_request_config_spec {
                    iam_fleet_role = "arn:aws:iam::123456789011:role/spot-fleet-role",
                    spot_price = "0.0153",
                    target_capacity = 5,
                    launch_specification = [
                      #ec2_instance_spec {
                        image_id = "ami-1ecae776",
                        instance_type = "m4.large",
                        subnet_id = "subnet-1a2b3c4d"
                      },
                      #ec2_instance_spec {
                        image_id = "ami-1ecae776",
                        instance_type = "m4.medium",
                        subnet_id = "subnet-1a2b3c4d"
                      }
                    ]
                  }
                }
              )),
             [{"Action", "RequestSpotFleet"},
              {"SpotFleetRequestConfig.IamFleetRole", "arn%3Aaws%3Aiam%3A%3A123456789011%3Arole%2Fspot-fleet-role"},
              {"SpotFleetRequestConfig.SpotPrice", "0.0153"},
              {"SpotFleetRequestConfig.TargetCapacity", "5"},
              {"SpotFleetRequestConfig.LaunchSpecifications.1.ImageId", "ami-1ecae776"},
              {"SpotFleetRequestConfig.LaunchSpecifications.1.InstanceType", "m4.large"},
              {"SpotFleetRequestConfig.LaunchSpecifications.1.SubnetId", "subnet-1a2b3c4d"},
              {"SpotFleetRequestConfig.LaunchSpecifications.1.Monitoring.Enabled", "false"},
              {"SpotFleetRequestConfig.LaunchSpecifications.1.EbsOptimized", "false"},
              {"SpotFleetRequestConfig.LaunchSpecifications.1.SecurityGroup.1", "default"},
              {"SpotFleetRequestConfig.LaunchSpecifications.2.ImageId", "ami-1ecae776"},
              {"SpotFleetRequestConfig.LaunchSpecifications.2.InstanceType", "m4.medium"},
              {"SpotFleetRequestConfig.LaunchSpecifications.2.SubnetId", "subnet-1a2b3c4d"},
              {"SpotFleetRequestConfig.LaunchSpecifications.2.Monitoring.Enabled", "false"},
              {"SpotFleetRequestConfig.LaunchSpecifications.2.EbsOptimized", "false"},
              {"SpotFleetRequestConfig.LaunchSpecifications.2.SecurityGroup.1", "default"}
             ]})],

    Response = "
<RequestSpotFleetResponse xmlns=\"http://ec2.amazonaws.com/doc/2015-10-01/\">
    <requestId>60262cc5-2bd4-4c8d-98ed-example</requestId>
    <spotFleetRequestId>sfr-123f8fc2-cb31-425e-abcd-example2710</spotFleetRequestId>
</RequestSpotFleetResponse>",
    input_tests(Response, Tests).

request_spot_fleet_output_tests(_) ->
    Tests =
        [?_ec2_test(
            {"This example creates a Spot fleet request with 2 launch specifications.", "
<RequestSpotFleetResponse xmlns=\"http://ec2.amazonaws.com/doc/2015-10-01/\">
    <requestId>60262cc5-2bd4-4c8d-98ed-example</requestId>
    <spotFleetRequestId>sfr-123f8fc2-cb31-425e-abcd-example2710</spotFleetRequestId>
</RequestSpotFleetResponse>",
             {ok, "sfr-123f8fc2-cb31-425e-abcd-example2710"}})],

    output_tests(?_f(erlcloud_ec2:request_spot_fleet(
                #ec2_spot_fleet_request{
                  spot_fleet_request_config = #spot_fleet_request_config_spec {
                    iam_fleet_role = "arn:aws:iam::123456789011:role/spot-fleet-role",
                    spot_price = "0.0153",
                    target_capacity = 5,
                    launch_specification = [
                      #ec2_instance_spec {
                        image_id = "ami-1ecae776",
                        instance_type = "m4.large",
                        subnet_id = "subnet-1a2b3c4d"
                      },
                      #ec2_instance_spec {
                        image_id = "ami-1ecae776",
                        instance_type = "m4.medium",
                        subnet_id = "subnet-1a2b3c4d"
                      }
                    ]
                  }
                }
              )), Tests).

%% CancelSpotFleetRequests test based on the API examples:
%% http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CancelSpotFleetRequests.html
cancel_spot_fleet_requests_input_tests(_) ->
    Tests =
        [?_ec2_test(
            {"This example cancels Spot fleet request sfr-123f8fc2-cb31-425e-abcd-example2710 and terminates all instances that were launched by the request.",
             ?_f(erlcloud_ec2:cancel_spot_fleet_requests(["sfr-123f8fc2-cb31-425e-abcd-example2710"], true)),
             [{"Action", "CancelSpotFleetRequests"},
              {"SpotFleetRequestId.1", "sfr-123f8fc2-cb31-425e-abcd-example2710"},
              {"TerminateInstances", "true"}
             ]})],

    Response = "
<CancelSpotFleetRequestsResponse xmlns=\"http://ec2.amazonaws.com/doc/2015-10-01/\">
    <requestId>e12d2fe5-6503-4b4b-911c-example</requestId>
    <unsuccessfulFleetRequestSet/>
    <successfulFleetRequestSet>
        <item>
            <spotFleetRequestId>sfr-123f8fc2-cb31-425e-abcd-example2710</spotFleetRequestId>
            <currentSpotFleetRequestState>cancelled_terminating</currentSpotFleetRequestState>
            <previousSpotFleetRequestState>active</previousSpotFleetRequestState>
        </item>
    </successfulFleetRequestSet>
</CancelSpotFleetRequestsResponse>",
    input_tests(Response, Tests).


cancel_spot_fleet_requests_output_tests(_) ->
    Tests =
        [?_ec2_test(
            {"This example cancels Spot fleet request sfr-123f8fc2-cb31-425e-abcd-example2710 and terminates all instances that were launched by the request.", "
<CancelSpotFleetRequestsResponse xmlns=\"http://ec2.amazonaws.com/doc/2015-10-01/\">
    <requestId>e12d2fe5-6503-4b4b-911c-example</requestId>
    <unsuccessfulFleetRequestSet/>
    <successfulFleetRequestSet>
        <item>
            <spotFleetRequestId>sfr-123f8fc2-cb31-425e-abcd-example2710</spotFleetRequestId>
            <currentSpotFleetRequestState>cancelled_terminating</currentSpotFleetRequestState>
            <previousSpotFleetRequestState>active</previousSpotFleetRequestState>
        </item>
    </successfulFleetRequestSet>
</CancelSpotFleetRequestsResponse>",
               {ok, [
                      {unsuccessful_fleet_request_set,[]},
                      {successful_fleet_request_set,[
                        [
                          {spot_fleet_request_id,"sfr-123f8fc2-cb31-425e-abcd-example2710"},
                          {current_spot_fleet_request_state, "cancelled_terminating"},
                          {previous_spot_fleet_request_state, "active"}
                        ]
                      ]}
                    ]
                }
                })],
    output_tests(?_f(erlcloud_ec2:cancel_spot_fleet_requests(["sfr-123f8fc2-cb31-425e-abcd-example2710"], true)), Tests).

%% ModifySpotFleetRequest test based on the API examples:
%% http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifySpotFleetRequest.html
modify_spot_fleet_request_input_tests(_) ->
    Tests =
        [?_ec2_test(
            {"This example modifies the fleet request capacity.",
             ?_f(erlcloud_ec2:modify_spot_fleet_request("sfr-123f8fc2-cb31-425e-abcd-example2710", 10, default)),
             [{"Action", "ModifySpotFleetRequest"},
              {"SpotFleetRequestId", "sfr-123f8fc2-cb31-425e-abcd-example2710"},
              {"ExcessCapacityTerminationPolicy", "Default"},
              {"TargetCapacity", "10"}
             ]})],

    Response = "
<ModifySpotFleetRequestResponse xmlns=\"http://ec2.amazonaws.com/doc/2015-10-01/\">
    <requestId>e12d2fe5-6503-4b4b-911c-example</requestId>
    <return>true</return>
</ModifySpotFleetRequestResponse>",
    input_tests(Response, Tests).

modify_spot_fleet_request_output_tests(_) ->
    Tests =
        [?_ec2_test(
            {"This example modifies the fleet request capacity.", "
<ModifySpotFleetRequestResponse xmlns=\"http://ec2.amazonaws.com/doc/2015-10-01/\">
    <requestId>e12d2fe5-6503-4b4b-911c-example</requestId>
    <return>true</return>
</ModifySpotFleetRequestResponse>", ok})],
    output_tests(?_f(erlcloud_ec2:modify_spot_fleet_request("sfr-123f8fc2-cb31-425e-abcd-example2710", 10, default)), Tests).

%% DescribeSpotFleetInstances test based on the API examples:
%% http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeSpotFleetInstances.html
describe_spot_fleet_request_input_tests(_) ->
    Tests =
        [?_ec2_test(
            {"This example describes the spot fleet.",
             ?_f(erlcloud_ec2:describe_spot_fleet_instances("sfr-123f8fc2-cb31-425e-abcd-example2710")),
             [{"Action", "DescribeSpotFleetInstances"},
              {"SpotFleetRequestId", "sfr-123f8fc2-cb31-425e-abcd-example2710"}
             ]})],

    Response = "
<DescribeSpotFleetInstancesResponse xmlns=\"http://ec2.amazonaws.com/doc/2015-10-01/\">
    <requestId>cfb09950-45e2-472d-a6a9-example</requestId>
    <spotFleetRequestId>sfr-123f8fc2-cb31-425e-abcd-example2710</spotFleetRequestId>
    <activeInstanceSet>
        <item>
            <instanceId>i-1a1a1a1a</instanceId>
            <spotInstanceRequestId>sir-1a1a1a1a</spotInstanceRequestId>
            <instanceType>m3.medium</instanceType>
        </item>
        <item>
            <instanceId>i-2b2b2b2b</instanceId>
            <spotInstanceRequestId>sir-2b2b2b2b</spotInstanceRequestId>
            <instanceType>m3.medium</instanceType>
        </item>
    </activeInstanceSet>
</DescribeSpotFleetInstancesResponse>",
    input_tests(Response, Tests).

describe_spot_fleet_request_output_tests(_) ->
    Tests =
        [?_ec2_test(
            {"This example describes the spot fleet.", "
<DescribeSpotFleetInstancesResponse xmlns=\"http://ec2.amazonaws.com/doc/2015-10-01/\">
    <requestId>cfb09950-45e2-472d-a6a9-example</requestId>
    <spotFleetRequestId>sfr-123f8fc2-cb31-425e-abcd-example2710</spotFleetRequestId>
    <activeInstanceSet>
        <item>
            <instanceId>i-1a1a1a1a</instanceId>
            <spotInstanceRequestId>sir-1a1a1a1a</spotInstanceRequestId>
            <instanceType>m3.medium</instanceType>
        </item>
        <item>
            <instanceId>i-2b2b2b2b</instanceId>
            <spotInstanceRequestId>sir-2b2b2b2b</spotInstanceRequestId>
            <instanceType>m3.medium</instanceType>
        </item>
    </activeInstanceSet>
</DescribeSpotFleetInstancesResponse>",
      {ok,[
        {instances,[
          [
            {instance_id,"i-1a1a1a1a"},
            {spot_instance_request_id,"sir-1a1a1a1a"},
            {instance_type,"m3.medium"}
          ],
          [
            {instance_id,"i-2b2b2b2b"},
            {spot_instance_request_id,"sir-2b2b2b2b"},
            {instance_type,"m3.medium"}
          ]
        ]},
        {next_token, undefined}
      ]}

    })],
    output_tests(?_f(erlcloud_ec2:describe_spot_fleet_instances("sfr-123f8fc2-cb31-425e-abcd-example2710")), Tests).

describe_images_tests(_) ->
  Tests =
    [?_ec2_test(
      {"This example describes AMIs.", "
<DescribeImagesResponse xmlns=\"http://ec2.amazonaws.com/doc/2015-04-15/\">
   <requestId>59dbff89-35bd-4eac-99ed-be587EXAMPLE</requestId>
   <imagesSet>
      <item>
         <imageId>ami-1a2b3c4d</imageId>
         <kernelId/>
         <ramdiskId/>
         <imageLocation>ec2-public-windows-images/Server2003r2-x86_64-Win-v1.07.manifest.xml</imageLocation>
         <imageState>available</imageState>
         <imageOwnerId>123456789012</imageOwnerId>
         <isPublic>true</isPublic>
         <architecture>x86_64</architecture>
         <imageType>machine</imageType>
         <platform>windows</platform>
         <imageOwnerAlias>amazon</imageOwnerAlias>
         <rootDeviceName>/dev/sda1</rootDeviceName>
         <rootDeviceType>instance-store</rootDeviceType>
         <creationDate>2016-03-26T12:00:13Z</creationDate>
         <blockDeviceMapping/>
         <virtualizationType>hvm</virtualizationType>
         <tagSet/>
         <hypervisor>xen</hypervisor>
      </item>
   </imagesSet>
</DescribeImagesResponse>",
        {ok, [[ {image_id, "ami-1a2b3c4d"},
          {image_location, "ec2-public-windows-images/Server2003r2-x86_64-Win-v1.07.manifest.xml"},
          {image_state, "available"},
          {image_owner_id, "123456789012"},
          {is_public, true},
          {architecture, "x86_64"},
          {image_type, "machine"},
          {kernel_id, []},
          {ramdisk_id, []},
          {image_owner_alias, "amazon"},
          {name, []},
          {description, []},
          {root_device_type, "instance-store"},
          {root_device_name, "/dev/sda1"},
          {creation_date, {{2016,3,26},{12,0,13}}},
          {platform, "windows"},
          {block_device_mapping, []},
          {product_codes, []}
        ]]}})],

  %% Remaining AWS API examples return subsets of the same data
  output_tests(?_f(erlcloud_ec2:describe_images()), Tests).

describe_account_attributes_test() ->
    XML = "<DescribeAccountAttributesResponse>
               <requestId>12345</requestId>
               <accountAttributeSet>
                   <item>
                       <attributeName>attribute123</attributeName>
                       <attributeValueSet>
                           <item>
                               <attributeValue>123</attributeValue>
                           </item>
                       </attributeValueSet>
                   </item>
                   <item>
                       <attributeName>attribute456</attributeName>
                       <attributeValueSet>
                           <item>
                               <attributeValue>456</attributeValue>
                           </item>
                       </attributeValueSet>
                   </item>
                   <item>
                       <attributeName>attribute789</attributeName>
                       <attributeValueSet>
                           <item>
                               <attributeValue>789</attributeValue>
                           </item>
                       </attributeValueSet>
                   </item>
               </accountAttributeSet>
           </DescribeAccountAttributesResponse>",
    XMERL = {ok, element(1, xmerl_scan:string(XML))},
    ExpectedResult =
        {ok,[
                [{attribute_name,"attribute123"},
                    {attribute_value_set,[[{attribute_value,"123"}]]}],
                [{attribute_name,"attribute456"},
                    {attribute_value_set,[[{attribute_value,"456"}]]}],
                [{attribute_name,"attribute789"},
                    {attribute_value_set,[[{attribute_value,"789"}]]}]
            ]},
    meck:new(erlcloud_aws, [passthrough]),
    meck:expect(erlcloud_aws, aws_request_xml4,
        fun(_,_,_,_,_,_) ->
            XMERL
        end),
    Result = erlcloud_ec2:describe_account_attributes(),
    meck:unload(erlcloud_aws),
    ?assertEqual(ExpectedResult, Result).
    
describe_nat_gateways_test() ->
    XML = "<DescribeNatGatewaysResponse>
               <requestId>bfed02c6-dae9-47c0-86a2-example</requestId>
               <natGatewaySet>
                   <item>
                       <createTime>2017-01-02T15:49:34.999Z</createTime>
                       <deleteTime>2017-01-02T15:49:34.999Z</deleteTime>
                       <failureCode>1234</failureCode>
                       <failureMessage>boom</failureMessage>
                       <natGatewayAddressSet>
                           <item>
                               <allocationId>allocid-123</allocationId>
                               <networkInterfaceId>ni-56789</networkInterfaceId>
                               <privateIp>10.0.0.0</privateIp>
                               <publicIp>123.12.123.12</publicIp>
                           </item>
                       </natGatewayAddressSet>
                       <natGatewayId>nat-04e77a5e9c34432f9</natGatewayId>
                       <state>available</state>
                       <subnetId>subnet-1a2a3a4a</subnetId>
                       <vpcId>vpc-4e20d42b</vpcId>
                   </item>
               </natGatewaySet>
           </DescribeNatGatewaysResponse>",
    XMERL = {ok, element(1, xmerl_scan:string(XML))},
    ExpectedResult =
        {ok,
            [
                [
                    {create_time, "2017-01-02T15:49:34.999Z"},
                    {delete_time, "2017-01-02T15:49:34.999Z"},
                    {failure_code, "1234"},
                    {failure_message, "boom"},
                    {nat_gateway_address_set,
                        [
                            [
                                {allocation_id, "allocid-123"},
                                {network_interface_id, "ni-56789"},
                                {private_ip, "10.0.0.0"},
                                {public_ip, "123.12.123.12"}
                            ]
                        ]
                    },
                    {nat_gateway_id, "nat-04e77a5e9c34432f9"},
                    {state, "available"},
                    {subnet_id, "subnet-1a2a3a4a"},
                    {vpc_id, "vpc-4e20d42b"}
                ]
            ]
        },
    meck:new(erlcloud_aws, [passthrough]),
    meck:expect(erlcloud_aws, aws_request_xml4,
        fun(_,_,_,_,_,_) ->
            XMERL
        end),
    Result = erlcloud_ec2:describe_nat_gateways(),
    meck:unload(erlcloud_aws),
    ?assertEqual(ExpectedResult, Result).

describe_vpc_peering_connections_test() ->
    XML = "<DescribeVpcPeeringConnectionsResponse>
       	       <requestId>lnwadt7-8adia7r-aadu8-EXAMPLE</requestId>
       	       <vpcPeeringConnectionSet>
                   <item>
                       <requesterVpcInfo>
                           <ownerId>777788889999</ownerId>
                           <vpcId>vpc-1a2b3c4d</vpcId>
                           <cidrBlock>172.31.0.0/16</cidrBlock>
                       </requesterVpcInfo>
       	               <expirationTime>2017.01.02 09:54:15</expirationTime>
       	               <tagSet>
                           <item>
       	                       <key>key1</key>
       	                       <value>value1</value>
       	                   </item>
                           <item>
       	                       <key>key2</key>
       	                       <value>value2</value>
       	                   </item>
       	                   <item>
       	                       <key>key3</key>
       	                       <value>value3</value>
       	                   </item>
       	               </tagSet>
       	               <vpcPeeringConnectionId>pcx-123abc69</vpcPeeringConnectionId>
       	           </item>
               </vpcPeeringConnectionSet>
           </DescribeVpcPeeringConnectionsResponse>",
    XMERL = {ok, element(1, xmerl_scan:string(XML))},
    ExpectedResult =
        {ok,
            [
                [
                    {expiration_time, "2017.01.02 09:54:15"},
                    {tag_set,
                        [
                            [
                                {key, "key1"},
                                {value, "value1"}
                            ],
                            [
                                {key, "key2"},
                                {value, "value2"}
                            ],
                            [
                                {key, "key3"},
                                {value, "value3"}
                            ]
                        ]
                    },
                    {vpc_peering_connection_id, "pcx-123abc69"},
                    {requester_vpc_info,
                        [
                            [
                                {cidr_block, "172.31.0.0/16"},
                                {owner_id, "777788889999"},
                                {vpc_id, "vpc-1a2b3c4d"}
                            ]
                        ]
                    }
                ]
            ]
        },
    meck:new(erlcloud_aws, [passthrough]),
    meck:expect(erlcloud_aws, aws_request_xml4,
        fun(_,_,_,_,_,_) ->
            XMERL
        end),
    Result = erlcloud_ec2:describe_vpc_peering_connections(),
    meck:unload(erlcloud_aws),
    ?assertEqual(ExpectedResult, Result).

create_flow_logs_input_tests(_) ->
    Tests = [
        ?_ec2_test({"This example creates flow log.",
             ?_f(erlcloud_ec2:create_flow_logs(
                    "TestLogGroup", network_interface, ["eni-aa22bb33", "eni-22aabb33"], reject,
                     "arn:aws:iam::123456789101:role/flowlogsrole")),
             [{"Action", "CreateFlowLogs"},
              {"LogGroupName", "TestLogGroup"},
              {"ResourceType", "NetworkInterface"},
              {"ResourceId.1", "eni-aa22bb33"},
              {"ResourceId.2", "eni-22aabb33"},
              {"TrafficType", "REJECT"},
              {"DeliverLogsPermissionArn", "arn%3Aaws%3Aiam%3A%3A123456789101%3Arole%2Fflowlogsrole"}]
        }),
        ?_ec2_test({"This example creates flow log with ClientToken.",
             ?_f(erlcloud_ec2:create_flow_logs(
                    "TestLogGroup", network_interface, ["eni-aa22bb33", "eni-22aabb33"], reject,
                     "arn:aws:iam::123456789101:role/flowlogsrole", "TestClientTokenValue")),
             [{"Action", "CreateFlowLogs"},
              {"LogGroupName", "TestLogGroup"},
              {"ResourceType", "NetworkInterface"},
              {"ResourceId.1", "eni-aa22bb33"},
              {"ResourceId.2", "eni-22aabb33"},
              {"TrafficType", "REJECT"},
              {"DeliverLogsPermissionArn", "arn%3Aaws%3Aiam%3A%3A123456789101%3Arole%2Fflowlogsrole"},
              {"ClientToken", "TestClientTokenValue"}]
        })
    ],
    Response = "
        <CreateFlowLogsResponse xmlns=\"http://ec2.amazonaws.com/doc/2016-11-15/\">
            <requestId>2d96dae3-504b-4fc4-bf50-266EXAMPLE</requestId>
            <unsuccessful/>
            <clientToken>L3rV9xpxjqioMR0mCYQFUV2PB0abfsU1WRAk</clientToken>
            <flowLogIdSet>
                <item>fl-1a2b3c4d</item>
            </flowLogIdSet>
        </CreateFlowLogsResponse>
    ",
    input_tests(Response, Tests).

create_flow_logs_output_tests(_) ->
    Tests = [
        ?_ec2_test(
            {"This example creates flow logs", "
            <CreateFlowLogsResponse xmlns=\"http://ec2.amazonaws.com/doc/2016-11-15/\">
                <requestId>2d96dae3-504b-4fc4-bf50-266EXAMPLE</requestId>
                <unsuccessful/>
                <clientToken>L3rV9xpxjqioMR0mCYQFUV2PB0abfsU1WRAk</clientToken>
                <flowLogIdSet>
                    <item>fl-1a2b3c4d</item>
                </flowLogIdSet>
            </CreateFlowLogsResponse>",
            {ok, [{flow_log_id_set, ["fl-1a2b3c4d"]},
                  {client_token, "L3rV9xpxjqioMR0mCYQFUV2PB0abfsU1WRAk"},
                  {unsuccessful,[[{resource_id,[]},
                     {error,[{code,[]},{message,[]}]}]]}]}}
        )
    ],
    output_tests(?_f(erlcloud_ec2:create_flow_logs(
                        "TestLogGroup", network_interface, ["eni-aa22bb33", "eni-22aabb33"], reject,
                        "arn:aws:iam::123456789101:role/flowlogsrole")), Tests).

delete_flow_logs_input_tests(_) ->
    Tests = [
        ?_ec2_test({"This example deletes flow logs.",
             ?_f(erlcloud_ec2:delete_flow_logs(["fl-ab12cd34", "fl-123abc45"])),
            [{"Action", "DeleteFlowLogs"},
             {"FlowLogId.1", "fl-ab12cd34"},
             {"FlowLogId.2", "fl-123abc45"}]
        })
    ],
    Response = "
        <DeleteFlowLogsResponse xmlns=\"http://ec2.amazonaws.com/doc/2016-11-15/\">
            <requestId>c5c4f51f-f4e9-42bc-8700-EXAMPLE</requestId>
            <unsuccessful/>
        </DeleteFlowLogsResponse>
    ",
    input_tests(Response, Tests).

delete_flow_logs_output_tests(_) ->
    Tests = [
        ?_ec2_test(
            {"This example deletes flow log", "
            <DeleteFlowLogsResponse xmlns=\"http://ec2.amazonaws.com/doc/2016-11-15/\">
                <requestId>c5c4f51f-f4e9-42bc-8700-EXAMPLE</requestId>
                <unsuccessful/>
            </DeleteFlowLogsResponse>",
            {ok, [{unsuccessful,[[{resource_id,[]},
                     {error,[{code,[]},{message,[]}]}]]}]}})
    ],
    output_tests(?_f(erlcloud_ec2:delete_flow_logs(["fl-ab12cd34"])), Tests).

describe_flow_logs_input_tests(_) ->
    Tests =
        [?_ec2_test(
            {"This example describes all flow logs in a region.",
             ?_f(erlcloud_ec2:describe_flow_logs()),
             [{"Action", "DescribeFlowLogs"}]}),
         ?_ec2_test({"This example describes fl-ab12cd34 flow log.",
             ?_f(erlcloud_ec2:describe_flow_logs(["fl-ab12cd34"], [])),
             [{"Action", "DescribeFlowLogs"},
              {"FlowLogId.1", "fl-ab12cd34"}]}),
         ?_ec2_test({"This example describes fl-ab12cd34 and fl-123abc45 flow logs.",
             ?_f(erlcloud_ec2:describe_flow_logs(["fl-ab12cd34", "fl-123abc45"], [])),
             [{"Action", "DescribeFlowLogs"},
              {"FlowLogId.1", "fl-ab12cd34"},
              {"FlowLogId.2", "fl-123abc45"}]}),
         ?_ec2_test({"This example retrieves 10 flow logs.",
             ?_f(erlcloud_ec2:describe_flow_logs([{'traffic-type', "ALL"}])),
             [{"Action", "DescribeFlowLogs"},
              {"Filter.1.Name", "traffic-type"},
              {"Filter.1.Value.1", "ALL"}]}),
         ?_ec2_test({"This example retrieves 10 flow logs using NextToken argument.",
             ?_f(erlcloud_ec2:describe_flow_logs(10, "eyJ2IjoiMSIsInMiOjEsImMiOiI3WmlqMWVFbEM4cHdzZnRlcHVlc3pCQWNSdlZGbFBMVWZaNzZkcEpCcmdQOExlSWVjUUdIQnlCTVJmODVWWlZ6N1JpLUVXbTJrYXpSMGVVU05IclZUM3RXd3hGY3UzQTZhSkhPR3pvMnJPN1htclpjYk40T2VIMUIifQ")),
             [{"Action", "DescribeFlowLogs"},
              {"MaxResults", "10"},
              {"NextToken", "eyJ2IjoiMSIsInMiOjEsImMiOiI3WmlqMWVFbEM4cHdzZnRlcHVlc3pCQWNSdlZGbFBMVWZaNzZkcEpCcmdQOExlSWVjUUdIQnlCTVJmODVWWlZ6N1JpLUVXbTJrYXpSMGVVU05IclZUM3RXd3hGY3UzQTZhSkhPR3pvMnJPN1htclpjYk40T2VIMUIifQ"}]})
        ],
    Response = "
        <DescribeFlowLogsResponse xmlns=\"http://ec2.amazonaws.com/doc/2016-11-15/\">
            <requestId>3cb46f23-099e-4bf0-891c-EXAMPLE</requestId>
            <flowLogSet>
                <item>
                    <deliverLogsErrorMessage>Access error</deliverLogsErrorMessage>
                    <resourceId>vpc-1a2b3c4d</resourceId>
                    <deliverLogsPermissionArn>arn:aws:iam::123456789101:role/flowlogsrole</deliverLogsPermissionArn>
                    <flowLogStatus>ACTIVE</flowLogStatus>
                    <creationTime>2015-05-19T08:48:59Z</creationTime>
                    <logGroupName>FlowLogsForSubnetA</logGroupName>
                    <trafficType>ALL</trafficType>
                    <flowLogId>fl-ab12cd34</flowLogId>
                </item>
                <item>
                    <resourceId>vpc-1122bbcc</resourceId>
                    <deliverLogsPermissionArn>arn:aws:iam::123456789101:role/flowlogsrole</deliverLogsPermissionArn>
                    <flowLogStatus>ACTIVE</flowLogStatus>
                    <creationTime>2015-05-19T10:42:32Z</creationTime>
                    <logGroupName>FlowLogsForSubnetB</logGroupName>
                    <trafficType>ALL</trafficType>
                    <flowLogId>fl-123abc45</flowLogId>
                </item>
            </flowLogSet>
        </DescribeFlowLogsResponse>
    ",
    input_tests(Response, Tests).

describe_flow_logs_output_tests(_) ->
    Tests =
        [?_ec2_test(
            {"This example descirbes all flow logs", "
                <DescribeFlowLogsResponse xmlns=\"http://ec2.amazonaws.com/doc/2016-11-15/\">
                    <requestId>3cb46f23-099e-4bf0-891c-EXAMPLE</requestId>
                    <flowLogSet>
                        <item>
                            <deliverLogsErrorMessage>Access error</deliverLogsErrorMessage>
                            <resourceId>vpc-1a2b3c4d</resourceId>
                            <deliverLogsPermissionArn>arn:aws:iam::123456789101:role/flowlogsrole</deliverLogsPermissionArn>
                            <flowLogStatus>ACTIVE</flowLogStatus>
                            <creationTime>2015-05-19T08:48:59Z</creationTime>
                            <logGroupName>FlowLogsForSubnetA</logGroupName>
                            <trafficType>ALL</trafficType>
                            <flowLogId>fl-ab12cd34</flowLogId>
                        </item>
                        <item>
                            <resourceId>vpc-1122bbcc</resourceId>
                            <deliverLogsPermissionArn>arn:aws:iam::123456789101:role/flowlogsrole</deliverLogsPermissionArn>
                            <flowLogStatus>ACTIVE</flowLogStatus>
                            <creationTime>2015-05-19T10:42:32Z</creationTime>
                            <logGroupName>FlowLogsForSubnetB</logGroupName>
                            <trafficType>ALL</trafficType>
                            <flowLogId>fl-123abc45</flowLogId>
                        </item>
                    </flowLogSet>
                </DescribeFlowLogsResponse>",
                {ok, [[{deliver_logs_error_message, "Access error"},
                      {resource_id,"vpc-1a2b3c4d"},
                      {deliver_logs_permission_arn,"arn:aws:iam::123456789101:role/flowlogsrole"},
                      {flow_log_status,"ACTIVE"},
                      {creation_time,{{2015,05,19},{8,48,59}}},
                      {log_group_name,"FlowLogsForSubnetA"},
                      {traffic_type,"ALL"},
                      {flow_log_id,"fl-ab12cd34"}],
                     [{deliver_logs_error_message,[]},
                      {resource_id,"vpc-1122bbcc"},
                      {deliver_logs_permission_arn,"arn:aws:iam::123456789101:role/flowlogsrole"},
                      {flow_log_status,"ACTIVE"},
                      {creation_time,{{2015,05,19},{10,42,32}}},
                      {log_group_name,"FlowLogsForSubnetB"},
                      {traffic_type,"ALL"},
                      {flow_log_id,"fl-123abc45"}]]}}
         )
    ],
    output_tests(?_f(erlcloud_ec2:describe_flow_logs()), Tests).

describe_instances_test_() ->
    Tests = [{30,6},{22,5},{3,7},{10,10},{0,10},{0,5},{0,1000}],
    {timeout, 60, fun () -> test_pagination(Tests, generate_instances_response, describe_instances, [], [[]]) end}
.

describe_instances_boundaries_test_() ->
    [
        ?_assertException(error, function_clause, erlcloud_ec2:describe_instances([], 4, undefined)),
        ?_assertException(error, function_clause, erlcloud_ec2:describe_instances([], 1001, undefined))
    ].

describe_snapshots_test_() ->
    Tests = [{30,6},{22,5},{3,7},{10,10},{0,10},{0,5},{0,1000}],
    {timeout, 60, fun () -> test_pagination(Tests, generate_snapshots_response, describe_snapshots, [], ["self", []]) end}
.

describe_snapshots_boundaries_test_() ->
    [
        ?_assertException(error, function_clause, erlcloud_ec2:describe_snapshots("self", [], 4, undefined)),
        ?_assertException(error, function_clause, erlcloud_ec2:describe_snapshots("self", [], 1001, undefined))
    ].

describe_tags_test_() ->
    Tests = [{30,6},{22,5},{3,7},{10,10},{0,10},{0,5},{0,1000}],
    {timeout, 60, fun () -> test_pagination(Tests, generate_tags_response, describe_tags, [], [[]]) end}
.

describe_tags_boundaries_test_() ->
    [
        ?_assertException(error, function_clause, erlcloud_ec2:describe_tags([], 4, undefined)),
        ?_assertException(error, function_clause, erlcloud_ec2:describe_tags([], 1001, undefined))
    ].
describe_spot_price_history_test_() ->
    Tests = [{30,6},{22,5},{3,7},{10,10},{0,10},{0,5},{0,1000}],
    {timeout, 60, fun () -> test_pagination(Tests, generate_spot_price_history_response, describe_spot_price_history, [], ["", "", [], ""]) end}
.

describe_spot_price_history_boundaries_test_() ->
    [
        ?_assertException(error, function_clause, erlcloud_ec2:describe_spot_price_history(["", "", [], ""], 4, undefined)),
        ?_assertException(error, function_clause, erlcloud_ec2:describe_spot_price_history(["", "", [], ""], 1001, undefined))
    ].

describe_instance_status_test_() ->
    Tests = [{30,6},{22,5},{3,7},{10,10},{0,10},{0,5},{0,1000}],
    {timeout, 60, fun () -> test_pagination(Tests, generate_instance_status_response, describe_instance_status, [[], []], [[], []]) end}
.

describe_instance_status_boundaries_test_() ->
    [
        ?_assertException(error, function_clause, erlcloud_ec2:describe_instance_status([], [], 4, undefined)),
        ?_assertException(error, function_clause, erlcloud_ec2:describe_instance_status([], [], 1001, undefined))
    ].

describe_reserved_instances_offerings_test_() ->
    Tests = [{30,6},{22,5},{3,7},{10,10},{0,10},{0,5},{0,1000}],
    {timeout, 60, fun () -> test_pagination(Tests, generate_reserved_instances_offerings_response, describe_reserved_instances_offerings, [], [[]]) end}
.

describe_reserved_instances_offerings_boundaries_test_() ->
    [
        ?_assertException(error, function_clause, erlcloud_ec2:describe_reserved_instances_offerings([], 4, undefined)),
        ?_assertException(error, function_clause, erlcloud_ec2:describe_reserved_instances_offerings([], 1001, undefined))
    ].

generate_one_instance(N) ->
    "<item>
        <reservationId>r-69</reservationId>
        <ownerId>12345</ownerId>
        <groupSet/>
        <instancesSet>
            <item>
                <instanceId>i-" ++ integer_to_list(N) ++ "f</instanceId>
                <imageId>ami-123</imageId>
                <instanceState>
                    <code>48</code>
                    <name>terminated</name>
                </instanceState>
                <privateDnsName/>
                <dnsName/>
                <reason>Because</reason>
                <keyName>Key</keyName>
                <amiLaunchIndex>123</amiLaunchIndex>
                <productCodes/>
                <instanceType>t2.small</instanceType>
                <launchTime>2016-09-26T10:35:00.000Z</launchTime>
                <placement>
                    <availabilityZone>us-east-1a</availabilityZone>
                    <groupName/>
                    <tenancy>default</tenancy>
                </placement>
                <monitoring>
                    <state>disabled</state>
                </monitoring>
                <groupSet/>
                <stateReason>
                    <code>code</code>
                    <message>message</message>
                </stateReason>
                <architecture>x86_64</architecture>
                <rootDeviceType>ebs</rootDeviceType>
                <rootDeviceName>/dev/null</rootDeviceName>
                <blockDeviceMapping/>
                <virtualizationType>hvm</virtualizationType>
                <clientToken>123-123-12345</clientToken>
                <tagSet/>
                <hypervisor>xen</hypervisor>
                <networkInterfaceSet/>
                <ebsOptimized>false</ebsOptimized>
            </item>
        </instancesSet>
    </item>".

generate_instances_response(Start, End, NT) ->
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
     <DescribeInstancesResponse
         xmlns=\"http://ec2.amazonaws.com/doc/2014-10-01/\">
         <requestId>abcdef-12345</requestId>
         <reservationSet>" ++
        generate_items(generate_one_instance, Start, End) ++
        "</reservationSet>" ++
        generate_next_token_xml(NT) ++
        "</DescribeInstancesResponse>".

generate_one_snapshot(N) ->
    "<item>
        <snapshotId>snap-" ++ integer_to_list(N) ++ "</snapshotId>
        <volumeId>vol-123</volumeId>
        <status>completed</status>
        <startTime>2016-10-04T17:02:55.000Z</startTime>
        <progress>100%</progress>
        <ownerId>12345</ownerId>
        <volumeSize>123</volumeSize>
        <description>bla</description>
        <encrypted>false</encrypted>
    </item>".

generate_snapshots_response(Start, End, NT) ->
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
     <DescribeSnapshotsResponse
         xmlns=\"http://ec2.amazonaws.com/doc/2014-10-01/\">
         <requestId>12345</requestId>
         <snapshotSet>" ++
        generate_items(generate_one_snapshot, Start, End) ++
        "</snapshotSet>" ++
        generate_next_token_xml(NT) ++
        "</DescribeSnapshotsResponse>".

generate_one_tag(N) ->
    "<item>
        <resourceId>ami-" ++ integer_to_list(N) ++ "</resourceId>
        <resourceType>type</resourceType>
        <key>key</key>
        <value>value</value>
     </item>".

generate_tags_response(Start, End, NT) ->
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
     <DescribeTagsResponse
         xmlns=\"http://ec2.amazonaws.com/doc/2014-10-01/\">
         <requestId>e64e4387-5542-4765-bd0b-12b7e7ba902d</requestId>
         <tagSet>" ++
        generate_items(generate_one_tag, Start, End) ++
        "</tagSet>" ++
        generate_next_token_xml(NT) ++
        "</DescribeTagsResponse>".

generate_one_spot_price(N) ->
    "<item>
        <instanceType>m8.xlarge</instanceType>
        <productDescription>Product</productDescription>
        <spotPrice>0." ++ integer_to_list(N) ++"</spotPrice>
        <timestamp>2016-10-12T16:22:33.000Z</timestamp>
        <availabilityZone>us-west-1d</availabilityZone>
     </item>".

generate_spot_price_history_response(Start, End, NT) ->
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
     <DescribeSpotPriceHistoryResponse
         xmlns=\"http://ec2.amazonaws.com/doc/2014-10-01/\">
         <requestId>12345678 </requestId>
         <spotPriceHistorySet>" ++
        generate_items(generate_one_spot_price, Start, End) ++
        "</spotPriceHistorySet>" ++
        generate_next_token_xml(NT) ++
        "</DescribeSpotPriceHistoryResponse>".

generate_one_instance_status(N) ->
    "<item>
        <instanceId>i-" ++ integer_to_list(N) ++ "</instanceId>
        <availabilityZone>us-east-1d</availabilityZone>
        <instanceState>
            <code>16</code>
            <name>running</name>
        </instanceState>
        <systemStatus>
            <status>ok</status>
            <details/>
        </systemStatus>
        <instanceStatus>
            <status>ok</status>
            <details/>
        </instanceStatus>
     </item>".

generate_instance_status_response(Start, End, NT) ->
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
     <DescribeInstanceStatusResponse
         xmlns=\"http://ec2.amazonaws.com/doc/2014-10-01/\">
         <requestId>12345</requestId>
         <instanceStatusSet>" ++
             generate_items(generate_one_instance_status, Start, End) ++
        "</instanceStatusSet>" ++
         generate_next_token_xml(NT) ++
        "</DescribeInstanceStatusResponse>".

generate_one_reserved_instance_offering(N) ->
    "<item>
           <reservedInstancesListingId>" ++ integer_to_list(N) ++ "</reservedInstancesListingId>
           <reservedInstancesId>12345</reservedInstancesId>
           <createDate>2016-10-13T17:38:57.000Z</createDate>
           <updateDate>2016-10-13T17:38:58.000Z</updateDate>
           <status>active</status>
           <statusMessage>ACTIVE</statusMessage>
           <instanceCounts/>
           <priceSchedules/>
           <tagSet/>
           <clientToken>myclienttoken1</clientToken>
    </item>".

generate_reserved_instances_offerings_response(Start, End, NT) ->
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
     <DescribeReservedInstancesOfferingsResponse
         xmlns=\"http://ec2.amazonaws.com/doc/2014-10-01/\">
          <requestId>12345</requestId>
          <reservedInstancesOfferingsSet>" ++
        generate_items(generate_one_reserved_instance_offering, Start, End) ++
        "</reservedInstancesOfferingsSet>" ++
        generate_next_token_xml(NT) ++
        "</DescribeReservedInstancesOfferingsResponse>".

generate_items(GenerateFunction, K, N) ->
    generate_items(GenerateFunction, K, N, []).

generate_items(_GenerateFunction, K, N, Acc) when K > N ->
    Acc;
generate_items(GenerateFunction, K, N, Acc) ->
    generate_items(GenerateFunction, K+1, N, Acc ++ apply(?MODULE, GenerateFunction, [K])).

generate_next_token_xml(undefined) ->
    "";
generate_next_token_xml(NT) ->
    NTString = integer_to_list(NT),
    "<nextToken>" ++ NTString ++ "</nextToken>".

test_pagination(TestCases, ResponseGenerator, OriginalFunction) ->
    test_pagination(TestCases, ResponseGenerator, OriginalFunction, [], []).

test_pagination([], _, _, _, _) -> ok;
test_pagination([{TotalResults, ResultsPerPage} | Rest], ResponseGenerator, OriginalFunction, NormalParams, PagedParams) ->
    meck:new(erlcloud_aws, [passthrough]),
    meck:expect(erlcloud_aws, aws_request_xml4,
        fun(_,_,_,Params,_,_) ->
            NextTokenString = proplists:get_value("NextToken", Params),
            MaxResults = proplists:get_value("MaxResults", Params),
            {Start, End, NT} =
                case {NextTokenString, MaxResults} of
                    {undefined, undefined} ->
                        {1, TotalResults, undefined};
                    {undefined, MaxResults} ->
                        case MaxResults >= TotalResults of
                            true ->
                                {1, TotalResults, undefined};
                            false ->
                                {1, MaxResults, MaxResults + 1}
                        end;
                    {NextTokenString, MaxResults} ->
                        NextToken = list_to_integer(NextTokenString),
                        case NextToken + MaxResults - 1 >= TotalResults of
                            true ->
                                {NextToken, TotalResults, undefined};
                            false ->
                                {NextToken, NextToken + MaxResults - 1,  NextToken + MaxResults}
                        end
                end,
            Response = apply(?MODULE, ResponseGenerator, [Start, End, NT]),
            {ok, element(1, xmerl_scan:string(Response))}
        end),
    {ok, AllResults} = apply(erlcloud_ec2, OriginalFunction, NormalParams),
    {ok, PagedResults} = describe_all(OriginalFunction, PagedParams, ResultsPerPage),
    meck:unload(erlcloud_aws),
    ?assertEqual(length(AllResults), TotalResults),
    ?assertEqual(length(AllResults), length(PagedResults)),
    ?assert(lists_are_the_same(AllResults, PagedResults)),
    test_pagination(Rest, ResponseGenerator, OriginalFunction, NormalParams, PagedParams).

describe_all(DescribeFunction, Params, MaxResults) ->
    describe_all(DescribeFunction, Params, MaxResults, undefined, []).

describe_all(DescribeFunction, Params, MaxResults, NextToken, Acc) ->
    AllParams = Params ++ [MaxResults, NextToken],
    case apply(erlcloud_ec2, DescribeFunction, AllParams) of
        {ok, Res, undefined} ->
            {ok, Acc ++ Res};
        {ok, Res, NewNextToken} ->
            describe_all(DescribeFunction, Params, MaxResults, NewNextToken, Acc ++ Res)
    end.

lists_are_the_same(List1, List2) ->
    lists:sort(List1) =:= lists:sort(List2).

