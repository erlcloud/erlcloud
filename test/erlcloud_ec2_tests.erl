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
                            
%%%===================================================================
%%% Test entry points
%%%===================================================================

describe_tags_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun describe_tags_input_tests/1,
      fun describe_tags_output_tests/1,
      fun describe_vpn_gateways_tests/1,
      fun describe_customer_gateways_tests/1,
      fun describe_vpn_connections_tests/1,
      fun describe_images_tests/1]}.

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
    case string:tokens(Param, "=") of
        [Key, Value] -> 
            ok;
        [Key] ->
            Value = "",
            ok
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


describe_images_tests(_) ->
    Tests = 
        [?_ec2_test(
            {"This example describes AMIs.", "
<DescribeImagesResponse xmlns=\"http://ec2.amazonaws.com/doc/2015-04-15/\">
   <requestId>59dbff89-35bd-4eac-99ed-be587EXAMPLE</requestId> 
   <imagesSet>
      <item>
         <imageId>ami-1a2b3c4d</imageId>
         <imageLocation>ec2-public-windows-images/Server2003r2-x86_64-Win-v1.07.manifest.xml</imageLocation>
         <imageState>available</imageState>
         <imageOwnerId>123456789012</imageOwnerId>
         <isPublic>true</isPublic>
         <architecture>x86_64</architecture>
         <imageType>machine</imageType>
         <platform>windows</platform>
         <imageOwnerAlias>amazon</imageOwnerAlias>
         <rootDeviceType>instance-store</rootDeviceType>
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
                     {root_device_name, []},
                     {platform, "windows"},
                     {block_device_mapping, []},
                     {product_codes, []}
                ]]}})],
    
    %% Remaining AWS API examples return subsets of the same data
    output_tests(?_f(erlcloud_ec2:describe_images()), Tests).
