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
      fun describe_images_tests/1
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
