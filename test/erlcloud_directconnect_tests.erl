%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_directconnect_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").

-define(_directconnect_test(T), {?LINE, T}).
-define(_f(F), fun() -> F end).

-export([validate_body/2]).

%%%===================================================================
%%% Test entry points
%%%===================================================================

operation_test_() ->
    {
        foreach,
        fun start/0,
        fun stop/1,
        [
            fun describe_connections_input_tests/1,
            fun describe_connections_output_tests/1,
            fun describe_connections_on_interconnect_input_tests/1,
            fun describe_connections_on_interconnect_output_tests/1,
            fun describe_interconnects_input_tests/1,
            fun describe_interconnects_output_tests/1,
            fun describe_locations_input_tests/1,
            fun describe_locations_output_tests/1,
            fun describe_virtual_gateways_input_tests/1,
            fun describe_virtual_gateways_output_tests/1,
            fun describe_virtual_interfaces_input_tests/1,
            fun describe_virtual_interfaces_output_tests/1
        ]
    }.

start() ->
    meck:new(erlcloud_httpc),
    ok.

stop(_) ->
    meck:unload(erlcloud_httpc).

%%%===================================================================
%%% Input test helpers
%%%===================================================================

-type expected_body() :: string().

sort_json([{_, _} | _] = Json) ->
    %% Value is an object
    SortedChildren = [{K, sort_json(V)} || {K,V} <- Json],
    lists:keysort(1, SortedChildren);
sort_json([_|_] = Json) ->
    %% Value is an array
    [sort_json(I) || I <- Json];
sort_json(V) ->
    V.

%% verifies that the parameters in the body match the expected parameters
-spec validate_body(binary(), expected_body()) -> ok.
validate_body(Body, Expected) ->
    Want = sort_json(jsx:decode(list_to_binary(Expected))),
    Actual = sort_json(jsx:decode(Body)),
    case Want =:= Actual of
        true -> ok;
        false ->
            ?debugFmt("~nEXPECTED~n~p~nACTUAL~n~p~n", [Want, Actual])
    end,
    ?assertEqual(Want, Actual).

%% returns the mock of the erlcloud_httpc function input tests expect to be called.
%% Validates the request body and responds with the provided response.
-spec input_expect(string(), expected_body()) -> fun().
input_expect(Response, Expected) ->
    fun(_Url, post, _Headers, Body, _Timeout, _Config) ->
            validate_body(Body, Expected),
            {ok, {{200, "OK"}, [], list_to_binary(Response)}}
    end.

%% input_test converts an input_test specifier into an eunit test generator
-type input_test_spec() :: {pos_integer(), {fun(), expected_body()} | {string(), fun(), expected_body()}}.
-spec input_test(string(), input_test_spec()) -> tuple().
input_test(Response, {Line, {Description, Fun, Expected}}) when
      is_list(Description) ->
    {Description,
     {Line,
      fun() ->
              meck:expect(erlcloud_httpc, request, input_expect(Response, Expected)),
              erlcloud_directconnect:configure(string:copies("A", 20), string:copies("a", 40)),
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
              erlcloud_directconnect:configure(string:copies("A", 20), string:copies("a", 40)),
              Actual = Fun(),
              case Result =:= Actual of
                  true -> ok;
                  false ->
                      ?debugFmt("~nEXPECTED~n~p~nACTUAL~n~p~n", [Result, Actual])
              end,
              ?assertEqual(Result, Actual)
      end}}.

%% output_tests converts a list of output_test specifiers into an eunit test generator
-spec output_tests(fun(), [output_test_spec()]) -> [term()].
output_tests(Fun, Tests) ->
    [output_test(Fun, Test) || Test <- Tests].

-define(DESCRIBE_CONNECTIONS_BY_ID_RESP, "
{
    \"connections\": [
        {
            \"ownerAccount\": \"451283894028\",
            \"connectionId\": \"dxcon-ffjyoq6j\",
            \"connectionState\": \"deleted\",
            \"bandwidth\": \"1Gbps\",
            \"location\": \"EqDC2\",
            \"connectionName\": \"test\",
            \"region\": \"us-east-1\"
        }
    ]
}").

-define(DESCRIBE_CONNECTIONS_RESP, "
{
    \"connections\": [
        {
            \"ownerAccount\": \"451283894028\",
            \"connectionId\": \"dxcon-ffjyoq6j\",
            \"connectionState\": \"deleted\",
            \"bandwidth\": \"1Gbps\",
            \"location\": \"EqDC2\",
            \"connectionName\": \"test\",
            \"region\": \"us-east-1\"
        },
        {
            \"ownerAccount\": \"451283894028\",
            \"connectionId\": \"dxcon-adefzxcv\",
            \"connectionState\": \"pending\",
            \"bandwidth\": \"2Gbps\",
            \"location\": \"EqDC2\",
            \"partnerName\" : \"partner\",
            \"connectionName\": \"test2\",
            \"region\": \"us-east-1\"
        }
    ]
}").


%% http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DescribeConnections.html
describe_connections_input_tests(_) ->
    Tests =
        [?_directconnect_test(
            {"DescribeConnections by ID example request",
             ?_f(erlcloud_directconnect:describe_connections("dxcon-ffjyoq6j")), "
{
  \"connectionId\": \"dxcon-ffjyoq6j\"
}"
            }),
        ?_directconnect_test(
            {"DescribeConnections example request",
             ?_f(erlcloud_directconnect:describe_connections()),
             "{}"
            })
        ],
    input_tests("{\"connections\": []}", Tests).


describe_connections_output_tests(_) ->
    Tests =
        [?_directconnect_test(
            {"DescribeConnections one element example response", ?DESCRIBE_CONNECTIONS_BY_ID_RESP,
        {ok, [[{bandwidth, <<"1Gbps">>},
              {connection_id, <<"dxcon-ffjyoq6j">>},
              {connection_name, <<"test">>},
              {connection_state, <<"deleted">>},
              {location, <<"EqDC2">>},
              {owner_account, <<"451283894028">>}, 
              {region, <<"us-east-1">>}]]}}),
        ?_directconnect_test(
            {"DescribeConnections example response", ?DESCRIBE_CONNECTIONS_RESP,
        {ok, [
              [{bandwidth, <<"1Gbps">>},
              {connection_id, <<"dxcon-ffjyoq6j">>},
              {connection_name, <<"test">>},
              {connection_state, <<"deleted">>},
              {location, <<"EqDC2">>},
              {owner_account, <<"451283894028">>}, 
              {region, <<"us-east-1">>}],
          
              [{bandwidth,<<"2Gbps">>},
              {connection_id,<<"dxcon-adefzxcv">>},
              {connection_name,<<"test2">>},
              {connection_state,<<"pending">>},
              {location,<<"EqDC2">>},
              {owner_account,<<"451283894028">>},
              {partner_name,<<"partner">>},
              {region,<<"us-east-1">>}]
             ]}})
        ],
    output_tests(?_f(erlcloud_directconnect:describe_connections()), Tests).

%% http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DescribeConnectionsOnInterconnect.html
describe_connections_on_interconnect_input_tests(_) ->
    Tests =
        [?_directconnect_test(
            {"DescribeConnectionsOnInterconnect example request",
             ?_f(erlcloud_directconnect:describe_connections_on_interconnect("inter-ffjyoq6j")), "
{
  \"interconnectId\": \"inter-ffjyoq6j\"
}"
            })
        ],
    input_tests("{\"connections\": []}", Tests).


describe_connections_on_interconnect_output_tests(_) ->
    Tests =
        [?_directconnect_test(
            {"DescribeConnectionsOnInterconnect one element example response", ?DESCRIBE_CONNECTIONS_BY_ID_RESP,
        {ok, [[{bandwidth, <<"1Gbps">>},
              {connection_id, <<"dxcon-ffjyoq6j">>},
              {connection_name, <<"test">>},
              {connection_state, <<"deleted">>},
              {location, <<"EqDC2">>},
              {owner_account, <<"451283894028">>}, 
              {region, <<"us-east-1">>}]]}})
        ],
    output_tests(?_f(erlcloud_directconnect:describe_connections_on_interconnect("inter-ffjyoq6j")), Tests).

-define(DESCRIBE_INTERCONNECTIONS_BY_ID_RESP, "
{
    \"interconnects\": [
        {
            \"interconnectId\": \"inter-ffjyoq6j\",
            \"interconnectState\": \"deleted\",
            \"bandwidth\": \"1Gbps\",
            \"location\": \"EqDC2\",
            \"interconnectName\": \"test\",
            \"region\": \"us-east-1\"
        }
    ]
}").

%% http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DescribeInterconnects.html
describe_interconnects_input_tests(_) ->
    Tests =
        [?_directconnect_test(
            {"DescribeInterconnects example request",
             ?_f(erlcloud_directconnect:describe_interconnects()), "{}"
            }),
        ?_directconnect_test(
            {"DescribeInterconnects example request",
             ?_f(erlcloud_directconnect:describe_interconnects("inter-ffjyoq6j")), "
{
  \"interconnectId\": \"inter-ffjyoq6j\"
}"
            }),
        ?_directconnect_test(
            {"DescribeInterconnects example request",
             ?_f(erlcloud_directconnect:describe_interconnects()), "{}"
            })
        ],
    input_tests("{\"interconnects\": []}", Tests).


describe_interconnects_output_tests(_) ->
    Tests =
        [?_directconnect_test(
            {"DescribeInterconnects example response", ?DESCRIBE_INTERCONNECTIONS_BY_ID_RESP,
        {ok, [[{bandwidth, <<"1Gbps">>},
              {interconnect_id, <<"inter-ffjyoq6j">>},
              {interconnect_name, <<"test">>},
              {interconnect_state, <<"deleted">>},
              {location, <<"EqDC2">>},
              {region, <<"us-east-1">>}]]}})
        ],
    output_tests(?_f(erlcloud_directconnect:describe_interconnects()), Tests).

-define(DESCRIBE_LOCATIONS_RESP, "
{
    \"locations\": [
        {
            \"locationName\": \"Equinix DC1 - DC6, DC10, Ashburn, VA\",
            \"locationCode\": \"EqDC2\"
        },
        {
            \"locationName\": \"CoreSite 32 Avenue of the Americas, New York, NY\",
            \"locationCode\": \"CS32A\"
        }
    ]
}").

%% http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DescribeLocations.html
describe_locations_input_tests(_) ->
    Tests =
        [?_directconnect_test(
            {"DescribeLocations example request",
             ?_f(erlcloud_directconnect:describe_locations()), "{}"
            })
        ],
    input_tests("{\"locations\": []}", Tests).


describe_locations_output_tests(_) ->
    Tests =
        [?_directconnect_test(
            {"DescribeLocations example response", ?DESCRIBE_LOCATIONS_RESP,
         {ok, [
                [
                    {location_code, <<"EqDC2">>},
                    {location_name, <<"Equinix DC1 - DC6, DC10, Ashburn, VA">>}
                    
                ],
                [
                    {location_code, <<"CS32A">>},
                    {location_name, <<"CoreSite 32 Avenue of the Americas, New York, NY">>}
                ]
             ]}})
        ],
    output_tests(?_f(erlcloud_directconnect:describe_locations()), Tests).

-define(DESCRIBE_VIRTUAL_GATEWAYS_RESP, "
{
    \"virtualGateways\": [
        {
            \"virtualGatewayId\": \"vgw-123er56\",
            \"virtualGatewayState\": \"Available\"
        }
    ]
}").

%% http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DescribeVirtualGateways.html
describe_virtual_gateways_input_tests(_) ->
    Tests =
        [?_directconnect_test(
            {"DescribeVirtualGateways example request",
             ?_f(erlcloud_directconnect:describe_virtual_gateways()), "{}"
            })
        ],
    input_tests("{\"virtualGateways\": []}", Tests).


describe_virtual_gateways_output_tests(_) ->
    Tests =
        [?_directconnect_test(
            {"DescribeVirtualGateways example response", ?DESCRIBE_VIRTUAL_GATEWAYS_RESP,
         {ok, [
                [
                    {virtual_gateway_id, <<"vgw-123er56">>},
                    {virtual_gateway_state, <<"Available">>}
                ]
             ]}})
        ],
    output_tests(?_f(erlcloud_directconnect:describe_virtual_gateways()), Tests).

-define(DESCRIBE_VIRTUAL_INTERFACES_RESP, "
{
    \"virtualInterfaces\": [
        {
            \"amazonAddress\": \"192.168.1.1/30\",
            \"asn\": 65000,
            \"authKey\": \"asdf34example\",
            \"connectionId\": \"dxcon-fg5678gh\",
            \"customerAddress\": \"192.168.1.2/30\",
            \"customerRouterConfig\": \"info\",
            \"location\": \"EqSV5\",
            \"ownerAccount\": \"98792387498\",
            \"routeFilterPrefixes\": [
                {
                    \"cidr\": \"10.10.10.0/24\"
                }
            ],
            \"virtualGatewayId\": \"vgw-123er56\",
            \"virtualInterfaceId\": \"dxvif-123dfg56\",
            \"virtualInterfaceName\": \"My VPC\",
            \"virtualInterfaceState\": \"confirming\",
            \"virtualInterfaceType\": \"private\",
            \"vlan\": 101
        }
    ]
}").

%% http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DescribeVirtualInterfaces.html
describe_virtual_interfaces_input_tests(_) ->
    Tests =
        [?_directconnect_test(
            {"DescribeVirtualInterfaces example request",
             ?_f(erlcloud_directconnect:describe_virtual_interfaces()), "{}"
            }),
         ?_directconnect_test(
            {"DescribeVirtualInterfaces by connectionId example request",
             ?_f(erlcloud_directconnect:describe_virtual_interfaces("dxcon-fg5678gh")), "
{
  \"connectionId\": \"dxcon-fg5678gh\"
}"
            }),
        ?_directconnect_test(
            {"DescribeVirtualInterfaces by connectionId example request",
             ?_f(erlcloud_directconnect:describe_virtual_interfaces("dxcon-fg5678gh", "dxvif-123dfg56")), "
{
  \"connectionId\": \"dxcon-fg5678gh\",
  \"virtualInterfaceId\": \"dxvif-123dfg56\"
}"
            })
        ],
    input_tests("{\"virtualInterfaces\": []}", Tests).


describe_virtual_interfaces_output_tests(_) ->
    Tests =
        [?_directconnect_test(
            {"DescribeVirtualInterfaces example response", ?DESCRIBE_VIRTUAL_INTERFACES_RESP,
         {ok, [
                [
                    {amazon_address, <<"192.168.1.1/30">>},
                    {asn, 65000},
                    {auth_key, <<"asdf34example">>},
                    {connection_id, <<"dxcon-fg5678gh">>},
                    {customer_address, <<"192.168.1.2/30">>},
                    {customer_router_config, <<"info">>},
                    {location, <<"EqSV5">>},
                    {owner_account, <<"98792387498">>},
                    {route_filter_prefixes, [[{cidr, <<"10.10.10.0/24">>}]]},
                    {virtual_gateway_id, <<"vgw-123er56">>},
                    {virtual_interface_id, <<"dxvif-123dfg56">>},
                    {virtual_interface_name, <<"My VPC">>},
                    {virtual_interface_state, <<"confirming">>},
                    {virtual_interface_type, <<"private">>},
                    {vlan, 101}
                ]
             ]}})
        ],
    output_tests(?_f(erlcloud_directconnect:describe_virtual_interfaces()), Tests).
