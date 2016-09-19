-module(erlcloud_directconnect).

%%% Library initialization.
-export([
    new/2, new/4, configure/2
]).

%% DirectConnect API
-export([
    describe_connections/0, describe_connections/1, describe_connections/2,
    describe_connections_on_interconnect/1, describe_connections_on_interconnect/2,
    describe_interconnects/0, describe_interconnects/1, describe_interconnects/2,
    describe_locations/0, describe_locations/1,
    describe_virtual_gateways/0, describe_virtual_gateways/1,
    describe_virtual_interfaces/0, describe_virtual_interfaces/1, describe_virtual_interfaces/2
]).

-define(API_VERSION, "2012-10-25").
-define(SERVICE_NAME, "directconnect").
-define(API_PREFIX, "OvertureService.").

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

%%%------------------------------------------------------------------------------
%%% Shared types
%%%------------------------------------------------------------------------------
-type connection() :: proplist().
-type interconnect() :: proplist().
-type location() :: proplist().
-type virtual_gateway() :: proplist().
-type virtual_interface() :: proplist().

-export_type([
    connection/0,
    interconnect/0,
    location/0,
    virtual_gateway/0,
    virtual_interface/0
]).

%%%------------------------------------------------------------------------------
%%% Library initialization
%%%------------------------------------------------------------------------------

-spec configure(string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey)),
    ok.

-spec new(string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{
        access_key_id = AccessKeyID,
        secret_access_key = SecretAccessKey
    }.

-spec new(string(), string(), string(), non_neg_integer()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host, Port) ->
    #aws_config{
       access_key_id = AccessKeyID,
       secret_access_key = SecretAccessKey,
       directconnect_host = Host,
       directconnect_port = Port
      }.


%%%==============================================================================
%%% API Functions
%%%==============================================================================

%%%------------------------------------------------------------------------------
%%% DescribeConnections
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% DirectConnect API:
%% http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DescribeConnections.html
%%
%% ===Example===
%%
%% Get all connections:
%%
%% {ok, Connections} = erlcloud_directconnect:describe_connections().
%%
%% Get specific connection:
%%
%% {ok, [Connection]} =
%%      erlcloud_directconnect:describe_connections("dxcon-fg5678gh").
%%
%%------------------------------------------------------------------------------
-spec describe_connections() ->
    {ok, [connection()]} | {error, term()}.
describe_connections() ->
    describe_connections(default_config()).

-spec describe_connections(aws_config() | string() | none) ->
    {ok, [connection()]} | {error, term()}.
describe_connections(Config) when is_record(Config, aws_config) ->
    describe_connections(none, Config);
describe_connections(ConnectionId) ->
    describe_connections(ConnectionId, default_config()).

-spec describe_connections(string() | none, aws_config()) ->
    {ok, [connection()]} | {error, term()}.
describe_connections(ConnectionId, Config) ->
    Params = [{<<"connectionId">>, ConnectionId}],
    case dc_query("DescribeConnections", Params, Config) of
        {ok, Response} ->
            Connections = proplists:get_value(<<"connections">>, Response),
            {ok, [ extract_connection(C) || C <- Connections ]};
        {error, Reason} ->
            {error, Reason}
    end.


%%%------------------------------------------------------------------------------
%%% DescribeConnectionsOnInterconnect
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% DirectConnect API:
%% http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DescribeConnectionsOnInterconnect.html
%%
%% ===Example===
%%
%% Get all connections for specific interconnect:
%%
%% {ok, Connections} =
%%  erlcloud_directconnect:describe_connections_on_interconnect("dxcon-abc123").
%%
%%------------------------------------------------------------------------------
-spec describe_connections_on_interconnect(string()) ->
    {ok, [connection()]} | {error, term()}.
describe_connections_on_interconnect(InterconnectId) ->
    describe_connections_on_interconnect(InterconnectId, default_config()).

-spec describe_connections_on_interconnect(string(), aws_config()) ->
    {ok, [connection()]} | {error, term()}.
describe_connections_on_interconnect(InterconnectId, Config) ->
    Params = [{<<"interconnectId">>, InterconnectId}],
    case dc_query("DescribeConnectionsOnInterconnect", Params, Config) of
        {ok, Response} ->
            Connections = proplists:get_value(<<"connections">>, Response),
            {ok, [ extract_connection(C) || C <- Connections ]};
        {error, Reason} ->
            {error, Reason}
    end.

%%%------------------------------------------------------------------------------
%%% DescribeInterconnects
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% DirectConnect API:
%% http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DescribeInterconnects.html
%%
%% ===Example===
%%
%% Get all interconnects:
%%
%% {ok, InterConnects} = erlcloud_directconnect:describe_interconnects().
%%
%% Get specific interconnect:
%%
%% {ok, [InterConnect]} =
%%      erlcloud_directconnect:describe_interconnects("dxcon-abc123").
%%
%%------------------------------------------------------------------------------
-spec describe_interconnects() ->
    {ok, [interconnect()]} | {error, term()}.
describe_interconnects() ->
    describe_interconnects(default_config()).

-spec describe_interconnects(aws_config() | string() | none) ->
    {ok, [interconnect()]} | {error, term()}.
describe_interconnects(Config) when is_record(Config, aws_config) ->
    describe_interconnects(none, Config);
describe_interconnects(InterconnectId) ->
    describe_interconnects(InterconnectId, default_config()).

-spec describe_interconnects(string() | none, aws_config()) ->
    {ok, [interconnect()]} | {error, term()}.
describe_interconnects(InterconnectId, Config) ->
    Params = [{<<"interconnectId">>, InterconnectId}],
    case dc_query("DescribeInterconnects", Params, Config) of
        {ok, Response} ->
            Interconnects = proplists:get_value(<<"interconnects">>, Response),
            {ok, [ extract_interconnect(I) || I <- Interconnects ]};
        {error, Reason} ->
            {error, Reason}
    end.

%%%------------------------------------------------------------------------------
%%% DescribeLocations
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% DirectConnect API:
%% http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DescribeLocations.html
%%
%% ===Example===
%%
%% Get all locations:
%%
%% {ok, Locations} = erlcloud_directconnect:describe_locations().
%%
%%------------------------------------------------------------------------------
-spec describe_locations() -> {ok, [location()]} | {error, term()}.
describe_locations() ->
    describe_locations(default_config()).

-spec describe_locations(aws_config()) -> {ok, [location()]} | {error, term()}.
describe_locations(Config) ->
    case dc_query("DescribeLocations", [], Config) of
        {ok, Response} ->
            Locations = proplists:get_value(<<"locations">>, Response),
            {ok, [ extract_location(L) || L <- Locations ]};
        {error, Reason} ->
            {error, Reason}
    end.


%%%------------------------------------------------------------------------------
%%% DescribeVirtualGateways
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% DirectConnect API:
%% http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DescribeVirtualGateways.html
%%
%% ===Example===
%%
%% Get all virtual gateways:
%%
%% {ok, VirtualGateways} = erlcloud_directconnect:describe_virtual_gateways().
%%
%%------------------------------------------------------------------------------
-spec describe_virtual_gateways() ->
    {ok, [virtual_gateway()]} | {error, term()}.
describe_virtual_gateways() ->
    describe_virtual_gateways(default_config()).

-spec describe_virtual_gateways(aws_config()) ->
    {ok, [virtual_gateway()]} | {error, term()}.
describe_virtual_gateways(Config) ->
    case dc_query("DescribeVirtualGateways", [], Config) of
        {ok, Response} ->
            VGateways = proplists:get_value(<<"virtualGateways">>, Response),
            {ok, [ extract_virtual_gateways(V) || V <- VGateways ]};
        {error, Reason} ->
            {error, Reason}
    end.

%%%------------------------------------------------------------------------------
%%% DescribeVirtualInterfaces
%%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% DirectConnect API:
%% http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DescribeVirtualInterfaces.html
%%
%% ===Example===
%%
%% Get all virtual interfaces:
%%
%% {ok, VirtualInterfaces} = erlcloud_directconnect:describe_virtual_interfaces().
%%
%%
%% Get virtual intefafaces for specific connection:
%%
%% {ok, VirtualInterfaces} =
%%      erlcloud_directconnect:describe_virtual_interfaces("dxcon-fg5678gh").
%%
%%
%% Get specific virtual interface by id:
%%
%% {ok, [VirtualInterface]} =
%%      erlcloud_directconnect:describe_virtual_interfaces(none, "dxvif-123dfg56").
%%
%%------------------------------------------------------------------------------
-spec describe_virtual_interfaces() ->
    {ok, [virtual_interface()]} | {error, term()}.
describe_virtual_interfaces() ->
    describe_virtual_interfaces(default_config()).

-spec describe_virtual_interfaces(aws_config() | string() | none) ->
    {ok, [virtual_interface()]} | {error, term()}.
describe_virtual_interfaces(Config) when is_record(Config, aws_config) ->
    describe_virtual_interfaces(none, Config);
describe_virtual_interfaces(ConnectionId) ->
    describe_virtual_interfaces(ConnectionId, default_config()).

-spec describe_virtual_interfaces(
    string() | none, aws_config() | string() | none) ->
    {ok, [virtual_interface()]} | {error, term()}.
describe_virtual_interfaces(ConnectionId, Config)
  when is_record(Config, aws_config) ->
    describe_virtual_interfaces(ConnectionId, none, Config);
describe_virtual_interfaces(ConnectionId, VirtualInterfaceId) ->
    describe_virtual_interfaces(ConnectionId, VirtualInterfaceId,
        default_config()).

-spec describe_virtual_interfaces(
    string() | none, string() | none, aws_config()) ->
    {ok, [virtual_interface()]} | {error, term()}.
describe_virtual_interfaces(ConnectionId, VirtualInterfaceId, Config) ->
    Params = [{<<"connectionId">>, ConnectionId},
              {<<"virtualInterfaceId">>, VirtualInterfaceId}],
    case dc_query("DescribeVirtualInterfaces", Params, Config) of
        {ok, Response} ->
            VIs = proplists:get_value(<<"virtualInterfaces">>, Response),
            {ok, [ extract_virtual_interface(VI) || VI <- VIs ]};
        {error, Reason} ->
            {error, Reason}
    end.

%%%------------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------------

extract_connection(J) ->
    erlcloud_json:decode(
        [
            {bandwidth, <<"bandwidth">>, optional_string},
            {connection_id, <<"connectionId">>, optional_string},
            {connection_name, <<"connectionName">>, optional_string},
            {connection_state, <<"connectionState">>, optional_string},
            {location, <<"location">>, optional_string},
            {owner_account, <<"ownerAccount">>, optional_string},
            {partner_name, <<"partnerName">>, optional_string},
            {region, <<"region">>, optional_string},
            {vlan, <<"vlan">>, optional_integer}
        ], J).

extract_interconnect(J) ->
    erlcloud_json:decode(
        [
            {bandwidth, <<"bandwidth">>, optional_string},
            {interconnect_id, <<"interconnectId">>, optional_string},
            {interconnect_name, <<"interconnectName">>, optional_string},
            {interconnect_state, <<"interconnectState">>, optional_string},
            {location, <<"location">>, optional_string},
            {region, <<"region">>, optional_string}
        ], J).

extract_location(J) ->
    erlcloud_json:decode(
        [
            {location_code, <<"locationCode">>, optional_string},
            {location_name, <<"locationName">>, optional_string}
        ], J).

extract_virtual_gateways(J) ->
    erlcloud_json:decode(
        [
            {virtual_gateway_id, <<"virtualGatewayId">>, optional_string},
            {virtual_gateway_state, <<"virtualGatewayState">>, optional_string}
        ], J).

extract_virtual_interface(J) ->
    erlcloud_json:decode(
        [
            {amazon_address, <<"amazonAddress">>, optional_string},
            {asn, <<"asn">>, optional_integer},
            {auth_key, <<"authKey">>, optional_string},
            {connection_id, <<"connectionId">>, optional_string},
            {customer_address, <<"customerAddress">>, optional_string},
            {customer_router_config, <<"customerRouterConfig">>, optional_string},
            {location, <<"location">>, optional_string},
            {owner_account, <<"ownerAccount">>, optional_string},
            {route_filter_prefixes, <<"routeFilterPrefixes">>,
                {optional_map, fun extract_route_filter_prefixes/1}},
            {virtual_gateway_id, <<"virtualGatewayId">>, optional_string},
            {virtual_interface_id, <<"virtualInterfaceId">>, optional_string},
            {virtual_interface_name, <<"virtualInterfaceName">>, optional_string},
            {virtual_interface_state, <<"virtualInterfaceState">>, optional_string},
            {virtual_interface_type, <<"virtualInterfaceType">>, optional_string},
            {vlan, <<"vlan">>, optional_integer}
        ], J).

extract_route_filter_prefixes(J) ->
    erlcloud_json:decode(
        [
            {cidr, <<"cidr">>, optional_string}
        ], J).

prepare_query_params({_K, none}) ->
    false;
prepare_query_params({K, V}) when is_integer(V) ->
    {true, {K, V}};
prepare_query_params({K, V}) when is_list(V) ->
    {true, {K, list_to_binary(V)}}.

default_config() ->
    erlcloud_aws:default_config().

dc_query(Operation, Params, Config) ->
    Body = case lists:filtermap(fun prepare_query_params/1, Params) of
        [] ->
            <<"{}">>;
        PrepParams ->
            jsx:encode(PrepParams)
    end,
    Headers = headers(Config, lists:flatten(?API_PREFIX, Operation), Body,
        ?SERVICE_NAME),
    case erlcloud_aws:http_headers_body(
                erlcloud_httpc:request(
                     url(Config), post,
                     [{<<"content-type">>, <<"application/x-amz-json-1.1">>} | Headers],
                     Body, 1000, Config)) of
       {ok, {_RespHeader, RespBody}} ->
            {ok, jsx:decode(RespBody)};
        {error, Reason} ->
            {error, Reason}
    end.

-type headers() :: [{string(), string()}].
-spec headers(aws_config(), string(), binary(), string()) -> headers().
headers(Config, Operation, Body, Service) ->
    Headers = [
               {"host", Config#aws_config.directconnect_host},
               {"x-amz-target", Operation}
               ],
    Region =
        erlcloud_aws:aws_region_from_host(Config#aws_config.directconnect_host),
    erlcloud_aws:sign_v4_headers(Config, Headers, Body, Region, Service).

url(#aws_config{directconnect_scheme = Scheme, directconnect_host = Host} = Config) ->
    lists:flatten([Scheme, Host, port_spec(Config)]).

port_spec(#aws_config{directconnect_port=80}) ->
    "";
port_spec(#aws_config{directconnect_port=Port}) ->
    [":", erlang:integer_to_list(Port)].

