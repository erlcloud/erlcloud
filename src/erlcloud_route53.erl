%% Amazon Route 53

-module(erlcloud_route53).

-ifdef(TEST).
-export([describe_all/4, describe_all/5]).
-endif.

-type(aws_route53_delegation_set() :: proplists:proplist()).
-type(aws_route53_zone() :: proplists:proplist()).
-type(aws_route53_resourceset() :: proplists:proplist()).

-export([configure/2, configure/3, new/2, new/3]).

-export([describe_zone/1, describe_zone/2, describe_zone/3]).

-export([describe_zones/0, describe_zones/1, describe_zones/2,
         describe_zones_all/0, describe_zones_all/1, describe_zones_all/2]).

-export([describe_resource_sets/1, describe_resource_sets/2,
         describe_resource_sets/3, describe_resource_sets_all/1,
         describe_resource_sets_all/2, describe_resource_sets_all/3]).

-export([describe_delegation_sets/0, describe_delegation_sets/1,
        describe_delegation_sets/2, describe_delegation_sets_all/0,
        describe_delegation_sets_all/1, describe_delegation_sets_all/2]).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

-define(API_VERSION, "2013-04-01").
-define(DEFAULT_MAX_RECORDS, 100).
-define(DESCRIBE_ZONES_PATH, "/ListHostedZonesResponse/HostedZones/HostedZone").
-define(DESCRIBE_ZONES_IS_TRUNCATED, "/ListHostedZonesResponse/IsTruncated").
-define(DESCRIBE_ZONES_NEXT_MARKER, "/ListHostedZonesResponse/NextMarker").
-define(DESCRIBE_ZONE_PATH, "/GetHostedZoneResponse/HostedZone").
-define(DESCRIBE_ZONE_VPCS_PATH, "/GetHostedZoneResponse/VPCs/VPC").
-define(DESCRIBE_ZONE_DS_PATH, "/GetHostedZoneResponse/DelegationSet").
-define(DESCRIBE_RS_PATH,
        "/ListResourceRecordSetsResponse/ResourceRecordSets/ResourceRecordSet").
-define(DESCRIBE_RS_IS_TRUNCATED,
        "/ListResourceRecordSetsResponse/IsTruncated").
-define(DESCRIBE_RS_NEXT_TYPE,
        "/ListResourceRecordSetsResponse/NextRecordType").
-define(DESCRIBE_RS_NEXT_NAME,
        "/ListResourceRecordSetsResponse/NextRecordName").
-define(DESCRIBE_RS_NEXT_ID,
        "/ListResourceRecordSetsResponse/NextRecordIdentifier").
-define(DESCRIBE_DS_PATH,
        "/ListReusableDelegationSetsResponse/DelegationSets/DelegationSet").
-define(DESCRIBE_DS_IS_TRUNCATED,
        "/ListReusableDelegationSetsResponse/IsTruncated").
-define(DESCRIBE_DS_NEXT_MARKER,
        "/ListReusableDelegationSetsResponse/NextMarker").

-spec new(string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey}.

-spec new(string(), string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                route53_host=Host}.

-spec configure(string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey) ->
    erlcloud_config:configure(AccessKeyID, SecretAccessKey, fun new/2).

-spec configure(string(), string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host) ->
    erlcloud_config:configure(AccessKeyID, SecretAccessKey, Host, fun new/3).

%% --------------------------------------------------------------------
%% @doc Describes provided zone using default config
%% @end
%% --------------------------------------------------------------------
-spec describe_zone(ZoneId :: string()) ->
             {ok, list(aws_route53_zone())} |
             {error, term()}.
describe_zone(ZoneId) ->
    describe_zone(ZoneId, erlcloud_aws:default_config()).

%% --------------------------------------------------------------------
%% @doc Describes provided zone using provided config
%% @end
%% --------------------------------------------------------------------
-spec describe_zone(ZoneId :: string(),
                    AwsConfig :: aws_config()) ->
             {ok, list(aws_route53_zone())} |
             {error, term()}.
describe_zone(ZoneId, AwsConfig) ->
    describe_zone(ZoneId, [], AwsConfig).

%% --------------------------------------------------------------------
%% @doc Describes provided zone using provided config + AWS options
%% @end
%% --------------------------------------------------------------------
-spec describe_zone(ZoneId :: string(),
                    Options   :: list({string(), string()}),
                    AwsConfig :: aws_config()) ->
    {ok, list(aws_route53_zone())} |
    {error, term()}.
describe_zone(ZoneId, Options, Config) when is_list(Options),
                                            is_record(Config, aws_config) ->
    do_describe_zone(ZoneId, Options, Config).

%% --------------------------------------------------------------------
%% @doc Describes all zones using default config
%% @end
%% --------------------------------------------------------------------
-spec describe_zones() ->
             {ok, list(aws_route53_zone())} |
             {ok, list(aws_route53_zone()), string()} |
             {error, term()}.
describe_zones() ->
    describe_zones(erlcloud_aws:default_config()).

%% --------------------------------------------------------------------
%% @doc Describes all zones using provided config
%% @end
%% --------------------------------------------------------------------
-spec describe_zones(AwsConfig :: aws_config()) ->
             {ok, list(aws_route53_zone())} |
             {ok, list(aws_route53_zone()), string()} |
             {error, term()}.
describe_zones(AwsConfig) ->
    describe_zones([], AwsConfig).

%% --------------------------------------------------------------------
%% @doc Describes all zones using provided config + AWS options
%% @end
%% --------------------------------------------------------------------
-spec describe_zones(Options   :: list({string(), string()}),
                     AwsConfig :: aws_config()) ->
             {ok, list(aws_route53_zone())} |
             {ok, list(aws_route53_zone()), string()} |
             {error, term()}.
describe_zones(Options, Config) when is_list(Options),
                                     is_record(Config, aws_config) ->
    do_describe_zones(Options, Config).

%% --------------------------------------------------------------------
%% @doc Describes all zones using default config
%% @end
%% --------------------------------------------------------------------
-spec describe_zones_all() ->
             {ok, list(aws_route53_zone())} |
             {error, term()}.
describe_zones_all() ->
    describe_all(fun describe_zones/2, [],
                 erlcloud_aws:default_config(), []).

%% --------------------------------------------------------------------
%% @doc Describes all zones using provided config
%% @end
%% --------------------------------------------------------------------
-spec describe_zones_all(AwsConfig :: aws_config()) ->
             {ok, list(aws_route53_zone())} |
             {error, term()}.
describe_zones_all(AwsConfig) ->
    describe_all(fun describe_zones/2, [], AwsConfig, []).

%% --------------------------------------------------------------------
%% @doc Describes all zones using provided config + AWS options
%% @end
%% --------------------------------------------------------------------
-spec describe_zones_all(Options   :: list({string(), string()}),
                         AwsConfig :: aws_config()) ->
             {ok, list(aws_route53_zone())} |
             {error, term()}.
describe_zones_all(Options, Config) when is_list(Options),
                                         is_record(Config, aws_config) ->
    describe_all(fun describe_zones/2, Options, Config, []).

%% --------------------------------------------------------------------
%% @doc Describes resource sets for a specific zone_id using default config
%% @end
%% --------------------------------------------------------------------
-spec describe_resource_sets(ZoneId :: string()) ->
             {ok, list(aws_route53_resourceset())} |
             {ok, list(aws_route53_resourceset()), string()} |
             {ok, list(aws_route53_resourceset()), {string(), string()}} |
             {ok, list(aws_route53_resourceset()), {string(), string(), string()}} |
             {error, term()}.
describe_resource_sets(ZoneId) ->
    describe_resource_sets(ZoneId, erlcloud_aws:default_config()).

%% --------------------------------------------------------------------
%% @doc Describes resource sets for a specific zone_id using provided config
%% @end
%% --------------------------------------------------------------------
-spec describe_resource_sets(ZoneId :: string(),
                             AwsConfig :: aws_config()) ->
             {ok, list(aws_route53_resourceset())} |
             {ok, list(aws_route53_resourceset()), string()} |
             {ok, list(aws_route53_resourceset()), {string(), string()}} |
             {ok, list(aws_route53_resourceset()), {string(), string(), string()}} |
             {error, term()}.
describe_resource_sets(ZoneId, AwsConfig) ->
    describe_resource_sets(ZoneId, [], AwsConfig).

%% --------------------------------------------------------------------
%% @doc Describes resource sets with provided aws_config and
%% ZoneID with a list of AWS options
%% @end
%% --------------------------------------------------------------------
-spec describe_resource_sets(ZoneID    :: string(),
                             Options   :: list({string(), string()}),
                             AwsConfig :: aws_config()) ->
             {ok, list(aws_route53_resourceset())} |
             {ok, list(aws_route53_resourceset()), string()} |
             {ok, list(aws_route53_resourceset()), {string(), string()}} |
             {ok, list(aws_route53_resourceset()), {string(), string(), string()}} |
             {error, term()}.
describe_resource_sets(ZoneId, Options, AwsConfig) ->
    do_describe_resource_sets(ZoneId, Options, AwsConfig).

%% --------------------------------------------------------------------
%% @doc Describes resource sets for a specific zone_id using default config
%% @end
%% --------------------------------------------------------------------
-spec describe_resource_sets_all(ZoneId :: string()) ->
             {ok, list(aws_route53_resourceset())} |
             {error, term()}.
describe_resource_sets_all(ZoneId) ->
    describe_all(fun describe_resource_sets/3, [ZoneId], [],
                 erlcloud_aws:default_config(), []).

%% --------------------------------------------------------------------
%% @doc Describes resource sets for a specific zone_id using provided config
%% @end
%% --------------------------------------------------------------------
-spec describe_resource_sets_all(ZoneId :: string(),
                                 AwsConfig :: aws_config()) ->
             {ok, list(aws_route53_resourceset())} |
             {error, term()}.
describe_resource_sets_all(ZoneId, AwsConfig) ->
    describe_all(fun describe_resource_sets/3, [ZoneId], [], AwsConfig, []).

%% --------------------------------------------------------------------
%% @doc Describes resource sets with provided aws_config and
%% ZoneID with a list of AWS options
%% @end
%% --------------------------------------------------------------------
-spec describe_resource_sets_all(ZoneID    :: string(),
                                 Options   :: list({string(), string()}),
                                 AwsConfig :: aws_config()) ->
             {ok, list(aws_route53_resourceset())} |
             {error, term()}.
describe_resource_sets_all(ZoneId, Options, AwsConfig) ->
    describe_all(fun describe_resource_sets/3, [ZoneId],
                 Options, AwsConfig, []).

%% --------------------------------------------------------------------
%% @doc Describes delegation sets using default config
%% @end
%% --------------------------------------------------------------------
-spec describe_delegation_sets() ->
             {ok, list(aws_route53_delegation_set())} |
             {ok, list(aws_route53_delegation_set()), string()} |
             {error, term()}.
describe_delegation_sets() ->
    describe_delegation_sets(erlcloud_aws:default_config()).

%% --------------------------------------------------------------------
%% @doc Describes delegation sets using provided config
%% @end
%% --------------------------------------------------------------------
-spec describe_delegation_sets(AwsConfig :: aws_config()) ->
             {ok, list(aws_route53_delegation_set())} |
             {ok, list(aws_route53_delegation_set()), string()} |
             {error, term()}.
describe_delegation_sets(AwsConfig) ->
    describe_delegation_sets([], AwsConfig).

%% --------------------------------------------------------------------
%% @doc Describes delegation sets with provided aws_config and a list of AWS
%% options
%% @end
%% --------------------------------------------------------------------
-spec describe_delegation_sets(Options   :: list({string(), string()}),
                               AwsConfig :: aws_config()) ->
             {ok, list(aws_route53_delegation_set())} |
             {ok, list(aws_route53_delegation_set()), string()} |
             {error, term()}.
describe_delegation_sets(Options, AwsConfig) ->
    do_describe_delegation_sets(Options, AwsConfig).

%% --------------------------------------------------------------------
%% @doc Describes delegation sets using default config
%% @end
%% --------------------------------------------------------------------
-spec describe_delegation_sets_all() ->
             {ok, list(aws_route53_delegation_set())} |
             {error, term()}.
describe_delegation_sets_all() ->
    describe_all(fun describe_delegation_sets/2, [],
                       erlcloud_aws:default_config(), []).

%% --------------------------------------------------------------------
%% @doc Describes delegation sets using provided config
%% @end
%% --------------------------------------------------------------------
-spec describe_delegation_sets_all(AwsConfig :: aws_config()) ->
             {ok, list(aws_route53_delegation_set())} |
             {error, term()}.
describe_delegation_sets_all(AwsConfig) ->
    describe_all(fun describe_delegation_sets/2, [], AwsConfig, []).

%% --------------------------------------------------------------------
%% @doc Describes delegation sets with provided aws_config and a list of AWS
%% options
%% @end
%% --------------------------------------------------------------------
-spec describe_delegation_sets_all(Options   :: list({string(), string()}),
                                   AwsConfig :: aws_config()) ->
             {ok, list(aws_route53_delegation_set())} |
             {error, term()}.
describe_delegation_sets_all(Options, AwsConfig) ->
    describe_all(fun describe_delegation_sets/2, Options, AwsConfig, []).


do_describe_delegation_sets(Params, AwsConfig) ->
    Path = "/" ++ ?API_VERSION ++ "/delegationset",
    case route53_query(get, AwsConfig, "ListReusableDelegationSets", Path,
                       Params, ?API_VERSION) of
        {ok, Doc} ->
            Sets = xmerl_xpath:string(?DESCRIBE_DS_PATH, Doc),
            Marker = erlcloud_xml:get_text(?DESCRIBE_DS_NEXT_MARKER, Doc),
            make_response(Doc, ?DESCRIBE_DS_IS_TRUNCATED, Marker,
                          [extract_delegation_set(X) || X <- Sets]);
        Error ->
            Error
    end.

do_describe_resource_sets(ZoneId, Params, AwsConfig) ->
    Path = "/" ++ ?API_VERSION ++ "/hostedzone/" ++ ZoneId ++ "/rrset",
    case route53_query(get, AwsConfig, "ListResourceRecordSets",
                       Path, Params, ?API_VERSION) of
        {ok, Doc} ->
            Sets = xmerl_xpath:string(?DESCRIBE_RS_PATH, Doc),
            Marker = case {erlcloud_xml:get_text(?DESCRIBE_RS_NEXT_NAME,
                                                 Doc, undefined),
                           erlcloud_xml:get_text(?DESCRIBE_RS_NEXT_TYPE,
                                                 Doc, undefined),
                           erlcloud_xml:get_text(?DESCRIBE_RS_NEXT_ID,
                                                 Doc, undefined)
						 } of
                         {NextName, NextType, undefined} ->
                             {NextName, NextType};
                         {undefined, undefined, NextId} ->
                             NextId;
                         {NextName, NextType, NextId} ->
                             {NextName, NextType, NextId}
                     end,
            make_response(Doc, ?DESCRIBE_RS_IS_TRUNCATED, Marker,
                          [extract_resource_set(X) || X <- Sets]);
        {error, Reason} ->
            {error, Reason}
    end.

do_describe_zone(ZoneId, Params, Config) ->
    Path = "/" ++ ?API_VERSION ++ "/hostedzone/" ++ ZoneId,
    case route53_query(get, Config, "GetHostedZone",
                       Path, Params, ?API_VERSION) of
        {ok, Doc} ->

            Zone = extract_zone(hd(xmerl_xpath:string(?DESCRIBE_ZONE_PATH,
                                                      Doc))),
            Extras = erlcloud_xml:decode(
                       [{vpcs, ?DESCRIBE_ZONE_VPCS_PATH, {optional_map, fun extract_vpcs/1}},
                        {delegation_set, ?DESCRIBE_ZONE_DS_PATH,
                         {single, fun extract_delegation_set/1}}], Doc),
            {ok, Zone ++ Extras};
        {error, Reason} ->
            {error, Reason}
    end.

do_describe_zones(Params, Config) ->
    Path = "/" ++ ?API_VERSION ++ "/hostedzone",
    case route53_query(get, Config, "ListHostedZones",
                       Path, Params, ?API_VERSION) of
        {ok, Doc} ->
            Zones = xmerl_xpath:string(?DESCRIBE_ZONES_PATH, Doc),
            Marker = erlcloud_xml:get_text(?DESCRIBE_ZONES_NEXT_MARKER, Doc),
            make_response(Doc, ?DESCRIBE_ZONES_IS_TRUNCATED, Marker,
                          [extract_zone(Z) || Z <- Zones]);
        {error, Reason} ->
            {error, Reason}
    end.


make_response(Xml, Path, Marker, Result) ->
    case erlcloud_xml:get_bool(Path, Xml) of
        false ->
            {ok, Result};
        true ->
            {ok, Result, Marker}
    end.

extract_geolocation(Xml) ->
    erlcloud_xml:decode([
                         {continent_code, "ContinentCode", text},
                         {country_code, "CountryCode", text},
                         {subdivision_code, "SubdivisionCode", text}
                        ], Xml).

extract_resource_records(Xml) ->
    case [erlcloud_xml:get_text("Value", X) || X <- Xml] of
        [] ->
            undefined;
        Res ->
            Res
    end.

extract_alias_target(Rs) ->
    erlcloud_xml:decode([
                         {hosted_zone_id, "HostedZoneId", text},
                         {dns_name, "DNSName", text},
                         {evaluate_target_health, "EvaluateTargetHealth",
                          optional_boolean}
                         ], Rs).

extract_delegation_set(Set) ->
    erlcloud_xml:decode([
                         {id, "Id", optional_text},
                         {caller_reference, "CallerReference", optional_text},
                         {marker, "Marker", optional_text},
                         {name_servers, "NameServers/NameServer", list}
                        ], Set).

extract_resource_set(Rs) ->
    erlcloud_xml:decode([
                         {name, "Name", text},
                         {type, "Type", text},
                         {set_identifier, "SetIdentifier", optional_text},
                         {weight, "Weight", optional_integer},
                         {region, "Region", optional_text},
                         {failover, "Failover", optional_text},
                         {health_check_id, "HealthCheckId", optional_text},
                         {ttl, "TTL", optional_integer},
                         {geo_location, "GeoLocation",
                          {single, fun extract_geolocation/1}},
                         {resource_records, "ResourceRecords/ResourceRecord",
                          fun extract_resource_records/1},
                         {alias_target, "AliasTarget",
                          {single, fun extract_alias_target/1}}
                        ], Rs).

extract_zone(Xml) ->
    erlcloud_xml:decode([
                         {zone_id, "Id", text},
                         {name, "Name", text},
                         {private, "Config/PrivateZone", optional_boolean},
                         {resourceRecordSetCount, "ResourceRecordSetCount",
                          integer},
                         {marker, "Marker", optional_text}
                        ], Xml).

extract_vpcs(Xml) ->
    erlcloud_xml:decode([
                         {vpc_id, "VPCId", text},
                         {vpc_region, "VPCRegion", text}
                        ], Xml).

describe_all(Fun, Options, Config, Acc) ->
    describe_all(Fun, [], Options, Config, Acc).

describe_all(Fun, Args, Options, Config, Acc) when is_list(Args) ->
    case apply(Fun, Args ++ [Options] ++ [Config]) of
        {ok, Res} ->
            {ok, lists:foldl(fun erlang:'++'/2, [], [Res | Acc])};
        {ok, Res, {Name, Type}} ->
            Options1 = key_replace_or_add("name", Name, Options),
            Options2 = key_replace_or_add("type", Type, Options1),
            describe_all(Fun, Args, Options2, Config, [Res | Acc]);
        {ok, Res, {Name, Type, Id}} ->
            Options1 = key_replace_or_add("name", Name, Options),
            Options2 = key_replace_or_add("type", Type, Options1),
            Options3 = key_replace_or_add("identifier", Id, Options2),
            describe_all(Fun, Args, Options3, Config, [Res | Acc]);
        {ok, Res, Marker} ->
            Options1 = key_replace_or_add("marker", Marker, Options),
            describe_all(Fun, Args, Options1,
                         Config, [Res | Acc]);
        {error, Reason} ->
            {error, Reason}
    end;
describe_all(Fun, Args, Options, Config, Acc) ->
    describe_all(Fun, [Args], Options, Config, Acc).

key_replace_or_add(Key, Value, List) ->
    case lists:keymember(Key, 1, List) of
        true ->
            lists:keyreplace(Key, 1, List, {Key, Value});
        false ->
            [{Key, Value} | List]
    end.

-spec route53_query(get | post, aws_config(), string(), string(),
                    list({string(), string()}), string()) ->
    {ok, term()} | {error, term}.
route53_query(Method, Config, Action, Path, Params, ApiVersion) ->
    QParams = [{"Action", Action}, {"Version", ApiVersion} | Params],
    erlcloud_aws:aws_request_xml4(Method, Config#aws_config.route53_host,
                                  Path, QParams, "route53", Config).
