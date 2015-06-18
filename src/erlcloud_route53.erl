-module(erlcloud_route53).

% %% Library initialization.
-export([configure/2, configure/3, new/2, new/3]).

%% ELB API Functions
-export([list_hosted_zones/0,list_hosted_zones/1,list_hosted_zones/2]).

-import(erlcloud_xml, [get_text/2, get_text/1]).

-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

-define(API_VERSION, "2013-04-01").

%-import(erlcloud_xml, [get_text/2, get_text/1]).

-spec(new/2 :: (string(), string()) -> aws_config()).
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey}.

-spec(new/3 :: (string(), string(), string()) -> aws_config()).
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                elb_host=Host}.

-spec(configure/2 :: (string(), string()) -> ok).
configure(AccessKeyID, SecretAccessKey) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey)),
    ok.

-spec(configure/3 :: (string(), string(), string()) -> ok).
configure(AccessKeyID, SecretAccessKey, Host) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host)),
    ok.

default_config() -> erlcloud_aws:default_config().

list_hosted_zones() ->
    list_hosted_zones(default_config()).
list_hosted_zones(Config) when is_record( Config, aws_config) ->
    list_all_hosted_zones([], Config, []).

list_all_hosted_zones(Params, Config, Acc) ->
  case list_hosted_zones(Params, Config) of
    {ok, HostedZones, []} ->
      {ok, Acc ++ HostedZones};
    {ok, HostedZones, NextMarker} ->
      NewParams= [{"Marker", NextMarker} | lists:keydelete("Marker", 1, Params)],
      list_all_hosted_zones(NewParams, Config, Acc ++ HostedZones);
    {error, Reason} ->
      {error, Reason}
  end.

list_hosted_zones(Params, Config) ->
  case dns_request(Config, "GET", "/2013-04-01/hostedzone", "hostedzone", Params, <<>>) of
    {ok, Doc} ->
      HostedZones = [extract_hosted_zones(Item) || 
      Item <- xmerl_xpath:string("/ListHostedZonesResponse/HostedZones/HostedZone", Doc)],
      NextMarker = get_text("/ListHostedZonesResponse/HostedZones/NextMarker", Doc),
      {ok, HostedZones, NextMarker};
    {error, Reason} ->
      {error, Reason}
  end.

extract_hosted_zones(Node) ->
  [{id, get_text("Id", Node)},
  {name, get_text("Name", Node)},
  {caller_reference, get_text("CallerReference", Node)},
  {resource_recordset_count, get_text("ResourceRecordSetCount", Node)},
  {comment, get_text("Config/comment", Node)},
  {private_zone, get_text("Config/PrivateZone", Node)}].

list_resource_record_sets(Name) ->
  list_resource_record_sets(Name, default_config()).
list_resource_record_sets(Name, Config) when is_list(Name), is_record(Config, aws_config) ->
  list_all_resource_record_sets(Name, [], Config, []).

list_all_resource_record_sets(Name, Params, Config, Acc) ->
   case list_resource_record_sets(Name, Params, Config) of
    {ok, RecordSets, []} ->
      {ok, Acc ++ RecordSets};
    {ok, RecordSets, NextRecordName} ->
      NewParams= [{"name", NextRecordName} | lists:keydelete("name", 1, Params)],
      list_all_resource_record_sets(Name, NewParams, Config, Acc ++ RecordSets);
    {error, Reason} ->
      {error, Reason}
  end.

list_resource_record_sets(Name, Params, Config) ->
  URI = lists:flatten(["/2013-04-01/hostedzone/", Name, "/", "rrset"]),
   case dns_request(Config, "GET", URI, "", Params, <<>>) of
    {ok, Doc} ->
      RRSet = [extract_rrset(Item) || 
       Item <- xmerl_xpath:string("/ListResourceRecordSetsResponse/ResourceRecordSets/ResourceRecordSet", Doc)],
      NextRecordName = get_text("/ListResourceRecordSetsResponse/NextRecordName", Doc),
      io:format("Size = ~p, NextRecord=~p~n ",[erlang:length(RRSet), NextRecordName]),
      {ok, RRSet, NextRecordName};
    {error, Reason} ->
      {error, Reason}
  end.

extract_rrset(Node) ->
  extract_rrset_common_fields(Node)
  ++ extract_rrset_rrecords(Node)
  ++ extract_rrset_aliastarget(Node)
  ++ extract_rrset_geolocation(Node)
  ++ extract_optional_field("SetIdentifier", Node)
  ++ extract_optional_field("Region", Node)
  ++ extract_optional_field("TTL", Node)
  ++ extract_optional_field("Weight", Node)
  ++ extract_optional_field("Failover", Node).

extract_rrset_common_fields(Node) ->
  [{name, get_text("Name", Node)},
  {type, get_text("Type", Node)},
  {health_checkid, get_text("HealthCheckId", Node)}
  ].

extract_rrset_rrecords(Node) ->
 case xmerl_xpath:string("ResourceRecords", Node) of
    [] ->
      [];
    [ResourceRecords] ->
      [{resource_records, 
      [{resource_record, get_text("Value", Record)} || Record <- xmerl_xpath:string("ResourceRecord", ResourceRecords)]
      }]
 end. 

extract_rrset_aliastarget(Node) ->
  case xmerl_xpath:string("AliasTarget", Node) of
    [] ->
      [];
    [AliasTarget] ->
      [{alias_target, 
      [{hosted_zone_id, get_text("HostedZoneId", AliasTarget)},
       {dns_name, get_text("DNSName", AliasTarget)},
       {evaluate_target_health, get_text("EvaluateTargetHealth", AliasTarget)}]
      }]
 end. 

extract_rrset_geolocation(Node) ->
  case xmerl_xpath:string("Geolocation", Node) of
    [] ->
      [];
    [Geolocation] ->
      [{geo_location, 
      [{continent_code, get_text("ContinentCode", Geolocation)},
       {country_code, get_text("CountryCode", Geolocation)},
       {subdivision_code, get_text("SubdivisionCode", Geolocation)}]
      }]
 end. 

extract_optional_field(NodeName, Node) when is_list(NodeName)->
 case get_text(NodeName, Node) of
    [] ->
      [];
    Value ->
      [{list_to_atom(string:to_lower(NodeName)), Value}]
 end.

%https://route53.amazonaws.com
dns_request(Config, Method, Path, Action, Params, Body) ->
    Headers = headers(Config, Method, Path, Params, Action, Body),
    QueryString = erlcloud_http:make_query_string(Params), 
    URIPath = lists:flatten([Path, "?",QueryString]),
    request(Method, URIPath, Headers, Params, Body, Config).

request(Method, Path, Headers, Params, Body, #aws_config{} = Config) ->
    case erlcloud_aws:http_headers_body(
                erlcloud_httpc:request(
                     route53ActionUrl(Config, Path),
                     Method,
                     Headers,
                     Body, Config#aws_config.timeout, Config)) of
       {ok, {_RespHeader, RespBody}} ->
            {ok, element(1, xmerl_scan:string(binary_to_list(RespBody)))};
        {error, Reason} ->
            {error, Reason}
    end.

headers(Config, Method, URI, Params,Operation, Body) ->
    Headers = [{"host", Config#aws_config.dns_host}
               %,{"x-amz-target", Operation}
               ],
    Region =
        case string:tokens(Config#aws_config.dns_host, ".") of
            [_, Value, _, _] ->
                Value;
            _ ->
                "us-east-1"
        end,
    erlcloud_aws:sign_v4(Config, Method, URI, Params, Headers, Body, Region, "route53").

route53ActionUrl(Config, Path) ->
  lists:flatten(["https://", Config#aws_config.dns_host, Path]).