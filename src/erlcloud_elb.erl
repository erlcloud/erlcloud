-module(erlcloud_elb).

%% Library initialization.
-export([configure/2, configure/3, new/2, new/3]).

%% ELB API Functions
-export([create_load_balancer/3, create_load_balancer/4, create_load_balancer/5,
         delete_load_balancer/1, delete_load_balancer/2,

         register_instance/2, register_instance/3,
         deregister_instance/2, deregister_instance/3,

         describe_load_balancer/1, describe_load_balancer/2,
         describe_load_balancers/0, describe_load_balancers/1, describe_load_balancers/2,

         configure_health_check/2, configure_health_check/3]).

-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

-define(API_VERSION, "2009-05-15").
-define(NEW_API_VERSION, "2012-06-01").

-import(erlcloud_xml, [get_text/2, get_text/1]).

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


create_load_balancer(LB, Port, Protocol) when is_list(LB),
                                              is_integer(Port),
                                              is_atom(Protocol) ->
    create_load_balancer(LB, Port, Protocol, default_config()).

create_load_balancer(LB, Port, Protocol, Config) when is_list(LB),
                                                      is_integer(Port),
                                                      is_atom(Protocol) ->
    create_load_balancer(LB, Port, Protocol, "us-east-1d", Config).

create_load_balancer(LB, Port, Protocol, Zone, Config) when is_list(LB),
                                                            is_integer(Port),
                                                            is_atom(Protocol),
                                                            is_list(Zone) ->
    XML = elb_request(Config,
                      "CreateLoadBalancer",
                      [{"AvailabilityZones.member.1", Zone},
                       {"LoadBalancerName", LB} |
                       erlcloud_aws:param_list([[{"LoadBalancerPort", Port},
                                                 {"InstancePort", Port},
                                                 {"Protocol", string:to_upper(atom_to_list(Protocol))}]],
                                               "Listeners.member")]),
    {ok, get_text("/CreateLoadBalancerResponse/CreateLoadBalancerResult/DNSName", XML)}.

delete_load_balancer(LB) when is_list(LB) ->
    delete_load_balancer(LB, default_config()).

delete_load_balancer(LB, Config) when is_list(LB) ->
    elb_simple_request(Config,
                       "DeleteLoadBalancer",
                       [{"LoadBalancerName", LB}]).


-spec register_instance/2 :: (string(), string()) -> proplist().
register_instance(LB, InstanceId) ->
    register_instance(LB, InstanceId, default_config()).

-spec register_instance/3 :: (string(), string(), aws_config()) -> proplist().
register_instance(LB, InstanceId, Config) when is_list(LB) ->
    elb_simple_request(Config,
                       "RegisterInstancesWithLoadBalancer",
                       [{"LoadBalancerName", LB} |
                        erlcloud_aws:param_list([[{"InstanceId", InstanceId}]], "Instances.member")]).


-spec deregister_instance/2 :: (string(), string()) -> proplist().
deregister_instance(LB, InstanceId) ->
    deregister_instance(LB, InstanceId, default_config()).

-spec deregister_instance/3 :: (string(), string(), aws_config()) -> proplist().
deregister_instance(LB, InstanceId, Config) when is_list(LB) ->
    elb_simple_request(Config,
                       "DeregisterInstancesFromLoadBalancer",
                       [{"LoadBalancerName", LB} |
                        erlcloud_aws:param_list([[{"InstanceId", InstanceId}]], "Instances.member")]).



-spec configure_health_check/2 :: (string(), string()) -> proplist().
configure_health_check(LB, Target) when is_list(LB),
                                        is_list(Target) ->
    configure_health_check(LB, Target, default_config()).

-spec configure_health_check/3 :: (string(), string(), aws_config()) -> proplist().
configure_health_check(LB, Target, Config) when is_list(LB) ->
    elb_simple_request(Config,
                       "ConfigureHealthCheck",
                       [{"LoadBalancerName", [LB]},
                        {"HealthCheck.Target", Target}]).


describe_load_balancer(Name) ->
    describe_load_balancer(Name, default_config()).
describe_load_balancer(Name, Config) ->
    describe_load_balancers([Name], Config).

describe_load_balancers() ->
    describe_load_balancers(default_config()).

describe_load_balancers(Config) ->
  describe_all_load_balancers([], Config, []);
describe_load_balancers(Names) when is_list(Names) ->
  describe_load_balancers(Names, default_config()).

describe_load_balancers(Names, Config) when is_list(Names) ->
  Params = [erlcloud_aws:param_list(Names, "LoadBalancerNames.member")],
  describe_all_load_balancers(Params, Config, []).

describe_all_load_balancers(Params, Config, Acc) when is_list(Params), is_list(Acc)->
    case load_balancer_request(Params, Config) of
      {ok, LoadBalancers, []} ->
        {ok, Acc ++ LoadBalancers};
      {error, Reason} ->
        {error, Reason};
      {ok, LoadBalancers, NextMarker} ->
        NewParams = [{"Marker", NextMarker} | lists:keydelete("Marker", 1, Params)],
        describe_all_load_balancers(NewParams, Config, Acc ++ LoadBalancers)
    end.

load_balancer_request(Params, Config)  when is_list(Params) ->
    case elb_request(Config, "DescribeLoadBalancers", Params, ?NEW_API_VERSION) of
        {ok, Doc} ->
            ElasticLoadBalancers = [extract_load_balancers(Item) || Item <- xmerl_xpath:string("/DescribeLoadBalancersResponse/DescribeLoadBalancersResult/LoadBalancerDescriptions/member", Doc)],
            NextMarker = get_text("/DescribeLoadBalancersResponse/DescribeLoadBalancersResult/NextMarker", Doc),
            {ok, ElasticLoadBalancers, NextMarker};
        {error, Reason} ->
            {error, Reason}
    end.

extract_load_balancers(Node) ->
    [{load_balancer_name, get_text("LoadBalancerName", Node)},
    {dns_name, get_text("DNSName", Node)},
    {availibility_zones, [ get_text(Member) || Member <- xmerl_xpath:string("AvailabilityZones/member", Node)]},
    {backend_server_descriptions,
        [extract_backend_server_descriptions(Member) || Member <- xmerl_xpath:string("BackendServerDescriptions/member", Node)]},
    {canonical_hosted_zone_name, get_text("CanonicalHostedZoneName", Node)},
    {canonical_hosted_zone_name_id, get_text("CanonicalHostedZoneNameID", Node)},
    {created_time, get_text("CreatedTime", Node)},
    {health_check, [{healthythreshold, get_text("HealthCheck/HealthyThreshold", Node)},
        {interval, get_text("HealthCheck/Interval", Node)},
        {target, get_text("HealthCheck/Target", Node)},
        {timeout, get_text("HealthCheck/Timeout", Node)},
        {unhealthy_threshold, get_text("HealthCheck/UnhealthyThreshold", Node)}]},
    {instances, [extract_instances(Member) || Member <- xmerl_xpath:string("Instances/member",Node)]},
    {listener_descriptions,
        [extract_listener_descriptions(Member) || Member <- xmerl_xpath:string("ListenerDescriptions/member", Node)]},
    {policies, [{app_cookie_stickiness_policies, [extract_app_cookie_stickiness_policy(Member) ||
                      Member <- xmerl_xpath:string("Policies/AppCookieStickinessPolicies/member", Node)]},
                    {lb_cookie_stickiness_policies, [extract_lb_cookie_stickiness_policy(Member) ||
                      Member <- xmerl_xpath:string("Policies/LBCookieStickinessPolicies/member", Node)]},
                    {other_policies, [ get_text(Member) || Member <- xmerl_xpath:string("OtherPolicies/member", Node)]}]},
    {scheme, get_text("Scheme", Node)},
    {security_groups, [get_text(Member) || Member <- xmerl_xpath:string("SecurityGroups/member", Node)]},
    {source_security_group, [{group_name, get_text("SourceSecurityGroup/GroupName", Node)},
                                          {owner_alias, get_text("SourceSecurityGroup/OwnerAlias", Node)}]},
    {subnets, [get_text(Member) || Member <- xmerl_xpath:string("Subnets/member", Node)]},
    {vpc_id, get_text("VPCId", Node)}].

extract_backend_server_descriptions(Member) ->
  [{instance_port, get_text("InstancePort", Member)},
   {policy_names, [ get_text(PolicyName) || PolicyName <- xmerl_xpath:string("PolicyNames/member", Member)]}].

extract_instances(Member) ->
{instance_id, get_text("InstanceId", Member)}.

extract_listener_descriptions(Member) ->
    [{listener, [{instance_port, get_text("Listener/InstancePort", Member)},
                  {instance_protocol, get_text("Listener/InstanceProtocol", Member)},
                  {load_balancer_port, get_text("Listener/LoadBalancerPort", Member)},
                  {protocol, get_text("Listener/protocol", Member)},
                  {ssl_certificate_id, get_text("Listener/SSLCertificateId", Member)}
                  ]},
      {policy_names, [ get_text(PolicyName) || PolicyName <- xmerl_xpath:string("PolicyNames/member", Member)]}
    ].

extract_app_cookie_stickiness_policy(Member) ->
  [{cookie_name, get_text("CookieName", Member)},
   {policy_name, get_text("PolicyName", Member)}].

extract_lb_cookie_stickiness_policy(Member) ->
  [{cookie_expiration_period, get_text("CookieExpirationPeriod", Member)},
   {policy_name, get_text("PolicyName", Member)}].

elb_request(Config, Action, Params) ->
    elb_request(Config, Action, Params, ?API_VERSION).

elb_request(Config, Action, Params, ApiVersion) ->
    QParams = [{"Action", Action}, {"Version", ApiVersion} | Params],
    erlcloud_aws:aws_request_xml2(get, Config#aws_config.elb_host,
                                 "/", QParams, Config).

elb_simple_request(Config, Action, Params) ->
    _Doc = elb_request(Config, Action, Params),
    ok.