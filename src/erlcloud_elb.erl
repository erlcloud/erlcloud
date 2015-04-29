-module(erlcloud_elb).

%% Library initialization.
-export([configure/2, configure/3, new/2, new/3]).

%% ELB API Functions
-export([create_load_balancer/3, create_load_balancer/4, create_load_balancer/5,
         delete_load_balancer/1, delete_load_balancer/2,

         register_instance/2, register_instance/3,
         deregister_instance/2, deregister_instance/3,

         describe_load_balancer/1, describe_load_balancer/2,
         describe_load_balancers/0, describe_load_balancers/1, describe_load_balancers/2, describe_load_balancers/3, describe_load_balancers/4,

         configure_health_check/2, configure_health_check/3]).

-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

%-define(API_VERSION, "2009-05-15").
-define(API_VERSION, "2012-06-01").

-define(DEFAULT_MAX_RECORDS, 400).

% xpath for elb descriptions used in describe_groups functions:
-define(DESCRIBE_ELBS_PATH, 
        "/DescribeLoadBalancersResponse/DescribeLoadBalancersResult/LoadBalancerDescriptions/member").
-define(DESCRIBE_ELBS_NEXT_TOKEN, 
        "/DescribeLoadBalancersResponse/DescribeLoadBalancersResult/NextMarker").

-import(erlcloud_xml, [get_text/1, get_text/2, get_integer/2]).

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

%% --------------------------------------------------------------------
%% @doc describe_load_balancer with a specific balancer name or with a 
%% specific configuration and specific balancer name. 
%% @end
%% --------------------------------------------------------------------
describe_load_balancer(Name) when is_list(Name) ->
    describe_load_balancer(Name, default_config()).
describe_load_balancer(Name, Config) ->
    describe_load_balancers([Name], Config).

%% --------------------------------------------------------------------
%% @doc Calls describe_load_balancer([], default_configuration())
%% @end
%% --------------------------------------------------------------------
describe_load_balancers() ->
    describe_load_balancers([], default_config()).

%% --------------------------------------------------------------------
%% @doc describe_load_balancers with specific balancer names or with a 
%% specific configuration.
%% @end
%% --------------------------------------------------------------------
describe_load_balancers(Names) when is_list(Names) ->
    describe_load_balancers(Names, default_config());
describe_load_balancers(Config) when is_record(Config, aws_config) ->
    describe_load_balancers([], Config).

%% --------------------------------------------------------------------
%% @doc Get descriptions of the given load balancers.
%%      The account calling this function needs permission for the
%%      elasticloadbalancing:DescribeLoadBalancers action.
%% 
%% Returns {{paged, NextPageId}, Results} if there are more than
%% the current maximum count of results, {ok, Results} if everything
%% fits and {error, Reason} if there was a problem.
%% @end
%% --------------------------------------------------------------------
-spec describe_load_balancers(list(string()), aws_config()) -> 
                             {ok, term()} | {{paged, string()}, term()} | {error, term()}.
describe_load_balancers(Names, Config) ->
    describe_load_balancers(Names, ?DEFAULT_MAX_RECORDS, none, Config).

%% --------------------------------------------------------------------
%% @doc Get descriptions of the given load balancers with a given
%%      maximum number of results and optional paging offset.
%% @end
%% --------------------------------------------------------------------
-spec describe_load_balancers(list(string()), integer(), string() | none, aws_config()) -> 
                             {ok, term()} | {{paged, string()}, term()} | {error, term()}.
describe_load_balancers(Names, PageSize, none, Config) ->
    describe_load_balancers(Names, [{"PageSize", PageSize}], Config);
describe_load_balancers(Names, PageSize, Marker, Config) ->
    describe_load_balancers(Names, [{"Marker", Marker}, {"PageSize", PageSize}], Config).

-spec describe_load_balancers(list(string()), list({string(), term()}), aws_config()) -> 
                             {ok, term()} | {{paged, string()}, term()} | {error, term()}.
describe_load_balancers(Names, Params, Config) ->
    P = member_params("LoadBalancerNames.member.", Names) ++ Params,
    case elb_query(Config, "DescribeLoadBalancers", P) of
        {ok, Doc} ->
            Elbs = xmerl_xpath:string(?DESCRIBE_ELBS_PATH, Doc),            
            {next_token(?DESCRIBE_ELBS_NEXT_TOKEN, Doc), [extract_elb(Elb) || Elb <- Elbs]};
        {error, Reason} ->
            {error, Reason}
    end.
    
extract_elb(Item) ->
    [
        {load_balancer_name, get_text("LoadBalancerName", Item)},
        {scheme, get_text("Scheme", Item)},
        {availability_zones, [get_text(Z) || Z <- xmerl_xpath:string("AvailabilityZones/member", Item)]},
        {dns_name, get_text("DNSName", Item)},
        {source_security_group, [
                                    {group_name, get_text("SourceSecurityGroup/GroupName", Item)},
                                    {owner_alias, get_text("SourceSecurityGroup/OwnerAlias", Item)}
                                ]},
        {security_groups, [get_text(G) || G <- xmerl_xpath:string("SecurityGroups/member", Item)]},
        {subnets, [get_text(S) || S <- xmerl_xpath:string("Subnets/member", Item)]},
        {vpc_id, get_text("VPCId", Item)},
        {instances, [get_text("InstanceId", I) || I <- xmerl_xpath:string("Instances/member", Item)]},
        {canonical_hosted_zone_name, get_text("CanonicalHostedZoneName", Item)},
        {canonical_hosted_zone_id, get_text("CanonicalHostedZoneNameID", Item)},
        {create_time, erlcloud_xml:get_time("CreatedTime", Item)},
        {listeners, [extract_listener(L) || L <- xmerl_xpath:string("ListenerDescriptions/member/Listener", Item)]}
    ].

extract_listener(Item) ->
    [
        {protocol, get_text("Listener/Protocol", Item)},
        {port, get_integer("Listener/LoadBalancerPort", Item)},
        {instance_protocol, get_text("Listener/InstanceProtocol", Item)},
        {instance_port, get_integer("Listener/InstancePort", Item)},
        {ssl_certificate_id, get_text("Listener/SSLCertificateId", Item)}
    ].


%% retrieve NextToken from the XML at Path location.  Path is expected to lead to a 
%% single occurrence and if it does not exist as such, this just returns ok.
-spec next_token(string(), term()) -> ok | {paged, string()}.
next_token(Path, XML) ->
    case xmerl_xpath:string(Path, XML) of
        [Next] ->
            {paged, erlcloud_xml:get_text(Next)};
        _ ->
            ok
    end.

%% given a list of member identifiers, return a list of 
%% {key with prefix, member identifier} for use in elb calls.
%% Example pair that could be returned in a list is 
%% {"LoadBalancerNames.member.1", "my-elb}.
-spec member_params(string(), list(string())) -> list({string(), string()}).
member_params(Prefix, MemberIdentifiers) ->
    MemberKeys = [Prefix ++ integer_to_list(I) || I <- lists:seq(1, length(MemberIdentifiers))],
    [{K, V} || {K, V} <- lists:zip(MemberKeys, MemberIdentifiers)].
 

elb_query(Config, Action, Params) ->
    elb_query(Config, Action, Params, ?API_VERSION).

elb_query(Config, Action, Params, ApiVersion) ->
    QParams = [{"Action", Action}, {"Version", ApiVersion}|Params],
    erlcloud_aws:aws_request_xml2(post, 
                                  Config#aws_config.elb_host,
                                  "/", QParams, Config).


elb_request(Config, Action, Params) ->
    QParams = [{"Action", Action}, {"Version", ?API_VERSION} | Params],
    erlcloud_aws:aws_request_xml(get, Config#aws_config.elb_host,
                                 "/", QParams, Config).

elb_simple_request(Config, Action, Params) ->
    _Doc = elb_request(Config, Action, Params),
    ok.
