-module(erlcloud_elb).

%% Library initialization.
-export([configure/2, configure/3, new/2, new/3]).

%% ELB API Functions
-export([create_load_balancer/3, create_load_balancer/4, create_load_balancer/5,
         delete_load_balancer/1, delete_load_balancer/2,

         register_instance/2, register_instance/3,
         deregister_instance/2, deregister_instance/3,

         describe_load_balancer/1, describe_load_balancer/2,
         describe_load_balancers/1, describe_load_balancers/2,

         configure_health_check/2, configure_health_check/3]).

-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

-define(API_VERSION, "2009-05-15").

-import(erlcloud_xml, [get_text/2]).

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


describe_load_balancers(Names) ->
    describe_load_balancers(Names, default_config()).
describe_load_balancers(Names, Config) ->
    elb_request(Config,
                "DescribeLoadBalancers",
                [erlcloud_aws:param_list(Names, "LoadBalancerNames.member")]).




elb_request(Config, Action, Params) ->
    QParams = [{"Action", Action}, {"Version", ?API_VERSION} | Params],
    erlcloud_aws:aws_request_xml(get, Config#aws_config.elb_host,
                                 "/", QParams, Config).

elb_simple_request(Config, Action, Params) ->
    _Doc = elb_request(Config, Action, Params),
    ok.
