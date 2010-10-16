-module(erlcloud_elb).

-type elb_attribute() :: {string(), string() | {replace, string()}}.
-type elb_attributes() :: [elb_attribute()].
-type elb_conditional() :: {string(), string() | exists | not_exists}.
-type elb_conditionals() :: [elb_conditional()].
-type elb_delete_attribute() :: {string(), string()} | string().
-type elb_delete_attributes() :: [elb_delete_attribute()].

%% Library initialization.
-export([configure/2, configure/3, new/2, new/3]).

%% ELB API Functions
-export([create_load_balancer/3, create_load_balancer/4,
         delete_load_balancer/1, delete_load_balancer/2,

         register_instance/2, register_instance/3,
         deregister_instance/2, deregister_instance/3]).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

-define(API_VERSION, "2009-05-15").

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
    elb_simple_request(Config,
                       "CreateLoadBalancer",
                       [{"AvailabilityZones.member.1", "us-east-1d"},
                        {"LoadBalancerName", LB} |
                        erlcloud_aws:param_list([[{"LoadBalancerPort", Port},
                                                  {"InstancePort", Port},
                                                  {"Protocol", string:to_upper(atom_to_list(Protocol))}]],
                                                "Listeners.member")]).


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
                       "DeregisterInstancesWithLoadBalancer",
                       [{"LoadBalancerName", [LB]} |
                        erlcloud_aws:param_list([[{"InstanceId", InstanceId}]], "Instances.member")]).





elb_request(Config, Action, Params) ->
    QParams = [{"Action", Action}, {"Version", ?API_VERSION} | Params],
    erlcloud_aws:aws_request_xml(get, Config#aws_config.elb_host,
                                 "/", QParams, Config#aws_config.access_key_id,
                                 Config#aws_config.secret_access_key).

elb_simple_request(Config, Action, Params) ->
    _Doc = elb_request(Config, Action, Params),
    ok.

