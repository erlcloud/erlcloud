-module(erlcloud_elb).

%% Library initialization.
-export([configure/2, configure/3, new/2, new/3]).

%% ELB API Functions
-export([create_load_balancer/3, create_load_balancer/4, create_load_balancer/5,
         delete_load_balancer/1, delete_load_balancer/2,

         register_instance/2, register_instance/3,
         deregister_instance/2, deregister_instance/3,

         describe_load_balancer/1, describe_load_balancer/2,

         describe_load_balancers/0, describe_load_balancers/1, 
         describe_load_balancers/2, describe_load_balancers/3, describe_load_balancers/4,
         describe_load_balancers_all/0, describe_load_balancers_all/1, describe_load_balancers_all/2,

         configure_health_check/2, configure_health_check/3,
         
         create_load_balancer_policy/3, create_load_balancer_policy/4, create_load_balancer_policy/5,
         delete_load_balancer_policy/2, delete_load_balancer_policy/3,
         
         describe_load_balancer_policies/0, describe_load_balancer_policies/1, 
         describe_load_balancer_policies/2, describe_load_balancer_policies/3,
         
         describe_load_balancer_policy_types/0, describe_load_balancer_policy_types/1, 
         describe_load_balancer_policy_types/2,

         describe_load_balancer_attributes/1, describe_load_balancer_attributes/2]).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

-define(API_VERSION, "2012-06-01").

-define(DEFAULT_MAX_RECORDS, 400).

% xpath for elb descriptions used in describe_groups functions:
-define(DESCRIBE_ELBS_PATH, 
        "/DescribeLoadBalancersResponse/DescribeLoadBalancersResult/LoadBalancerDescriptions/member").
-define(DESCRIBE_ELBS_NEXT_TOKEN, 
        "/DescribeLoadBalancersResponse/DescribeLoadBalancersResult/NextMarker").
-define(DESCRIBE_ELB_POLICIES_PATH, 
        "/DescribeLoadBalancerPoliciesResponse/DescribeLoadBalancerPoliciesResult/PolicyDescriptions/member").
-define(DESCRIBE_ELB_POLICY_TYPE_PATH, 
        "/DescribeLoadBalancerPolicyTypesResponse/DescribeLoadBalancerPolicyTypesResult/PolicyTypeDescriptions/member").

-import(erlcloud_xml, [get_text/2, get_integer/2, get_list/2]).

%%%===================================================================
%%% API
%%%===================================================================
-spec new(string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey}.

-spec new(string(), string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                elb_host=Host}.

-spec configure(string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey) ->
    erlcloud_config:configure(AccessKeyID, SecretAccessKey, fun new/2).

-spec configure(string(), string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host) ->
    erlcloud_config:configure(AccessKeyID, SecretAccessKey, Host, fun new/3).

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


-spec register_instance(string(), string()) -> ok | no_return().
register_instance(LB, InstanceId) ->
    register_instance(LB, InstanceId, default_config()).

-spec register_instance(string(), string(), aws_config()) -> ok | no_return().
register_instance(LB, InstanceId, Config) when is_list(LB) ->
    elb_simple_request(Config,
                       "RegisterInstancesWithLoadBalancer",
                       [{"LoadBalancerName", LB} |
                        erlcloud_aws:param_list([[{"InstanceId", InstanceId}]], "Instances.member")]).


-spec deregister_instance(string(), string()) -> ok | no_return().
deregister_instance(LB, InstanceId) ->
    deregister_instance(LB, InstanceId, default_config()).

-spec deregister_instance(string(), string(), aws_config()) -> ok | no_return().
deregister_instance(LB, InstanceId, Config) when is_list(LB) ->
    elb_simple_request(Config,
                       "DeregisterInstancesFromLoadBalancer",
                       [{"LoadBalancerName", LB} |
                        erlcloud_aws:param_list([[{"InstanceId", InstanceId}]], "Instances.member")]).



-spec configure_health_check(string(), string()) -> ok | no_return().
configure_health_check(LB, Target) when is_list(LB),
                                        is_list(Target) ->
    configure_health_check(LB, Target, default_config()).


-spec configure_health_check(string(), string(), aws_config()) -> ok | no_return().
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
-spec describe_load_balancer(string()) -> {ok, term()} | {{paged, string()}, term()} | {error, metadata_not_available | container_credentials_unavailable | erlcloud_aws:httpc_result_error()}.
describe_load_balancer(Name) ->
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
describe_load_balancers(Config = #aws_config{}) ->
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
                             {ok, term()} | {{paged, string()}, term()} | {error, metadata_not_available | container_credentials_unavailable | erlcloud_aws:httpc_result_error()}.
describe_load_balancers(Names, Config) ->
    describe_load_balancers(Names, ?DEFAULT_MAX_RECORDS, none, Config).

%% --------------------------------------------------------------------
%% @doc Get descriptions of the given load balancers with a given
%%      maximum number of results and optional paging offset.
%% @end
%% --------------------------------------------------------------------
-spec describe_load_balancers(list(string()), integer(), string() | none, aws_config()) -> 
                             {ok, term()} | {{paged, string()}, term()} | {error, metadata_not_available | container_credentials_unavailable | erlcloud_aws:httpc_result_error()}.
describe_load_balancers(Names, PageSize, none, Config) ->
    describe_load_balancers(Names, [{"PageSize", PageSize}], Config);
describe_load_balancers(Names, PageSize, Marker, Config) ->
    describe_load_balancers(Names, [{"Marker", Marker}, {"PageSize", PageSize}], Config).

-spec describe_load_balancers(list(string()), list({string(), term()}), aws_config()) -> 
                             {ok, term()} | {{paged, string()}, term()} | {error, metadata_not_available | container_credentials_unavailable | erlcloud_aws:httpc_result_error()}.
describe_load_balancers(Names, Params, Config) ->
    P = member_params("LoadBalancerNames.member.", Names) ++ Params,
    case elb_query(Config, "DescribeLoadBalancers", P) of
        {ok, Doc} ->
            Elbs = xmerl_xpath:string(?DESCRIBE_ELBS_PATH, Doc),            
            {erlcloud_util:next_token(?DESCRIBE_ELBS_NEXT_TOKEN, Doc), [extract_elb(Elb) || Elb <- Elbs]};
        {error, Reason} ->
            {error, Reason}
    end.

-spec describe_load_balancers_all() ->
    {ok, [term()]} | {error, term()}.
describe_load_balancers_all() ->
    describe_load_balancers_all(default_config()).

-spec describe_load_balancers_all(list(string()) | aws_config()) ->
    {ok, [term()]} | {error, term()}.
describe_load_balancers_all(Config) when is_record(Config, aws_config) ->
    describe_load_balancers_all([], default_config());
describe_load_balancers_all(Names) ->
    describe_load_balancers_all(Names, default_config()).

-spec describe_load_balancers_all(list(string()), aws_config()) ->
    {ok, [term()]} | {error, term()}.
describe_load_balancers_all(Names, Config) ->
    describe_all(
        fun(Marker, Cfg) ->
            describe_load_balancers(
                Names, ?DEFAULT_MAX_RECORDS, Marker, Cfg
            )
        end, Config, none, []).
    
    
extract_elb(Item) ->
    [
        {load_balancer_name, get_text("LoadBalancerName", Item)},
        {scheme, get_text("Scheme", Item)},
        {availability_zones, get_list("AvailabilityZones/member", Item)},
        {dns_name, get_text("DNSName", Item)},
        {source_security_group, [
                                    {group_name, get_text("SourceSecurityGroup/GroupName", Item)},
                                    {owner_alias, get_text("SourceSecurityGroup/OwnerAlias", Item)}
                                ]},
        {security_groups, get_list("SecurityGroups/member", Item)},
        {subnets, get_list("Subnets/member", Item)},
        {vpc_id, get_text("VPCId", Item)},
        {instances, get_list("Instances/member/InstanceId", Item)},
        {canonical_hosted_zone_name, get_text("CanonicalHostedZoneName", Item)},
        {canonical_hosted_zone_id, get_text("CanonicalHostedZoneNameID", Item)},
        {create_time, erlcloud_xml:get_time("CreatedTime", Item)},
        {listeners, [extract_listener(L) || L <- xmerl_xpath:string("ListenerDescriptions/member", Item)]},
        {policies, [
                        {app_cookie_stickiness, get_list("Policies/AppCookieStickinessPolicies/member", Item)},
                        {lb_cookie_stickiness, get_list("Policies/LBCookieStickinessPolicies/member", Item)},
                        {other, get_list("Policies/OtherPolicies/member", Item)}
                    ]}
    ].

extract_listener(Item) ->
    [
        {protocol, get_text("Listener/Protocol", Item)},
        {port, get_integer("Listener/LoadBalancerPort", Item)},
        {instance_protocol, get_text("Listener/InstanceProtocol", Item)},
        {instance_port, get_integer("Listener/InstancePort", Item)},
        {ssl_certificate_id, get_text("Listener/SSLCertificateId", Item)},
        {policy_names, get_list("PolicyNames/member", Item)}
    ].

%% --------------------------------------------------------------------
%% @doc Calls describe_load_balancer_policies([], 
%%              default_configuration())
%% @end
%% --------------------------------------------------------------------
-spec describe_load_balancer_policies() -> 
                             {ok, term()} | {error, term()}.
describe_load_balancer_policies() ->
    describe_load_balancer_policies([], [], default_config()).

%% --------------------------------------------------------------------
%% @doc describe_load_balancer_policies with specific config.
%% @end
%% --------------------------------------------------------------------
-spec describe_load_balancer_policies(aws_config()) -> 
                             {ok, term()} | {error, term()}.
describe_load_balancer_policies(Config = #aws_config{}) ->
    describe_load_balancer_policies([], [], Config).

%% --------------------------------------------------------------------
%% @doc describe_load_balancer_policies for specified ELB 
%%      with specificied policy names using default config.
%% @end
%% --------------------------------------------------------------------
-spec describe_load_balancer_policies(string(), list() | aws_config()) -> 
                             {ok, term()} | {error, term()}.
describe_load_balancer_policies(ElbName, PolicyNames) 
    when is_list(ElbName),
         is_list(PolicyNames) ->
    describe_load_balancer_policies(ElbName, PolicyNames, default_config());
describe_load_balancer_policies(PolicyNames, Config = #aws_config{}) 
    when is_list(PolicyNames) ->
    describe_load_balancer_policies([], PolicyNames, Config).


%% --------------------------------------------------------------------
%% @doc Get descriptions of the given load balancer policies
%%      with specified config.
%% @end
%% --------------------------------------------------------------------
-spec describe_load_balancer_policies(string(), list(), aws_config()) -> 
                             {ok, term()} | {error, term()}.
describe_load_balancer_policies(ElbName, PolicyNames, Config)
    when is_list(ElbName),
         is_list(PolicyNames) ->
    ElbNameParam = case ElbName of
        [] ->
            [];
        _ ->
            [{"LoadBalancerName", ElbName}]
    end,
    Params = ElbNameParam ++ member_params("PolicyNames.member.", PolicyNames),
    case elb_query(Config, "DescribeLoadBalancerPolicies", Params) of
        {ok, Doc} ->
            ElbPolicies = xmerl_xpath:string(?DESCRIBE_ELB_POLICIES_PATH, Doc),            
            {ok, [extract_elb_policy(ElbPolicy) || ElbPolicy <- ElbPolicies]};
        {error, Reason} ->
            {error, Reason}
    end.

extract_elb_policy(Item) ->
    [
        {policy_name, get_text("PolicyName", Item)},
        {policy_type_name, get_text("PolicyTypeName", Item)},
        {policy_attributes, 
            [extract_policy_attribute(A) || 
                A <- xmerl_xpath:string("PolicyAttributeDescriptions/member", Item)]}
    ].

extract_policy_attribute(Item) ->
    [
        {attr_name, get_text("AttributeName", Item)},
        {attr_value, get_text("AttributeValue", Item)}
    ].

%% --------------------------------------------------------------------
%% @doc Calls describe_load_balancer_policy_types([], 
%%                  default_configuration())
%% @end
%% --------------------------------------------------------------------
describe_load_balancer_policy_types() ->
    describe_load_balancer_policy_types([], default_config()).

%% --------------------------------------------------------------------
%% @doc describe_load_balancer_policy_types() with specific 
%% policy type names.
%% @end
%% --------------------------------------------------------------------
describe_load_balancer_policy_types(PolicyTypeNames) when is_list(PolicyTypeNames) ->
    describe_load_balancer_policy_types(PolicyTypeNames, default_config());
describe_load_balancer_policy_types(Config = #aws_config{}) ->
    describe_load_balancer_policy_types([], Config).

%% --------------------------------------------------------------------
%% @doc Get descriptions of the given load balancer policy types.
%% @end
%% --------------------------------------------------------------------
-spec describe_load_balancer_policy_types(list(string()), aws_config()) -> 
                             {ok, term()} | {error, term()}.
describe_load_balancer_policy_types(PolicyTypeNames, Config) ->
    P = member_params("PolicyTypeNames.member.", PolicyTypeNames),
    case elb_query(Config, "DescribeLoadBalancerPolicyTypes", P) of
        {ok, Doc} ->
            ElbPolicyTypes = xmerl_xpath:string(?DESCRIBE_ELB_POLICY_TYPE_PATH, Doc),            
            {ok, [extract_elb_policy_type(ElbPolicyType) || ElbPolicyType <- ElbPolicyTypes]};
        {error, Reason} ->
            {error, Reason}
    end.

extract_elb_policy_type(Item) ->
    [
        {policy_type_name, get_text("PolicyTypeName", Item)},
        {policy_type_description, get_text("Description", Item)},
        {policy_type_attributes, 
            [extract_policy_type_attribute(A) || 
                A <- xmerl_xpath:string("PolicyAttributeTypeDescriptions/member", Item)]}
    ].

extract_policy_type_attribute(Item) ->
    [
        {attr_name, get_text("AttributeName", Item)},
        {attr_type, get_text("AttributeType", Item)},
        {cardinality, get_text("Cardinality", Item)},
        {description, get_text("Description", Item)},
        {default_value, get_text("DefaultValue", Item)}
    ].
%% --------------------------------------------------------------------
%% @doc Calls create_load_balancer_policy() without attributes and 
%% with default aws config.
%% @end
%% --------------------------------------------------------------------
-spec create_load_balancer_policy(string(), string(), string()) -> 
                                    ok | {error, term()} | no_return().
create_load_balancer_policy(LB, PolicyName, PolicyTypeName) 
    when is_list(LB),
         is_list(PolicyName),
         is_list(PolicyTypeName) ->
    create_load_balancer_policy(LB, PolicyName, PolicyTypeName, [], default_config()).

%% --------------------------------------------------------------------
%% @doc Calls create_load_balancer_policy() with default aws config.
%% @end
%% --------------------------------------------------------------------
-spec create_load_balancer_policy(string(), string(), string(), list({string(), string()})) -> 
                                    ok | {error, term()} | no_return().
create_load_balancer_policy(LB, PolicyName, PolicyTypeName, Attrs) 
    when is_list(LB),
         is_list(PolicyName),
         is_list(PolicyTypeName),
         is_list(Attrs)->
    create_load_balancer_policy(LB, PolicyName, PolicyTypeName, Attrs, default_config()).

%% --------------------------------------------------------------------
%% @doc Create a load balancer policy with given parameters.
%% http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_CreateLoadBalancerPolicy.html
%% @end
%% --------------------------------------------------------------------
-spec create_load_balancer_policy(string(), string(), string(), list({string(), string()}), aws_config()) -> 
                                    ok | {error, term()} | no_return().
create_load_balancer_policy(LB, PolicyName, PolicyTypeName, AttrList, Config) 
    when is_list(LB),
         is_list(PolicyName),
         is_list(PolicyTypeName),
         is_list(AttrList)->
    _XML = elb_request(Config,
                      "CreateLoadBalancerPolicy",
                      [{"LoadBalancerName", LB},
                       {"PolicyName", PolicyName},
                       {"PolicyTypeName", PolicyTypeName} |
                       erlcloud_aws:param_list([[{"AttributeName", AttrName},
                                                 {"AttributeValue", AttrValue}] || 
                                                {AttrName, AttrValue} <- AttrList],
                                               "PolicyAttributes.member")]),
    ok.


-spec describe_load_balancer_attributes(string()) -> proplist() | no_return().
describe_load_balancer_attributes(Name) ->
    describe_load_balancer_attributes(Name, default_config()).

-spec describe_load_balancer_attributes(string(), aws_config()) -> proplist() | no_return().
describe_load_balancer_attributes(Name, Config) ->
    Node = elb_request(Config,
        "DescribeLoadBalancerAttributes",
        [{"LoadBalancerName", Name}]),
    extract_elb_attribs(Node).


%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec extract_elb_attribs(proplist()) -> proplist().
extract_elb_attribs(Node) ->
    RootPath = "DescribeLoadBalancerAttributesResult/LoadBalancerAttributes",
    erlcloud_xml:decode(
        [
            {access_log_enabled, RootPath ++ "/AccessLog/Enabled", boolean},
            {access_log_s3_name, RootPath ++ "/AccessLog/S3BucketName", text},
            {access_log_s3_prefix, RootPath ++ "/AccessLog/S3BucketPrefix", text},
            {access_log_emit_interval, RootPath ++ "/AccessLog/EmitInterval", integer},

            {connection_settings_idletimeout, RootPath ++ "/ConnectionSettings/IdleTimeout", integer},

            {cross_zone_load_balancing_enabled, RootPath ++ "/CrossZoneLoadBalancing/Enabled", boolean},

            {connection_draining_enabled, RootPath ++ "/ConnectionDraining/Enabled", boolean},
            {connection_draining_timeout, RootPath ++ "/ConnectionDraining/Timeout", integer}
        ], Node).

%% --------------------------------------------------------------------
%% @doc Calls delete_load_balancer_policy() with default aws config.
%% @end
%% --------------------------------------------------------------------
-spec delete_load_balancer_policy(string(), string()) -> ok | no_return().
delete_load_balancer_policy(LB, PolicyName) when is_list(LB), 
                                                 is_list(PolicyName) ->
    delete_load_balancer_policy(LB, PolicyName, default_config()).

%% --------------------------------------------------------------------
%% @doc Deletes the specified policy from the specified load balancer. 
%% This policy must not be enabled for any listeners.
%% @end
%% --------------------------------------------------------------------
-spec delete_load_balancer_policy(string(), string(), aws_config()) -> ok | no_return().
delete_load_balancer_policy(LB, PolicyName, Config) when is_list(LB),
                                             is_list(PolicyName)->
    elb_simple_request(Config,
                       "DeleteLoadBalancerPolicy",
                       [{"LoadBalancerName", LB},
                        {"PolicyName", PolicyName}]).

%% given a list of member identifiers, return a list of 
%% {key with prefix, member identifier} for use in elb calls.
%% Example pair that could be returned in a list is 
%% {"LoadBalancerNames.member.1", "my-elb}.
-spec member_params(string(), list(string())) -> list({string(), string()}).
member_params(Prefix, MemberIdentifiers) ->
    MemberKeys = [Prefix ++ integer_to_list(I) || I <- lists:seq(1, length(MemberIdentifiers))],
    [{K, V} || {K, V} <- lists:zip(MemberKeys, MemberIdentifiers)].
 

describe_all(Fun, AwsConfig, Marker, Acc) ->
    case Fun(Marker, AwsConfig) of
        {ok, Res} ->
            {ok, lists:append(Acc, Res)};
        {{paged, NewMarker}, Res} ->
            describe_all(Fun, AwsConfig, NewMarker, lists:append(Acc, Res));
        {error, Reason} ->
            {error, Reason}
    end.


elb_query(Config, Action, Params) ->
    elb_query(Config, Action, Params, ?API_VERSION).

elb_query(Config, Action, Params, ApiVersion) ->
    QParams = [{"Action", Action}, {"Version", ApiVersion}|Params],
    erlcloud_aws:aws_request_xml4(post,
                                  Config#aws_config.elb_host,
                                  "/", QParams, "elasticloadbalancing", Config).

elb_simple_request(Config, Action, Params) ->
    _Doc = elb_request(Config, Action, Params),
    ok.

elb_request(Config, Action, Params) ->
    QParams = [{"Action", Action}, {"Version", ?API_VERSION} | Params],
    case erlcloud_aws:aws_request_xml4(post,
                                  Config#aws_config.elb_host,
                                  "/", QParams, "elasticloadbalancing", Config)
    of
        {ok, Body} ->
            Body;
        {error, Reason} ->
            erlang:error({aws_error, Reason})
    end.
