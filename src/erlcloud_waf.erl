-module(erlcloud_waf).
-author('pavel@alertlogic.com').

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").
-include("erlcloud_waf.hrl").

%%% Library initialization.
-export([configure/2, configure/3, configure/4, new/2, new/3, new/4]).

-export([create_byte_match_set/2, create_byte_match_set/3,
        create_ip_set/2, create_ip_set/3,
        create_rule/3, create_rule/4,
        create_size_constraint_set/2, create_size_constraint_set/3,
        create_sql_injection_match_set/2, create_sql_injection_match_set/3,
        create_web_acl/4, create_web_acl/5,
        create_xss_match_set/2, create_xss_match_set/3,
        delete_byte_match_set/2, delete_byte_match_set/3,
        delete_ip_set/2, delete_ip_set/3,
        delete_rule/2, delete_rule/3,
        delete_size_constraint_set/2, delete_size_constraint_set/3,
        delete_sql_injection_match_set/2, delete_sql_injection_match_set/3,
        delete_web_acl/2, delete_web_acl/3,
        delete_xss_match_set/2, delete_xss_match_set/3,
        get_byte_match_set/1, get_byte_match_set/2,
        get_change_token/0, get_change_token/1,
        get_change_token_status/1, get_change_token_status/2,
        get_ip_set/1, get_ip_set/2,
        get_rule/1, get_rule/2,
        get_sampled_requests/5, get_sampled_requests/6,
        get_size_constraint_set/1, get_size_constraint_set/2,
        get_sql_injection_match_set/1, get_sql_injection_match_set/2,
        get_web_acl/1, get_web_acl/2,
        get_xss_match_set/1, get_xss_match_set/2,
        list_byte_match_sets/0, list_byte_match_sets/1, list_byte_match_sets/2, list_byte_match_sets/3,
        list_ip_sets/0, list_ip_sets/1, list_ip_sets/2, list_ip_sets/3,
        list_rules/0, list_rules/1, list_rules/2, list_rules/3,
        list_size_constraint_sets/0, list_size_constraint_sets/1, list_size_constraint_sets/2, list_size_constraint_sets/3,
        list_sql_injection_match_sets/0, list_sql_injection_match_sets/1, list_sql_injection_match_sets/2, list_sql_injection_match_sets/3,
        list_web_acls/0, list_web_acls/1, list_web_acls/2, list_web_acls/3,
        list_xss_match_sets/0, list_xss_match_sets/1, list_xss_match_sets/2, list_xss_match_sets/3,
        update_byte_match_set/3, update_byte_match_set/4,
        update_ip_set/3, update_ip_set/4,
        update_rule/3, update_rule/4,
        update_size_constraint_set/3, update_size_constraint_set/4,
        update_sql_injection_match_set/3, update_sql_injection_match_set/4,
        update_web_acl/3, update_web_acl/4,
        update_xss_match_set/3, update_xss_match_set/4
        ]).


-define(API_VERSION, "20150824").
-define(LIMIT_MAX, 100).

-type json_proplist() :: proplists:proplist().

%%%------------------------------------------------------------------------------
%%% Shared types
%%%------------------------------------------------------------------------------
-type waf_return_val() :: {ok, json_proplist()} | {error, term()}.

-export_type([waf_return_val/0]).

%%%------------------------------------------------------------------------------
%%% Library initialization.
%%%------------------------------------------------------------------------------

-spec new(string(), string()) -> aws_config().

new(AccessKeyID, SecretAccessKey) ->
    #aws_config{
       access_key_id=AccessKeyID,
       secret_access_key=SecretAccessKey
      }.

-spec new(string(), string(), string()) -> aws_config().

new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{
       access_key_id=AccessKeyID,
       secret_access_key=SecretAccessKey,
       waf_host=Host
      }.

-spec new(string(), string(), string(), non_neg_integer()) -> aws_config().

new(AccessKeyID, SecretAccessKey, Host, Port) ->
    #aws_config{
       access_key_id=AccessKeyID,
       secret_access_key=SecretAccessKey,
       waf_host=Host,
       waf_port=Port
      }.

-spec configure(string(), string()) -> ok.

configure(AccessKeyID, SecretAccessKey) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey)),
    ok.

-spec configure(string(), string(), string()) -> ok.

configure(AccessKeyID, SecretAccessKey, Host) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host)),
    ok.

-spec configure(string(), string(), string(), non_neg_integer()) -> ok.

configure(AccessKeyID, SecretAccessKey, Host, Port) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host, Port)),
    ok.

default_config() ->
    erlcloud_aws:default_config().

%%%------------------------------------------------------------------------------
%%% AWS WAF API Functions
%%%------------------------------------------------------------------------------

%%%------------------------------------------------------------------------------
%%% CreateByteMatchSet 
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_CreateByteMatchSet.html
%%%------------------------------------------------------------------------------
-spec create_byte_match_set(ChangeToken :: string() | binary(),
                            Name :: string() | binary()) ->
    waf_return_val().
create_byte_match_set(ChangeToken, Name) ->
    create_byte_match_set(ChangeToken, Name, default_config()).

-spec create_byte_match_set(ChangeToken :: string() | binary(),
                            Name :: string() | binary(),
                            Config :: aws_config()) ->
    waf_return_val().
create_byte_match_set(ChangeToken, Name, Config) ->
    Json = [{<<"ChangeToken">>, to_binary(ChangeToken)},
            {<<"Name">>, to_binary(Name)}],
    waf_request(Config, "CreateByteMatchSet", Json).


%%%------------------------------------------------------------------------------
%%% CreateIPSet
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_CreateIPSet.html
%%%------------------------------------------------------------------------------
-spec create_ip_set(ChangeToken :: string() | binary(),
                    Name :: string() | binary()) ->
    waf_return_val().
create_ip_set(ChangeToken, Name) ->
    create_ip_set(ChangeToken, Name, default_config()).

-spec create_ip_set(ChangeToken :: string() | binary(),
                    Name :: string() | binary(),
                    Config :: aws_config()) ->
    waf_return_val().
create_ip_set(ChangeToken, Name, Config) ->
    Json = [{<<"ChangeToken">>, to_binary(ChangeToken)},
            {<<"Name">>, to_binary(Name)}],
    waf_request(Config, "CreateIPSet", Json).


%%%------------------------------------------------------------------------------
%%% CreateRule
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_CreateRule.html
%%%------------------------------------------------------------------------------
-spec create_rule(ChangeToken :: string() | binary(),
                  Name :: string() | binary(),
                  MetricName :: string() | binary()) ->
    waf_return_val().
create_rule(ChangeToken, Name, MetricName) ->
    create_rule(ChangeToken, Name, MetricName, default_config()).

-spec create_rule(ChangeToken :: string() | binary(),
                  Name :: string() | binary(),
                  MetricName :: string() | binary(),
                  Config :: aws_config()) ->
    waf_return_val().
create_rule(ChangeToken, Name, MetricName, Config) ->
    Json = [{<<"ChangeToken">>, to_binary(ChangeToken)},
            {<<"MetricName">>, to_binary(MetricName)},
            {<<"Name">>, to_binary(Name)}],
    waf_request(Config, "CreateRule", Json).


%%%------------------------------------------------------------------------------
%%% CreateSizeConstraintSet
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_CreateSizeConstraintSet.html
%%%------------------------------------------------------------------------------
-spec create_size_constraint_set(ChangeToken :: string() | binary(),
                                 Name :: string() | binary()) ->
    waf_return_val().
create_size_constraint_set(ChangeToken, Name) ->
    create_size_constraint_set(ChangeToken, Name, default_config()).

-spec create_size_constraint_set(ChangeToken :: string() | binary(),
                                 Name :: string() | binary(),
                                 Config :: aws_config()) ->
    waf_return_val().
create_size_constraint_set(ChangeToken, Name, Config) ->
    Json = [{<<"ChangeToken">>, to_binary(ChangeToken)},
            {<<"Name">>, to_binary(Name)}],
    waf_request(Config, "CreateSizeConstraintSet", Json).


%%%------------------------------------------------------------------------------
%%% CreateSqlInjectionMatchSet 
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_CreateSqlInjectionMatchSet.html
%%%------------------------------------------------------------------------------
-spec create_sql_injection_match_set(ChangeToken :: string() | binary(),
                                     Name :: string() | binary()) ->
    waf_return_val().
create_sql_injection_match_set(ChangeToken, Name) ->
    create_sql_injection_match_set(ChangeToken, Name, default_config()).

-spec create_sql_injection_match_set(ChangeToken :: string() | binary(),
                                     Name :: string() | binary(),
                                     Config :: aws_config()) ->
    waf_return_val().
create_sql_injection_match_set(ChangeToken, Name, Config) ->
    Json = [{<<"ChangeToken">>, to_binary(ChangeToken)},
            {<<"Name">>, to_binary(Name)}],
    waf_request(Config, "CreateSqlInjectionMatchSet", Json).


%%%------------------------------------------------------------------------------
%%% CreateWebACL
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_CreateWebACL.html
%%%------------------------------------------------------------------------------
-spec create_web_acl(ChangeToken :: string() | binary(),
                     Name :: string() | binary(),
                     MetricName :: string() | binary(),
                     DefaultAction:: waf_acl_action_type()) ->
    waf_return_val().
create_web_acl(ChangeToken, Name, MetricName, DefaultAction) ->
    create_web_acl(ChangeToken, Name, MetricName, DefaultAction, default_config()).

-spec create_web_acl(ChangeToken :: string() | binary(),
                     Name :: string() | binary(),
                     MetricName :: string() | binary(),
                     DefaultAction:: waf_acl_action_type(),
                     Config :: aws_config()) ->
    waf_return_val().
create_web_acl(ChangeToken, Name, MetricName, DefaultAction, Config) ->
    Json = [{<<"ChangeToken">>, to_binary(ChangeToken)},
            {<<"DefaultAction">>, [{<<"Type">>, get_default_action_type(DefaultAction)}]},
            {<<"MetricName">>, to_binary(MetricName)},
            {<<"Name">>, to_binary(Name)}],
    waf_request(Config, "CreateWebACL", Json).

%%%------------------------------------------------------------------------------
%%% CreateXssMatchSet
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_CreateXssMatchSet.html
%%%------------------------------------------------------------------------------
-spec create_xss_match_set(ChangeToken :: string() | binary(),
                           Name :: string() | binary()) ->
    waf_return_val().
create_xss_match_set(ChangeToken, Name) ->
    create_xss_match_set(ChangeToken, Name, default_config()).

-spec create_xss_match_set(ChangeToken :: string() | binary(),
                           Name :: string() | binary(),
                           Config :: aws_config()) ->
    waf_return_val().
create_xss_match_set(ChangeToken, Name, Config) ->
    Json = [{<<"ChangeToken">>, to_binary(ChangeToken)},
            {<<"Name">>, to_binary(Name)}],
    waf_request(Config, "CreateXssMatchSet", Json).



%%%------------------------------------------------------------------------------
%%% DeleteByteMatchSet
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_DeleteByteMatchSet.html
%%%------------------------------------------------------------------------------
-spec delete_byte_match_set(ChangeToken :: string() | binary(),
                            ByteMatchSetId :: string() | binary()) ->
    waf_return_val().
delete_byte_match_set(ChangeToken, ByteMatchSetId) ->
    delete_byte_match_set(ChangeToken, ByteMatchSetId, default_config()).

-spec delete_byte_match_set(ChangeToken:: string() | binary(),
                            ByteMatchSetId :: string() | binary(),
                            Config :: aws_config()) ->
    waf_return_val().
delete_byte_match_set(ChangeToken, ByteMatchSetId, Config) ->
    Json = [{<<"ChangeToken">>, to_binary(ChangeToken)},
            {<<"ByteMatchSetId">>, to_binary(ByteMatchSetId)}],
    waf_request(Config, "DeleteByteMatchSet", Json).


%%%------------------------------------------------------------------------------
%%% DeleteIPSet
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_DeleteIPSet.html
%%%------------------------------------------------------------------------------
-spec delete_ip_set(ChangeToken :: string() | binary(),
                    IPSetId :: string() | binary()) ->
    waf_return_val().
delete_ip_set(ChangeToken, IPSetId) ->
    delete_ip_set(ChangeToken, IPSetId, default_config()).

-spec delete_ip_set(ChangeToken:: string() | binary(),
                    IPSetId :: string() | binary(),
                    Config :: aws_config()) ->
    waf_return_val().
delete_ip_set(ChangeToken, IPSetId, Config) ->
    Json = [{<<"ChangeToken">>, to_binary(ChangeToken)},
            {<<"IPSetId">>, to_binary(IPSetId)}],
    waf_request(Config, "DeleteIPSet", Json).


%%%------------------------------------------------------------------------------
%%% DeleteRule
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_DeleteRule.html
%%%------------------------------------------------------------------------------
-spec delete_rule(ChangeToken :: string() | binary(),
                  RuleId :: string() | binary()) ->
    waf_return_val().
delete_rule(ChangeToken, RuleId) ->
    delete_rule(ChangeToken, RuleId, default_config()).

-spec delete_rule(ChangeToken:: string() | binary(),
                  RuleId :: string() | binary(),
                  Config :: aws_config()) ->
    waf_return_val().
delete_rule(ChangeToken, RuleId, Config) ->
    Json = [{<<"ChangeToken">>, to_binary(ChangeToken)},
            {<<"RuleId">>, to_binary(RuleId)}],
    waf_request(Config, "DeleteRule", Json).


%%%------------------------------------------------------------------------------
%%% DeleteSizeConstraintSet
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_DeleteSizeConstraintSet.html
%%%------------------------------------------------------------------------------
-spec delete_size_constraint_set(ChangeToken :: string() | binary(),
                                 SizeConstraintSetId :: string() | binary()) ->
    waf_return_val().
delete_size_constraint_set(ChangeToken, SizeConstraintSetId) ->
    delete_size_constraint_set(ChangeToken, SizeConstraintSetId, default_config()).

-spec delete_size_constraint_set(ChangeToken:: string() | binary(),
                                 SizeConstraintSetId :: string() | binary(),
                                 Config :: aws_config()) ->
    waf_return_val().
delete_size_constraint_set(ChangeToken, SizeConstraintSetId, Config) ->
    Json = [{<<"ChangeToken">>, to_binary(ChangeToken)},
            {<<"SizeConstraintSetId">>, to_binary(SizeConstraintSetId)}],
    waf_request(Config, "DeleteSizeConstraintSet", Json).


%%%------------------------------------------------------------------------------
%%% DeleteSqlInjectionMatchSet
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_DeleteSqlInjectionMatchSet.html
%%%------------------------------------------------------------------------------
-spec delete_sql_injection_match_set(ChangeToken :: string() | binary(),
                                     SqlInjectionMatchSetId ::
                                         string() | binary()) ->
    waf_return_val().
delete_sql_injection_match_set(ChangeToken, SqlInjectionMatchSetId) ->
    delete_sql_injection_match_set(ChangeToken, SqlInjectionMatchSetId, default_config()).

-spec delete_sql_injection_match_set(ChangeToken:: string() | binary(),
                                     SqlInjectionMatchSetId ::
                                         string() | binary(),
                                     Config :: aws_config()) ->
    waf_return_val().
delete_sql_injection_match_set(ChangeToken, SqlInjectionMatchSetId, Config) ->
    Json = [{<<"ChangeToken">>, to_binary(ChangeToken)}, 
            {<<"SqlInjectionMatchSetId">>, to_binary(SqlInjectionMatchSetId)}],
    waf_request(Config, "DeleteSqlInjectionMatchSet", Json).

%%%------------------------------------------------------------------------------
%%% DeleteWebACL
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_DeleteWebACL.html
%%%------------------------------------------------------------------------------
-spec delete_web_acl(ChangeToken :: string() | binary(),
                     WebACLId :: string() | binary()) ->
    waf_return_val().
delete_web_acl(ChangeToken, WebACLId) ->
    delete_web_acl(ChangeToken, WebACLId, default_config()).

-spec delete_web_acl(ChangeToken:: string() | binary(),
                     WebACLId :: string() | binary(),
                     Config :: aws_config()) ->
    waf_return_val().
delete_web_acl(ChangeToken, WebACLId, Config) ->
    Json = [{<<"ChangeToken">>, to_binary(ChangeToken)}, 
            {<<"WebACLId">>, to_binary(WebACLId)}],
    waf_request(Config, "DeleteWebACL", Json).


%%%------------------------------------------------------------------------------
%%% DeleteXssMatchSet
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_DeleteXssMatchSet.html
%%%------------------------------------------------------------------------------
-spec delete_xss_match_set(ChangeToken :: string() | binary(),
                           XssMatchSetId :: string() | binary()) ->
    waf_return_val().
delete_xss_match_set(ChangeToken, XssMatchSetId) ->
    delete_xss_match_set(ChangeToken, XssMatchSetId, default_config()).

-spec delete_xss_match_set(ChangeToken:: string() | binary(),
                           XssMatchSetId :: string() | binary(),
                           Config :: aws_config()) ->
    waf_return_val().
delete_xss_match_set(ChangeToken, XssMatchSetId, Config) ->
    Json = [{<<"ChangeToken">>, to_binary(ChangeToken)},
            {<<"XssMatchSetId">>, to_binary(XssMatchSetId)}],
    waf_request(Config, "DeleteXssMatchSet", Json).

%%%------------------------------------------------------------------------------
%%% GetByteMatchSet
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_GetByteMatchSet.html
%%%------------------------------------------------------------------------------
-spec get_byte_match_set(ByteMatchSetId :: string() | binary()) ->
    waf_return_val().
get_byte_match_set(ByteMatchSetId) ->
    get_byte_match_set(ByteMatchSetId, default_config()).

-spec get_byte_match_set(ByteMatchSetId:: string() | binary(),
                         Config :: aws_config()) ->
    waf_return_val().
get_byte_match_set(ByteMatchSetId, Config) ->
    Json = [{<<"ByteMatchSetId">>, to_binary(ByteMatchSetId)}],
    waf_request(Config, "GetByteMatchSet", Json).


%%%------------------------------------------------------------------------------
%%% GetChangeToken
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_GetChangeToken.html
%%%------------------------------------------------------------------------------
-spec get_change_token() -> waf_return_val().
get_change_token() ->
    get_change_token(default_config()).

-spec get_change_token(Config :: aws_config()) -> waf_return_val().
get_change_token(Config) ->
    waf_request(Config, "GetChangeToken", []).


%%%------------------------------------------------------------------------------
%%% GetChangeTokenStatus
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_GetChangeTokenStatus.html
%%%------------------------------------------------------------------------------
-spec get_change_token_status(ChangeToken :: string() | binary()) ->
    waf_return_val().
get_change_token_status(ChangeToken) ->
    get_change_token_status(ChangeToken, default_config()).

-spec get_change_token_status(ChangeToken:: string() | binary(),
                              Config :: aws_config()) ->
    waf_return_val().
get_change_token_status(ChangeToken, Config) ->
    Json = [{<<"ChangeToken">>, to_binary(ChangeToken)}],
    waf_request(Config, "GetChangeTokenStatus", Json).


%%%------------------------------------------------------------------------------
%%% GetIPSet
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_GetIPSet.html
%%%------------------------------------------------------------------------------
-spec get_ip_set(IPSetId :: string() | binary()) ->
    waf_return_val().
get_ip_set(IPSetId) ->
    get_ip_set(IPSetId, default_config()).

-spec get_ip_set(IPSetId :: string() | binary(),
                 Config :: aws_config()) ->
    waf_return_val().
get_ip_set(IPSetId, Config) ->
    Json = [{<<"IPSetId">>, to_binary(IPSetId)}],
    waf_request(Config, "GetIPSet", Json).


%%%------------------------------------------------------------------------------
%%% GetRule
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_GetRule.html
%%%------------------------------------------------------------------------------
-spec get_rule(RuleId :: string() | binary()) ->
    waf_return_val().
get_rule(RuleId) ->
    get_rule(RuleId, default_config()).

-spec get_rule(RuleId :: string() | binary(),
               Config :: aws_config()) ->
    waf_return_val().
get_rule(RuleId, Config) ->
    Json = [{<<"RuleId">>, to_binary(RuleId)}],
    waf_request(Config, "GetRule", Json).


%%%------------------------------------------------------------------------------
%%% GetSampledRequests
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_GetSampledRequests.html
%%%------------------------------------------------------------------------------
-spec get_sampled_requests(WebAclId :: string() | binary(),
                           RuleId :: string() | binary(),
                           StartTime :: pos_integer(),
                           EndTime :: pos_integer(),
                           MaxItems :: 1..100) ->
    waf_return_val().
get_sampled_requests(WebAclId, RuleId, StartTime, EndTime, MaxItems) ->
    get_sampled_requests(WebAclId, RuleId, StartTime, EndTime, MaxItems, default_config()).

-spec get_sampled_requests(WebAclId :: string() | binary(),
                           RuleId :: string() | binary(),
                           StartTime :: pos_integer(),
                           EndTime :: pos_integer(),
                           MaxItems :: 1..100,
                           Config :: aws_config()) ->
    waf_return_val().
get_sampled_requests(WebAclId, RuleId, StartTime, EndTime, MaxItems, Config) when is_integer(MaxItems) ->
    Json = [{<<"MaxItems">>, MaxItems},
            {<<"RuleId">>, to_binary(RuleId)},
            {<<"TimeWindow">>, [{<<"StartTime">>, StartTime}, {<<"EndTime">>, EndTime}]},
            {<<"WebAclId">>, to_binary(WebAclId)}],
    waf_request(Config, "GetSampledRequests", Json).


%%%------------------------------------------------------------------------------
%%% GetSizeConstraintSet
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_GetSizeConstraintSet.html
%%%------------------------------------------------------------------------------
-spec get_size_constraint_set(SizeConstraintSetId :: string() | binary()) ->
    waf_return_val().
get_size_constraint_set(SizeConstraintSetId) ->
    get_size_constraint_set(SizeConstraintSetId, default_config()).

-spec get_size_constraint_set(SizeConstraintSetId :: string() | binary(),
                              Config :: aws_config()) ->
    waf_return_val().
get_size_constraint_set(SizeConstraintSetId, Config) ->
    Json = [{<<"SizeConstraintSetId">>, to_binary(SizeConstraintSetId)}],
    waf_request(Config, "GetSizeConstraintSet", Json).


%%%------------------------------------------------------------------------------
%%% GetSqlInjectionMatchSet
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_GetSqlInjectionMatchSet.html
%%%------------------------------------------------------------------------------
-spec get_sql_injection_match_set(SqlInjectionMatchSetId ::
                                      string() | binary()) ->
    waf_return_val().
get_sql_injection_match_set(SqlInjectionMatchSetId) ->
    get_sql_injection_match_set(SqlInjectionMatchSetId, default_config()).

-spec get_sql_injection_match_set(SqlInjectionMatchSetId ::
                                      string() | binary(),
                                  Config :: aws_config()) ->
    waf_return_val().
get_sql_injection_match_set(SqlInjectionMatchSetId, Config) ->
    Json = [{<<"SqlInjectionMatchSetId">>, to_binary(SqlInjectionMatchSetId)}],
    waf_request(Config, "GetSqlInjectionMatchSet", Json).


%%%------------------------------------------------------------------------------
%%% GetWebACL 
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_GetWebACL.html
%%%------------------------------------------------------------------------------
-spec get_web_acl(WebACLId :: string() | binary()) ->
    waf_return_val().
get_web_acl(WebACLId) ->
    get_web_acl(WebACLId, default_config()).

-spec get_web_acl(WebACLId :: string() | binary(),
                  Config :: aws_config()) ->
    waf_return_val().
get_web_acl(WebACLId, Config) ->
    Json = [{<<"WebACLId">>, to_binary(WebACLId)}],
    waf_request(Config, "GetWebACL", Json).


%%%------------------------------------------------------------------------------
%%% GetXssMatchSet
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_GetXssMatchSet.html
%%%------------------------------------------------------------------------------
-spec get_xss_match_set(XssMatchSetId :: string() | binary()) ->
    waf_return_val().
get_xss_match_set(XssMatchSetId) ->
    get_xss_match_set(XssMatchSetId, default_config()).

-spec get_xss_match_set(XssMatchSetId :: string() | binary(),
                        Config :: aws_config()) ->
    waf_return_val().
get_xss_match_set(XssMatchSetId, Config) ->
    Json = [{<<"XssMatchSetId">>, to_binary(XssMatchSetId)}],
    waf_request(Config, "GetXssMatchSet", Json).



%%%------------------------------------------------------------------------------
%%% ListByteMatchSets
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_ListByteMatchSets.html
%%%------------------------------------------------------------------------------
-spec list_byte_match_sets() -> waf_return_val().
list_byte_match_sets() ->
    list_byte_match_sets(?LIMIT_MAX, default_config()).

-spec list_byte_match_sets(Limit :: 1..100) -> waf_return_val().
list_byte_match_sets(Limit) when is_integer(Limit) ->
    list_byte_match_sets(Limit, default_config()).

-spec list_byte_match_sets(Limit :: 1..100,
                           binary() | aws_config()) ->
    waf_return_val().
list_byte_match_sets(Limit, NextMarker) when is_binary(NextMarker) ->
    list_byte_match_sets(Limit, NextMarker, default_config());

list_byte_match_sets(Limit, Config) when is_integer(Limit), is_record(Config, aws_config) ->
    Json = [{<<"Limit">>, Limit}],
    waf_request(Config, "ListByteMatchSets", Json).

-spec list_byte_match_sets(Limit :: 1..100,
                           NextMarker :: binary(),
                           Config :: aws_config()) ->
    waf_return_val().
list_byte_match_sets(Limit, NextMarker, Config) when is_integer(Limit), is_binary(NextMarker) ->
    Json = [{<<"Limit">>, Limit},
            {<<"NextMarker">>, NextMarker}],
    waf_request(Config, "ListByteMatchSets", Json).


%%%------------------------------------------------------------------------------
%%% ListIPSets
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_ListIPSets.html
%%%------------------------------------------------------------------------------
-spec list_ip_sets() -> waf_return_val().
list_ip_sets() ->
    list_ip_sets(?LIMIT_MAX, default_config()).

-spec list_ip_sets(Limit :: 1..100) -> waf_return_val().
list_ip_sets(Limit) when is_integer(Limit) ->
    list_ip_sets(Limit, default_config()).

-spec list_ip_sets(Limit :: 1..100,
                   binary() | aws_config()) ->
    waf_return_val().
list_ip_sets(Limit, NextMarker) when is_binary(NextMarker) ->
    list_ip_sets(Limit, NextMarker, default_config());

list_ip_sets(Limit, Config) when is_integer(Limit), is_record(Config, aws_config) ->
    Json = [{<<"Limit">>, Limit}],
    waf_request(Config, "ListIPSets", Json).

-spec list_ip_sets(Limit :: 1..100,
                   NextMarker :: binary(),
                   Config :: aws_config()) ->
    waf_return_val().
list_ip_sets(Limit, NextMarker, Config) when is_integer(Limit), is_binary(NextMarker) ->
    Json = [{<<"Limit">>, Limit},
            {<<"NextMarker">>, NextMarker}],
    waf_request(Config, "ListIPSets", Json).


%%%------------------------------------------------------------------------------
%%% ListRules
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_ListRules.html
%%%------------------------------------------------------------------------------
-spec list_rules() -> waf_return_val().
list_rules() ->
    list_rules(?LIMIT_MAX, default_config()).

-spec list_rules(Limit :: 1..100) -> waf_return_val().
list_rules(Limit) when Limit > 0, Limit =< 100 ->
    list_rules(Limit, default_config()).

-spec list_rules(Limit :: 1..100,
                 binary() | aws_config()) ->
    waf_return_val().
list_rules(Limit, NextMarker) when is_binary(NextMarker) ->
    list_rules(Limit, NextMarker, default_config());

list_rules(Limit, Config) when is_integer(Limit), is_record(Config, aws_config) ->
    Json = [{<<"Limit">>, Limit}],
    waf_request(Config, "ListRules", Json).

-spec list_rules(Limit :: 1..100,
                 NextMarker :: binary(),
                 Config :: aws_config()) ->
    waf_return_val().
list_rules(Limit, NextMarker, Config) when is_integer(Limit) ->
    Json = [{<<"Limit">>, Limit},
            {<<"NextMarker">>, NextMarker}],
    waf_request(Config, "ListRules", Json).


%%%------------------------------------------------------------------------------
%%% ListSizeConstraintSets
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_ListSizeConstraintSets.html
%%%------------------------------------------------------------------------------
-spec list_size_constraint_sets() -> waf_return_val().
list_size_constraint_sets() ->
    list_size_constraint_sets(?LIMIT_MAX, default_config()).

-spec list_size_constraint_sets(Limit :: 1..100) ->
    waf_return_val().
list_size_constraint_sets(Limit) when is_integer(Limit) ->
    list_size_constraint_sets(Limit, default_config()).

-spec list_size_constraint_sets(Limit :: 1..100,
                                binary() | aws_config()) ->
    waf_return_val().
list_size_constraint_sets(Limit, NextMarker) when is_binary(NextMarker) ->
    list_size_constraint_sets(Limit, NextMarker, default_config());

list_size_constraint_sets(Limit, Config) when is_integer(Limit), is_record(Config, aws_config) ->
    Json = [{<<"Limit">>, Limit}],
    waf_request(Config, "ListSizeConstraintSets", Json).

-spec list_size_constraint_sets(Limit :: 1..100,
                                NextMarker :: binary(),
                                Config :: aws_config()) ->
    waf_return_val().
list_size_constraint_sets(Limit, NextMarker, Config) when is_integer(Limit) ->
    Json = [{<<"Limit">>, Limit},
            {<<"NextMarker">>, NextMarker}],
    waf_request(Config, "ListSizeConstraintSets", Json).


%%%------------------------------------------------------------------------------
%%% ListSqlInjectionMatchSets
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_ListSqlInjectionMatchSets.html
%%%------------------------------------------------------------------------------
-spec list_sql_injection_match_sets() ->
    waf_return_val().
list_sql_injection_match_sets() ->
    list_sql_injection_match_sets(?LIMIT_MAX, default_config()).

-spec list_sql_injection_match_sets(Limit :: 1..100) ->
    waf_return_val().
list_sql_injection_match_sets(Limit) when is_integer(Limit) ->
    list_sql_injection_match_sets(Limit, default_config()).

-spec list_sql_injection_match_sets(Limit :: 1..100,
                                    binary() | aws_config()) ->
    waf_return_val().
list_sql_injection_match_sets(Limit, NextMarker) when is_binary(NextMarker) ->
    list_sql_injection_match_sets(Limit, NextMarker, default_config());

list_sql_injection_match_sets(Limit, Config) when is_integer(Limit), is_record(Config, aws_config) ->
    Json = [{<<"Limit">>, Limit}],
    waf_request(Config, "ListSqlInjectionMatchSets", Json).

-spec list_sql_injection_match_sets(Limit :: 1..100,
                                    NextMarker :: binary(),
                                    Config :: aws_config()) ->
    waf_return_val().
list_sql_injection_match_sets(Limit, NextMarker, Config) when is_integer(Limit) ->
    Json = [{<<"Limit">>, Limit},
            {<<"NextMarker">>, NextMarker}],
    waf_request(Config, "ListSqlInjectionMatchSets", Json).


%%%------------------------------------------------------------------------------
%%% ListWebACLs
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_ListWebACLs.html
%%%------------------------------------------------------------------------------
-spec list_web_acls() -> waf_return_val().
list_web_acls() ->
    list_web_acls(?LIMIT_MAX, default_config()).

-spec list_web_acls(Limit :: 1..100) -> waf_return_val().
list_web_acls(Limit) when is_integer(Limit) ->
    list_web_acls(Limit, default_config()).

-spec list_web_acls(Limit :: 1..100,
                    binary() | aws_config()) ->
    waf_return_val().
list_web_acls(Limit, NextMarker) when is_binary(NextMarker) ->
    list_web_acls(Limit, NextMarker, default_config());

list_web_acls(Limit, Config) when is_integer(Limit), is_record(Config, aws_config) ->
    Json = [{<<"Limit">>, Limit}],
    waf_request(Config, "ListWebACLs", Json).

-spec list_web_acls(Limit :: 1..100,
                    NextMarker :: binary(),
                    Config :: aws_config()) ->
    waf_return_val().
list_web_acls(Limit, NextMarker, Config) when is_integer(Limit) ->
    Json = [{<<"Limit">>, Limit},
            {<<"NextMarker">>, NextMarker}],
    waf_request(Config, "ListWebACLs", Json).


%%%------------------------------------------------------------------------------
%%% ListXssMatchSets
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_ListXssMatchSets.html
%%%------------------------------------------------------------------------------
-spec list_xss_match_sets() -> waf_return_val().
list_xss_match_sets() ->
    list_xss_match_sets(?LIMIT_MAX, default_config()).


-spec list_xss_match_sets(Limit :: 1..100) -> waf_return_val().
list_xss_match_sets(Limit) when is_integer(Limit) ->
    list_xss_match_sets(Limit, default_config()).

-spec list_xss_match_sets(Limit :: 1..100,
                          binary() | aws_config()) ->
    waf_return_val().
list_xss_match_sets(Limit, NextMarker) when is_binary(NextMarker) ->
    list_xss_match_sets(Limit, NextMarker, default_config());

list_xss_match_sets(Limit, Config) when is_integer(Limit), is_record(Config, aws_config) ->
    Json = [{<<"Limit">>, Limit}],
    waf_request(Config, "ListXssMatchSets", Json).

-spec list_xss_match_sets(Limit :: 1..100,
                          NextMarker :: binary(),
                          Config :: aws_config()) ->
    waf_return_val().
list_xss_match_sets(Limit, NextMarker, Config) when is_integer(Limit) ->
    Json = [{<<"Limit">>, Limit},
            {<<"NextMarker">>, NextMarker}],
    waf_request(Config, "ListXssMatchSets", Json).


%%%------------------------------------------------------------------------------
%%% UpdateByteMatchSet
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_UpdateByteMatchSet.html
%%%------------------------------------------------------------------------------
-spec update_byte_match_set(ChangeToken :: string() | binary(),
                            ByteMatchSetId :: string() | binary(),
                            Updates :: [waf_byte_match_set_update()]) ->
    waf_return_val().
update_byte_match_set(ChangeToken, ByteMatchSetId, Updates) ->
    update_byte_match_set(ChangeToken, ByteMatchSetId, Updates, default_config()).

-spec update_byte_match_set(ChangeToken :: string() | binary(),
                            ByteMatchSetId :: string() | binary(),
                            Updates :: [waf_byte_match_set_update()],
                            Config :: aws_config()) ->
    waf_return_val().
update_byte_match_set(ChangeToken, ByteMatchSetId, Updates, Config) ->
    Json = [{<<"ChangeToken">>, to_binary(ChangeToken)},
            {<<"ByteMatchSetId">>, to_binary(ByteMatchSetId)},
            {<<"Updates">>, [transform_to_proplist(Update) || Update <- Updates]}],
    waf_request(Config, "UpdateByteMatchSet", Json).
 

%%%------------------------------------------------------------------------------
%%% UpdateIPSet 
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_UpdateIPSet.html
%%%------------------------------------------------------------------------------
-spec update_ip_set(ChangeToken :: string() | binary(),
                    IPSetId :: string() | binary(),
                    Updates :: [waf_ip_set_update()]) ->
    waf_return_val().
update_ip_set(ChangeToken, IPSetId, Updates) ->
    update_ip_set(ChangeToken, IPSetId, Updates, default_config()).

-spec update_ip_set(ChangeToken :: string() | binary(),
                    IPSetId :: string() | binary(),
                    Updates :: [waf_ip_set_update()],
                    Config :: aws_config()) ->
    waf_return_val().
update_ip_set(ChangeToken, IPSetId, Updates, Config) ->
    Json = [{<<"ChangeToken">>, to_binary(ChangeToken)},
            {<<"IPSetId">>, to_binary(IPSetId)},
            {<<"Updates">>, [transform_to_proplist(Update) || Update <- Updates]}],
    waf_request(Config, "UpdateIPSet", Json).
 

%%%------------------------------------------------------------------------------
%%% UpdateRule
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_UpdateRule.html
%%%------------------------------------------------------------------------------
-spec update_rule(ChangeToken :: string() | binary(),
                  RuleId :: string(),
                  Updates :: [waf_rule_update()]) ->
    waf_return_val().
update_rule(ChangeToken, RuleId, Updates) ->
    update_rule(ChangeToken, RuleId, Updates, default_config()).

-spec update_rule(ChangeToken :: string() | binary(),
                  RuleId :: string() | binary(),
                  Updates :: [waf_rule_update()],
                  Config :: aws_config()) ->
    waf_return_val().
update_rule(ChangeToken, RuleId, Updates, Config) ->
    Json = [{<<"ChangeToken">>, to_binary(ChangeToken)},
            {<<"RuleId">>, to_binary(RuleId)},
            {<<"Updates">>, [transform_to_proplist(Update) || Update <- Updates]}],
    waf_request(Config, "UpdateRule", Json).


%%%------------------------------------------------------------------------------
%%% UpdateSizeConstraintSet
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_UpdateSizeConstraintSet.html
%%%------------------------------------------------------------------------------
-spec update_size_constraint_set(ChangeToken :: string() | binary(),
                                 SizeConstraintSetId :: string() | binary(),
                                 Updates :: [waf_size_constraint_update()]) ->
    waf_return_val().
update_size_constraint_set(ChangeToken, SizeConstraintSetId, Updates) ->
    update_size_constraint_set(ChangeToken, SizeConstraintSetId, Updates, default_config()).

-spec update_size_constraint_set(ChangeToken :: string() | binary(),
                                 SizeConstraintSetId :: string() | binary(),
                                 Updates :: [waf_size_constraint_update()],
                                 Config :: aws_config()) ->
    waf_return_val().
update_size_constraint_set(ChangeToken, SizeConstraintSetId, Updates, Config) ->
    Json = [{<<"ChangeToken">>, to_binary(ChangeToken)},
            {<<"SizeConstraintSetId">>, to_binary(SizeConstraintSetId)},
            {<<"Updates">>, [transform_to_proplist(Update) || Update <- Updates]}],
    waf_request(Config, "UpdateSizeConstraintSet", Json).

%%%------------------------------------------------------------------------------
%%% UpdateSqlInjectionMatchSet
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_UpdateSqlInjectionMatchSet.html
%%%------------------------------------------------------------------------------
-spec update_sql_injection_match_set(
    ChangeToken :: string() | binary(),
    SqlInjectionMatchSetId :: string() | binary(),
    Updates :: [waf_sql_injection_match_set_update()]) -> waf_return_val().
update_sql_injection_match_set(ChangeToken, SqlInjectionMatchSetId, Updates) ->
    update_sql_injection_match_set(ChangeToken, SqlInjectionMatchSetId, Updates, default_config()).

-spec update_sql_injection_match_set(
    ChangeToken :: string() | binary(),
    SqlInjectionMatchSetId :: string() | binary(),
    Updates :: [waf_sql_injection_match_set_update()],
    Config :: aws_config()) -> waf_return_val().
update_sql_injection_match_set(ChangeToken, SqlInjectionMatchSetId, Updates, Config) ->
    Json = [{<<"ChangeToken">>, to_binary(ChangeToken)},
            {<<"SqlInjectionMatchSetId">>, to_binary(SqlInjectionMatchSetId)},
            {<<"Updates">>, [transform_to_proplist(Update) || Update <- Updates]}],
    waf_request(Config, "UpdateSqlInjectionMatchSet", Json).

%%%------------------------------------------------------------------------------
%%% UpdateWebACL 
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_UpdateWebACL.html
%%%------------------------------------------------------------------------------
-type update_web_acl_opt() :: {default_action, waf_acl_action_type()} |
                              {updates, [waf_web_acl_update()]}.
-type update_web_acl_opts() :: [update_web_acl_opt()].

get_web_acl_opts(Opts) ->
    get_web_acl_opts(Opts, []).

get_web_acl_opts([], Res) -> Res;

get_web_acl_opts([{default_action, DefaultAction} | T], Res) ->
    get_web_acl_opts(T, [{<<"DefaultAction">>, [{<<"Type">>, get_acl_action_type(DefaultAction)}]} | Res]);

get_web_acl_opts([{updates, Updates} | T], Res) ->
    get_web_acl_opts(T, [{<<"Updates">>, [transform_to_proplist(Update) || Update <- Updates]} | Res]);

get_web_acl_opts([_ | T], Res) -> get_web_acl_opts(T, Res).

-spec update_web_acl(ChangeToken :: string() | binary(),
                     WebACLId :: string() | binary(),
                     Opts :: update_web_acl_opts()) ->
    waf_return_val().
update_web_acl(ChangeToken, WebACLId, Opts) ->
    update_web_acl(ChangeToken, WebACLId, Opts, default_config()).

-spec update_web_acl(ChangeToken :: string() | binary(),
                     WebACLId :: string() | binary(),
                     Opts :: update_web_acl_opts(),
                     Config :: aws_config()) ->
    waf_return_val().
update_web_acl(ChangeToken, WebACLId, Opts, Config) ->
    Json = [{<<"ChangeToken">>, to_binary(ChangeToken)},
            {<<"WebACLId">>, to_binary(WebACLId)} | get_web_acl_opts(Opts)],
    waf_request(Config, "UpdateWebACL", Json).


%%%------------------------------------------------------------------------------
%%% UpdateXssMatchSet
%%
%% http://docs.aws.amazon.com/waf/latest/APIReference/API_UpdateXssMatchSet.html
%%%------------------------------------------------------------------------------
-spec update_xss_match_set(ChangeToken :: string() | binary(),
                           XssMatchSetId :: string() | binary(),
                           Updates :: [waf_xss_match_set_update()]) ->
    waf_return_val().
update_xss_match_set(ChangeToken, XssMatchSetId, Updates) ->
    update_xss_match_set(ChangeToken, XssMatchSetId, Updates, default_config()).

-spec update_xss_match_set(ChangeToken :: string() | binary(),
                           XssMatchSetId :: string() | binary(),
                           Updates :: [waf_xss_match_set_update()],
                           Config :: aws_config()) ->
    waf_return_val().
update_xss_match_set(ChangeToken, XssMatchSetId, Updates, Config) ->
    Json = [{<<"ChangeToken">>, to_binary(ChangeToken)},
            {<<"XssMatchSetId">>, to_binary(XssMatchSetId)},
            {<<"Updates">>, [transform_to_proplist(Update) || Update <- Updates]}],
    waf_request(Config, "UpdateByteMatchSet", Json).

%%%------------------------------------------------------------------------------
%%% Internal Functions
%%%------------------------------------------------------------------------------
-spec transform_to_proplist(waf_byte_match_set_update() |
                            waf_ip_set_update() |
                            waf_rule_update() |
                            waf_size_constraint_update() |
                            waf_sql_injection_match_set_update() |
                            waf_web_acl_update()) ->
    proplists:proplist().
transform_to_proplist(#waf_byte_match_set_update{action = Action, byte_match_tuple = ByteMatchTuple}) ->
    [{<<"Action">>, get_update_action(Action)},
     {<<"ByteMatchTuple">>, record_to_proplist(ByteMatchTuple)}];

transform_to_proplist(#waf_ip_set_update{action = Action, ip_set_descriptor = IPSetDescriptor}) ->
    [{<<"Action">>, get_update_action(Action)},
     {<<"IPSetDescriptor">>, record_to_proplist(IPSetDescriptor)}];

transform_to_proplist(#waf_rule_update{action = Action, predicate = Predicate}) ->
    [{<<"Action">>, get_update_action(Action)},
     {<<"Predicate">>, record_to_proplist(Predicate)}];

transform_to_proplist(#waf_size_constraint_update{action = Action, size_constraint = SizeConstraint}) ->
    [{<<"Action">>, get_update_action(Action)},
     {<<"SizeConstraint">>, record_to_proplist(SizeConstraint)}];
    
transform_to_proplist(#waf_sql_injection_match_set_update{action = Action, sql_injection_match_tuple = SqlInjectionMatchTuple}) ->
    [{<<"Action">>, get_update_action(Action)},
     {<<"SqlInjectionMatchTuple">>, record_to_proplist(SqlInjectionMatchTuple)}];

transform_to_proplist(#waf_web_acl_update{action = Action, activated_rule = ActivatedRule}) ->
    [{<<"Action">>, get_update_action(Action)},
     {<<"ActivatedRule">>, record_to_proplist(ActivatedRule)}];
    
transform_to_proplist(#waf_xss_match_set_update{action = Action, xss_match_tuple = XssMatchTuple}) ->
    [{<<"Action">>, get_update_action(Action)},
     {<<"XssMatchTuple">>, record_to_proplist(XssMatchTuple)}].

-spec record_to_proplist(waf_rule_predicate() |
                         waf_ip_set_descriptor() |
                         waf_byte_match_tuple() |
                         waf_size_constraint() |
                         waf_sql_injection_match_tuple() |
                         waf_activated_rule() |
                         waf_xss_match_tuple()) ->
    proplists:proplist().
record_to_proplist(#waf_rule_predicate{data_id = DataId, negated = Negated, type = Type}) ->
    [{<<"DataId">>, to_binary(DataId)},
     {<<"Negated">>, to_binary(Negated)},
     {<<"Type">>, get_predicate_type(Type)}];

record_to_proplist(#waf_ip_set_descriptor{type = Type, value = Value}) ->
    [{<<"Type">>, get_ip_address_type(Type)},
     {<<"Value">>, to_binary(Value)}];

record_to_proplist(#waf_byte_match_tuple{
        field_to_match = FieldToMatch,
        positional_constraint = PositionalConstraint,
        target_string = TargetString,
        text_transformation = TextTransformation}) ->
    [{<<"FieldToMatch">>, get_field_to_match(FieldToMatch)},
     {<<"PositionalConstraint">>, get_positional_constraint(PositionalConstraint)},
     {<<"TargetString">>, to_binary(TargetString)},
     {<<"TextTransformation">>, get_text_transformation(TextTransformation)}];

record_to_proplist(#waf_size_constraint{
        comparison_operator = ComparisonOperator,
        field_to_match = FieldToMatch,
        size = Size,
        text_transformation = TextTransformation}) ->
    [{<<"FieldToMatch">>, get_field_to_match(FieldToMatch)},
     {<<"ComparisonOperator">>, get_comparison_operator(ComparisonOperator)},
     {<<"Size">>, Size},
     {<<"TextTransformation">>, get_text_transformation(TextTransformation)}];
    
record_to_proplist(#waf_sql_injection_match_tuple{field_to_match = FieldToMatch, text_transformation = TextTransformation}) ->
    [{<<"FieldToMatch">>, get_field_to_match(FieldToMatch)},
     {<<"TextTransformation">>, get_text_transformation(TextTransformation)}];
        
record_to_proplist(#waf_activated_rule{action = Action, priority = Priority, rule_id = RuleId}) ->
    [{<<"Action">>, [{<<"Type">>, get_acl_action_type(Action)}]},
     {<<"Priority">>, Priority},
     {<<"RuleId">>, to_binary(RuleId)}];

record_to_proplist(#waf_xss_match_tuple{field_to_match = FieldToMatch, text_transformation = TextTransformation}) ->
    [{<<"FieldToMatch">>, get_field_to_match(FieldToMatch)},
     {<<"TextTransformation">>, get_text_transformation(TextTransformation)}].

get_update_action(insert) -> <<"INSERT">>;
get_update_action(delete) -> <<"DELETE">>.

get_predicate_type(ip_match) -> <<"IPMatch">>;
get_predicate_type(byte_match) -> <<"ByteMatch">>;
get_predicate_type(sql_injection_match) -> <<"SqlInjectionMatch">>;
get_predicate_type(size_constraint) -> <<"SizeConstraint">>.

get_ip_address_type(ip_v4) -> <<"IPV4">>.

get_field_to_match(#waf_field_to_match{data = undefined, type = Type}) ->
    [{<<"Type">>, get_field_to_match_type(Type)}];

get_field_to_match(#waf_field_to_match{data = Data, type = Type}) ->
        [{<<"Data">>, to_binary(Data)},
        {<<"Type">>, get_field_to_match_type(Type)}].

-spec get_field_to_match_type(atom()) -> binary().
get_field_to_match_type(uri) -> <<"URI">>;
get_field_to_match_type(query_string) -> <<"QUERY_STRING">>;
get_field_to_match_type(header) -> <<"HEADER">>;
get_field_to_match_type(method) -> <<"METHOD">>;
get_field_to_match_type(body) -> <<"BODY">>.

get_positional_constraint(exactly) -> <<"EXACTLY">>;
get_positional_constraint(starts_with) -> <<"STARTS_WITH">>;
get_positional_constraint(ends_with) -> <<"ENDS_WITH">>;
get_positional_constraint(contains) -> <<"CONTAINS">>;
get_positional_constraint(contains_word) -> <<"CONTAINS_WORD">>.

get_text_transformation(none) -> <<"NONE">>;
get_text_transformation(compress_white_space) -> <<"COMPRESS_WHITE_SPACE">>;
get_text_transformation(html_entity_decode) -> <<"HTML_ENTITY_DECODE">>;
get_text_transformation(lowercase) -> <<"LOWERCASE">>;
get_text_transformation(cmd_line) -> <<"CMD_LINE">>;
get_text_transformation(url_decode) -> <<"URL_DECODE">>.

get_comparison_operator(eq) -> <<"EQ">>;
get_comparison_operator(ne) -> <<"NE">>;
get_comparison_operator(le) -> <<"LE">>;
get_comparison_operator(lt) -> <<"LT">>;
get_comparison_operator(ge) -> <<"GE">>;
get_comparison_operator(gt) -> <<"GT">>.

get_acl_action_type(block) -> <<"BLOCK">>;
get_acl_action_type(allow) -> <<"ALLOW">>;
get_acl_action_type(count) -> <<"COUNT">>.

-spec waf_request(aws_config(), string(), json_proplist()) -> {error, term()}|{ok, json_proplist()}.
waf_request(Config, Operation, Body) ->
    case erlcloud_aws:update_config(Config) of
        {ok, Config1} ->
            waf_request_no_update(Config1, Operation, Body);
        {error, Reason} ->
            {error, Reason}
    end.


-spec waf_request_no_update(aws_config(), string(), json_proplist()) -> {error, term()}|{ok, json_proplist()}.
waf_request_no_update(Config, Operation, Body) ->
    Payload = case Body of
               [] -> <<"{}">>;
               _ -> jsx:encode(Body)
           end,
    Headers = headers(Config, Operation, Payload),
    Request = #aws_request{service = waf,
                           uri = uri(Config),
                           method = post,
                           request_headers = Headers,
                           request_body = Payload},
    case erlcloud_aws:request_to_return(erlcloud_retry:request(Config, Request, fun waf_result_fun/1)) of
        {ok, {_RespHeaders, <<>>}} -> {ok, []};
        {ok, {_RespHeaders, RespBody}} -> {ok, jsx:decode(RespBody)};
        {error, _} = Error-> Error
    end.

-spec waf_result_fun(Request :: aws_request()) -> aws_request().
waf_result_fun(#aws_request{response_type = ok} = Request) ->
    Request;
waf_result_fun(#aws_request{response_type = error,
                                  error_type = aws,
                                  response_status = Status} = Request) when Status >= 500 ->
    Request#aws_request{should_retry = true};
waf_result_fun(#aws_request{response_type = error, error_type = aws} = Request) ->
    Request#aws_request{should_retry = false}.

headers(Config, Operation, Body) ->
    Headers = [{"host", Config#aws_config.waf_host},
               {"x-amz-target", lists:append(["AWSWAF_", ?API_VERSION, ".", Operation])},
               {"content-type", "application/x-amz-json-1.1"}],
    Region = erlcloud_aws:aws_region_from_host(Config#aws_config.waf_host),
    erlcloud_aws:sign_v4_headers(Config, Headers, Body, Region, "waf").

uri(#aws_config{waf_scheme = Scheme, waf_host = Host} = Config) ->
    lists:flatten([Scheme, Host, port_spec(Config)]).


port_spec(#aws_config{waf_port=80}) ->
    "";
port_spec(#aws_config{waf_port=Port}) ->
    [":", erlang:integer_to_list(Port)].


get_default_action_type(allow) -> <<"ALLOW">>;
get_default_action_type(block) -> <<"BLOCK">>;
get_default_action_type(count) -> <<"COUNT">>.

to_binary(true) -> <<"true">>;
to_binary(false) -> <<"false">>;
to_binary(L) when is_list(L) -> list_to_binary(L);
to_binary(B) when is_binary(B) -> B.
