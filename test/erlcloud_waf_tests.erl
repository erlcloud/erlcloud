%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_waf_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud_waf.hrl").

%% Unit tests for erlcloud_waf.
%% These tests work by using meck to mock erlcloud_httpc. There are two classes of test: input and output.
%%
%% Input tests verify that different function args produce the desired JSON request.
%% An input test list provides a list of funs and the JSON that is expected to result.
%%
%% Output tests verify that the http response produces the correct return from the fun.
%% An output test lists provides a list of response bodies and the expected return.

%% The _waf_test macro provides line number annotation to a test, similar to _test, but doesn't wrap in a fun
-define(_waf_test(T), {?LINE, T}).
%% The _f macro is a terse way to wrap code in a fun. Similar to _test but doesn't annotate with a line number
-define(_f(F), fun() -> F end).

-export([validate_body/2]).

%%%===================================================================
%%% Common Test Values
%%%===================================================================
-define(CHANGE_TOKEN, <<"bdebed50-2acb-4cfa-81ad-fae0027aa006">>).
-define(NAME_PARAM, <<"TestName">>).
-define(METRIC_NAME_PARAM, <<"TestMetricName">>).
-define(CREATE_ID, <<"623348a6-3702-4e64-83f4-9871e02bc41e">>).
-define(RULE_ID, <<"a1e318e0-a887-49b9-99d0-dfd0ca975b09">>).
-define(WEB_ACL_ID, <<"356c0d63-4f52-4535-8dff-474f8b3b32e6">>).
-define(NEXT_MARKER, <<"623348a6-3702-4e64-83f4-9871e02bc41e">>).
-define(DEFAULT_ACTION_TYPE, block).
-define(START_TIME, 1457024400).
-define(END_TIME, 1457033216).
-define(LIMIT_SIZE, 10).
-define(LIMIT_MAX, 100).

-define(UPDATE_BYTE_MATCH_SET,
    #waf_byte_match_set_update{
        action = insert,
        byte_match_tuple = #waf_byte_match_tuple{
                                field_to_match = #waf_field_to_match{type = query_string},
                                positional_constraint = contains,
                                target_string = "foobar",
                                text_transformation = none}}). 

-define(UPDATE_IP_SET,
    #waf_ip_set_update{
        action = insert,
        ip_set_descriptor = #waf_ip_set_descriptor{
                                type = ip_v4,
                                value = "10.0.4.0/24"}}). 

-define(UPDATE_RULE,
    #waf_rule_update{
        action = insert,
        predicate = #waf_rule_predicate{
                        data_id = ?CREATE_ID,
                        negated = true,
                        type = ip_match}}). 

-define(UPDATE_SIZE_CONSTRAINT_SET,
    #waf_size_constraint_update{
        action = insert,
        size_constraint = #waf_size_constraint{
                            comparison_operator = ne,
                            size = 100,
                            text_transformation = none,
                            field_to_match = #waf_field_to_match{
                                type = header,
                                data = "User-Agent"}}}).

-define(UPDATE_SQL_INJECTION_MATCH_SET,
    #waf_sql_injection_match_set_update{
        action = insert,
        sql_injection_match_tuple = #waf_sql_injection_match_tuple{
                                        field_to_match = #waf_field_to_match{
                                            type = query_string},
                                        text_transformation = cmd_line}}).

-define(UPDATE_WEB_ACL,
    [{updates,
      [#waf_web_acl_update{
        action = insert,
        activated_rule = #waf_activated_rule{
            action = block,
            priority = 1,
            rule_id = ?RULE_ID}}]}]).
 
-define(UPDATE_XSS_MATCH_SET,
    #waf_xss_match_set_update{
        action = insert,
        xss_match_tuple = #waf_xss_match_tuple{
            field_to_match = #waf_field_to_match{type = query_string},
            text_transformation = none}}).

%%%===================================================================
%%% Test entry points
%%%===================================================================
operation_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun create_byte_match_set_tests/1,
      fun create_ip_set_tests/1,
      fun create_rule_tests/1,
      fun create_size_constraint_set_tests/1,
      fun create_sql_injection_match_set_tests/1,
      fun create_web_acl_tests/1,
      fun create_xss_match_set_tests/1,

      fun delete_byte_match_set_tests/1,
      fun delete_ip_set_tests/1,
      fun delete_rule_tests/1,
      fun delete_size_constraint_set_tests/1,
      fun delete_sql_injection_match_set_tests/1,
      fun delete_web_acls_tests/1,
      fun delete_xss_match_set_tests/1,

      fun get_change_token_status_tests/1,
      fun get_byte_match_set_tests/1,
      fun get_ip_set_tests/1,
      fun get_rule_tests/1,
      fun get_size_constraint_set_tests/1,
      fun get_sql_injection_match_set_tests/1,
      fun get_web_acl_tests/1,
      fun get_sampled_requests_tests/1,
      fun get_xss_match_set_tests/1,
      
      fun list_byte_match_sets_tests/1,
      fun list_ip_sets_tests/1,
      fun list_rules_tests/1,
      fun list_size_constraint_sets_tests/1,
      fun list_sql_injection_match_sets_tests/1,
      fun list_web_acls_tests/1,
      fun list_xss_match_sets_tests/1,

      fun update_byte_match_set_tests/1,
      fun update_ip_set_tests/1,
      fun update_rule_tests/1,
      fun update_size_constraint_set_tests/1,
      fun update_sql_injection_match_set_tests/1,
      fun update_web_acl_tests/1,
      fun update_xss_match_set_tests/1
     ]
    }.

start() ->
    meck:new(erlcloud_httpc),
    ok.


stop(_) ->
    meck:unload(erlcloud_httpc).

%%%===================================================================
%%% Actual test specifiers
%%%===================================================================

create_byte_match_set_tests(_) ->
    Action = "CreateByteMatchSet",
    Function = ?_f(erlcloud_waf:create_byte_match_set(?CHANGE_TOKEN, ?NAME_PARAM)),
    PostData = jsx:encode([{<<"ChangeToken">>, ?CHANGE_TOKEN},
                           {<<"Name">>, ?NAME_PARAM}]),
    Response = [{<<"ByteMatchSet">>,
                 [{<<"ByteMatchSetId">>, ?CREATE_ID},
                  {<<"ByteMatchTuples">>,[]},
                  {<<"Name">>, ?NAME_PARAM}]},
                {<<"ChangeToken">>, ?CHANGE_TOKEN}],
    all_tests(Action, Function, PostData, Response).

create_ip_set_tests(_) ->
    Action = "CreateIPSet",
    Function = ?_f(erlcloud_waf:create_ip_set(?CHANGE_TOKEN, ?NAME_PARAM)),
    PostData = jsx:encode([{<<"ChangeToken">>, ?CHANGE_TOKEN},
                           {<<"Name">>, ?NAME_PARAM}]),
    Response = [{<<"ChangeToken">>, ?CHANGE_TOKEN},
                {<<"IPSet">>,
                 [{<<"IPSetDescriptors">>, []},
                  {<<"IPSetId">>, ?CREATE_ID},
                  {<<"Name">>, ?NAME_PARAM}]}],
    all_tests(Action, Function, PostData, Response).

create_rule_tests(_) ->
    Action = "CreateRule",
    Function = ?_f(erlcloud_waf:create_rule(?CHANGE_TOKEN, ?NAME_PARAM, ?METRIC_NAME_PARAM)),
    PostData = jsx:encode([{<<"ChangeToken">>, ?CHANGE_TOKEN},
                           {<<"MetricName">>, ?METRIC_NAME_PARAM},
                           {<<"Name">>, ?NAME_PARAM}]),
    Response = [{<<"ChangeToken">>, ?CHANGE_TOKEN},
                {<<"Rule">>,
                 [{<<"MetricName">>, ?METRIC_NAME_PARAM},
                   {<<"Name">>, ?NAME_PARAM},
                   {<<"Predicates">>,[]},
                   {<<"RuleId">>, ?CREATE_ID}]}],
    all_tests(Action, Function, PostData, Response).

create_size_constraint_set_tests(_) ->
    Action = "CreateSizeConstraintSet",
    Function = ?_f(erlcloud_waf:create_size_constraint_set(?CHANGE_TOKEN, ?NAME_PARAM)),
    PostData = jsx:encode([{<<"ChangeToken">>, ?CHANGE_TOKEN},
                           {<<"Name">>, ?NAME_PARAM}]),
    Response = [{<<"ChangeToken">>, ?CHANGE_TOKEN},
                {<<"SizeConstraintSet">>,
                 [{<<"Name">>, ?NAME_PARAM},
                  {<<"SizeConstraintSetId">>, ?CREATE_ID},
                  {<<"SizeConstraints">>,[]}]}],
    all_tests(Action, Function, PostData, Response).

create_sql_injection_match_set_tests(_) ->
    Action = "CreateSqlInjectionMatchSet",
    Function = ?_f(erlcloud_waf:create_sql_injection_match_set(?CHANGE_TOKEN, ?NAME_PARAM)),
    PostData = jsx:encode([{<<"ChangeToken">>, ?CHANGE_TOKEN},
                           {<<"Name">>, ?NAME_PARAM}]),
    Response = [{<<"ChangeToken">>, ?CHANGE_TOKEN},
                {<<"SqlInjectionMatchSet">>,
                 [{<<"Name">>, ?NAME_PARAM},
                  {<<"SqlInjectionMatchSetId">>, ?CREATE_ID},
                  {<<"SqlInjectionMatchTuples">>,[]}]}],
    all_tests(Action, Function, PostData, Response).

create_web_acl_tests(_) ->
    Action = "CreateWebACL",
    Function = ?_f(erlcloud_waf:create_web_acl(?CHANGE_TOKEN, ?NAME_PARAM, ?METRIC_NAME_PARAM, ?DEFAULT_ACTION_TYPE)),
    PostData = jsx:encode([{<<"ChangeToken">>, ?CHANGE_TOKEN},
                           {<<"DefaultAction">>, [{<<"Type">>, <<"BLOCK">>}]},
                           {<<"MetricName">>, ?METRIC_NAME_PARAM},
                           {<<"Name">>, ?NAME_PARAM}]),
    Response = [{<<"ChangeToken">>, ?CHANGE_TOKEN},
                {<<"WebACL">>,
                 [{<<"DefaultAction">>, [{<<"Type">>, <<"BLOCK">>}]},
                  {<<"MetricName">>, ?METRIC_NAME_PARAM},
                  {<<"Name">>, ?NAME_PARAM},
                  {<<"Rules">>,[]},
                  {<<"WebACLId">>, ?CREATE_ID}]}],
    all_tests(Action, Function, PostData, Response).

create_xss_match_set_tests(_) ->
    Action = "CreateXssMatchSet",
    Function = ?_f(erlcloud_waf:create_xss_match_set(?CHANGE_TOKEN, ?NAME_PARAM)),
    PostData = jsx:encode([{<<"ChangeToken">>, ?CHANGE_TOKEN},
                           {<<"Name">>, ?NAME_PARAM}]),
    Response = [{<<"XssMatchSet">>,
                 [{<<"XssMatchSetId">>, ?CREATE_ID},
                  {<<"XssMatchTuples">>,[]},
                  {<<"Name">>, ?NAME_PARAM}]},
                {<<"ChangeToken">>, ?CHANGE_TOKEN}],
    all_tests(Action, Function, PostData, Response).


delete_byte_match_set_tests(_) ->
    Action = "DeleteByteMatchSet",
    Function = ?_f(erlcloud_waf:delete_byte_match_set(?CHANGE_TOKEN, ?CREATE_ID)),
    PostData = jsx:encode([{<<"ChangeToken">>, ?CHANGE_TOKEN},
                           {<<"ByteMatchSetId">>, ?CREATE_ID}]),
    Response = [{<<"ChangeToken">>, ?CHANGE_TOKEN}],
    all_tests(Action, Function, PostData, Response).

delete_ip_set_tests(_) ->
    Action = "DeleteIPSet",
    Function = ?_f(erlcloud_waf:delete_ip_set(?CHANGE_TOKEN, ?CREATE_ID)),
    PostData = jsx:encode([{<<"ChangeToken">>, ?CHANGE_TOKEN},
                           {<<"IPSetId">>, ?CREATE_ID}]),
    Response = [{<<"ChangeToken">>, ?CHANGE_TOKEN}],
    all_tests(Action, Function, PostData, Response).

delete_rule_tests(_) ->
    Action = "DeleteRule",
    Function = ?_f(erlcloud_waf:delete_rule(?CHANGE_TOKEN, ?CREATE_ID)),
    PostData = jsx:encode([{<<"ChangeToken">>, ?CHANGE_TOKEN},
                           {<<"RuleId">>, ?CREATE_ID}]),
    Response = [{<<"ChangeToken">>, ?CHANGE_TOKEN}],
    all_tests(Action, Function, PostData, Response).

delete_size_constraint_set_tests(_) ->
    Action = "DeleteSizeConstraintSet",
    Function = ?_f(erlcloud_waf:delete_size_constraint_set(?CHANGE_TOKEN, ?CREATE_ID)),
    PostData = jsx:encode([{<<"ChangeToken">>, ?CHANGE_TOKEN},
                           {<<"SizeConstraintSetId">>, ?CREATE_ID}]),
    Response = [{<<"ChangeToken">>, ?CHANGE_TOKEN}],
    all_tests(Action, Function, PostData, Response).

delete_sql_injection_match_set_tests(_) ->
    Action = "DeleteSqlInjectionMatchSet",
    Function = ?_f(erlcloud_waf:delete_sql_injection_match_set(?CHANGE_TOKEN, ?CREATE_ID)),
    PostData = jsx:encode([{<<"ChangeToken">>, ?CHANGE_TOKEN},
                           {<<"SqlInjectionMatchSetId">>, ?CREATE_ID}]),
    Response = [{<<"ChangeToken">>, ?CHANGE_TOKEN}],
    all_tests(Action, Function, PostData, Response).

delete_web_acls_tests(_) ->
    Action = "DeleteWebACL",
    Function = ?_f(erlcloud_waf:delete_web_acl(?CHANGE_TOKEN, ?CREATE_ID)),
    PostData = jsx:encode([{<<"ChangeToken">>, ?CHANGE_TOKEN},
                           {<<"WebACLId">>, ?CREATE_ID}]),
    Response = [{<<"ChangeToken">>, ?CHANGE_TOKEN}],
    all_tests(Action, Function, PostData, Response).

delete_xss_match_set_tests(_) ->
    Action = "DeleteXssMatchSet",
    Function = ?_f(erlcloud_waf:delete_xss_match_set(?CHANGE_TOKEN, ?CREATE_ID)),
    PostData = jsx:encode([{<<"ChangeToken">>, ?CHANGE_TOKEN},
                           {<<"XssMatchSetId">>, ?CREATE_ID}]),
    Response = [{<<"ChangeToken">>, ?CHANGE_TOKEN}],
    all_tests(Action, Function, PostData, Response).


get_change_token_status_tests(_) ->
    Action = "GetChangeTokenStatus",
    Function = ?_f(erlcloud_waf:get_change_token_status(?CHANGE_TOKEN)),
    PostData = jsx:encode([{<<"ChangeToken">>, ?CHANGE_TOKEN}]),
    Response = [{<<"ChangeTokenStatus">>, <<"INSYNC">>}],
    all_tests(Action, Function, PostData, Response).
    
get_byte_match_set_tests(_) ->
    Action = "GetByteMatchSet",
    Function = ?_f(erlcloud_waf:get_byte_match_set(?CREATE_ID)),
    PostData = jsx:encode([{<<"ByteMatchSetId">>, ?CREATE_ID}]),
    Response = [{<<"ByteMatchSet">>,
                [{<<"ByteMatchSetId">>, ?CREATE_ID},
                 {<<"ByteMatchTuples">>,[]},
                 {<<"Name">>, ?NAME_PARAM}]}],
    all_tests(Action, Function, PostData, Response).

get_ip_set_tests(_) ->
    Action = "GetIPSet",
    Function = ?_f(erlcloud_waf:get_ip_set(?CREATE_ID)),
    PostData = jsx:encode([{<<"IPSetId">>, ?CREATE_ID}]),
    Response = [{<<"IPSetDescriptors">>,
                [{<<"IPSetId">>, ?CREATE_ID},
                 {<<"Name">>, ?NAME_PARAM}]}],
    all_tests(Action, Function, PostData, Response).

get_rule_tests(_) ->
    Action = "GetRule",
    Function = ?_f(erlcloud_waf:get_rule(?CREATE_ID)),
    PostData = jsx:encode([{<<"RuleId">>, ?CREATE_ID}]),
    Response = [{<<"Rule">>,
                [{<<"MetricName">>, ?METRIC_NAME_PARAM},
                 {<<"Name">>, ?NAME_PARAM},
                 {<<"Predicates">>,[]},
                 {<<"RuleId">>, ?CREATE_ID}]}],
    all_tests(Action, Function, PostData, Response).

get_size_constraint_set_tests(_) ->
    Action = "GetSizeConstraintSet",
    Function = ?_f(erlcloud_waf:get_size_constraint_set(?CREATE_ID)),
    PostData = jsx:encode([{<<"SizeConstraintSetId">>, ?CREATE_ID}]),
    Response = [{<<"SizeConstraintSet">>,
                [{<<"Name">>, ?NAME_PARAM},
                 {<<"SizeConstraintSetId">>, ?CREATE_ID},
                 {<<"SizeConstraints">>,[]}]}],
    all_tests(Action, Function, PostData, Response).

get_sql_injection_match_set_tests(_) ->
    Action = "GetSqlInjectionMatchSet",
    Function = ?_f(erlcloud_waf:get_sql_injection_match_set(?CREATE_ID)),
    PostData = jsx:encode([{<<"SqlInjectionMatchSetId">>, ?CREATE_ID}]),
    Response = [{<<"SqlInjectionMatchSet">>,
                [{<<"Name">>, ?NAME_PARAM},
                 {<<"SqlInjectionMatchSetId">>, ?CREATE_ID},
                 {<<"SqlInjectionMatchTuples">>,[]}]}],
    all_tests(Action, Function, PostData, Response).

get_web_acl_tests(_) ->
    Action = "GetWebACL",
    Function = ?_f(erlcloud_waf:get_web_acl(?CREATE_ID)),
    PostData = jsx:encode([{<<"WebACLId">>, ?CREATE_ID}]),
    Response = [{<<"WebACL">>,
                [{<<"DefaultAction">>, [{<<"Type">>,<<"BLOCK">>}]},
                 {<<"MetricName">>, ?METRIC_NAME_PARAM},
                 {<<"Name">>, ?NAME_PARAM},
                 {<<"Rules">>,[]},
                 {<<"WebACLId">>, ?CREATE_ID}]}],
    all_tests(Action, Function, PostData, Response).

get_xss_match_set_tests(_) ->
    Action = "GetXssMatchSet",
    Function = ?_f(erlcloud_waf:get_xss_match_set(?CREATE_ID)),
    PostData = jsx:encode([{<<"XssMatchSetId">>, ?CREATE_ID}]),
    Response = [{<<"XssMatchSet">>,
                [{<<"XssMatchSetId">>, ?CREATE_ID},
                 {<<"XssMatchTuples">>,[]},
                 {<<"Name">>, ?NAME_PARAM}]}],
    all_tests(Action, Function, PostData, Response).


get_sampled_requests_tests(_) ->
    Action = "GetSampledRequests",
    Function = ?_f(erlcloud_waf:get_sampled_requests(?WEB_ACL_ID, ?RULE_ID, ?START_TIME, ?END_TIME, ?LIMIT_SIZE)),
    PostData = jsx:encode([{<<"MaxItems">>, ?LIMIT_SIZE},
                           {<<"RuleId">>, ?RULE_ID},
                           {<<"TimeWindow">>, [{<<"StartTime">>, ?START_TIME}, {<<"EndTime">>, ?END_TIME}]},
                           {<<"WebAclId">>, ?WEB_ACL_ID}]),
    Response = [{<<"PopulationSize">>,0},
                {<<"SampledRequests">>,[]},
                {<<"TimeWindow">>,
                    [{<<"EndTime">>, 1457033216.0},
                     {<<"StartTime">>, 1.4570244e9}]}],
    all_tests(Action, Function, PostData, Response).


list_byte_match_sets_tests(_) ->
    Action = "ListByteMatchSets",
    Function = ?_f(erlcloud_waf:list_byte_match_sets(?LIMIT_SIZE)),
    PostData = jsx:encode([{<<"Limit">>, ?LIMIT_SIZE}]),
    Response = [{<<"ByteMatchSets">>,
                [[{<<"ByteMatchSetId">>, ?CREATE_ID},
                  {<<"Name">>, ?NAME_PARAM}]]},
                {<<"NextMarker">>, ?NEXT_MARKER}],
    all_tests(Action, Function, PostData, Response).

list_ip_sets_tests(_) ->
    Action = "ListIPSets",
    Function = ?_f(erlcloud_waf:list_ip_sets()),
    PostData = jsx:encode([{<<"Limit">>, ?LIMIT_MAX}]),
    Response = [{<<"IPSets">>,
                [[{<<"IPSetId">>, ?CREATE_ID},
                  {<<"Name">>, ?NAME_PARAM}]]},
                {<<"NextMarker">>, ?NEXT_MARKER}],
    all_tests(Action, Function, PostData, Response).

list_rules_tests(_) ->
    Action = "ListRules",
    Function = ?_f(erlcloud_waf:list_rules(?LIMIT_SIZE)),
    PostData = jsx:encode([{<<"Limit">>, ?LIMIT_SIZE}]),
    Response = [{<<"Rules">>,
                [[{<<"RuleId">>, ?CREATE_ID},
                  {<<"Name">>, ?NAME_PARAM}]]},
                {<<"NextMarker">>, ?NEXT_MARKER}],
    all_tests(Action, Function, PostData, Response).

list_size_constraint_sets_tests(_) ->
    Action = "ListSizeConstraintSets",
    Function = ?_f(erlcloud_waf:list_size_constraint_sets(?LIMIT_SIZE)),
    PostData = jsx:encode([{<<"Limit">>, ?LIMIT_SIZE}]),
    Response = [{<<"SizeConstraintSets">>,
                [[{<<"SizeConstraintSetId">>, ?CREATE_ID},
                  {<<"Name">>, ?NAME_PARAM}]]},
                {<<"NextMarker">>, ?NEXT_MARKER}],
    all_tests(Action, Function, PostData, Response).

list_sql_injection_match_sets_tests(_) ->
    Action = "ListSqlInjectionMatchSets",
    Function = ?_f(erlcloud_waf:list_sql_injection_match_sets(?LIMIT_SIZE)),
    PostData = jsx:encode([{<<"Limit">>, ?LIMIT_SIZE}]),
    Response = [{<<"SqlInjectionMatchSets">>,
                [[{<<"SqlInjectionMatchSetId">>, ?CREATE_ID},
                  {<<"Name">>, ?NAME_PARAM}]]},
                {<<"NextMarker">>, ?NEXT_MARKER}],
    all_tests(Action, Function, PostData, Response).

list_web_acls_tests(_) ->
    Action = "ListWebACLs",
    Function = ?_f(erlcloud_waf:list_web_acls(?LIMIT_SIZE)),
    PostData = jsx:encode([{<<"Limit">>, ?LIMIT_SIZE}]),
    Response = [{<<"WebACLs">>,
                [[{<<"WebACLId">>, ?CREATE_ID},
                  {<<"Name">>, ?NAME_PARAM}]]},
                {<<"NextMarker">>, ?NEXT_MARKER}],
    all_tests(Action, Function, PostData, Response).

list_xss_match_sets_tests(_) ->
    Action = "ListXssMatchSets",
    Function = ?_f(erlcloud_waf:list_xss_match_sets(?LIMIT_SIZE)),
    PostData = jsx:encode([{<<"Limit">>, ?LIMIT_SIZE}]),
    Response = [{<<"XssMatchSets">>,
                [[{<<"XssMatchSetId">>, ?CREATE_ID},
                  {<<"Name">>, ?NAME_PARAM}]]},
                {<<"NextMarker">>, ?NEXT_MARKER}],
    all_tests(Action, Function, PostData, Response).


update_byte_match_set_tests(_) ->
    Action = "UpdateByteMatchSet",
    Function = ?_f(erlcloud_waf:update_byte_match_set(?CHANGE_TOKEN, ?CREATE_ID, [?UPDATE_BYTE_MATCH_SET])),
    PostData = jsx:encode(
        [{<<"ChangeToken">>, ?CHANGE_TOKEN},
         {<<"ByteMatchSetId">>, ?CREATE_ID},
         {<<"Updates">>,
            [[{<<"Action">>, <<"INSERT">>},
             {<<"ByteMatchTuple">>,
                [{<<"FieldToMatch">>, [{<<"Type">>, <<"QUERY_STRING">>}]},
                 {<<"PositionalConstraint">>, <<"CONTAINS">>},
                 {<<"TargetString">>, <<"foobar">>},
                 {<<"TextTransformation">>, <<"NONE">>}]}]]}]),
    Response = [{<<"ByteMatchSet">>,
                [{<<"ByteMatchSetId">>, ?CREATE_ID},
                 {<<"ByteMatchTuples">>,
                  [[{<<"FieldToMatch">>, [{<<"Type">>, <<"QUERY_STRING">>}]},
                    {<<"PositionalConstraint">>, <<"CONTAINS">>},
                    {<<"TargetString">>,<<"foobar">>},
                    {<<"TextTransformation">>,<<"NONE">>}]]},
                  {<<"Name">>,<<"TestByteMatchSet">>}]}],
    all_tests(Action, Function, PostData, Response).

update_ip_set_tests(_) ->
    Action = "UpdateIPSet",
    Function = ?_f(erlcloud_waf:update_ip_set(?CHANGE_TOKEN, ?CREATE_ID, [?UPDATE_IP_SET])),
    PostData = jsx:encode(
        [{<<"ChangeToken">>, ?CHANGE_TOKEN},
         {<<"IPSetId">>, ?CREATE_ID},
         {<<"Updates">>,
            [[{<<"Action">>, <<"INSERT">>},
              {<<"IPSetDescriptor">>,
               [{<<"Type">>, <<"IPV4">>},
                {<<"Value">>, <<"10.0.4.0/24">>}]}]]}]),
    Response = [{<<"ChangeToken">>, ?CHANGE_TOKEN}],
    all_tests(Action, Function, PostData, Response).

update_rule_tests(_) ->
    Action = "UpdateRule",
    Function = ?_f(erlcloud_waf:update_rule(?CHANGE_TOKEN, ?RULE_ID, [?UPDATE_RULE])),
    PostData = jsx:encode(
        [{<<"ChangeToken">>, ?CHANGE_TOKEN},
         {<<"RuleId">>, ?RULE_ID},
         {<<"Updates">>,
            [[{<<"Action">>, <<"INSERT">>},
              {<<"Predicate">>,
                [{<<"DataId">>, ?CREATE_ID},
                 {<<"Negated">>, <<"true">>},
                 {<<"Type">>, <<"IPMatch">>}]}]]}]),
    Response = [{<<"ChangeToken">>, ?CHANGE_TOKEN}],
    all_tests(Action, Function, PostData, Response).

update_size_constraint_set_tests(_) ->
    Action = "UpdateSizeConstraintSet",
    Function = ?_f(erlcloud_waf:update_size_constraint_set(?CHANGE_TOKEN, ?CREATE_ID, [?UPDATE_SIZE_CONSTRAINT_SET])),
    PostData = jsx:encode(
        [{<<"ChangeToken">>, ?CHANGE_TOKEN},
         {<<"SizeConstraintSetId">>, ?CREATE_ID},
         {<<"Updates">>,
            [[{<<"Action">>, <<"INSERT">>},
              {<<"SizeConstraint">>,
               [{<<"FieldToMatch">>, [{<<"Data">>, <<"User-Agent">>}, {<<"Type">>, <<"HEADER">>}]},
                {<<"ComparisonOperator">>, <<"NE">>},
                {<<"Size">>, 100},
                {<<"TextTransformation">>, <<"NONE">>}]}]]}]),
    Response = [{<<"ChangeToken">>, ?CHANGE_TOKEN}],
    all_tests(Action, Function, PostData, Response).

update_sql_injection_match_set_tests(_) ->
    Action = "UpdateSqlInjectionMatchSet",
    Function = ?_f(erlcloud_waf:update_sql_injection_match_set(?CHANGE_TOKEN, ?CREATE_ID, [?UPDATE_SQL_INJECTION_MATCH_SET])),
    PostData = jsx:encode(
        [{<<"ChangeToken">>, ?CHANGE_TOKEN},
         {<<"SqlInjectionMatchSetId">>, ?CREATE_ID},
         {<<"Updates">>,
            [[{<<"Action">>, <<"INSERT">>},
              {<<"SqlInjectionMatchTuple">>,
              [{<<"FieldToMatch">>, [{<<"Type">>, <<"QUERY_STRING">>}]},
               {<<"TextTransformation">>, <<"CMD_LINE">>}]}]]}]),
    Response = [{<<"ChangeToken">>, ?CHANGE_TOKEN}],
    all_tests(Action, Function, PostData, Response).

update_web_acl_tests(_) ->
    Action = "UpdateWebACL",
    Function = ?_f(erlcloud_waf:update_web_acl(?CHANGE_TOKEN, ?CREATE_ID, ?UPDATE_WEB_ACL)),
    PostData = jsx:encode(
        [{<<"ChangeToken">>, ?CHANGE_TOKEN},
         {<<"WebACLId">>, ?CREATE_ID},
         {<<"Updates">>,
            [[{<<"Action">>, <<"INSERT">>},
              {<<"ActivatedRule">>,
               [{<<"Action">>, [{<<"Type">>, <<"BLOCK">>}]},
                {<<"Priority">>, 1},
                {<<"RuleId">>, ?RULE_ID}]}]]}]),
    Response = [{<<"ChangeToken">>, ?CHANGE_TOKEN}],
    all_tests(Action, Function, PostData, Response).
   
update_xss_match_set_tests(_) ->
    Action = "UpdateXssMatchSet",
    Function = ?_f(erlcloud_waf:update_xss_match_set(?CHANGE_TOKEN, ?CREATE_ID, [?UPDATE_XSS_MATCH_SET])),
    PostData = jsx:encode(
        [{<<"ChangeToken">>, ?CHANGE_TOKEN},
         {<<"XssMatchSetId">>, ?CREATE_ID},
         {<<"Updates">>,
            [[{<<"Action">>, <<"INSERT">>},
             {<<"XssMatchTuple">>,
                [{<<"FieldToMatch">>, [{<<"Type">>, <<"QUERY_STRING">>}]},
                 {<<"TextTransformation">>, <<"NONE">>}]}]]}]),
    Response = [{<<"XssMatchSet">>,
                [{<<"XssMatchSetId">>, ?CREATE_ID},
                 {<<"XssMatchTuples">>,
                  [[{<<"FieldToMatch">>, [{<<"Type">>, <<"QUERY_STRING">>}]},
                    {<<"TextTransformation">>,<<"NONE">>}]]},
                  {<<"Name">>,<<"TestXssMatchSet">>}]}],
    all_tests(Action, Function, PostData, Response).



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
    Want = sort_json(jsx:decode(Expected)),
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
            {ok, {{200, "OK"}, [], Response}}
    end.


%% input_test converts an input_test specifier into an eunit test generator
-type input_test_spec() :: {pos_integer(), {fun(), expected_body()} | {string(), fun(), expected_body()}}.
-spec input_test(string(), input_test_spec()) -> tuple().
input_test(Response, {Line, {Description, Fun, Expected}})
  when is_list(Description) ->
    {Description,
     {Line,
      fun() ->
              meck:expect(erlcloud_httpc, request, input_expect(Response, Expected)),
              erlcloud_waf:configure(string:copies("A", 20), string:copies("a", 40)),
              Fun()
      end}}.


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
            {ok, {{200, "OK"}, [], Response}}
    end.

%% output_test converts an output_test specifier into an eunit test generator
-type output_test_spec() :: {pos_integer(), {string(), term()} | {string(), string(), term()}}.
-spec output_test(fun(), output_test_spec()) -> tuple().
output_test(Fun, {Line, {Description, Response, Result}}) ->
    {Description,
     {Line,
      fun() ->
              meck:expect(erlcloud_httpc, request, output_expect(Response)),
              erlcloud_waf:configure(string:copies("A", 20), string:copies("a", 40)),
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

all_tests(Action, Function, PostData, Response) ->
    InputTests = [?_waf_test(
                  {Action ++ " input test",
                   Function,
                   PostData
                  }
                  )],
    OutputTests = [?_waf_test(
                   {Action ++ " example response",
                    jsx:encode(Response),
                    {ok, Response}}
                   )],
    
    input_tests(<<>>, InputTests) ++
        output_tests(Function, OutputTests).
