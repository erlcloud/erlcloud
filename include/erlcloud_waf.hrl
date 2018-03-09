-ifndef(erlcloud_waf_hrl).
-define(erlcloud_waf_hrl, 0).

-type waf_update_action() :: insert | delete.
-type waf_text_transformation() :: none | compress_white_space | html_entity_decode | lowercase | cmd_line | url_decode.
-type waf_positional_constraint() :: exactly | starts_with | ends_with | contains | contains_word.
-type waf_comparison_operator() :: eq | ne | le | lt | ge | gt.
-type waf_predicate_type() :: ip_match | byte_match | sql_injection_match | size_constraint.
-type waf_acl_action_type() :: block | allow | count.

-record(waf_field_to_match, {
    type :: uri | 'query_string' | header | method | body,
    data :: string()|undefined
}).
-type(waf_field_to_match() :: #waf_field_to_match{}).

%%%------------------------------------------------------------------------------
%%
%% byte_match_set data types
%%
%%%------------------------------------------------------------------------------
-record(waf_byte_match_tuple, {
        field_to_match :: waf_field_to_match(),
        positional_constraint :: waf_positional_constraint(),
        target_string :: binary(),                              % must be base64 encoded
        text_transformation :: waf_text_transformation()
}).
-type(waf_byte_match_tuple() :: #waf_byte_match_tuple{}).

-record(waf_byte_match_set_update, {
        action :: waf_update_action(),
        byte_match_tuple :: waf_byte_match_tuple()
}).
-type(waf_byte_match_set_update() :: #waf_byte_match_set_update{}).


%%%------------------------------------------------------------------------------
%%
%% ip_set data types
%%
%%%------------------------------------------------------------------------------
-type waf_ip_address_type() :: ip_v4.
-record(waf_ip_set_descriptor, {
        type :: waf_ip_address_type(),
        value :: string() | binary()
}).
-type(waf_ip_set_descriptor() :: #waf_ip_set_descriptor{}).

-record(waf_ip_set_update, {
        action :: waf_update_action(),
        ip_set_descriptor :: waf_ip_set_descriptor()
}).
-type(waf_ip_set_update() :: #waf_ip_set_update{}).


%%%------------------------------------------------------------------------------
%%
%% rule data types
%%
%%%------------------------------------------------------------------------------
-record(waf_rule_predicate, {
    data_id :: string() | binary(),
    negated :: boolean(),
    type :: waf_predicate_type()
}).
-type(waf_rule_predicate() :: #waf_rule_predicate{}).

-record(waf_rule_update, {
        action :: waf_update_action(),
        predicate :: waf_rule_predicate()
}).
-type(waf_rule_update() :: #waf_rule_update{}).


%%%------------------------------------------------------------------------------
%%
%% size_constraint_set data types
%%
%%%------------------------------------------------------------------------------
-record(waf_size_constraint, {
    comparison_operator :: waf_comparison_operator(),
    field_to_match :: waf_field_to_match(),
    size :: pos_integer(),
    text_transformation :: waf_text_transformation()
}).
-type(waf_size_constraint() :: #waf_size_constraint{}).

-record(waf_size_constraint_update, {
        action :: waf_update_action(),
        size_constraint :: waf_size_constraint()
}).
-type(waf_size_constraint_update() :: #waf_size_constraint_update{}).

%%%------------------------------------------------------------------------------
%%
%% sql_injection_match_set data types
%%
%%%------------------------------------------------------------------------------
-record(waf_sql_injection_match_tuple, {
    field_to_match :: waf_field_to_match(),
    text_transformation :: waf_text_transformation()
}).
-type(waf_sql_injection_match_tuple() :: #waf_sql_injection_match_tuple{}).

-record(waf_sql_injection_match_set_update, {
        action :: waf_update_action(),
        sql_injection_match_tuple :: waf_sql_injection_match_tuple()
}).
-type(waf_sql_injection_match_set_update() :: #waf_sql_injection_match_set_update{}).

%%%------------------------------------------------------------------------------
%%
%% update_web_acl data types
%%
%%%------------------------------------------------------------------------------
-record(waf_activated_rule, {
        action :: waf_acl_action_type(),
        priority :: pos_integer(),
        rule_id :: string() | binary()
}).
-type(waf_activated_rule() :: #waf_activated_rule{}).

-record(waf_web_acl_update, {
        action :: waf_update_action(),
        activated_rule :: waf_activated_rule()
}).
-type(waf_web_acl_update() :: #waf_web_acl_update{}).

%%%------------------------------------------------------------------------------
%%
%% update_xss_match_set data types
%%
%%%------------------------------------------------------------------------------
-record(waf_xss_match_tuple, {
        field_to_match :: waf_field_to_match(),
        text_transformation :: waf_text_transformation()
}).
-type(waf_xss_match_tuple() :: #waf_xss_match_tuple{}).

-record(waf_xss_match_set_update, {
        action :: waf_update_action(),
        xss_match_tuple :: waf_xss_match_tuple()
}).
-type(waf_xss_match_set_update() :: #waf_xss_match_set_update{}).

-endif.