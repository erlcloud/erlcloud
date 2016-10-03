-module(erlcloud_cloudsearch_tests).
-author('jkubiak@alertlogic.com').
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud_aws.hrl").

erlcloud_cloudsearch_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun add_tags/0,
      fun create_domain/0,
      fun define_index_fields/0,
      fun delete_domain/0,
      fun delete_index_field/0,
      fun describe_domains/0,
      fun describe_index_fields/0,
      fun index_documents/0,
      fun list_domain_names/0,
      fun list_tags/0,
      fun remove_tags/0,
      fun get_tag_params/0,
      fun create_tag_list/0,
      fun date_field_options/0,
      fun date_array_field_options/0,
      fun double_field_options/0,
      fun double_array_field_options/0,
      fun int_field_options/0,
      fun int_array_field_options/0,
      fun latlon_field_options/0,
      fun literal_field_options/0,
      fun literal_array_field_options/0,
      fun text_field_options/0,
      fun text_array_field_options/0]}.

start() ->
    meck:new(HTTPMock = erlcloud_httpc),
    meck:new(CFGMock = erlcloud_aws, [non_strict, passthrough]),
    meck:expect(erlcloud_httpc, request, fun(_,_,_,_,_,_) -> {ok, {{200, "OK"}, [], <<"{\"status\": \"OK\"}">>}} end),
    Config = #aws_config{access_key_id="AccessId",
                         secret_access_key="SecretAccessKey",
                         security_token="SecurityToken"},
    meck:expect(CFGMock, default_config, 0, Config),
    meck:expect(CFGMock, update_config, 1, {ok, Config}),
    [HTTPMock, CFGMock].

stop(Modules) ->
    meck:unload(Modules).

-define(ANALYSIS_SCHEME, "_en_english").
-define(DEFAULT_VALUE_DATE, "1970-01-01").
-define(DEFAULT_VALUE_DOUBLE, 0.0).
-define(DEFAULT_VALUE_INT, 0).
-define(DEFAULT_VALUE_LATLON, "53.112345N").
-define(DEFAULT_VALUE_LITERAL, "Literal-string").
-define(DEFAULT_VALUE_TEXT, "Text-string").
-define(DOMAIN_ARN, "arn:aws:cloudsearch:us-east-1:012345678901:domain/my-test-domain").
-define(DOMAIN_NAME, "test-domain-name").
-define(FIELD_NAME, "TestField").
-define(FACET_ENABLED, true).
-define(HIGHLIGHT_ENABLED, false).
-define(RETURN_ENABLED, true).
-define(SEARCH_ENABLED, false).
-define(SORT_ENABLED, false).
-define(SOURCE_FIELD, "TestSourceField").

add_tags() ->
    TagList = erlcloud_cloudsearch:create_tag_list([{"name", "my test domain"},
                                                    {"base-stack-name", "my stack"}]),
    ?assertMatch({ok, _}, erlcloud_cloudsearch:add_tags(?DOMAIN_ARN, TagList)),
    meck:validate(erlcloud_httpc).

create_domain() ->
    ?assertMatch({ok, _}, erlcloud_cloudsearch:create_domain(?DOMAIN_NAME)),
    meck:validate(erlcloud_httpc).

define_index_fields() ->
    IndexFields = [erlcloud_cloudsearch:int_field_options("field1"),
                   erlcloud_cloudsearch:int_array_field_options("field2"),
                   erlcloud_cloudsearch:text_field_options("field3")],
    [Field1 | _T] = IndexFields,
    ?assertMatch({ok, _}, erlcloud_cloudsearch:define_index_field(?DOMAIN_NAME, Field1)),
    ?assertMatch({ok, _}, erlcloud_cloudsearch:define_index_fields(?DOMAIN_NAME, IndexFields)),
    meck:validate(erlcloud_httpc).

delete_domain() ->
    ?assertMatch({ok, _}, erlcloud_cloudsearch:delete_domain(?DOMAIN_NAME)),
    meck:validate(erlcloud_httpc).

delete_index_field() ->
    ?assertMatch({ok, _}, erlcloud_cloudsearch:delete_index_field(?DOMAIN_NAME, ?FIELD_NAME)),
    meck:validate(erlcloud_httpc).

describe_domains() ->
    ?assertMatch({ok, _}, erlcloud_cloudsearch:describe_domains()),
    ?assertMatch({ok, _}, erlcloud_cloudsearch:describe_domains([?DOMAIN_NAME,
                                                                 ?DOMAIN_NAME ++ "1"])),
    meck:validate(erlcloud_httpc).

describe_index_fields() ->
    ?assertMatch({ok, _}, erlcloud_cloudsearch:describe_index_fields(?DOMAIN_NAME)),
    ?assertMatch({ok, _}, erlcloud_cloudsearch:describe_index_fields(?DOMAIN_NAME,
                                                                     true,
                                                                     erlcloud_aws:default_config())),
    ?assertMatch({ok, _}, erlcloud_cloudsearch:describe_index_fields(?DOMAIN_NAME,
                                                                     true,
                                                                     [?FIELD_NAME,
                                                                      ?FIELD_NAME ++ "1"],
                                                                     erlcloud_aws:default_config())),
    meck:validate(erlcloud_httpc).

index_documents() ->
    ?assertMatch({ok, _}, erlcloud_cloudsearch:index_documents(?DOMAIN_NAME)),
    meck:validate(erlcloud_httpc).

list_domain_names() ->
    ?assertMatch({ok, _}, erlcloud_cloudsearch:list_domain_names()),
    meck:validate(erlcloud_httpc).

list_tags() ->
    ?assertMatch({ok, _}, erlcloud_cloudsearch:list_tags(?DOMAIN_ARN)),
    meck:validate(erlcloud_httpc).

remove_tags() ->
    InTags = ["Tag1Key", "Tag2Key"],
    ?assertMatch({ok, _}, erlcloud_cloudsearch:remove_tags(?DOMAIN_ARN, InTags)),
    meck:validate(erlcloud_httpc).

get_tag_params() ->
    InTags = [{"Key1", "Value1"},
              {"Key2", "Value2"}],
    ExpectedOutput = [{"TagList.member.1",{"Key1","Value1"}},
                      {"TagList.member.2",{"Key2","Value2"}}],
    ?assertEqual(ExpectedOutput, erlcloud_cloudsearch:get_tag_params(InTags)).

create_tag_list() ->
    InTags = [{"Key1", "Value1"},
              {"Key2", "Value2"}],
    ExpectedOutput = [[{<<"Key">>, "Key1"}, {<<"Value">>, "Value1"}],
                      [{<<"Key">>, "Key2"}, {<<"Value">>, "Value2"}]],
    ?assertEqual(ExpectedOutput, erlcloud_cloudsearch:create_tag_list(InTags)).

date_field_options() ->
    FieldType = "date",
    ExpectedOutput1 = [{"IndexFieldName", ?FIELD_NAME},
                       {"IndexFieldType", FieldType}],
    ExpectedOutput2 = ExpectedOutput1 ++
            [{"DateOptions.DefaultValue", ?DEFAULT_VALUE_DATE},
             {"DateOptions.FacetEnabled", ?FACET_ENABLED},
             {"DateOptions.ReturnEnabled", ?RETURN_ENABLED},
             {"DateOptions.SearchEnabled", ?SEARCH_ENABLED},
             {"DateOptions.SortEnabled", ?SORT_ENABLED},
             {"DateOptions.SourceField", ?SOURCE_FIELD}],
    ?assertEqual(ExpectedOutput1, erlcloud_cloudsearch:date_field_options(?FIELD_NAME)),
    ?assertEqual(ExpectedOutput2,
                 erlcloud_cloudsearch:date_field_options(?FIELD_NAME, ?DEFAULT_VALUE_DATE,
                                                         ?FACET_ENABLED, ?RETURN_ENABLED,
                                                         ?SEARCH_ENABLED, ?SORT_ENABLED,
                                                         ?SOURCE_FIELD)).

date_array_field_options() ->
    FieldType = "date-array",
    ExpectedOutput1 = [{"IndexFieldName", ?FIELD_NAME},
                       {"IndexFieldType", FieldType}],
    ExpectedOutput2 = ExpectedOutput1 ++
            [{"DateArrayOptions.DefaultValue", ?DEFAULT_VALUE_DATE},
             {"DateArrayOptions.FacetEnabled", ?FACET_ENABLED},
             {"DateArrayOptions.ReturnEnabled", ?RETURN_ENABLED},
             {"DateArrayOptions.SearchEnabled", ?SEARCH_ENABLED},
             {"DateArrayOptions.SourceField", ?SOURCE_FIELD}],
    ?assertEqual(ExpectedOutput1, erlcloud_cloudsearch:date_array_field_options(?FIELD_NAME)),
    ?assertEqual(ExpectedOutput2,
                 erlcloud_cloudsearch:date_array_field_options(?FIELD_NAME, ?DEFAULT_VALUE_DATE,
                                                               ?FACET_ENABLED, ?RETURN_ENABLED,
                                                               ?SEARCH_ENABLED, ?SOURCE_FIELD)).

double_field_options() ->
    FieldType = "double",
    ExpectedOutput1 = [{"IndexFieldName", ?FIELD_NAME},
                       {"IndexFieldType", FieldType},
                       {"DoubleOptions.DefaultValue", ?DEFAULT_VALUE_DOUBLE}],
    ExpectedOutput2 = ExpectedOutput1 ++
            [{"DoubleOptions.FacetEnabled", ?FACET_ENABLED},
             {"DoubleOptions.ReturnEnabled", ?RETURN_ENABLED},
             {"DoubleOptions.SearchEnabled", ?SEARCH_ENABLED},
             {"DoubleOptions.SortEnabled", ?SORT_ENABLED},
             {"DoubleOptions.SourceField", ?SOURCE_FIELD}],
    ?assertEqual(ExpectedOutput1, erlcloud_cloudsearch:double_field_options(?FIELD_NAME)),
    ?assertEqual(ExpectedOutput2,
                 erlcloud_cloudsearch:double_field_options(?FIELD_NAME, ?DEFAULT_VALUE_DOUBLE,
                                                           ?FACET_ENABLED, ?RETURN_ENABLED,
                                                           ?SEARCH_ENABLED, ?SORT_ENABLED,
                                                           ?SOURCE_FIELD)).

double_array_field_options() ->
    FieldType = "double-array",
    ExpectedOutput1 = [{"IndexFieldName", ?FIELD_NAME},
                       {"IndexFieldType", FieldType},
                       {"DoubleArrayOptions.DefaultValue", ?DEFAULT_VALUE_DOUBLE}],
    ExpectedOutput2 = ExpectedOutput1 ++
            [{"DoubleArrayOptions.FacetEnabled", ?FACET_ENABLED},
             {"DoubleArrayOptions.ReturnEnabled", ?RETURN_ENABLED},
             {"DoubleArrayOptions.SearchEnabled", ?SEARCH_ENABLED},
             {"DoubleArrayOptions.SourceField", ?SOURCE_FIELD}],
    ?assertEqual(ExpectedOutput1, erlcloud_cloudsearch:double_array_field_options(?FIELD_NAME)),
    ?assertEqual(ExpectedOutput2,
                 erlcloud_cloudsearch:double_array_field_options(?FIELD_NAME, ?DEFAULT_VALUE_DOUBLE,
                                                                 ?FACET_ENABLED, ?RETURN_ENABLED,
                                                                 ?SEARCH_ENABLED, ?SOURCE_FIELD)).

int_field_options() ->
    FieldType = "int",
    ExpectedOutput1 = [{"IndexFieldName", ?FIELD_NAME},
                       {"IndexFieldType", FieldType},
                       {"IntOptions.DefaultValue", ?DEFAULT_VALUE_INT}],
    ExpectedOutput2 = ExpectedOutput1 ++
            [{"IntOptions.FacetEnabled", ?FACET_ENABLED},
             {"IntOptions.ReturnEnabled", ?RETURN_ENABLED},
             {"IntOptions.SearchEnabled", ?SEARCH_ENABLED},
             {"IntOptions.SortEnabled", ?SORT_ENABLED},
             {"IntOptions.SourceField", ?SOURCE_FIELD}],
    ?assertEqual(ExpectedOutput1, erlcloud_cloudsearch:int_field_options(?FIELD_NAME)),
    ?assertEqual(ExpectedOutput2,
                 erlcloud_cloudsearch:int_field_options(?FIELD_NAME, ?DEFAULT_VALUE_INT,
                                                        ?FACET_ENABLED, ?RETURN_ENABLED,
                                                        ?SEARCH_ENABLED, ?SORT_ENABLED,
                                                        ?SOURCE_FIELD)).

int_array_field_options() ->
    FieldType = "int-array",
    ExpectedOutput1 = [{"IndexFieldName", ?FIELD_NAME},
                       {"IndexFieldType", FieldType},
                       {"IntArrayOptions.DefaultValue", ?DEFAULT_VALUE_INT}],
    ExpectedOutput2 = ExpectedOutput1 ++
            [{"IntArrayOptions.FacetEnabled", ?FACET_ENABLED},
             {"IntArrayOptions.ReturnEnabled", ?RETURN_ENABLED},
             {"IntArrayOptions.SearchEnabled", ?SEARCH_ENABLED},
             {"IntArrayOptions.SourceField", ?SOURCE_FIELD}],
    ?assertEqual(ExpectedOutput1, erlcloud_cloudsearch:int_array_field_options(?FIELD_NAME)),
    ?assertEqual(ExpectedOutput2,
                 erlcloud_cloudsearch:int_array_field_options(?FIELD_NAME, ?DEFAULT_VALUE_INT,
                                                              ?FACET_ENABLED, ?RETURN_ENABLED,
                                                              ?SEARCH_ENABLED, ?SOURCE_FIELD)).

latlon_field_options() ->
    FieldType = "latlon",
    ExpectedOutput1 = [{"IndexFieldName", ?FIELD_NAME},
                       {"IndexFieldType", FieldType}],
    ExpectedOutput2 = ExpectedOutput1 ++
            [{"LatLonOptions.DefaultValue", ?DEFAULT_VALUE_LATLON},
             {"LatLonOptions.FacetEnabled", ?FACET_ENABLED},
             {"LatLonOptions.ReturnEnabled", ?RETURN_ENABLED},
             {"LatLonOptions.SearchEnabled", ?SEARCH_ENABLED},
             {"LatLonOptions.SortEnabled", ?SORT_ENABLED},
             {"LatLonOptions.SourceField", ?SOURCE_FIELD}],
    ?assertEqual(ExpectedOutput1, erlcloud_cloudsearch:latlon_field_options(?FIELD_NAME)),
    ?assertEqual(ExpectedOutput2,
                 erlcloud_cloudsearch:latlon_field_options(?FIELD_NAME, ?DEFAULT_VALUE_LATLON,
                                                           ?FACET_ENABLED, ?RETURN_ENABLED,
                                                           ?SEARCH_ENABLED, ?SORT_ENABLED,
                                                           ?SOURCE_FIELD)).

literal_field_options() ->
    FieldType = "literal",
    ExpectedOutput1 = [{"IndexFieldName", ?FIELD_NAME},
                       {"IndexFieldType", FieldType}],
    ExpectedOutput2 = ExpectedOutput1 ++
            [{"LiteralOptions.DefaultValue", ?DEFAULT_VALUE_LITERAL},
             {"LiteralOptions.FacetEnabled", ?FACET_ENABLED},
             {"LiteralOptions.ReturnEnabled", ?RETURN_ENABLED},
             {"LiteralOptions.SearchEnabled", ?SEARCH_ENABLED},
             {"LiteralOptions.SortEnabled", ?SORT_ENABLED},
             {"LiteralOptions.SourceField", ?SOURCE_FIELD}],
    ?assertEqual(ExpectedOutput1, erlcloud_cloudsearch:literal_field_options(?FIELD_NAME)),
    ?assertEqual(ExpectedOutput2,
                 erlcloud_cloudsearch:literal_field_options(?FIELD_NAME, ?DEFAULT_VALUE_LITERAL,
                                                            ?FACET_ENABLED, ?RETURN_ENABLED,
                                                            ?SEARCH_ENABLED, ?SORT_ENABLED,
                                                            ?SOURCE_FIELD)).

literal_array_field_options() ->
    FieldType = "literal-array",
    ExpectedOutput1 = [{"IndexFieldName", ?FIELD_NAME},
                       {"IndexFieldType", FieldType}],
    ExpectedOutput2 = ExpectedOutput1 ++
            [{"LiteralArrayOptions.DefaultValue", ?DEFAULT_VALUE_LITERAL},
             {"LiteralArrayOptions.FacetEnabled", ?FACET_ENABLED},
             {"LiteralArrayOptions.ReturnEnabled", ?RETURN_ENABLED},
             {"LiteralArrayOptions.SearchEnabled", ?SEARCH_ENABLED},
             {"LiteralArrayOptions.SourceField", ?SOURCE_FIELD}],
    ?assertEqual(ExpectedOutput1, erlcloud_cloudsearch:literal_array_field_options(?FIELD_NAME)),
    ?assertEqual(ExpectedOutput2,
                 erlcloud_cloudsearch:literal_array_field_options(?FIELD_NAME, ?DEFAULT_VALUE_LITERAL,
                                                                  ?FACET_ENABLED, ?RETURN_ENABLED,
                                                                  ?SEARCH_ENABLED, ?SOURCE_FIELD)).

text_field_options() ->
    FieldType = "text",
    ExpectedOutput1 = [{"IndexFieldName", ?FIELD_NAME},
                       {"IndexFieldType", FieldType}],
    ExpectedOutput2 = ExpectedOutput1 ++
            [{"TextOptions.AnalysisScheme", ?ANALYSIS_SCHEME},
             {"TextOptions.DefaultValue", ?DEFAULT_VALUE_TEXT},
             {"TextOptions.HighlightEnabled", ?HIGHLIGHT_ENABLED},
             {"TextOptions.ReturnEnabled", ?RETURN_ENABLED},
             {"TextOptions.SortEnabled", ?SORT_ENABLED},
             {"TextOptions.SourceField", ?SOURCE_FIELD}],
    ?assertEqual(ExpectedOutput1, erlcloud_cloudsearch:text_field_options(?FIELD_NAME)),
    ?assertEqual(ExpectedOutput2,
                 erlcloud_cloudsearch:text_field_options(?FIELD_NAME, ?ANALYSIS_SCHEME,
                                                         ?DEFAULT_VALUE_TEXT, ?HIGHLIGHT_ENABLED,
                                                         ?RETURN_ENABLED, ?SORT_ENABLED,
                                                         ?SOURCE_FIELD)).

text_array_field_options() ->
    FieldType = "text-array",
    ExpectedOutput1 = [{"IndexFieldName", ?FIELD_NAME},
                       {"IndexFieldType", FieldType}],
    ExpectedOutput2 = ExpectedOutput1 ++
            [{"TextArrayOptions.AnalysisScheme", ?ANALYSIS_SCHEME},
             {"TextArrayOptions.DefaultValue", ?DEFAULT_VALUE_TEXT},
             {"TextArrayOptions.HighlightEnabled", ?HIGHLIGHT_ENABLED},
             {"TextArrayOptions.ReturnEnabled", ?RETURN_ENABLED},
             {"TextArrayOptions.SourceField", ?SOURCE_FIELD}],
    ?assertEqual(ExpectedOutput1, erlcloud_cloudsearch:text_array_field_options(?FIELD_NAME)),
    ?assertEqual(ExpectedOutput2,
                 erlcloud_cloudsearch:text_array_field_options(?FIELD_NAME, ?ANALYSIS_SCHEME,
                                                               ?DEFAULT_VALUE_TEXT, ?HIGHLIGHT_ENABLED,
                                                               ?RETURN_ENABLED, ?SOURCE_FIELD)).
