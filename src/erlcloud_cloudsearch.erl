%% Amazon CloudSearch Service

-module(erlcloud_cloudsearch).
-author('jkubiak@alertlogic.com').

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

%% Library initialization
-export([configure/2, configure/3, configure/4, new/2, new/3, new/4]).

%% CloudSearch Configuration API
-export([add_tags/2, add_tags/3,
         create_domain/1, create_domain/2,
         delete_domain/1, delete_domain/2,
         delete_index_field/2, delete_index_field/3,
         define_index_field/2, define_index_field/3,
         define_index_fields/2, define_index_fields/3,
         describe_domains/0, describe_domains/1, describe_domains/2,
         describe_index_fields/1, describe_index_fields/2,
         describe_index_fields/3, describe_index_fields/4,
         index_documents/1, index_documents/2,
         list_domain_names/0, list_domain_names/1,
         list_tags/1, list_tags/2,
         remove_tags/2, remove_tags/3]).

%% CloudSearch Utils API
-export([create_tag_list/1,
         date_field_options/1, date_field_options/7,
         date_array_field_options/1, date_array_field_options/6,
         double_field_options/1, double_field_options/7,
         double_array_field_options/1, double_array_field_options/6,
         int_field_options/1, int_field_options/7,
         int_array_field_options/1, int_array_field_options/6,
         latlon_field_options/1, latlon_field_options/7,
         literal_field_options/1, literal_field_options/7,
         literal_array_field_options/1, literal_array_field_options/6,
         text_field_options/1, text_field_options/7,
         text_array_field_options/1, text_array_field_options/6]).


%% Export private functions for unit tests
-ifdef(TEST).
-export([get_tag_params/1]).
-endif.


%%==============================================================================
%% Shared types
%%==============================================================================
-type cloudsearch_return_val() :: {ok, proplists:proplist()} | {error, term()}.
-type cloudsearch_domain_tag() :: [{binary(), string()}].

-export_type([cloudsearch_return_val/0, cloudsearch_domain_tag/0]).

-define(API_VERSION, '2013-01-01').

%%==============================================================================
%% Library initialization
%%==============================================================================

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
       cloudsearch_host=Host
      }.

-spec new(string(), string(), string(), non_neg_integer()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host, Port) ->
    #aws_config{
       access_key_id=AccessKeyID,
       secret_access_key=SecretAccessKey,
       cloudsearch_host=Host,
       cloudsearch_port=Port
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


%%==============================================================================
%% CloudSearch Configuration API
%%==============================================================================

%%------------------------------------------------------------------------------
%% AddTags
%%
%% http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_AddTags.html
%%
%% Example:
%%
%% DomainARN = "arn:aws:cloudsearch:us-east-1:012345678901:domain/my-test-domain".
%% TagList = erlcloud_cloudsearch:create_tag_list([{"name", "my test domain"},
%%                                                 {"base-stack-name", "my stack"}]).
%% {ok, Response} = erlcloud_cloudsearch:add_tags(DomainARN, TagList).
%%------------------------------------------------------------------------------

-spec add_tags(DomainARN :: string(),
               TagList :: [cloudsearch_domain_tag()]) -> cloudsearch_return_val().
add_tags(DomainARN, TagList) ->
    add_tags(DomainARN, TagList, default_config()).

-spec add_tags(DomainName :: string(),
               TagList :: [cloudsearch_domain_tag()],
               Config :: aws_config()) -> cloudsearch_return_val().
add_tags(DomainARN, TagList, Config) ->
    TagParams = get_tag_params(TagList),
    cloudsearch_query(Config, "AddTags", [{"ARN", DomainARN} | TagParams]).

%%------------------------------------------------------------------------------
%% CreateDomain
%%
%% http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_CreateDomain.html
%%------------------------------------------------------------------------------

-spec create_domain(DomainName :: string()) -> cloudsearch_return_val().
create_domain(DomainName) ->
    create_domain(DomainName, default_config()).

-spec create_domain(DomainName :: string(), Config :: aws_config()) ->
    cloudsearch_return_val().
create_domain(DomainName, Config) ->
    cloudsearch_query(Config, "CreateDomain", [{"DomainName", DomainName}]).

%%------------------------------------------------------------------------------
%% DefineIndexField
%%
%% http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DefineIndexField.html
%%
%% `define_index_field/2` and `define_index_field/3` functions allow to configure
%% single field for the search domain.
%% Whereas `define_index_fields/2` and `define_index_fields/3` allow to configure
%% multiple fields at a time. Check AWS documentation for details.
%%
%% Example (multiple fields in single call):
%% IndexFields = [erlcloud_cloudsearch:int_field_options("field1"),
%%                erlcloud_cloudsearch:int_array_field_options("field2"),
%%                erlcloud_cloudsearch:text_field_options("field3")].
%% {ok, Response} = define_index_fields("my-test-domain", IndexFields).
%%------------------------------------------------------------------------------

-spec define_index_field(DomainName :: string(), IndexField :: proplist()) ->
    cloudsearch_return_val().
define_index_field(DomainName, IndexField) ->
    define_index_field(DomainName, IndexField, default_config()).

-spec define_index_field(DomainName :: string(),
                         IndexField :: proplist(),
                         Config :: aws_config()) ->
    cloudsearch_return_val().
define_index_field(DomainName, IndexField, Config) ->
    define_index_fields(DomainName, [IndexField], Config).

-spec define_index_fields(DomainName :: string(),  IndexFields :: [proplist()]) ->
    cloudsearch_return_val().
define_index_fields(DomainName, [[{_,_}|_]|_] = IndexFields) ->
    define_index_fields(DomainName, IndexFields, default_config()).

-spec define_index_fields(DomainName :: string(),
                          IndexFields :: [proplist()],
                          Config :: aws_config()) ->
    cloudsearch_return_val().
define_index_fields(DomainName, [[{_,_}|_]|_] = IndexFields, Config) ->
    IndexFieldParams = erlcloud_aws:param_list(IndexFields, "IndexFields.IndexField"),
    cloudsearch_query(Config, "DefineIndexFields", [{"DomainName", DomainName} |
                                                    IndexFieldParams]).

%%------------------------------------------------------------------------------
%% DeleteDomain
%%
%% http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DeleteDomain.html
%%------------------------------------------------------------------------------

-spec delete_domain(DomainName :: string()) -> cloudsearch_return_val().
delete_domain(DomainName) ->
    delete_domain(DomainName, default_config()).

-spec delete_domain(DomainName :: string(), Config :: aws_config()) -> cloudsearch_return_val().
delete_domain(DomainName, Config) ->
    cloudsearch_query(Config, "DeleteDomain", [{"DomainName", DomainName}]).

%%------------------------------------------------------------------------------
%% DeleteIndexField
%%
%% http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DeleteIndexField.html
%%------------------------------------------------------------------------------

-spec delete_index_field(DomainName :: string(), IndexFieldName :: string()) ->
    cloudsearch_return_val().
delete_index_field(DomainName, IndexFieldName) ->
    delete_index_field(DomainName, IndexFieldName, default_config()).

-spec delete_index_field(DomainName :: string(),
                         IndexFieldName :: string(),
                         Config :: aws_config()) ->
    cloudsearch_return_val().
delete_index_field(DomainName, IndexFieldName, Config) ->
    cloudsearch_query(Config, "DeleteIndexField", [{"DomainName", DomainName},
                                                   {"IndexFieldName", IndexFieldName}]).

%%------------------------------------------------------------------------------
%% DescribeDomains
%%
%% http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DescribeDomains.html
%%------------------------------------------------------------------------------

-spec describe_domains() -> cloudsearch_return_val().
describe_domains() ->
    describe_domains(default_config()).

-spec describe_domains(aws_config() | list()) -> cloudsearch_return_val().
describe_domains(Config) when is_record(Config, aws_config) ->
    cloudsearch_query(Config, "DescribeDomains", []);
describe_domains(DomainNames) when is_list(DomainNames) ->
    describe_domains(DomainNames, default_config()).

-spec describe_domains(DomainNames :: [list()], Config :: aws_config()) ->
    cloudsearch_return_val().
describe_domains(DomainNames, Config)
        when is_list(DomainNames),
             is_record(Config, aws_config)->
    Params = erlcloud_aws:param_list(DomainNames, "DomainNames.member"),
    cloudsearch_query(Config, "DescribeDomains", Params).

%%------------------------------------------------------------------------------
%% DescribeIndexFields
%%
%% http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DescribeIndexFields.html
%%------------------------------------------------------------------------------

-spec describe_index_fields(DomainName :: string()) -> cloudsearch_return_val().
describe_index_fields(DomainName) ->
    describe_index_fields(DomainName, default_config()).

-spec describe_index_fields(DomainName :: string(),
                            Config :: aws_config()) -> cloudsearch_return_val().
describe_index_fields(DomainName, Config) ->
    describe_index_fields(DomainName, false, [], Config).

-spec describe_index_fields(DomainName :: string(),
                            Deployed :: boolean(),
                            Config :: aws_config()) -> cloudsearch_return_val().
describe_index_fields(DomainName, Deployed, Config)
        when Deployed =:= true; Deployed =:= false ->
    describe_index_fields(DomainName, Deployed, [], Config).

-spec describe_index_fields(DomainName :: string(),
                            Deployed :: boolean(),
                            FieldNames :: [string()],
                            Config :: aws_config()) -> cloudsearch_return_val().
describe_index_fields(DomainName, Deployed, FieldNames, Config)
        when Deployed =:= true; Deployed =:= false ->
    FieldParams = erlcloud_aws:param_list(FieldNames, "FieldNames.member"),
    cloudsearch_query(Config, "DescribeIndexFields", [{"DomainName", DomainName},
                                                      {"Deployed", Deployed} |
                                                      FieldParams]).

%%------------------------------------------------------------------------------
%% IndexDocuments
%%
%% http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_IndexDocuments.html
%%------------------------------------------------------------------------------

-spec index_documents(DomainName :: string()) -> cloudsearch_return_val().
index_documents(DomainName) ->
    index_documents(DomainName, default_config()).

-spec index_documents(DomainName :: string(), Config :: aws_config()) ->
    cloudsearch_return_val().
index_documents(DomainName, Config) ->
    cloudsearch_query(Config, "IndexDocuments", [{"DomainName", DomainName}]).

%%------------------------------------------------------------------------------
%% ListDomainNames
%%
%% http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_ListDomainNames.html
%%------------------------------------------------------------------------------

-spec list_domain_names() -> cloudsearch_return_val().
list_domain_names() ->
    list_domain_names(default_config()).

-spec list_domain_names(Config :: aws_config()) -> cloudsearch_return_val().
list_domain_names(Config) ->
    cloudsearch_query(Config, "ListDomainNames", []).

%%------------------------------------------------------------------------------
%% ListTags
%%
%% http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_ListTags.html
%%------------------------------------------------------------------------------

-spec list_tags(DomainARN :: string()) -> cloudsearch_return_val().
list_tags(DomainARN) ->
    list_tags(DomainARN, default_config()).

-spec list_tags(DomainARN :: string(), Config :: aws_config()) ->
    cloudsearch_return_val().
list_tags(DomainARN, Config) ->
    cloudsearch_query(Config, "ListTags", [{"ARN", DomainARN}]).

%%------------------------------------------------------------------------------
%% RemoveTags
%%
%% http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_RemoveTags.html
%%------------------------------------------------------------------------------

-spec remove_tags(DomainARN :: string(),
                  TagKeys :: [string()]) -> cloudsearch_return_val().
remove_tags(DomainARN, TagKeys) ->
    remove_tags(DomainARN, TagKeys, default_config()).

-spec remove_tags(DomainARN :: string(),
                  TagKeys :: [string()],
                  Config :: aws_config()) -> cloudsearch_return_val().
remove_tags(DomainARN, TagKeys, Config) ->
    Params = erlcloud_aws:param_list(TagKeys, "TagKeys.member"),
    cloudsearch_query(Config, "RemoveTags", [{"ARN", DomainARN} | Params]).

%%==============================================================================
%% CloudSearch Utils API
%%==============================================================================

-spec create_tag_list([{Key :: string(), Value :: string}]) ->
    cloudsearch_domain_tag().
create_tag_list([{_Key, _Value} | _] = TagList) ->
    [ [{<<"Key">>, Key}, {<<"Value">>, Value}] || {Key, Value} <- TagList ].

%%------------------------------------------------------------------------------
%% Index field: DateOptions
%%
%% http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DateOptions.html
%%------------------------------------------------------------------------------
-spec date_field_options(FieldName :: string()) -> proplist().
date_field_options(FieldName) ->
    [{"IndexFieldName", FieldName},
     {"IndexFieldType", "date"}].

-spec date_field_options(FieldName :: string(),
                         DefaultValue :: string(),
                         FacetEnabled :: boolean(),
                         ReturnEnabled :: boolean(),
                         SearchEnabled :: boolean(),
                         SortEnabled :: boolean(),
                         SourceField :: string()) -> proplist().
date_field_options(FieldName, DefaultValue, FacetEnabled, ReturnEnabled,
                   SearchEnabled, SortEnabled, SourceField) ->
    [{"IndexFieldName", FieldName},
     {"IndexFieldType", "date"},
     {"DateOptions.DefaultValue", DefaultValue},
     {"DateOptions.FacetEnabled", FacetEnabled},
     {"DateOptions.ReturnEnabled", ReturnEnabled},
     {"DateOptions.SearchEnabled", SearchEnabled},
     {"DateOptions.SortEnabled", SortEnabled},
     {"DateOptions.SourceField", SourceField}].

%%------------------------------------------------------------------------------
%% Index field: DateArrayOptions

%% http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DateArrayOptions.html
%%------------------------------------------------------------------------------
-spec date_array_field_options(FieldName :: string()) -> proplist().
date_array_field_options(FieldName) ->
    [{"IndexFieldName", FieldName},
     {"IndexFieldType", "date-array"}].

-spec date_array_field_options(FieldName :: string(),
                               DefaultValue :: string(),
                               FacetEnabled :: boolean(),
                               ReturnEnabled :: boolean(),
                               SearchEnabled :: boolean(),
                               SourceField :: string()) -> proplist().
date_array_field_options(FieldName, DefaultValue, FacetEnabled, ReturnEnabled,
                         SearchEnabled, SourceField) ->
    [{"IndexFieldName", FieldName},
     {"IndexFieldType", "date-array"},
     {"DateArrayOptions.DefaultValue", DefaultValue},
     {"DateArrayOptions.FacetEnabled", FacetEnabled},
     {"DateArrayOptions.ReturnEnabled", ReturnEnabled},
     {"DateArrayOptions.SearchEnabled", SearchEnabled},
     {"DateArrayOptions.SourceField", SourceField}].

%%------------------------------------------------------------------------------
%% Index field: DoubleOptions
%%
%% http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DoubleOptions.html
%%------------------------------------------------------------------------------
-spec double_field_options(FieldName :: string()) -> proplist().
double_field_options(FieldName) ->
    [{"IndexFieldName", FieldName},
     {"IndexFieldType", "double"},
     {"DoubleOptions.DefaultValue", 0.0}].

-spec double_field_options(FieldName :: string(),
                           DefaultValue :: float(),
                           FacetEnabled :: boolean(),
                           ReturnEnabled :: boolean(),
                           SearchEnabled :: boolean(),
                           SortEnabled :: boolean(),
                           SourceField :: string()) -> proplist().
double_field_options(FieldName, DefaultValue, FacetEnabled, ReturnEnabled,
                     SearchEnabled, SortEnabled, SourceField) ->
    [{"IndexFieldName", FieldName},
     {"IndexFieldType", "double"},
     {"DoubleOptions.DefaultValue", DefaultValue},
     {"DoubleOptions.FacetEnabled", FacetEnabled},
     {"DoubleOptions.ReturnEnabled", ReturnEnabled},
     {"DoubleOptions.SearchEnabled", SearchEnabled},
     {"DoubleOptions.SortEnabled", SortEnabled},
     {"DoubleOptions.SourceField", SourceField}].

%%------------------------------------------------------------------------------
%% Index field: DoubleArrayOptions

%% http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DoubleArrayOptions.html
%%------------------------------------------------------------------------------
-spec double_array_field_options(FieldName :: string()) -> proplist().
double_array_field_options(FieldName) ->
    [{"IndexFieldName", FieldName},
     {"IndexFieldType", "double-array"},
     {"DoubleArrayOptions.DefaultValue", 0.0}].

-spec double_array_field_options(FieldName :: string(),
                                 DefaultValue :: float(),
                                 FacetEnabled :: boolean(),
                                 ReturnEnabled :: boolean(),
                                 SearchEnabled :: boolean(),
                                 SourceField :: string()) -> proplist().
double_array_field_options(FieldName, DefaultValue, FacetEnabled, ReturnEnabled,
                           SearchEnabled, SourceField) ->
    [{"IndexFieldName", FieldName},
     {"IndexFieldType", "double-array"},
     {"DoubleArrayOptions.DefaultValue", DefaultValue},
     {"DoubleArrayOptions.FacetEnabled", FacetEnabled},
     {"DoubleArrayOptions.ReturnEnabled", ReturnEnabled},
     {"DoubleArrayOptions.SearchEnabled", SearchEnabled},
     {"DoubleArrayOptions.SourceField", SourceField}].

%%------------------------------------------------------------------------------
%% Index field: IntOptions
%%
%% http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_IntOptions.html
%%------------------------------------------------------------------------------
-spec int_field_options(FieldName :: string()) -> proplist().
int_field_options(FieldName) ->
    [{"IndexFieldName", FieldName},
     {"IndexFieldType", "int"},
     {"IntOptions.DefaultValue", 0}].

-spec int_field_options(FieldName :: string(),
                        DefaultValue :: integer(),
                        FacetEnabled :: boolean(),
                        ReturnEnabled :: boolean(),
                        SearchEnabled :: boolean(),
                        SortEnabled :: boolean(),
                        SourceField :: string()) -> proplist().
int_field_options(FieldName, DefaultValue, FacetEnabled, ReturnEnabled,
                  SearchEnabled, SortEnabled, SourceField) ->
    [{"IndexFieldName", FieldName},
     {"IndexFieldType", "int"},
     {"IntOptions.DefaultValue", DefaultValue},
     {"IntOptions.FacetEnabled", FacetEnabled},
     {"IntOptions.ReturnEnabled", ReturnEnabled},
     {"IntOptions.SearchEnabled", SearchEnabled},
     {"IntOptions.SortEnabled", SortEnabled},
     {"IntOptions.SourceField", SourceField}].

%%------------------------------------------------------------------------------
%% Index field: IntArrayOptions

%% http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_IntArrayOptions.html
%%------------------------------------------------------------------------------
-spec int_array_field_options(FieldName :: string()) -> proplist().
int_array_field_options(FieldName) ->
    [{"IndexFieldName", FieldName},
     {"IndexFieldType", "int-array"},
     {"IntArrayOptions.DefaultValue", 0}].

-spec int_array_field_options(FieldName :: string(),
                              DefaultValue :: integer(),
                              FacetEnabled :: boolean(),
                              ReturnEnabled :: boolean(),
                              SearchEnabled :: boolean(),
                              SourceField :: string()) -> proplist().
int_array_field_options(FieldName, DefaultValue, FacetEnabled, ReturnEnabled,
                        SearchEnabled, SourceField) ->
    [{"IndexFieldName", FieldName},
     {"IndexFieldType", "int-array"},
     {"IntArrayOptions.DefaultValue", DefaultValue},
     {"IntArrayOptions.FacetEnabled", FacetEnabled},
     {"IntArrayOptions.ReturnEnabled", ReturnEnabled},
     {"IntArrayOptions.SearchEnabled", SearchEnabled},
     {"IntArrayOptions.SourceField", SourceField}].

%%------------------------------------------------------------------------------
%% Index field: LatLonOptions

%% http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_LatLonOptions.html
%%------------------------------------------------------------------------------
-spec latlon_field_options(FieldName :: string()) -> proplist().
latlon_field_options(FieldName) ->
    [{"IndexFieldName", FieldName},
     {"IndexFieldType", "latlon"}].

-spec latlon_field_options(FieldName :: string(),
                           DefaultValue :: string(),
                           FacetEnabled :: boolean(),
                           ReturnEnabled :: boolean(),
                           SearchEnabled :: boolean(),
                           SortEnabled :: boolean(),
                           SourceField :: string()) -> proplist().
latlon_field_options(FieldName, DefaultValue, FacetEnabled, ReturnEnabled,
                     SearchEnabled, SortEnabled, SourceField) ->
    [{"IndexFieldName", FieldName},
     {"IndexFieldType", "latlon"},
     {"LatLonOptions.DefaultValue", DefaultValue},
     {"LatLonOptions.FacetEnabled", FacetEnabled},
     {"LatLonOptions.ReturnEnabled", ReturnEnabled},
     {"LatLonOptions.SearchEnabled", SearchEnabled},
     {"LatLonOptions.SortEnabled", SortEnabled},
     {"LatLonOptions.SourceField", SourceField}].

%%------------------------------------------------------------------------------
%% Index field: LiteralOptions

%% http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_LiteralOptions.html
%%------------------------------------------------------------------------------
-spec literal_field_options(FieldName :: string()) -> proplist().
literal_field_options(FieldName) ->
    [{"IndexFieldName", FieldName},
     {"IndexFieldType", "literal"}].

-spec literal_field_options(FieldName :: string(),
                            DefaultValue :: string(),
                            FacetEnabled :: boolean(),
                            ReturnEnabled :: boolean(),
                            SearchEnabled :: boolean(),
                            SortEnabled :: boolean(),
                            SourceField :: string()) -> proplist().
literal_field_options(FieldName, DefaultValue, FacetEnabled, ReturnEnabled,
                      SearchEnabled, SortEnabled, SourceField) ->
    [{"IndexFieldName", FieldName},
     {"IndexFieldType", "literal"},
     {"LiteralOptions.DefaultValue", DefaultValue},
     {"LiteralOptions.FacetEnabled", FacetEnabled},
     {"LiteralOptions.ReturnEnabled", ReturnEnabled},
     {"LiteralOptions.SearchEnabled", SearchEnabled},
     {"LiteralOptions.SortEnabled", SortEnabled},
     {"LiteralOptions.SourceField", SourceField}].

%%------------------------------------------------------------------------------
%% Index field: LiteralArrayOptions

%% http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_LiteralArrayOptions.html
%%------------------------------------------------------------------------------
-spec literal_array_field_options(FieldName :: string()) -> proplist().
literal_array_field_options(FieldName) ->
    [{"IndexFieldName", FieldName},
     {"IndexFieldType", "literal-array"}].

-spec literal_array_field_options(FieldName :: string(),
                                  DefaultValue :: string(),
                                  FacetEnabled :: boolean(),
                                  ReturnEnabled :: boolean(),
                                  SearchEnabled :: boolean(),
                                  SourceField :: string()) -> proplist().
literal_array_field_options(FieldName, DefaultValue, FacetEnabled, ReturnEnabled,
                            SearchEnabled, SourceField) ->
    [{"IndexFieldName", FieldName},
     {"IndexFieldType", "literal-array"},
     {"LiteralArrayOptions.DefaultValue", DefaultValue},
     {"LiteralArrayOptions.FacetEnabled", FacetEnabled},
     {"LiteralArrayOptions.ReturnEnabled", ReturnEnabled},
     {"LiteralArrayOptions.SearchEnabled", SearchEnabled},
     {"LiteralArrayOptions.SourceField", SourceField}].

%%------------------------------------------------------------------------------
%% Index field: TextOptions

%% http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_TextOptions.html
%%------------------------------------------------------------------------------
-spec text_field_options(FieldName :: string()) -> proplist().
text_field_options(FieldName) ->
    [{"IndexFieldName", FieldName},
     {"IndexFieldType", "text"}].

-spec text_field_options(FieldName :: string(),
                         AnalysisScheme :: string(),
                         DefaultValue :: string(),
                         HighlightEnabled :: boolean(),
                         ReturnEnabled :: boolean(),
                         SortEnabled :: boolean(),
                         SourceField :: string()) -> proplist().
text_field_options(FieldName, AnalysisScheme, DefaultValue, HighlightEnabled,
                   ReturnEnabled, SortEnabled, SourceField) ->
    [{"IndexFieldName", FieldName},
     {"IndexFieldType", "text"},
     {"TextOptions.AnalysisScheme", AnalysisScheme},
     {"TextOptions.DefaultValue", DefaultValue},
     {"TextOptions.HighlightEnabled", HighlightEnabled},
     {"TextOptions.ReturnEnabled", ReturnEnabled},
     {"TextOptions.SortEnabled", SortEnabled},
     {"TextOptions.SourceField", SourceField}].

%%------------------------------------------------------------------------------
%% Index field: TextArrayOptions

%% http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_TextArrayOptions.html
%%------------------------------------------------------------------------------
-spec text_array_field_options(FieldName :: string()) -> proplist().
text_array_field_options(FieldName) ->
    [{"IndexFieldName", FieldName},
     {"IndexFieldType", "text-array"}].

-spec text_array_field_options(FieldName :: string(),
                               AnalysisScheme :: string(),
                               DefaultValue :: string(),
                               HighlightEnabled :: boolean(),
                               ReturnEnabled :: boolean(),
                               SourceField :: string()) -> proplist().
text_array_field_options(FieldName, AnalysisScheme, DefaultValue,
                         HighlightEnabled, ReturnEnabled, SourceField) ->
    [{"IndexFieldName", FieldName},
     {"IndexFieldType", "text-array"},
     {"TextArrayOptions.AnalysisScheme", AnalysisScheme},
     {"TextArrayOptions.DefaultValue", DefaultValue},
     {"TextArrayOptions.HighlightEnabled", HighlightEnabled},
     {"TextArrayOptions.ReturnEnabled", ReturnEnabled},
     {"TextArrayOptions.SourceField", SourceField}].

%%==============================================================================
%% Internal Functions
%%==============================================================================

cloudsearch_query(Config, Action, Params) ->
    cloudsearch_query(Config, Action, Params, ?API_VERSION).

cloudsearch_query(Config, Action, Params, ApiVersion) ->
    QParams = [{"Action", Action}, {"Version", ApiVersion}|Params],
    case erlcloud_aws:aws_request4(post, undefined,
                                   Config#aws_config.cloudsearch_host,
                                   undefined, "/", QParams, "cloudsearch",
                                   [{"Accept", "application/json"}], Config) of
    {ok, Response} ->
        {ok, jsx:decode(Response)};
    {error, Reason} ->
        {error, Reason}
    end.

get_tag_params(TagList) ->
    erlcloud_aws:param_list(TagList, "TagList.member").
