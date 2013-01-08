-module(erlcloud_sdb).

-type sdb_attribute() :: {string(), string() | {replace, string()}}.
-type sdb_attributes() :: [sdb_attribute()].
-type sdb_conditional() :: {string(), string() | exists | not_exists}.
-type sdb_conditionals() :: [sdb_conditional()].
-type sdb_delete_attribute() :: {string(), string()} | string().
-type sdb_delete_attributes() :: [sdb_delete_attribute()].

%% Library initialization.
-export([configure/2, configure/3, new/2, new/3]).

%% SDB API Functions
-export([
    %% Domains
    create_domain/1, create_domain/2,
    delete_domain/1, delete_domain/2,
    domain_metadata/1, domain_metadata/2,
    list_domains/0, list_domains/1, list_domains/2, list_domains/3,
    
    %% Items
    batch_put_attributes/2, batch_put_attributes/3,
    delete_attributes/2, delete_attributes/3,
    delete_attributes/4, delete_attributes/5,
    get_attributes/2, get_attributes/3, get_attributes/4, get_attributes/5,
    put_attributes/3, put_attributes/4, put_attributes/5,
    select/1, select/2, select/3, select/4
]).

-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

-define(API_VERSION, "2009-04-15").

-spec(new/2 :: (string(), string()) -> aws_config()).
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey}.

-spec(new/3 :: (string(), string(), string()) -> aws_config()).
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                sdb_host=Host}.

-spec(configure/2 :: (string(), string()) -> ok).
configure(AccessKeyID, SecretAccessKey) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey)),
    ok.

-spec(configure/3 :: (string(), string(), string()) -> ok).
configure(AccessKeyID, SecretAccessKey, Host) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host)),
    ok.

default_config() -> erlcloud_aws:default_config().

-spec create_domain/1 :: (string()) -> proplist().
create_domain(Name) ->
    create_domain(Name, default_config()).

-spec create_domain/2 :: (string(), aws_config()) -> proplist().
create_domain(Name, Config)
  when is_list(Name) ->
    sdb_simple_request(Config, "CreateDomain", [{"DomainName", Name}]).

-spec delete_domain/1 :: (string()) -> proplist().
delete_domain(Name) ->
    delete_domain(Name, default_config()).

-spec delete_domain/2 :: (string(), aws_config()) -> proplist().
delete_domain(Name, Config)
  when is_list(Name) ->
    sdb_simple_request(Config, "DeleteDomain", [{"DomainName", Name}]).

-spec domain_metadata/1 :: (string()) -> proplist().
domain_metadata(Name) ->
    domain_metadata(Name, default_config()).

-spec domain_metadata/2 :: (string(), aws_config()) -> proplist().
domain_metadata(Name, Config)
  when is_list(Name) ->
    {Doc, Result} = sdb_request(Config, "DomainMetadata", [{"DomainName", Name}]),
    MR = hd(xmerl_xpath:string("/DomainMetadataResponse/DomainMetadataResult", Doc)),
    Metadata = erlcloud_xml:decode([
        {item_count, "ItemCount", integer},
        {item_names_size_bytes, "ItemNamesSizeBytes", integer},
        {attribute_name_count, "AttributeNameCount", integer},
        {attribute_names_size_bytes, "AttributeNamesSizeBytes", integer},
        {attribute_value_count, "AttributeValueCount", integer},
        {attribute_values_size_bytes, "AttributeValuesSizeBytes", integer},
        {timestamp, "Timestamp", integer}
    ], MR),
    [{domain_metadata, Metadata}|Result].

-spec batch_put_attributes/2 :: (string(), [{string(), sdb_attributes()}]) -> proplist().
batch_put_attributes(DomainName, Items) ->
    batch_put_attributes(DomainName, Items, default_config()).

-spec batch_put_attributes/3 :: (string(), [{string(), sdb_attributes()}], aws_config()) -> proplist().
batch_put_attributes(DomainName, Items, Config)
  when is_list(DomainName), is_list(Items) ->
    ItemParams = [[{"ItemName", Name}|attributes_list(Attrs)] || {Name, Attrs} <- Items],
    sdb_simple_request(Config, "BatchPutAttributes",
        [{"DomainName", DomainName}|erlcloud_aws:param_list(ItemParams, "Item")]).

-spec delete_attributes/2 :: (string(), string()) -> proplist().
delete_attributes(DomainName, ItemName) ->
    delete_attributes(DomainName, ItemName, []).

-spec delete_attributes/3 :: (string(), string(), sdb_delete_attributes() | aws_config()) -> proplist().
delete_attributes(DomainName, ItemName, Config)
  when is_record(Config, aws_config) ->
    delete_attributes(DomainName, ItemName, [], Config);
delete_attributes(DomainName, ItemName, Attributes) ->
    delete_attributes(DomainName, ItemName, Attributes, []).

-spec delete_attributes/4 :: (string(), string(), sdb_delete_attributes(), sdb_conditionals() | aws_config()) -> proplist().
delete_attributes(DomainName, ItemName, Attributes, Config)
  when is_record(Config, aws_config) ->
    delete_attributes(DomainName, ItemName, Attributes, [], Config);
delete_attributes(DomainName, ItemName, Attributes, Conditionals) ->
    delete_attributes(DomainName, ItemName, Attributes, Conditionals, default_config()).

-spec delete_attributes/5 :: (string(), string(), sdb_delete_attributes(), sdb_conditionals(), aws_config()) -> proplist().
delete_attributes(DomainName, ItemName, Attributes, Conditionals, Config)
  when is_list(DomainName), is_list(ItemName), is_list(Attributes),
       is_list(Conditionals) ->
    Params = [{"DomainName", DomainName}, {"ItemName", ItemName}|
              delete_attributes_list(Attributes)] ++ conditionals_list(Conditionals),
    sdb_simple_request(Config, "DeleteAttributes", Params).

-spec get_attributes/2 :: (string(), string()) -> proplist().
get_attributes(DomainName, ItemName) ->
    get_attributes(DomainName, ItemName, []).

-spec get_attributes/3 :: (string(), string(), [string()] | boolean() | aws_config()) -> proplist().
get_attributes(DomainName, ItemName, Config)
  when is_record(Config, aws_config) ->
    get_attributes(DomainName, ItemName, [], Config);
get_attributes(DomainName, ItemName, ConsistentRead)
  when is_boolean(ConsistentRead) ->
    get_attributes(DomainName, ItemName, [], ConsistentRead);
get_attributes(DomainName, ItemName, AttributeNames) ->
    get_attributes(DomainName, ItemName, AttributeNames, false).

-spec get_attributes/4 :: (string(), string(), [string()], boolean() | aws_config()) -> proplist().
get_attributes(DomainName, ItemName, AttributeNames, Config)
  when is_record(Config, aws_config) ->
    get_attributes(DomainName, ItemName, AttributeNames, false, Config);
get_attributes(DomainName, ItemName, AttributeNames, ConsistentRead) ->
    get_attributes(DomainName, ItemName, AttributeNames, ConsistentRead,
                   default_config()).

-spec get_attributes/5 :: (string(), string(), [string()], boolean(), aws_config()) -> proplist().
get_attributes(DomainName, ItemName, AttributeNames, ConsistentRead, Config) ->
    {Doc, Result} = sdb_request(Config, "GetAttributes",
        [{"DomainName", DomainName}, {"ItemName", ItemName},
         {"ConsistentRead", ConsistentRead}|
         erlcloud_aws:param_list(AttributeNames, "AttributeName")]),
    [{attributes,
      extract_attributes(xmerl_xpath:string("/GetAttributesResponse/GetAttributesResult/Attribute", Doc))}|
     Result].

extract_attributes(Attributes) ->
    [extract_attribute(Attr) || Attr <- Attributes].

extract_attribute(Node) ->
    {erlcloud_xml:get_text("Name", Node), erlcloud_xml:get_text("Value", Node)}.

-spec list_domains/0 :: () -> proplist().
list_domains() ->
    list_domains(default_config()).

-spec list_domains/1 :: (string() | 1..100 | none | aws_config()) -> proplist().
list_domains(Config) when is_record(Config, aws_config) ->
    list_domains("", Config);
list_domains(MaxDomains) when is_integer(MaxDomains); MaxDomains =:= none ->
    list_domains("", MaxDomains);
list_domains(FirstToken) ->
    list_domains(FirstToken, none).

-spec list_domains/2 :: (string(), 1..100 | none | aws_config()) -> proplist().
list_domains(FirstToken, Config) when is_record(Config, aws_config) ->
    list_domains(FirstToken, none, Config);
list_domains(FirstToken, MaxDomains) ->
    list_domains(FirstToken, MaxDomains, default_config()).

-spec list_domains/3 :: (string(), 1..100 | none, aws_config()) -> proplist().
list_domains(FirstToken, MaxDomains, Config)
  when is_list(FirstToken),
       is_integer(MaxDomains) orelse MaxDomains =:= none ->
    {Doc, Result} = sdb_request(Config, "ListDomains",
        [{"MaxNumberOfDomains", MaxDomains}, {"FirstToken", FirstToken}]),
    [{domains, erlcloud_xml:get_list("/ListDomainsResponse/ListDomainsResult/DomainName", Doc)}|Result].

-spec put_attributes/3 :: (string(), string(), sdb_attributes()) -> proplist().
put_attributes(DomainName, ItemName, Attributes) ->
    put_attributes(DomainName, ItemName, Attributes, []).

-spec put_attributes/4 :: (string(), string(), sdb_attributes(), sdb_conditionals() | aws_config()) -> proplist().
put_attributes(DomainName, ItemName, Attributes, Config)
  when is_record(Config, aws_config) ->
    put_attributes(DomainName, ItemName, Attributes, [], Config);
put_attributes(DomainName, ItemName, Attributes, Conditionals) ->
    put_attributes(DomainName, ItemName, Attributes, Conditionals, default_config()).

-spec put_attributes/5 :: (string(), string(), sdb_attributes(), sdb_conditionals(), aws_config()) -> proplist().
put_attributes(DomainName, ItemName, Attributes, Conditionals, Config)
  when is_list(DomainName), is_list(ItemName), is_list(Attributes),
       is_list(Conditionals) ->
    Params = [{"DomainName", DomainName}, {"ItemName", ItemName}|
              attributes_list(Attributes)] ++ conditionals_list(Conditionals),
    sdb_simple_request(Config, "PutAttributes", Params).

-spec select/1 :: (string()) -> proplist().
select(SelectExpression) -> select(SelectExpression, none).

-spec select/2 :: (string(), string() | none | boolean() | aws_config()) -> proplist().
select(SelectExpression, Config)
  when is_record(Config, aws_config) ->
    select(SelectExpression, none, Config);
select(SelectExpression, ConsistentRead)
  when is_boolean(ConsistentRead) ->
    select(SelectExpression, none, ConsistentRead);
select(SelectExpression, NextToken) ->
    select(SelectExpression, NextToken, false).

-spec select/3 :: (string(), string() | none, boolean() | aws_config()) -> proplist().
select(SelectExpression, NextToken, Config)
  when is_record(Config, aws_config) ->
    select(SelectExpression, NextToken, false, Config);
select(SelectExpression, NextToken, ConsistentRead) ->
    select(SelectExpression, NextToken, ConsistentRead, default_config()).

-spec select/4 :: (string(), string() | none, boolean(), aws_config()) -> proplist().
select(SelectExpression, NextToken, ConsistentRead, Config)
  when is_list(SelectExpression),
       is_list(NextToken) orelse NextToken =:= none,
       is_boolean(ConsistentRead) ->
    {Doc, Result} = sdb_request(Config, "Select",
        [{"SelectExpression", SelectExpression}, {"NextToken", NextToken},
         {"ConsistentRead", ConsistentRead}]),
    [{items, extract_items(xmerl_xpath:string("/SelectResponse/SelectResult/Item", Doc))}|
     Result].

extract_items(Items) ->
    [extract_item(Item) || Item <- Items].

extract_item(Item) ->
    [
        {name, erlcloud_xml:get_text("Name", Item)},
        {attributes, extract_attributes(xmerl_xpath:string("Attribute", Item))}
    ].

sdb_request(Config, Action, Params) ->
    QParams = [{"Action", Action}, {"Version", ?API_VERSION}|Params],
    Doc = erlcloud_aws:aws_request_xml(post, Config#aws_config.sdb_host,
        "/", QParams, Config),
    {Doc, [{box_usage, erlcloud_xml:get_float("/*/ResponseMetadata/BoxUsage", Doc)}]}.

sdb_simple_request(Config, Action, Params) ->
    {_Doc, Result} = sdb_request(Config, Action, Params),
    Result.

delete_attributes_list(Attrs) ->
    erlcloud_aws:param_list(
        [case Attr of
             {Name, Value} -> [{"Name", Name}, {"Value", Value}];
             Name when is_list(Name) -> [{"Name", Name}]
         end || Attr <- Attrs],
        "Attribute").

attributes_list(Attrs) ->
    erlcloud_aws:param_list(
        [[{"Name", Name}|
          case Value of
              {replace, V} -> [{"Value", V}, {"Replace", true}];
              V -> [{"Value", V}]
          end] ||
         {Name, Value} <- Attrs],
        "Attribute").

conditionals_list(Conditionals) ->
    erlcloud_aws:param_list(
        [[{"Name", Name},
          case Value of
              exists -> {"Exists", "true"};
              not_exists -> {"Exists", "false"};
              V -> {"Value", V}
          end] ||
         {Name, Value} <- Conditionals],
        "Expected").
