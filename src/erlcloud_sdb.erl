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
         select/1, select/2, select/3, select/4,
         select_all/1, select_all/2, select_all/3
        ]).

-ifdef(TEST).
-export([extract_token/1]).
-endif.

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").

-define(API_VERSION, "2009-04-15").
-define(SDB_TIMEOUT, 10000).

-spec new(string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey}.

-spec new(string(), string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                sdb_host=Host}.

-spec configure(string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey) ->
    erlcloud_config:configure(AccessKeyID, SecretAccessKey, fun new/2).

-spec configure(string(), string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host) ->
    erlcloud_config:configure(AccessKeyID, SecretAccessKey, Host, fun new/3).

default_config() -> erlcloud_aws:default_config().

-spec create_domain(string()) -> proplist() | no_return().
create_domain(Name) ->
    create_domain(Name, default_config()).

-spec create_domain(string(), aws_config()) -> proplist() | no_return().
create_domain(Name, Config)
  when is_list(Name) ->
    sdb_simple_request(Config, "CreateDomain", [{"DomainName", Name}]).

-spec delete_domain(string()) -> proplist() | no_return().
delete_domain(Name) ->
    delete_domain(Name, default_config()).

-spec delete_domain(string(), aws_config()) -> proplist() | no_return().
delete_domain(Name, Config)
  when is_list(Name) ->
    sdb_simple_request(Config, "DeleteDomain", [{"DomainName", Name}]).

-spec domain_metadata(string()) -> proplist() | no_return().
domain_metadata(Name) ->
    domain_metadata(Name, default_config()).

-spec domain_metadata(string(), aws_config()) -> proplist() | no_return().
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

-spec batch_put_attributes(string(), [{string(), sdb_attributes()}]) -> proplist() | no_return().
batch_put_attributes(DomainName, Items) ->
    batch_put_attributes(DomainName, Items, default_config()).

-spec batch_put_attributes(string(), [{string(), sdb_attributes()}], aws_config()) -> proplist() | no_return().
batch_put_attributes(DomainName, Items, Config)
  when is_list(DomainName), is_list(Items) ->
    ItemParams = [[{"ItemName", Name}|attributes_list(Attrs)] || {Name, Attrs} <- Items],
    sdb_simple_request(Config, "BatchPutAttributes",
                       [{"DomainName", DomainName}|erlcloud_aws:param_list(ItemParams, "Item")]).

-spec delete_attributes(string(), string()) -> proplist() | no_return().
delete_attributes(DomainName, ItemName) ->
    delete_attributes(DomainName, ItemName, []).

-spec delete_attributes(string(), string(), sdb_delete_attributes() | aws_config()) -> proplist() | no_return().
delete_attributes(DomainName, ItemName, Config)
  when is_record(Config, aws_config) ->
    delete_attributes(DomainName, ItemName, [], Config);
delete_attributes(DomainName, ItemName, Attributes) ->
    delete_attributes(DomainName, ItemName, Attributes, []).

-spec delete_attributes(string(), string(), sdb_delete_attributes(), sdb_conditionals() | aws_config()) -> proplist() | no_return().
delete_attributes(DomainName, ItemName, Attributes, Config)
  when is_record(Config, aws_config) ->
    delete_attributes(DomainName, ItemName, Attributes, [], Config);
delete_attributes(DomainName, ItemName, Attributes, Conditionals) ->
    delete_attributes(DomainName, ItemName, Attributes, Conditionals, default_config()).

-spec delete_attributes(string(), string(), sdb_delete_attributes(), sdb_conditionals(), aws_config()) -> proplist() | no_return().
delete_attributes(DomainName, ItemName, Attributes, Conditionals, Config)
  when is_list(DomainName), is_list(ItemName), is_list(Attributes),
       is_list(Conditionals) ->
    Params = [{"DomainName", DomainName}, {"ItemName", ItemName}|
              delete_attributes_list(Attributes)] ++ conditionals_list(Conditionals),
    sdb_simple_request(Config, "DeleteAttributes", Params).

-spec get_attributes(string(), string()) -> proplist() | no_return().
get_attributes(DomainName, ItemName) ->
    get_attributes(DomainName, ItemName, []).

-spec get_attributes(string(), string(), [string()] | boolean() | aws_config()) -> proplist() | no_return().
get_attributes(DomainName, ItemName, Config)
  when is_record(Config, aws_config) ->
    get_attributes(DomainName, ItemName, [], Config);
get_attributes(DomainName, ItemName, ConsistentRead)
  when is_boolean(ConsistentRead) ->
    get_attributes(DomainName, ItemName, [], ConsistentRead);
get_attributes(DomainName, ItemName, AttributeNames) ->
    get_attributes(DomainName, ItemName, AttributeNames, false).

-spec get_attributes(string(), string(), [string()], boolean() | aws_config()) -> proplist() | no_return().
get_attributes(DomainName, ItemName, AttributeNames, Config)
  when is_record(Config, aws_config) ->
    get_attributes(DomainName, ItemName, AttributeNames, false, Config);
get_attributes(DomainName, ItemName, AttributeNames, ConsistentRead) ->
    get_attributes(DomainName, ItemName, AttributeNames, ConsistentRead,
                   default_config()).

-spec get_attributes(string(), string(), [string()], boolean(), aws_config()) -> proplist() | no_return().
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

-spec list_domains() -> proplist() | no_return().
list_domains() ->
    list_domains(default_config()).

-spec list_domains(string() | 1..100 | none | aws_config()) -> proplist() | no_return().
list_domains(Config) when is_record(Config, aws_config) ->
    list_domains("", Config);
list_domains(MaxDomains) when is_integer(MaxDomains); MaxDomains =:= none ->
    list_domains("", MaxDomains);
list_domains(FirstToken) ->
    list_domains(FirstToken, none).

-spec list_domains(string(), 1..100 | none | aws_config()) -> proplist() | no_return().
list_domains(FirstToken, Config) when is_record(Config, aws_config) ->
    list_domains(FirstToken, none, Config);
list_domains(FirstToken, MaxDomains) ->
    list_domains(FirstToken, MaxDomains, default_config()).

maybe_add_maxdomains(none, Params) ->
    Params;
maybe_add_maxdomains(MaxDomains, Params) ->
    [{"MaxNumberOfDomains", MaxDomains} | Params].

maybe_add_nexttoken([], Params) ->
    Params;
maybe_add_nexttoken(Token, Params) ->
    [{"NextToken", Token} | Params].

-spec list_domains(string(), 1..100 | none, aws_config()) -> proplist() | no_return().
list_domains(FirstToken, MaxDomains, Config)
  when is_list(FirstToken),
       is_integer(MaxDomains) orelse MaxDomains =:= none ->

    Params = 
    maybe_add_nexttoken(FirstToken, 
    maybe_add_maxdomains(MaxDomains, [])),

    {Doc, Result} = sdb_request(Config, "ListDomains", Params),

    [{domains, erlcloud_xml:get_list("/ListDomainsResponse/ListDomainsResult/DomainName", Doc)},
     {next_token, erlcloud_xml:get_text("/ListDomainsResponse/ListDomainsResult/NextToken", Doc)}|Result].

-spec put_attributes(string(), string(), sdb_attributes()) -> proplist() | no_return().
put_attributes(DomainName, ItemName, Attributes) ->
    put_attributes(DomainName, ItemName, Attributes, []).

-spec put_attributes(string(), string(), sdb_attributes(), sdb_conditionals() | aws_config()) -> proplist() | no_return().
put_attributes(DomainName, ItemName, Attributes, Config)
  when is_record(Config, aws_config) ->
    put_attributes(DomainName, ItemName, Attributes, [], Config);
put_attributes(DomainName, ItemName, Attributes, Conditionals) ->
    put_attributes(DomainName, ItemName, Attributes, Conditionals, default_config()).

-spec put_attributes(string(), string(), sdb_attributes(), sdb_conditionals(), aws_config()) -> proplist() | no_return().
put_attributes(DomainName, ItemName, Attributes, Conditionals, Config)
  when is_list(DomainName), is_list(ItemName), is_list(Attributes),
       is_list(Conditionals) ->
    Params = [{"DomainName", DomainName}, {"ItemName", ItemName}|
              attributes_list(Attributes)] ++ conditionals_list(Conditionals),
    sdb_simple_request(Config, "PutAttributes", Params).

%% These functions will return the first page of results along with
%% a token to retrieve the next page, if any.

-spec select(string()) -> proplist() | no_return().
select(SelectExpression) -> select(SelectExpression, none).

-spec select(string(), string() | none | boolean() | aws_config()) -> proplist() | no_return().
select(SelectExpression, Config)
  when is_record(Config, aws_config) ->
    select(SelectExpression, none, Config);
select(SelectExpression, ConsistentRead)
  when is_boolean(ConsistentRead) ->
    select(SelectExpression, none, ConsistentRead);
select(SelectExpression, NextToken) ->
    select(SelectExpression, NextToken, false).

-spec select(string(), string() | none, boolean() | aws_config()) -> proplist() | no_return().
select(SelectExpression, NextToken, Config)
  when is_record(Config, aws_config) ->
    select(SelectExpression, NextToken, false, Config);
select(SelectExpression, NextToken, ConsistentRead) ->
    select(SelectExpression, NextToken, ConsistentRead, default_config()).

-spec select(string(), string() | none, boolean(), aws_config()) -> proplist() | no_return().
select(SelectExpression, NextToken, ConsistentRead, Config)
  when is_list(SelectExpression),
       is_list(NextToken) orelse NextToken =:= none,
       is_boolean(ConsistentRead) ->
    {Items, NewNextToken, Metadata} = sdb_select_request(SelectExpression,
                                                         NextToken,
                                                         ConsistentRead,
                                                         Config),
    Metadata2 = case NewNextToken of
                    done -> Metadata;
                    Token -> [{next_token, Token}|Metadata]
                end,
    [{items, Items}|Metadata2].

%% These functions will make multiple requests until all
%% pages of results have been consumed.

-spec select_all(string()) -> proplist() | no_return().
select_all(SelectExpression) ->
    select_all(SelectExpression, false).

-spec select_all(string(), boolean()) -> proplist() | no_return().
select_all(SelectExpression, ConsistentRead)
    when is_boolean(ConsistentRead) ->
    select_all(SelectExpression, ConsistentRead, default_config());
select_all(SelectExpression, Config) ->
    select_all(SelectExpression, false, Config).

-spec select_all(string(), boolean(), aws_config()) -> proplist() | no_return().
select_all(SelectExpression, ConsistentRead, Config)
  when is_list(SelectExpression),
       is_boolean(ConsistentRead) ->
    select_all(SelectExpression, none, ConsistentRead, Config, [], []).

-spec select_all(string(), string() | none | done, boolean(), aws_config(), proplist(), proplist()) -> proplist() | no_return().
select_all(_, done, _, _, Items, Metadata) ->
    [{items, Items}|Metadata];
select_all(SelectExpression, NextToken, ConsistentRead, Config, Items, Metadata) ->
    {NewItems, NewNextToken, NewMetadata} = sdb_select_request(SelectExpression,
                                                               NextToken,
                                                               ConsistentRead,
                                                               Config),
    select_all(SelectExpression, NewNextToken, ConsistentRead,
               Config, Items ++ NewItems, Metadata ++ NewMetadata).

sdb_select_request(SelectExpression, NextToken, ConsistentRead, Config) ->
    {Doc, Metadata} = sdb_request(Config, "Select",
                                  [{"SelectExpression", SelectExpression},
                                   {"NextToken", NextToken},
                                   {"ConsistentRead", ConsistentRead}]),
    NewNextToken = extract_token(Doc),
    Items = extract_items(xmerl_xpath:string("/SelectResponse/SelectResult/Item", Doc)),
    {Items, NewNextToken, Metadata}.

extract_token(Doc) ->
    case xmerl_xpath:string("/SelectResponse/SelectResult/NextToken", Doc) of
        [] ->
            done;
        [Token] ->
            erlcloud_xml:get_text(Token)
    end.

extract_items(Items) ->
    [extract_item(Item) || Item <- Items].

extract_item(Item) ->
    [
     {name, erlcloud_xml:get_text("Name", Item)},
     {attributes, extract_attributes(xmerl_xpath:string("Attribute", Item))}
    ].

sdb_request(Config, Action, Params) ->
    case sdb_request_with_retry(Config, Action, Params, 1, ?SDB_TIMEOUT, os:timestamp()) of
        {ok, {Doc, Metadata}} ->
            {Doc, Metadata};
        {error, Error} ->
            erlang:error({aws_error, Error})
    end.

sdb_request_with_retry(Config, Action, Params, Try, Timeout, StartTime) ->
    case sdb_request_safe(Config, Action, Params) of
        {ok, Doc, Metadata} ->
            {ok, {Doc, Metadata}};
        {error, {http_error, 503, _StatusLine, _Body}} ->
            %% Convert from microseconds to milliseconds
            Waited = timer:now_diff(os:timestamp(), StartTime) / 1000.0,
            case Waited of
                _TooLong when Waited > Timeout ->
                    {error, retry_timeout};
                _ ->
                    Wait = math:pow(2, Try) * 200.0,
                    %% Not exactly random since we're relying on the
                    %% calling process to hold our PRNG state.
                    FuzzWait = erlcloud_util:rand_uniform(round(Wait)),
                    timer:sleep(FuzzWait),
                    sdb_request_with_retry(Config, Action, Params,
                                           Try + 1, Timeout, StartTime)
            end;
        {error, _} = Error ->
            Error
    end.

sdb_request_safe(Config, Action, Params) ->
    QParams = [{"Action", Action}, {"Version", ?API_VERSION}|Params],
    case erlcloud_aws:aws_request_xml2(post, Config#aws_config.sdb_host,
                                       "/", QParams, Config) of
        {ok, Doc} ->
            {ok, Doc, [{box_usage, erlcloud_xml:get_float("/*/ResponseMetadata/BoxUsage", Doc)}]};
        {error, Error} ->
            {error, Error}
    end.

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
