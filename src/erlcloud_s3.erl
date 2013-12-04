%% Amazon Simple Storage Service (S3)

-module(erlcloud_s3).
-export([new/2, new/3, new/4, configure/2, configure/3, configure/4,
         create_bucket/1, create_bucket/2, create_bucket/3,
         delete_bucket/1, delete_bucket/2,
         get_bucket_attribute/2, get_bucket_attribute/3,
         list_buckets/0, list_buckets/1,
         set_bucket_attribute/3, set_bucket_attribute/4,
         list_objects/1, list_objects/2, list_objects/3,
         list_object_versions/1, list_object_versions/2, list_object_versions/3,
         copy_object/4, copy_object/5, copy_object/6,
         delete_object/2, delete_object/3,
         delete_object_version/3, delete_object_version/4,
         get_object/2, get_object/3, get_object/4,
         get_object_acl/2, get_object_acl/3, get_object_acl/4,
         get_object_torrent/2, get_object_torrent/3,
         get_object_metadata/2, get_object_metadata/3, get_object_metadata/4,
         put_object/3, put_object/4, put_object/5, put_object/6,
         set_object_acl/3, set_object_acl/4,
         make_link/3, make_link/4,
         make_get_url/3, make_get_url/4,
         start_multipart/2, start_multipart/5,
         upload_part/5, upload_part/7,
         complete_multipart/4, complete_multipart/6,
         abort_multipart/3, abort_multipart/6,
         list_multipart_uploads/1, list_multipart_uploads/2
        ]).

-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include_lib("xmerl/include/xmerl.hrl").


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
       s3_host=Host
      }.


-spec new(string(), string(), string(), non_neg_integer()) -> aws_config().

new(AccessKeyID, SecretAccessKey, Host, Port) ->
    #aws_config{
       access_key_id=AccessKeyID,
       secret_access_key=SecretAccessKey,
       s3_host=Host,
       s3_port=Port
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

-type s3_bucket_attribute_name() :: acl
                                  | location
                                  | logging
                                  | request_payment
                                  | versioning.

-type s3_bucket_acl() :: private
                       | public_read
                       | public_read_write
                       | authenticated_read
                       | bucket_owner_read
                       | bucket_owner_full_control.

-type s3_location_constraint() :: none
                                | us_west_1
                                | eu.

-define(XMLNS_S3, "http://s3.amazonaws.com/doc/2006-03-01/").

-spec copy_object(string(), string(), string(), string()) -> proplist().

copy_object(DestBucketName, DestKeyName, SrcBucketName, SrcKeyName) ->
    copy_object(DestBucketName, DestKeyName, SrcBucketName, SrcKeyName, []).

-spec copy_object(string(), string(), string(), string(), proplist() | aws_config()) -> proplist().

copy_object(DestBucketName, DestKeyName, SrcBucketName, SrcKeyName, Config)
  when is_record(Config, aws_config) ->
    copy_object(DestBucketName, DestKeyName, SrcBucketName, SrcKeyName, [], Config);

copy_object(DestBucketName, DestKeyName, SrcBucketName, SrcKeyName, Options) ->
    copy_object(DestBucketName, DestKeyName, SrcBucketName, SrcKeyName,
                Options, default_config()).

-spec copy_object(string(), string(), string(), string(), proplist(), aws_config()) -> proplist().
copy_object(DestBucketName, DestKeyName, SrcBucketName, SrcKeyName, Options, Config) ->
    SrcVersion = case proplists:get_value(version_id, Options) of
                     undefined -> "";
                     VersionID -> ["?versionId=", VersionID]
                 end,
    RequestHeaders =
        [{"x-amz-copy-source", [SrcBucketName, $/, SrcKeyName, SrcVersion]},
         {"x-amz-metadata-directive", proplists:get_value(metadata_directive, Options)},
         {"x-amz-copy-source-if-match", proplists:get_value(if_match, Options)},
         {"x-amz-copy-source-if-none-match", proplists:get_value(if_none_match, Options)},
         {"x-amz-copy-source-if-unmodified-since", proplists:get_value(if_unmodified_since, Options)},
         {"x-amz-copy-source-if-modified-since", proplists:get_value(if_modified_since, Options)},
         {"x-amz-acl", encode_acl(proplists:get_value(acl, Options))}],
    {Headers, _Body} = s3_request(Config, put, DestBucketName, [$/|DestKeyName],
                                  "", [], <<>>, RequestHeaders),
    [{copy_source_version_id, proplists:get_value("x-amz-copy-source-version-id", Headers, "false")},
     {version_id, proplists:get_value("x-amz-version-id", Headers, "null")}].

-spec create_bucket(string()) -> ok.

create_bucket(BucketName) ->
    create_bucket(BucketName, private).

-spec create_bucket(string(), s3_bucket_acl() | aws_config()) -> ok.

create_bucket(BucketName, Config)
  when is_record(Config, aws_config) ->
    create_bucket(BucketName, private, Config);

create_bucket(BucketName, ACL) ->
    create_bucket(BucketName, ACL, none).

-spec create_bucket(string(), s3_bucket_acl(), s3_location_constraint() | aws_config()) -> ok.

create_bucket(BucketName, ACL, Config)
  when is_record(Config, aws_config) ->
    create_bucket(BucketName, ACL, none, Config);

create_bucket(BucketName, ACL, LocationConstraint) ->
    create_bucket(BucketName, ACL, LocationConstraint, default_config()).

-spec create_bucket(string(), s3_bucket_acl(), s3_location_constraint(), aws_config()) -> ok.

create_bucket(BucketName, ACL, LocationConstraint, Config)
  when is_list(BucketName), is_atom(ACL), is_atom(LocationConstraint) ->
    Headers = case ACL of
                  private -> [];  %% private is the default
                  _       -> [{"x-amz-acl", encode_acl(ACL)}]
              end,
    POSTData = case LocationConstraint of
                   none -> <<>>;
                   Location when Location =:= eu; Location =:= us_west_1 ->
                       LocationName = case Location of eu -> "EU"; us_west_1 -> "us-west-1" end,
                       XML = {'CreateBucketConfiguration', [{xmlns, ?XMLNS_S3}],
                              [{'LocationConstraint', [LocationName]}]},
                       list_to_binary(xmerl:export_simple([XML], xmerl_xml))
               end,
    s3_simple_request(Config, put, BucketName, "/", "", [], POSTData, Headers).

encode_acl(undefined)                 -> undefined;
encode_acl(private)                   -> "private";
encode_acl(public_read)               -> "public-read";
encode_acl(public_read_write)         -> "public-read-write";
encode_acl(authenticated_read)        -> "authenticated-read";
encode_acl(bucket_owner_read)         -> "bucket-owner-read";
encode_acl(bucket_owner_full_control) -> "bucket-owner-full-control".

-spec delete_bucket(string()) -> ok.

delete_bucket(BucketName) ->
    delete_bucket(BucketName, default_config()).

-spec delete_bucket(string(), aws_config()) -> ok.

delete_bucket(BucketName, Config)
  when is_list(BucketName) ->
    s3_simple_request(Config, delete, BucketName, "/", "", [], <<>>, []).

-spec delete_object(string(), string()) -> proplist().

delete_object(BucketName, Key) ->
    delete_object(BucketName, Key, default_config()).

-spec delete_object(string(), string(), aws_config()) -> proplist().

delete_object(BucketName, Key, Config)
  when is_list(BucketName), is_list(Key) ->
    {Headers, _Body} = s3_request(Config, delete, BucketName, [$/|Key], "", [], <<>>, []),
    Marker = proplists:get_value("x-amz-delete-marker", Headers, "false"),
    Id = proplists:get_value("x-amz-version-id", Headers, "null"),
    [{delete_marker, list_to_existing_atom(Marker)},
     {version_id, Id}].

-spec delete_object_version(string(), string(), string()) -> proplist().

delete_object_version(BucketName, Key, Version) ->
    delete_object_version(BucketName, Key, Version, default_config()).

-spec delete_object_version(string(), string(), string(), aws_config()) -> proplist().

delete_object_version(BucketName, Key, Version, Config)
  when is_list(BucketName),
       is_list(Key),
       is_list(Version)->
    {Headers, _Body} = s3_request(Config, delete, BucketName, [$/|Key],
                                  ["versionId=", Version], [], <<>>, []),
    Marker = proplists:get_value("x-amz-delete-marker", Headers, "false"),
    Id = proplists:get_value("x-amz-version-id", Headers, "null"),
    [{delete_marker, list_to_existing_atom(Marker)},
     {version_id, Id}].

-spec list_buckets() -> proplist().

list_buckets() ->
    list_buckets(default_config()).

-spec list_buckets(aws_config()) -> proplist().

list_buckets(Config) ->
    Doc = s3_xml_request(Config, get, "", "/", "", [], <<>>, []),
    Buckets = [extract_bucket(Node) || Node <- xmerl_xpath:string("/*/Buckets/Bucket", Doc)],
    [{buckets, Buckets}].

-spec list_objects(string()) -> proplist().

list_objects(BucketName) ->
    list_objects(BucketName, []).

-spec list_objects(string(), proplist() | aws_config()) -> proplist().

list_objects(BucketName, Config)
  when is_record(Config, aws_config) ->
    list_objects(BucketName, [], Config);

list_objects(BucketName, Options) ->
    list_objects(BucketName, Options, default_config()).

-spec list_objects(string(), proplist(), aws_config()) -> proplist().

list_objects(BucketName, Options, Config)
  when is_list(BucketName),
       is_list(Options) ->
    Params = [{"delimiter", proplists:get_value(delimiter, Options)},
              {"marker", proplists:get_value(marker, Options)},
              {"max-keys", proplists:get_value(max_keys, Options)},
              {"prefix", proplists:get_value(prefix, Options)}],
    Doc = s3_xml_request(Config, get, BucketName, "/", "", Params, <<>>, []),
    Attributes = [{name, "Name", text},
                  {prefix, "Prefix", text},
                  {marker, "Marker", text},
                  {delimiter, "Delimiter", text},
                  {max_keys, "MaxKeys", integer},
                  {is_truncated, "IsTruncated", boolean},
                  {contents, "Contents", fun extract_contents/1}],
    erlcloud_xml:decode(Attributes, Doc).

extract_contents(Nodes) ->
    Attributes = [{key, "Key", text},
                  {last_modified, "LastModified", time},
                  {etag, "ETag", text},
                  {size, "Size", integer},
                  {storage_class, "StorageClass", text},
                  {owner, "Owner", fun extract_user/1}],
    [erlcloud_xml:decode(Attributes, Node) || Node <- Nodes].

extract_user([]) ->
    [];
extract_user([Node]) ->
    Attributes = [{id, "ID", text},
                  {display_name, "DisplayName", optional_text}],
    erlcloud_xml:decode(Attributes, Node).

-spec get_bucket_attribute(string(), s3_bucket_attribute_name()) -> term().

get_bucket_attribute(BucketName, AttributeName) ->
    get_bucket_attribute(BucketName, AttributeName, default_config()).

-spec get_bucket_attribute(string(), s3_bucket_attribute_name(), aws_config()) -> term().

get_bucket_attribute(BucketName, AttributeName, Config)
  when is_list(BucketName), is_atom(AttributeName) ->
    Attr = case AttributeName of
               acl             -> "acl";
               location        -> "location";
               logging         -> "logging";
               request_payment -> "requestPayment";
               versioning      -> "versioning"
           end,
    Doc = s3_xml_request(Config, get, BucketName, "/", Attr, [], <<>>, []),
    case AttributeName of
        acl ->
            Attributes = [{owner, "Owner", fun extract_user/1},
                          {access_control_list, "AccessControlList/Grant", fun extract_acl/1}],
            erlcloud_xml:decode(Attributes, Doc);
        location ->
            erlcloud_xml:get_text("/LocationConstraint", Doc);
        logging ->
            case xmerl_xpath:string("/BucketLoggingStatus/LoggingEnabled", Doc) of
                [] ->
                    {enabled, false};
                [LoggingEnabled] ->
                    Attributes = [{target_bucket, "TargetBucket", text},
                                  {target_prefix, "TargetPrefix", text},
                                  {target_trants, "TargetGrants/Grant", fun extract_acl/1}],
                    [{enabled, true}|erlcloud_xml:decode(Attributes, LoggingEnabled)]
            end;
        request_payment ->
            case erlcloud_xml:get_text("/RequestPaymentConfiguration/Payer", Doc) of
                "Requester" -> requester;
                _           -> bucket_owner
            end;
        versioning ->
            case erlcloud_xml:get_text("/VersioningConfiguration/Status", Doc) of
                "Enabled"   -> enabled;
                "Suspended" -> suspended;
                _           -> disabled
            end
    end.

extract_acl(ACL) ->
    [extract_grant(Item) || Item <- ACL].

extract_grant(Node) ->
    [{grantee, extract_user(xmerl_xpath:string("Grantee", Node))},
     {permission, decode_permission(erlcloud_xml:get_text("Permission", Node))}].

encode_permission(full_control) -> "FULL_CONTROL";
encode_permission(write)        -> "WRITE";
encode_permission(write_acp)    -> "WRITE_ACP";
encode_permission(read)         -> "READ";
encode_permission(read_acp) -> "READ_ACP".

decode_permission("FULL_CONTROL") -> full_control;
decode_permission("WRITE")        -> write;
decode_permission("WRITE_ACP")    -> write_acp;
decode_permission("READ")         -> read;
decode_permission("READ_ACP")     -> read_acp.

-spec get_object(string(), string()) -> proplist().

get_object(BucketName, Key) ->
    get_object(BucketName, Key, []).

-spec get_object(string(), string(), proplist() | aws_config()) -> proplist().

get_object(BucketName, Key, Config)
  when is_record(Config, aws_config) ->
    get_object(BucketName, Key, [], Config);

get_object(BucketName, Key, Options) ->
    get_object(BucketName, Key, Options, default_config()).

-spec get_object(string(), string(), proplist(), aws_config()) -> proplist().

get_object(BucketName, Key, Options, Config) ->
    RequestHeaders = [{"Range", proplists:get_value(range, Options)},
                      {"If-Modified-Since", proplists:get_value(if_modified_since, Options)},
                      {"If-Unmodified-Since", proplists:get_value(if_unmodified_since, Options)},
                      {"If-Match", proplists:get_value(if_match, Options)},
                      {"If-None-Match", proplists:get_value(if_none_match, Options)}],
    Subresource = case proplists:get_value(version_id, Options) of
                      undefined -> "";
                      Version   -> ["versionId=", Version]
                  end,
    {Headers, Body} = s3_request(Config, get, BucketName, [$/|Key], Subresource, [], <<>>, RequestHeaders),
    [{etag, proplists:get_value("etag", Headers)},
     {content_length, proplists:get_value("content-length", Headers)},
     {content_type, proplists:get_value("content-type", Headers)},
     {delete_marker, list_to_existing_atom(proplists:get_value("x-amz-delete-marker", Headers, "false"))},
     {version_id, proplists:get_value("x-amz-version-id", Headers, "null")},
     {content, list_to_binary(Body)}|
     extract_metadata(Headers)].

-spec get_object_acl(string(), string()) -> proplist().

get_object_acl(BucketName, Key) ->
    get_object_acl(BucketName, Key, default_config()).

-spec get_object_acl(string(), string(), proplist() | aws_config()) -> proplist().

get_object_acl(BucketName, Key, Config)
  when is_record(Config, aws_config) ->
    get_object_acl(BucketName, Key, [], Config);

get_object_acl(BucketName, Key, Options) ->
    get_object_acl(BucketName, Key, Options, default_config()).

-spec get_object_acl(string(), string(), proplist(), aws_config()) -> proplist().

get_object_acl(BucketName, Key, Options, Config)
  when is_list(BucketName), is_list(Key), is_list(Options) ->
    Subresource = case proplists:get_value(version_id, Options) of
                      undefined -> "";
                      Version   -> ["&versionId=", Version]
                  end,
    Doc = s3_xml_request(Config, get, BucketName, [$/|Key], "acl" ++ Subresource, [], <<>>, []),
    Attributes = [{owner, "Owner", fun extract_user/1},
                  {access_control_list, "AccessControlList/Grant", fun extract_acl/1}],
    erlcloud_xml:decode(Attributes, Doc).

-spec get_object_metadata(string(), string()) -> proplist().

get_object_metadata(BucketName, Key) ->
    get_object_metadata(BucketName, Key, []).

-spec get_object_metadata(string(), string(), proplist() | aws_config()) -> proplist().

get_object_metadata(BucketName, Key, Config)
  when is_record(Config, aws_config) ->
    get_object_metadata(BucketName, Key, [], Config);

get_object_metadata(BucketName, Key, Options) ->
    get_object_metadata(BucketName, Key, Options, default_config()).

-spec get_object_metadata(string(), string(), proplist(), proplist() | aws_config()) -> proplist().

get_object_metadata(BucketName, Key, Options, Config) ->
    RequestHeaders = [{"If-Modified-Since", proplists:get_value(if_modified_since, Options)},
                      {"If-Unmodified-Since", proplists:get_value(if_unmodified_since, Options)},
                      {"If-Match", proplists:get_value(if_match, Options)},
                      {"If-None-Match", proplists:get_value(if_none_match, Options)}],
    Subresource = case proplists:get_value(version_id, Options) of
                      undefined -> "";
                      Version   -> ["versionId=", Version]
                  end,
    {Headers, _Body} = s3_request(Config, head, BucketName, [$/|Key], Subresource, [], <<>>, RequestHeaders),
    [{last_modified, proplists:get_value("last-modified", Headers)},
     {etag, proplists:get_value("etag", Headers)},
     {content_length, proplists:get_value("content-length", Headers)},
     {content_type, proplists:get_value("content-type", Headers)},
     {delete_marker, list_to_existing_atom(proplists:get_value("x-amz-delete-marker", Headers, "false"))},
     {version_id, proplists:get_value("x-amz-version-id", Headers, "false")}|extract_metadata(Headers)].

extract_metadata(Headers) ->
    [{Key, Value} || {Key = "x-amz-meta-" ++ _, Value} <- Headers].

-spec get_object_torrent(string(), string()) -> proplist().

get_object_torrent(BucketName, Key) ->
    get_object_torrent(BucketName, Key, default_config()).

-spec get_object_torrent(string(), string(), aws_config()) -> proplist().

get_object_torrent(BucketName, Key, Config) ->
    {Headers, Body} = s3_request(Config, get, BucketName, [$/|Key], "torrent", [], <<>>, []),
    [{delete_marker, list_to_existing_atom(proplists:get_value("x-amz-delete-marker", Headers, "false"))},
     {version_id, proplists:get_value("x-amz-delete-marker", Headers, "false")},
     {torrent, list_to_binary(Body)}].

-spec list_object_versions(string()) -> proplist().

list_object_versions(BucketName) ->
    list_object_versions(BucketName, []).

-spec list_object_versions(string(), proplist() | aws_config()) -> proplist().

list_object_versions(BucketName, Config)
  when is_record(Config, aws_config) ->
    list_object_versions(BucketName, [], Config);

list_object_versions(BucketName, Options) ->
    list_object_versions(BucketName, Options, default_config()).

-spec list_object_versions(string(), proplist(), aws_config()) -> proplist().

list_object_versions(BucketName, Options, Config)
  when is_list(BucketName), is_list(Options) ->
    Params = [{"delimiter", proplists:get_value(delimiter, Options)},
              {"key-marker", proplists:get_value(key_marker, Options)},
              {"max-keys", proplists:get_value(max_keys, Options)},
              {"prefix", proplists:get_value(prefix, Options)},
              {"version-id-marker", proplists:get_value(version_id_marker, Options)}],
    Doc = s3_xml_request(Config, get, BucketName, "/", "versions", Params, <<>>, []),
    Attributes = [{name, "Name", text},
                  {prefix, "Prefix", text},
                  {key_marker, "KeyMarker", text},
                  {next_key_marker, "NextKeyMarker", optional_text},
                  {version_id_marker, "VersionIdMarker", text},
                  {next_version_id_marker, "NextVersionIdMarker", optional_text},
                  {max_keys, "MaxKeys", integer},
                  {is_truncated, "Istruncated", boolean},
                  {versions, "Version", fun extract_versions/1},
                  {delete_markers, "DeleteMarker", fun extract_delete_markers/1}],
    erlcloud_xml:decode(Attributes, Doc).

extract_versions(Nodes) ->
    [extract_version(Node) || Node <- Nodes].

extract_version(Node) ->
    Attributes = [{key, "Key", text},
                  {version_id, "VersionId", text},
                  {is_latest, "IsLatest", boolean},
                  {etag, "ETag", text},
                  {size, "Size", integer},
                  {owner, "Owner", fun extract_user/1},
                  {storage_class, "StorageClass", text},
                  {last_modified, "LastModified", time}],
    erlcloud_xml:decode(Attributes, Node).

extract_delete_markers(Nodes) ->
    [extract_delete_marker(Node) || Node <- Nodes].

extract_delete_marker(Node) ->
    Attributes = [{key, "Key", text},
                  {version_id, "VersionId", text},
                  {is_latest, "IsLatest", boolean},
                  {owner, "Owner", fun extract_user/1}],
    erlcloud_xml:decode(Attributes, Node).

extract_bucket(Node) ->
    erlcloud_xml:decode([{name, "Name", text},
                         {creation_date, "CreationDate", time}],
                        Node).

-spec put_object(string(), string(), iolist()) -> proplist().

put_object(BucketName, Key, Value) ->
    put_object(BucketName, Key, Value, []).

-spec put_object(string(), string(), iolist(), proplist() | aws_config()) -> proplist().

put_object(BucketName, Key, Value, Config)
  when is_record(Config, aws_config) ->
    put_object(BucketName, Key, Value, [], Config);

put_object(BucketName, Key, Value, Options) ->
    put_object(BucketName, Key, Value, Options, default_config()).

-spec put_object(string(), string(), iolist(), proplist(), [{string(), string()}] | aws_config()) -> proplist().

put_object(BucketName, Key, Value, Options, Config)
  when is_record(Config, aws_config) ->
    put_object(BucketName, Key, Value, Options, [], Config);

put_object(BucketName, Key, Value, Options, HTTPHeaders) ->
    put_object(BucketName, Key, Value, Options, HTTPHeaders, default_config()).

-spec put_object(string(), string(), iolist(), proplist(), [{string(), string()}], aws_config()) -> proplist().

put_object(BucketName, Key, Value, Options, HTTPHeaders, Config)
  when is_list(BucketName), is_list(Key), is_list(Value) orelse is_binary(Value),
       is_list(Options) ->
    RequestHeaders = [{"x-amz-acl", encode_acl(proplists:get_value(acl, Options))}|HTTPHeaders]
        ++ [{"x-amz-meta-" ++ string:to_lower(MKey), MValue} ||
               {MKey, MValue} <- proplists:get_value(meta, Options, [])],
    ContentType = proplists:get_value("content-type", HTTPHeaders, "application/octet_stream"),
    POSTData = {iolist_to_binary(Value), ContentType},
    {Headers, _Body} = s3_request(Config, put, BucketName, [$/|Key], "", [],
                                  POSTData, RequestHeaders),
    [{version_id, proplists:get_value("x-amz-version-id", Headers, "null")}].

-spec set_object_acl(string(), string(), proplist()) -> ok.

set_object_acl(BucketName, Key, ACL) ->
    set_object_acl(BucketName, Key, ACL, default_config()).

-spec set_object_acl(string(), string(), proplist(), aws_config()) -> ok.

set_object_acl(BucketName, Key, ACL, Config)
  when is_list(BucketName), is_list(Key), is_list(ACL) ->
    Id = proplists:get_value(id, proplists:get_value(owner, ACL)),
    DisplayName = proplists:get_value(display_name, proplists:get_value(owner, ACL)),
    ACL1 = proplists:get_value(access_control_list, ACL),
    XML = {'AccessControlPolicy',
           [{'Owner', [{'ID', [Id]}, {'DisplayName', [DisplayName]}]},
            {'AccessControlList', encode_grants(ACL1)}]},
    XMLText = list_to_binary(xmerl:export_simple([XML], xmerl_xml)),
    s3_simple_request(Config, put, BucketName, [$/|Key], "acl", [], XMLText, []).

-spec sign_get(integer(), string(), string(), aws_config()) -> {string(), string()}.
sign_get(Expire_time, BucketName, Key, Config)
  when is_integer(Expire_time), is_list(BucketName), is_list(Key) ->
    {Mega, Sec, _Micro} = os:timestamp(),
    Datetime = (Mega * 1000000) + Sec,
    Expires = integer_to_list(Expire_time + Datetime),
    To_sign = lists:flatten(["GET\n\n\n", Expires, "\n/", BucketName, "/", Key]),
    Sig = base64:encode(erlcloud_util:sha_mac(Config#aws_config.secret_access_key, To_sign)),
    {Sig, Expires}.

-spec make_link(integer(), string(), string()) -> {integer(), string(), string()}.

make_link(Expire_time, BucketName, Key) ->
    make_link(Expire_time, BucketName, Key, default_config()).

-spec make_link(integer(), string(), string(), aws_config()) -> {integer(), string(), string()}.

make_link(Expire_time, BucketName, Key, Config) ->
    {Sig, Expires} = sign_get(Expire_time, BucketName, erlcloud_http:url_encode_loose(Key), Config),
    Host = lists:flatten(["http://", BucketName, ".", Config#aws_config.s3_host, port_spec(Config)]),
    URI = lists:flatten(["/", Key, "?AWSAccessKeyId=", erlcloud_http:url_encode(Config#aws_config.access_key_id), "&Signature=", erlcloud_http:url_encode(Sig), "&Expires=", Expires]),
    {list_to_integer(Expires),
     binary_to_list(erlang:iolist_to_binary(Host)),
     binary_to_list(erlang:iolist_to_binary(URI))}.

-spec make_get_url(integer(), string(), string()) -> iolist().

make_get_url(Expire_time, BucketName, Key) ->
    make_get_url(Expire_time, BucketName, Key, default_config()).

-spec make_get_url(integer(), string(), string(), aws_config()) -> iolist().

make_get_url(Expire_time, BucketName, Key, Config) ->
    {Sig, Expires} = sign_get(Expire_time, BucketName, erlcloud_http:url_encode_loose(Key), Config),
    [Config#aws_config.s3_scheme, BucketName, ".", Config#aws_config.s3_host, port_spec(Config), "/", Key,
     "?AWSAccessKeyId=", erlcloud_http:url_encode(Config#aws_config.access_key_id),
     "&Signature=", erlcloud_http:url_encode(Sig),
     "&Expires=", Expires].

-spec start_multipart(string(), string()) -> {ok, proplist()} | {error, any()}.
start_multipart(BucketName, Key)
  when is_list(BucketName), is_list(Key) ->
    start_multipart(BucketName, Key, [], [], default_config()).

-spec start_multipart(string(), string(), proplist(), [{string(), string()}], aws_config()) -> {ok, proplist()} | {error, any()}.
start_multipart(BucketName, Key, Options, HTTPHeaders, Config)
  when is_list(BucketName), is_list(Key), is_list(Options), is_list(HTTPHeaders), is_record(Config, aws_config) ->

    RequestHeaders = [{"x-amz-acl", encode_acl(proplists:get_value(acl, Options))}|HTTPHeaders]
        ++ [{"x-amz-meta-" ++ string:to_lower(MKey), MValue} ||
               {MKey, MValue} <- proplists:get_value(meta, Options, [])],
    POSTData = <<>>,
    case s3_xml_request2(Config, post, BucketName, [$/|Key], "uploads", [],
                         POSTData, RequestHeaders) of
        {ok, Doc} ->
            Attributes = [{uploadId, "UploadId", text}],
            {ok, erlcloud_xml:decode(Attributes, Doc)};

        Error ->
            Error
    end.

-spec upload_part(string(), string(), string(), integer(), iolist()) -> {ok, proplist()} | {error, any()}.
upload_part(BucketName, Key, UploadId, PartNumber, Value) ->
    upload_part(BucketName, Key, UploadId, PartNumber, Value, [], default_config()).

-spec upload_part(string(), string(), string(), integer(), iolist(), [{string(), string()}], aws_config()) -> {ok, proplist()} | {error, any()}.
upload_part(BucketName, Key, UploadId, PartNumber, Value, HTTPHeaders, Config)
  when is_list(BucketName), is_list(Key), is_list(UploadId), is_integer(PartNumber),
       is_list(Value) orelse is_binary(Value),
       is_list(HTTPHeaders), is_record(Config, aws_config) ->

    ContentType = proplists:get_value("content-type", HTTPHeaders, "application/octet_stream"),
    POSTData = {iolist_to_binary(Value), ContentType},

    case s3_request2(Config, put, BucketName, [$/|Key], [], [{"uploadId", UploadId},
                                                             {"partNumber", integer_to_list(PartNumber)}],
                     POSTData, HTTPHeaders) of
        {ok, {Headers, _Body}} ->
            {ok, [{etag, proplists:get_value("etag", Headers)}]};
        Error ->
            Error
    end.

-spec complete_multipart(string(), string(), string(), [{integer(), string()}]) -> {ok, proplist()} | {error, any()}.
complete_multipart(BucketName, Key, UploadId, ETags)
  when is_list(BucketName), is_list(Key), is_list(UploadId), is_list(ETags) ->
    complete_multipart(BucketName, Key, UploadId, ETags, [], default_config()).

-spec complete_multipart(string(), string(), string(), [{integer(), string()}], [{string(), string()}], aws_config()) -> ok | {error, any()}.
complete_multipart(BucketName, Key, UploadId, ETags, HTTPHeaders, Config)
  when is_list(BucketName), is_list(Key), is_list(UploadId), is_list(ETags), is_list(HTTPHeaders), is_record(Config, aws_config) ->
    POSTData = list_to_binary(xmerl:export_simple([{'CompleteMultipartUpload',
                                                    [{'Part',
                                                      [{'PartNumber', [integer_to_list(Num)]},
                                                       {'ETag', [ETag]}] } || {Num, ETag} <- ETags]}], xmerl_xml)),

    case s3_request2(Config, post, BucketName, [$/|Key], [], [{"uploadId", UploadId}],
                     POSTData, HTTPHeaders) of
        {ok, {_Headers, _Body}} ->
            ok;
        Error ->
            Error
    end.

-spec abort_multipart(string(), string(), string()) -> ok | {error, any()}.
abort_multipart(BucketName, Key, UploadId)
  when is_list(BucketName), is_list(Key), is_list(UploadId) ->
    abort_multipart(BucketName, Key, UploadId, [], [], default_config()).

-spec abort_multipart(string(), string(), string(), proplist(), [{string(), string()}], aws_config()) -> ok | {error, any()}.
abort_multipart(BucketName, Key, UploadId, Options, HTTPHeaders, Config)
  when is_list(BucketName), is_list(Key), is_list(UploadId), is_list(Options),
       is_list(HTTPHeaders), is_record(Config, aws_config) ->

    case s3_request2(Config, delete, BucketName, [$/|Key], [], [{"uploadId", UploadId}],
                     <<>>, HTTPHeaders) of
        {ok, _} ->
            ok;
        Error ->
            Error
    end.

-spec list_multipart_uploads(string()) -> {ok, proplist()} | {error, any()}.
list_multipart_uploads(BucketName)
  when is_list(BucketName) ->

    list_multipart_uploads(BucketName, [], [], default_config()).

-spec list_multipart_uploads(string(), proplist()) -> {ok, proplist()} | {error, any()}.
list_multipart_uploads(BucketName, Options)
  when is_list(BucketName), is_list(Options) ->

    list_multipart_uploads(BucketName, Options, [], default_config()).

-spec list_multipart_uploads(string(), proplist(), [{string(), string()}], aws_config()) -> {ok, proplist()} | {error, any()}.
list_multipart_uploads(BucketName, Options, HTTPHeaders, Config)
  when is_list(BucketName), is_list(Options),
       is_list(HTTPHeaders), is_record(Config, aws_config) ->

    Params = [
              {"uploads", ""},
              {"delimiter", proplists:get_value(delimiter, Options)},
              {"prefix", proplists:get_value(prefix, Options)},
              {"max-uploads", proplists:get_value(max_uploads, Options)},
              {"key-marker", proplists:get_value(key_marker, Options)},
              {"upload-id-marker", proplists:get_value(upload_id_marker, Options)}
             ],

    case s3_xml_request2(Config, get, BucketName, "/", "", Params, <<>>, HTTPHeaders) of
        {ok, Xml} ->
            Uploads = [erlcloud_xml:decode([{key, "Key", text},
                                            {uploadId, "UploadId", text}], Node) || Node <- xmerl_xpath:string("/ListMultipartUploadsResult/Upload", Xml)],

            CommonPrefixes = [erlcloud_xml:get_text("Prefix", Node) || Node <- xmerl_xpath:string("/ListMultipartUploadsResult/CommonPrefixes", Xml)],

            {ok, [{uploads, Uploads},
                  {common_prefixes, CommonPrefixes}]};
        Error ->
            Error
    end.


-spec set_bucket_attribute(string(), atom(), term()) -> ok.

set_bucket_attribute(BucketName, AttributeName, Value) ->
    set_bucket_attribute(BucketName, AttributeName, Value, default_config()).

-spec set_bucket_attribute(string(), atom(), term(), aws_config()) -> ok.

set_bucket_attribute(BucketName, AttributeName, Value, Config)
  when is_list(BucketName) ->
    {Subresource, XML} =
        case AttributeName of
            acl ->
                ACLXML = {'AccessControlPolicy',
                          [{'Owner',
                            [{'ID', [proplists:get_value(id, proplists:get_value(owner, Value))]},
                             {'DisplayName', [proplists:get_value(display_name, proplists:get_value(owner, Value))]}]},
                           {'AccessControlList', encode_grants(proplists:get_value(access_control_list, Value))}]},
                {"acl", ACLXML};
            logging ->
                LoggingXML = {'BucketLoggingStatus',
                              [{xmlns, ?XMLNS_S3}],
                              case proplists:get_bool(enabled, Value) of
                                  true ->
                                      [{'LoggingEnabled',
                                        [
                                         {'TargetBucket', [proplists:get_value(target_bucket, Value)]},
                                         {'TargetPrefix', [proplists:get_value(target_prefix, Value)]},
                                         {'TargetGrants', encode_grants(proplists:get_value(target_grants, Value, []))}
                                        ]
                                       }];
                                  false ->
                                      []
                              end},
                {"logging", LoggingXML};
            request_payment ->
                PayerName = case Value of
                                requester -> "Requester";
                                bucket_owner -> "BucketOwner"
                            end,
                RPXML = {'RequestPaymentConfiguration', [{xmlns, ?XMLNS_S3}],
                         [
                          {'Payer', [PayerName]}
                         ]
                        },
                {"requestPayment", RPXML};
            versioning ->
                Status = case proplists:get_value(status, Value) of
                             suspended -> "Suspended";
                             enabled -> "Enabled"
                         end,
                MFADelete = case proplists:get_value(mfa_delete, Value, disabled) of
                                enabled -> "Enabled";
                                disabled -> "Disabled"
                            end,
                VersioningXML = {'VersioningConfiguration', [{xmlns, ?XMLNS_S3}],
                                 [{'Status', [Status]},
                                  {'MfaDelete', [MFADelete]}]},
                {"versioning", VersioningXML}
        end,
    POSTData = list_to_binary(xmerl:export_simple([XML], xmerl_xml)),
    Headers = [{"content-type", "application/xml"}],
    s3_simple_request(Config, put, BucketName, "/", Subresource, [], POSTData, Headers).

encode_grants(Grants) ->
    [encode_grant(Grant) || Grant <- Grants].

encode_grant(Grant) ->
    Grantee = proplists:get_value(grantee, Grant),
    {'Grant',
     [{'Grantee', [{xmlns, ?XMLNS_S3}],
       [{'ID', [proplists:get_value(id, proplists:get_value(owner, Grantee))]},
        {'DisplayName', [proplists:get_value(display_name, proplists:get_value(owner, Grantee))]}]},
      {'Permission', [encode_permission(proplists:get_value(permission, Grant))]}]}.

s3_simple_request(Config, Method, Host, Path, Subresource, Params, POSTData, Headers) ->
    case s3_request(Config, Method, Host, Path, Subresource, Params, POSTData, Headers) of
        {_Headers, ""} -> ok;
        {_Headers, Body} ->
            XML = element(1,xmerl_scan:string(Body)),
            case XML of
                #xmlElement{name='Error'} ->
                    ErrCode = erlcloud_xml:get_text("/Error/Code", XML),
                    ErrMsg = erlcloud_xml:get_text("/Error/Message", XML),
                    erlang:error({s3_error, ErrCode, ErrMsg});
                _ ->
                    ok
            end
    end.

s3_xml_request(Config, Method, Host, Path, Subresource, Params, POSTData, Headers) ->
    {_Headers, Body} = s3_request(Config, Method, Host, Path, Subresource, Params, POSTData, Headers),
    XML = element(1,xmerl_scan:string(Body)),
    case XML of
        #xmlElement{name='Error'} ->
            ErrCode = erlcloud_xml:get_text("/Error/Code", XML),
            ErrMsg = erlcloud_xml:get_text("/Error/Message", XML),
            erlang:error({s3_error, ErrCode, ErrMsg});
        _ ->
            XML
    end.

s3_request(Config, Method, Host, Path, Subreasource, Params, POSTData, Headers) ->
    case s3_request2(Config, Method, Host, Path, Subreasource, Params, POSTData, Headers) of
        {ok, Result} ->
            Result;
        {error, Reason} ->
            erlang:error({aws_error, Reason})
    end.

%% s3_request2 returns {ok, Body} or {error, Reason} instead of throwing as s3_request does
%% This is the preferred pattern for new APIs
s3_request2(Config, Method, Host, Path, Subresource, Params, POSTData, Headers) ->
    case erlcloud_aws:update_config(Config) of
        {ok, Config1} ->
            s3_request2_no_update(Config1, Method, Host, Path, Subresource, Params, POSTData, Headers);
        {error, Reason} ->
            {error, Reason}
    end.

s3_xml_request2(Config, Method, Host, Path, Subresource, Params, POSTData, Headers) ->
    case s3_request2(Config, Method, Host, Path, Subresource, Params, POSTData, Headers) of
        {ok, {_Headers, Body}} ->
            XML = element(1,xmerl_scan:string(Body)),
            case XML of
                #xmlElement{name='Error'} ->
                    ErrCode = erlcloud_xml:get_text("/Error/Code", XML),
                    ErrMsg = erlcloud_xml:get_text("/Error/Message", XML),
                    {error, {s3_error, ErrCode, ErrMsg}};
                _ ->
                    {ok, XML}
            end;
        Error ->
            Error
    end.

s3_request2_no_update(Config, Method, Host, Path, Subresource, Params, POSTData, Headers0) ->
    {ContentMD5, ContentType, Body} =
        case POSTData of
            {PD, CT} -> {base64:encode(erlcloud_util:md5(PD)), CT, PD}; PD -> {"", "", PD}
        end,
    Headers = case Config#aws_config.security_token of
                  undefined -> Headers0;
                  Token when is_list(Token) -> [{"x-amz-security-token", Token} | Headers0]
              end,
    FHeaders = [Header || {_, Value} = Header <- Headers, Value =/= undefined],
    AmzHeaders = [Header || {"x-amz-" ++ _, _} = Header <- FHeaders],
    Date = httpd_util:rfc1123_date(erlang:localtime()),
    EscapedPath = erlcloud_http:url_encode_loose(Path),
    Authorization = make_authorization(Config, Method, ContentMD5, ContentType,
                                       Date, AmzHeaders, Host, EscapedPath, Subresource, Params),
    RequestHeaders = [{"date", Date}, {"authorization", Authorization}|FHeaders] ++
        case ContentMD5 of
            "" -> [];
            _ -> [{"content-md5", binary_to_list(ContentMD5)}]
        end,
    RequestURI = lists:flatten([
                                Config#aws_config.s3_scheme,
                                case Host of "" -> ""; _ -> [Host, $.] end,
                                Config#aws_config.s3_host, port_spec(Config),
                                EscapedPath,
                                case Subresource of "" -> ""; _ -> [$?, Subresource] end,
                                if
                                    Params =:= [] -> "";
                                    Subresource =:= "" -> [$?, erlcloud_http:make_query_string(Params)];
                                    true -> [$&, erlcloud_http:make_query_string(Params)]
                                end
                               ]),

    Response = case Method of
                   get -> httpc:request(Method, {RequestURI, RequestHeaders}, [], []);
                   head -> httpc:request(Method, {RequestURI, RequestHeaders}, [], []);
                   delete -> httpc:request(Method, {RequestURI, RequestHeaders}, [], []);
                   _ -> httpc:request(Method, {RequestURI, RequestHeaders, ContentType, Body}, [], [])
               end,
    erlcloud_aws:http_headers_body(Response).

make_authorization(Config, Method, ContentMD5, ContentType, Date, AmzHeaders,
                   Host, Resource, Subresource, Params) ->
    CanonizedAmzHeaders =
        [[Name, $:, Value, $\n] || {Name, Value} <- lists:sort(AmzHeaders)],

    SubResourcesToInclude = ["acl", "lifecycle", "location", "logging", "notification", "partNumber", "policy", "requestPayment", "torrent", "uploadId", "uploads", "versionId", "versioning", "versions", "website"],
    FilteredParams = [{Name, Value} || {Name, Value} <- Params,
                                       lists:member(Name, SubResourcesToInclude)],

    ParamsQueryString = erlcloud_http:make_query_string(lists:keysort(1, FilteredParams)),
    StringToSign = [string:to_upper(atom_to_list(Method)), $\n,
                    ContentMD5, $\n,
                    ContentType, $\n,
                    Date, $\n,
                    CanonizedAmzHeaders,
                    case Host of "" -> ""; _ -> [$/, Host] end,
                    Resource,
                    case Subresource of "" -> ""; _ -> [$?, Subresource] end,
                    if
                        ParamsQueryString =:= "" -> "";
                        Subresource =:= "" -> [$?, ParamsQueryString];
                        true -> [$&, ParamsQueryString]
                    end
                   ],
    Signature = base64:encode(erlcloud_util:sha_mac(Config#aws_config.secret_access_key, StringToSign)),
    ["AWS ", Config#aws_config.access_key_id, $:, Signature].

default_config() -> erlcloud_aws:default_config().

port_spec(#aws_config{s3_port=80}) ->
    "";
port_spec(#aws_config{s3_port=Port}) ->
    [":", erlang:integer_to_list(Port)].
