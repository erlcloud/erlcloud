%% Amazon Simple Storage Service (S3)

-module(erlcloud_s3).
-export([
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
    get_object/2, get_object/3,
    get_object_acl/2, get_object_acl/3, get_object_acl/4,
    get_object_torrent/2, get_object_torrent/3,
    get_object_metadata/2, get_object_metadata/3, get_object_metadata/4,
    put_object/3, put_object/4, put_object/5,
    set_object_acl/3, set_object_acl/4
]).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-type(s3_bucket_attribute_name() :: acl | location | logging | request_payment | versioning).
-type(s3_bucket_acl() :: private | public_read | public_read_write | authenticated_read | bucket_owner_read | bucket_owner_full_control).
-type(s3_location_constraint() :: none | us_west_1 | eu).

-define(XMLNS_S3, "http://s3.amazonaws.com/doc/2006-03-01/").

-spec copy_object/4 :: (string(), string(), string(), string()) -> proplist().
copy_object(DestBucketName, DestKeyName, SrcBucketName, SrcKeyName) ->
    copy_object(DestBucketName, DestKeyName, SrcBucketName, SrcKeyName, []).

-spec copy_object/5 :: (string(), string(), string(), string(), proplist() | aws_config()) -> proplist().
copy_object(DestBucketName, DestKeyName, SrcBucketName, SrcKeyName, Config)
  when is_record(Config, aws_config) ->
    copy_object(DestBucketName, DestKeyName, SrcBucketName, SrcKeyName, [], Config);
copy_object(DestBucketName, DestKeyName, SrcBucketName, SrcKeyName, Options) ->
    copy_object(DestBucketName, DestKeyName, SrcBucketName, SrcKeyName,
                Options, default_config()).

-spec copy_object/6 :: (string(), string(), string(), string(), proplist(), aws_config()) -> proplist().
copy_object(DestBucketName, DestKeyName, SrcBucketName, SrcKeyName, Options, Config) ->
    SrcVersion = case proplists:get_value(version_id, Options) of
        undefined -> "";
        VersionID -> ["?versionId=", VersionID]
    end,
    RequestHeaders = [
        {"x-amz-copy-source", [SrcBucketName, $/, SrcKeyName, SrcVersion]},
        {"x-amz-metadata-directive", proplists:get_value(metadata_directive, Options)},
        {"x-amz-copy-source-if-match", proplists:get_value(if_match, Options)},
        {"x-amz-copy-source-if-none-match", proplists:get_value(if_none_match, Options)},
        {"x-amz-copy-source-if-unmodified-since", proplists:get_value(if_unmodified_since, Options)},
        {"x-amz-copy-source-if-modified-since", proplists:get_value(if_modified_since, Options)},
        {"x-amz-acl", encode_acl(proplists:get_value(acl, Options))}
    ],
    {Headers, _Body} = s3_request(Config, put, DestBucketName, [$/|DestKeyName],
        "", [], <<>>, RequestHeaders),
    [
        {copy_source_version_id, proplists:get_value("x-amz-copy-source-version-id", Headers, "false")},
        {version_id, proplists:get_value("x-amz-version-id", Headers, "null")}
    ].

-spec create_bucket/1 :: (string()) -> ok.
create_bucket(BucketName) ->
    create_bucket(BucketName, private).

-spec create_bucket/2 :: (string(), s3_bucket_acl() | aws_config()) -> ok.
create_bucket(BucketName, Config)
  when is_record(Config, aws_config) ->
    create_bucket(BucketName, private, Config);
create_bucket(BucketName, ACL) ->
    create_bucket(BucketName, ACL, none).

-spec create_bucket/3 :: (string(), s3_bucket_acl(), s3_location_constraint() | aws_config()) -> ok.
create_bucket(BucketName, ACL, Config)
  when is_record(Config, aws_config) ->
    create_bucket(BucketName, ACL, none, Config);
create_bucket(BucketName, ACL, LocationConstraint) ->
    create_bucket(BucketName, ACL, LocationConstraint, default_config()).

-spec create_bucket/4 :: (string(), s3_bucket_acl(), s3_location_constraint(), aws_config()) -> ok.
create_bucket(BucketName, ACL, LocationConstraint, Config)
  when is_list(BucketName), is_atom(ACL), is_atom(LocationConstraint) ->
    Headers = case ACL of
        private -> [];  %% private is the default
        _ -> [{"x-amz-acl", encode_acl(ACL)}]
    end,
    POSTData = case LocationConstraint of
        none -> <<>>;
        Location when Location =:= eu; Location =:= us_west_1 ->
            LocationName = case Location of eu -> "EU"; us_west_1 -> "us-west-1" end,
            XML = {'CreateBucketConfiguration', [{xmlns, ?XMLNS_S3}],
                [
                    {'LocationConstraint', [LocationName]}
                ]
            },
            list_to_binary(xmerl:export_simple([XML], xmerl_xml))
    end,
    s3_simple_request(Config, put, BucketName, "/", "", [], POSTData, Headers).

encode_acl(undefined) -> undefined;
encode_acl(private) -> "private";
encode_acl(public_read) -> "public-read";
encode_acl(public_read_write) -> "public-read-write";
encode_acl(authenticated_read) -> "authenticated-read";
encode_acl(bucket_owner_read) -> "bucket-owner-read";
encode_acl(bucket_owner_full_control) -> "bucket-owner-full-control".

    

-spec delete_bucket/1 :: (string()) -> ok.
delete_bucket(BucketName) ->
    delete_bucket(BucketName, default_config()).

-spec delete_bucket/2 :: (string(), aws_config()) -> ok.
delete_bucket(BucketName, Config)
  when is_list(BucketName) ->
    s3_simple_request(Config, delete, BucketName, "/", "", [], <<>>, []).

-spec delete_object/2 :: (string(), string()) -> proplist().
delete_object(BucketName, Key) ->
    delete_object(BucketName, Key, default_config()).

-spec delete_object/3 :: (string(), string(), aws_config()) -> proplist().
delete_object(BucketName, Key, Config)
  when is_list(BucketName), is_list(Key) ->
    {Headers, _Body} = s3_request(Config, delete, BucketName, [$/|Key], "", [], <<>>, []),
    [
        {delete_marker, list_to_existing_atom(proplists:get_value("x-amz-delete-marker", Headers, "false"))},
        {version_id, proplists:get_value("x-amz-version-id", Headers, "null")}
    ].

-spec delete_object_version/3 :: (string(), string(), string()) -> proplist().
delete_object_version(BucketName, Key, Version) ->
    delete_object_version(BucketName, Key, Version, default_config()).

-spec delete_object_version/4 :: (string(), string(), string(), aws_config()) -> proplist().
delete_object_version(BucketName, Key, Version, Config)
  when is_list(BucketName), is_list(Key), is_list(Version)->
    {Headers, _Body} = s3_request(Config, delete, BucketName, [$/|Key], ["versionId=", Version], [], <<>>, []),
    [
        {delete_marker, list_to_existing_atom(proplists:get_value("x-amz-delete-marker", Headers, "false"))},
        {version_id, proplists:get_value("x-amz-version-id", Headers, "null")}
    ].

-spec list_buckets/0 :: () -> proplist().
list_buckets() ->
    list_buckets(default_config()).

-spec list_buckets/1 :: (aws_config()) -> proplist().
list_buckets(Config) ->
    Doc = s3_xml_request(Config, get, "", "/", "", [], <<>>, []),
    Buckets = [extract_bucket(Node) || Node <- xmerl_xpath:string("/*/Buckets/Bucket", Doc)],
    [{buckets, Buckets}].

-spec list_objects/1 :: (string()) -> proplist().
list_objects(BucketName) ->
    list_objects(BucketName, []).

-spec list_objects/2 :: (string(), proplist() | aws_config()) -> proplist().
list_objects(BucketName, Config)
  when is_record(Config, aws_config) ->
    list_objects(BucketName, [], Config);
list_objects(BucketName, Options) ->
    list_objects(BucketName, Options, default_config()).

-spec list_objects/3 :: (string(), proplist(), aws_config()) -> proplist().
list_objects(BucketName, Options, Config)
  when is_list(BucketName), is_list(Options) ->
    Params = [
        {"delimiter", proplists:get_value(delimiter, Options)},
        {"marker", proplists:get_value(marker, Options)},
        {"max-keys", proplists:get_value(max_keys, Options)},
        {"prefix", proplists:get_value(prefix, Options)}
    ],
    Doc = s3_xml_request(Config, get, BucketName, "/", "", Params, <<>>, []),
    erlcloud_xml:decode(
        [
            {name, "Name", text},
            {prefix, "Prefix", text},
            {marker, "Marker", text},
            {delimiter, "Delimiter", text},
            {max_keys, "MaxKeys", integer},
            {is_truncated, "IsTruncated", boolean},
            {contents, "Contents", fun extract_contents/1}
        ],
        Doc
    ).

extract_contents(Nodes) ->
    [erlcloud_xml:decode(
        [
            {key, "Key", text},
            {last_modified, "LastModified", time},
            {etag, "ETag", text},
            {size, "Size", integer},
            {storage_class, "StorageClass", text},
            {owner, "Owner", fun extract_user/1}
        ], Node) ||
     Node <- Nodes].

extract_user([Node]) ->
    erlcloud_xml:decode(
        [
            {id, "ID", text},
            {display_name, "DisplayName", optional_text}
        ],
        Node
    ).

-spec get_bucket_attribute/2 :: (string(), s3_bucket_attribute_name()) -> term().
get_bucket_attribute(BucketName, AttributeName) ->
    get_bucket_attribute(BucketName, AttributeName, default_config()).

-spec get_bucket_attribute/3 :: (string(), s3_bucket_attribute_name(), aws_config()) -> term().
get_bucket_attribute(BucketName, AttributeName, Config)
  when is_list(BucketName), is_atom(AttributeName) ->
    Attr = case AttributeName of
        acl -> "acl";
        location -> "location";
        logging -> "logging";
        request_payment -> "requestPayment";
        versioning -> "versioning"
    end,
    Doc = s3_xml_request(Config, get, BucketName, "/", Attr, [], <<>>, []),
    case AttributeName of
        acl ->
            erlcloud_xml:decode(
                [
                    {owner, "Owner", fun extract_user/1},
                    {access_control_list, "AccessControlList/Grant", fun extract_acl/1}
                ],
                Doc
            );
        location ->
            erlcloud_xml:get_text("/LocationConstraint", Doc);
        logging ->
            case xmerl_xpath:string("/BucketLoggingStatus/LoggingEnabled", Doc) of
                [] -> {enabled, false};
                [LoggingEnabled] ->
                    [{enabled, true}|erlcloud_xml:decode(
                        [
                            {target_bucket, "TargetBucket", text},
                            {target_prefix, "TargetPrefix", text},
                            {target_trants, "TargetGrants/Grant", fun extract_acl/1}
                        ],
                        LoggingEnabled
                     )]
            end;
        request_payment ->
            case erlcloud_xml:get_text("/RequestPaymentConfiguration/Payer", Doc) of
                "Requester" -> requester;
                _ -> bucket_owner
            end;
        versioning ->
            case erlcloud_xml:get_text("/VersioningConfiguration/Status", Doc) of
                "Enabled" -> enabled;
                "Suspended" -> suspended;
                _ -> disabled
            end
    end.

extract_acl(ACL) ->
    [extract_grant(Item) || Item <- ACL].

extract_grant(Node) ->
    [
        {grantee, extract_user(xmerl_xpath:string("Grantee", Node))},
        {permission, decode_permission(erlcloud_xml:get_text("Permission", Node))}
    ].

encode_permission(full_control) -> "FULL_CONTROL";
encode_permission(write) -> "WRITE";
encode_permission(write_acp) -> "WRITE_ACP";
encode_permission(read) -> "READ";
encode_permission(read_acp) -> "READ_ACP".

decode_permission("FULL_CONTROL") -> full_control;
decode_permission("WRITE") -> write;
decode_permission("WRITE_ACP") -> write_acp;
decode_permission("READ") -> read;
decode_permission("READ_ACP") -> read_acp.

-spec get_object/2 :: (string(), string()) -> proplist().
get_object(BucketName, Key) ->
    get_object(BucketName, Key, []).

-spec get_object/3 :: (string(), string(), proplist() | aws_config()) -> proplist().
get_object(BucketName, Key, Config)
  when is_record(Config, aws_config) ->
    get_object(BucketName, Key, [], Config);
get_object(BucketName, Key, Options) ->
    get_object(BucketName, Key, Options, default_config()).

-spec get_object/4 :: (string(), string(), proplist(), aws_config()) -> proplist().
get_object(BucketName, Key, Options, Config) ->
    RequestHeaders = [
        {"Range", proplists:get_value(range, Options)},
        {"If-Modified-Since", proplists:get_value(if_modified_since, Options)},
        {"If-Unmodified-Since", proplists:get_value(if_unmodified_since, Options)},
        {"If-Match", proplists:get_value(if_match, Options)},
        {"If-None-Match", proplists:get_value(if_none_match, Options)}
    ],
    Subresource = case proplists:get_value(version_id, Options) of
        undefined -> "";
        Version -> ["versionId=", Version]
    end,
    {Headers, Body} = s3_request(Config, get, BucketName, [$/|Key], Subresource, [], <<>>,
        RequestHeaders),
    [
        {etag, proplists:get_value("etag", Headers)},
        {content_length, proplists:get_value("content-length", Headers)},
        {content_type, proplists:get_value("content-type", Headers)},
        {delete_marker, list_to_existing_atom(proplists:get_value("x-amz-delete-marker", Headers, "false"))},
        {version_id, proplists:get_value("x-amz-version-id", Headers, "null")},
        {content, list_to_binary(Body)}|
        extract_metadata(Headers)
    ].

-spec get_object_acl/2 :: (string(), string()) -> proplist().
get_object_acl(BucketName, Key) ->
    get_object_acl(BucketName, Key, default_config()).

-spec get_object_acl/3 :: (string(), string(), proplist() | aws_config()) -> proplist().
get_object_acl(BucketName, Key, Config)
  when is_record(Config, aws_config) ->
    get_object_acl(BucketName, Key, [], Config);
get_object_acl(BucketName, Key, Options) ->
    get_object_acl(BucketName, Key, Options, default_config()).

-spec get_object_acl/4 :: (string(), string(), proplist(), aws_config()) -> proplist().
get_object_acl(BucketName, Key, Options, Config)
  when is_list(BucketName), is_list(Key), is_list(Options) ->
    Subresource = case proplists:get_value(version_id, Options) of
        undefined -> "";
        Version -> ["&versionId=", Version]
    end,
    Doc = s3_xml_request(Config, get, BucketName, [$/|Key], "acl" ++ Subresource, [], <<>>, []),
    erlcloud_xml:decode(
        [
            {owner, "Owner", fun extract_user/1},
            {access_control_list, "AccessControlList/Grant", fun extract_acl/1}
        ],
        Doc
    ).

-spec get_object_metadata/2 :: (string(), string()) -> proplist().
get_object_metadata(BucketName, Key) ->
    get_object_metadata(BucketName, Key, []).

-spec get_object_metadata/3 :: (string(), string(), proplist() | aws_config()) -> proplist().
get_object_metadata(BucketName, Key, Config)
  when is_record(Config, aws_config) ->
    get_object_metadata(BucketName, Key, [], Config);
get_object_metadata(BucketName, Key, Options) ->
    get_object_metadata(BucketName, Key, Options, default_config()).

-spec get_object_metadata/4 :: (string(), string(), proplist(), proplist() | aws_config()) -> proplist().
get_object_metadata(BucketName, Key, Options, Config) ->
    RequestHeaders = [
        {"If-Modified-Since", proplists:get_value(if_modified_since, Options)},
        {"If-Unmodified-Since", proplists:get_value(if_unmodified_since, Options)},
        {"If-Match", proplists:get_value(if_match, Options)},
        {"If-None-Match", proplists:get_value(if_none_match, Options)}
    ],
    Subresource = case proplists:get_value(version_id, Options) of
        undefined -> "";
        Version -> ["versionId=", Version]
    end,
    {Headers, _Body} = s3_request(Config, get, BucketName, [$/|Key], Subresource, [], <<>>,
        RequestHeaders),
    [
        {last_modified, proplists:get_value("last-modified", Headers)},
        {etag, proplists:get_value("etag", Headers)},
        {content_length, proplists:get_value("content-length", Headers)},
        {content_type, proplists:get_value("content-type", Headers)},
        {delete_marker, list_to_existing_atom(proplists:get_value("x-amz-delete-marker", Headers, "false"))},
        {version_id, proplists:get_value("x-amz-version-id", Headers, "false")}|
        extract_metadata(Headers)
    ].

extract_metadata(Headers) ->
    [{Key, Value} || {["x-amz-meta-"|Key], Value} <- Headers].

-spec get_object_torrent/2 :: (string(), string()) -> proplist().
get_object_torrent(BucketName, Key) ->
    get_object_torrent(BucketName, Key, default_config()).

-spec get_object_torrent/3 :: (string(), string(), aws_config()) -> proplist().
get_object_torrent(BucketName, Key, Config) ->
    {Headers, Body} = s3_request(Config, get, BucketName, [$/|Key], "torrent",
        [], <<>>, []),
    [
        {delete_marker, list_to_existing_atom(proplists:get_value("x-amz-delete-marker", Headers, "false"))},
        {version_id, proplists:get_value("x-amz-delete-marker", Headers, "false")},
        {torrent, list_to_binary(Body)}
    ].

-spec list_object_versions/1 :: (string()) -> proplist().
list_object_versions(BucketName) ->
    list_object_versions(BucketName, []).

-spec list_object_versions/2 :: (string(), proplist() | aws_config()) -> proplist().
list_object_versions(BucketName, Config)
  when is_record(Config, aws_config) ->
    list_object_versions(BucketName, [], Config);
list_object_versions(BucketName, Options) ->
    list_object_versions(BucketName, Options, default_config()).

-spec list_object_versions/3 :: (string(), proplist(), aws_config()) -> proplist().
list_object_versions(BucketName, Options, Config)
  when is_list(BucketName), is_list(Options) ->
    Params = [
        {"delimiter", proplists:get_value(delimiter, Options)},
        {"key-marker", proplists:get_value(key_marker, Options)},
        {"max-keys", proplists:get_value(max_keys, Options)},
        {"prefix", proplists:get_value(prefix, Options)},
        {"version-id-marker", proplists:get_value(version_id_marker, Options)}
    ],
    Doc = s3_xml_request(Config, get, BucketName, "/", "versions", Params, <<>>, []),
    erlcloud_xml:decode(
        [
            {name, "Name", text},
            {prefix, "Prefix", text},
            {key_marker, "KeyMarker", text},
            {next_key_marker, "NextKeyMarker", optional_text},
            {version_id_marker, "VersionIdMarker", text},
            {next_version_id_marker, "NextVersionIdMarker", optional_text},
            {max_keys, "MaxKeys", integer},
            {is_truncated, "Istruncated", boolean},
            {versions, "Version", fun extract_versions/1},
            {delete_markers, "DeleteMarker", fun extract_delete_markers/1}
        ],
        Doc
    ).
    
extract_versions(Nodes) ->
    [extract_version(Node) || Node <- Nodes].

extract_version(Node) ->
    erlcloud_xml:decode(
        [
            {key, "Key", text},
            {version_id, "VersionId", text},
            {is_latest, "IsLatest", boolean},
            {etag, "ETag", text},
            {size, "Size", integer},
            {owner, "Owner", fun extract_user/1},
            {storage_class, "StorageClass", text}
        ],
        Node
    ).

extract_delete_markers(Nodes) ->
    [extract_delete_marker(Node) || Node <- Nodes].

extract_delete_marker(Node) ->
    erlcloud_xml:decode(
        [
            {key, "Key", text},
            {version_id, "VersionId", text},
            {is_latest, "IsLatest", boolean},
            {owner, "Owner", fun extract_user/1}
        ],
        Node
    ).

extract_bucket(Node) ->
    erlcloud_xml:decode(
        [
            {name, "Name", text},
            {creation_date, "CreationDate", time}
        ], Node).

-spec put_object/3 :: (string(), string(), iolist()) -> proplist().
put_object(BucketName, Key, Value) ->
    put_object(BucketName, Key, Value, []).

-spec put_object/4 :: (string(), string(), iolist(), proplist() | aws_config()) -> proplist().
put_object(BucketName, Key, Value, Config)
  when is_record(Config, aws_config) ->
    put_object(BucketName, Key, Value, [], Config);
put_object(BucketName, Key, Value, Options) ->
    put_object(BucketName, Key, Value, Options, default_config()).

-spec put_object/5 :: (string(), string(), iolist(), proplist(), [{string(), string()}] | aws_config()) -> proplist().
put_object(BucketName, Key, Value, Options, Config)
  when is_record(Config, aws_config) ->
    put_object(BucketName, Key, Value, Options, [], Config);
put_object(BucketName, Key, Value, Options, HTTPHeaders) ->
    put_object(BucketName, Key, Value, Options, HTTPHeaders, default_config()).

-spec put_object/6 :: (string(), string(), iolist(), proplist(), [{string(), string()}], aws_config()) -> proplist().
put_object(BucketName, Key, Value, Options, HTTPHeaders, Config)
  when is_list(BucketName), is_list(Key), is_list(Value) orelse is_binary(Value),
       is_list(Options) ->
    RequestHeaders = [
        {"x-amz-acl", encode_acl(proplists:get_value(acl, Options))}
        |HTTPHeaders
    ] ++ [{["x-amz-meta-"|string:to_lower(MKey)], MValue} || {MKey, MValue} <- proplists:get_value(meta, Options, [])],
    POSTData = {iolist_to_binary(Value), proplists:get_value("content-type", HTTPHeaders, "application/octet_stream")},
    {Headers, _Body} = s3_request(Config, put, BucketName, [$/|Key], "", [],
        POSTData, RequestHeaders),
    [
        {version_id, proplists:get_value("x-amz-version-id", Headers, "null")}
    ].

-spec set_object_acl/3 :: (string(), string(), proplist()) -> ok.
set_object_acl(BucketName, Key, ACL) ->
    set_object_acl(BucketName, Key, ACL, default_config()).

-spec set_object_acl/4 :: (string(), string(), proplist(), aws_config()) -> ok.
set_object_acl(BucketName, Key, ACL, Config)
  when is_list(BucketName), is_list(Key), is_list(ACL) ->
    XML = {'AccessControlPolicy',
      [
          {'Owner',
              [
                  {'ID', [proplists:get_value(id, proplists:get_value(owner, ACL))]},
                  {'DisplayName', [proplists:get_value(display_name, proplists:get_value(owner, ACL))]}
              ]
          },
          {'AccessControlList', encode_grants(proplists:get_value(access_control_list, ACL))}
      ]
    },
    XMLText = list_to_binary(xmerl:export_simple([XML], xmerl_xml)),
    s3_simple_request(Config, put, BucketName, [$/|Key], "acl", [], XMLText, []).

-spec set_bucket_attribute/3 :: (string(), atom(), term()) -> ok.
set_bucket_attribute(BucketName, AttributeName, Value) ->
    set_bucket_attribute(BucketName, AttributeName, Value, default_config()).

-spec set_bucket_attribute/4 :: (string(), atom(), term(), aws_config()) -> ok.
set_bucket_attribute(BucketName, AttributeName, Value, Config)
  when is_list(BucketName) ->
    {Subresource, XML} = case AttributeName of
        acl ->
            ACLXML = {'AccessControlPolicy',
                [
                    {'Owner',
                        [
                            {'ID', [proplists:get_value(id, proplists:get_value(owner, Value))]},
                            {'DisplayName', [proplists:get_value(display_name, proplists:get_value(owner, Value))]}
                        ]
                    },
                    {'AccessControlList', encode_grants(proplists:get_value(access_control_list, Value))}
                ]
            },
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
                [
                    {'Status', [Status]},
                    {'MfaDelete', [MFADelete]}
                ]
            },
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
        [
            {'Grantee', [{xmlns, ?XMLNS_S3}],
                [
                    {'ID', [proplists:get_value(id, proplists:get_value(owner, Grantee))]},
                    {'DisplayName', [proplists:get_value(display_name, proplists:get_value(owner, Grantee))]}
                ]
            },
            {'Permission', [encode_permission(proplists:get_value(permission, Grant))]}
        ]
    }.


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

s3_request(Config, Method, Host, Path, Subresource, Params, POSTData, Headers) ->
    {ContentMD5, ContentType, Body} = case POSTData of
        {PD, CT} ->
            {base64:encode(crypto:md5(PD)), CT, PD};
        PD -> {"", "", PD}
    end,
    AmzHeaders = lists:filter(fun ({"x-amz-" ++ _, V}) when V =/= undefined -> true; (_) -> false end, Headers),
    Date = httpd_util:rfc1123_date(erlang:localtime()),
    EscapedPath = erlcloud_http:url_encode_loose(Path),
    Authorization = make_authorization(Config, Method, ContentMD5, ContentType,
        Date, AmzHeaders, Host, EscapedPath, Subresource),
    
    FHeaders = [Header || {_, Value} = Header <- Headers, Value =/= undefined],
    RequestHeaders = [{"date", Date}, {"authorization", Authorization}|FHeaders] ++
        case ContentMD5 of
            "" -> [];
            _ -> [{"content-md5", binary_to_list(ContentMD5)}]
        end,

    RequestURI = lists:flatten([
        "https://",
        case Host of "" -> ""; _ -> [Host, $.] end,
        Config#aws_config.s3_host,
        EscapedPath,
        case Subresource of "" -> ""; _ -> [$?, Subresource] end,
        if
            Params =:= [] -> "";
            Subresource =:= "" -> [$?, erlcloud_http:make_query_string(Params)];
            true -> [$&, erlcloud_http:make_query_string(Params)]
        end
    ]),
    
    Response = case Method of
        get -> http:request(Method, {RequestURI, RequestHeaders}, [], []);
        delete -> http:request(Method, {RequestURI, RequestHeaders}, [], []);
        _ -> http:request(Method, {RequestURI, RequestHeaders, ContentType, Body}, [], [])
    end,
    
    case Response of
        {ok, {{_HTTPVer, OKStatus, _StatusLine}, ResponseHeaders, ResponseBody}}
          when OKStatus >= 200, OKStatus =< 299 ->
            {ResponseHeaders, ResponseBody};
        {ok, {{_HTTPVer, Status, _StatusLine}, _ResponseHeaders, _ResponseBody}} ->
            erlang:error({aws_error, {http_error, Status, _StatusLine, _ResponseBody}});
        {error, Error} ->
            erlang:error({aws_error, {socket_error, Error}})
    end.

make_authorization(Config, Method, ContentMD5, ContentType, Date, AmzHeaders,
                   Host, Resource, Subresource) ->
    CanonizedAmzHeaders = 
        [[Name, $:, Value, $\n] || {Name, Value} <- lists:sort(AmzHeaders)],

    StringToSign = [
        string:to_upper(atom_to_list(Method)), $\n,
        ContentMD5, $\n,
        ContentType, $\n,
        Date, $\n,
        CanonizedAmzHeaders,
        case Host of "" -> ""; _ -> [$/, Host] end,
        Resource, case Subresource of "" -> ""; _ -> [$?, Subresource] end
    ],
    
    Signature = base64:encode(crypto:sha_mac(Config#aws_config.secret_access_key, StringToSign)),
    
    ["AWS ", Config#aws_config.access_key_id, $:, Signature].

default_config() -> erlcloud_aws:default_config().
