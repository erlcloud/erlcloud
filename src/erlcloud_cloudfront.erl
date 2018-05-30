-module(erlcloud_cloudfront).

-include("erlcloud_xmerl.hrl").

%% Library initialization.
-export([configure/2, configure/3, new/2, new/3]).

-export([list_distributions/0, list_distributions/1,
         list_distributions/2, list_distributions/3,
         get_distribution/1, get_distribution/2]).

-define(API_VERSION, "2016-11-25").

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").
-include("erlcloud_ec2.hrl").

-type ok_error(Ok) :: {ok, Ok} | {error, term()}.
-type ok_error(Ok1, Ok2) :: {ok, Ok1, Ok2} | {error, term()}.

-define(MAX_RESULTS, 100).

-spec extract_distribution_summary(Node :: list()) -> proplist().
extract_distribution_summary(Node) ->
    erlcloud_xml:decode(
      [
       {distribution_id, "Id", text},
       {distribution_arn, "ARN", text},
       {aliases, "Aliases/Items/CNAME", list},
       {comment, "Comment", optional_text},
       {default_cache_behaviour, "DefaultCacheBehavior", {single, fun extract_cache_behavior/1}},
       {domain_name, "DomainName", text},
       {http_version, "HttpVersion", optional_text},
       {is_ipv6_enabled, "IsIPV6Enabled", boolean},
       {enabled, "Enabled", boolean},
       {last_modified_time, "LastModifiedTime", time},
       {price_class, "PriceClass", text},
       {geo_restrictions, "Restrictions/GeoRestriction/Items/Location", list},
       {geo_restriction_type, "Restrictions/GeoRestriction/RestrictionType", optional_text},
       {viewer_certificate, "ViewerCertificate", {single, fun extract_viewer_certificate/1}},
       {status, "Status", text},
       {web_acl_id, "WebACLId", optional_text},
       {custom_error_responses, "CustomErrorResponses/Items/CustomErrorResponse", {map, fun extract_custom_error_response/1}},
       {cache_behaviours, "CacheBehaviors/Items/CacheBehavior", {map, fun extract_cache_behavior/1}},
       {origins, "Origins/Items/Origin", {map, fun extract_origin/1}}
    ], Node).


-spec extract_distribution(Node :: xmerl_xpath_doc_nodes()) -> proplist().
extract_distribution(Node) ->
    erlcloud_xml:decode(
      [
       {distribution_id, "Id", text},
       {distribution_arn, "ARN", text},
       {domain_name, "DomainName", text},
       {last_modified_time, "LastModifiedTime", time},
       {status, "Status", text}
      ], Node) ++ extract_distribution_config(hd(xmerl_xpath:string("DistributionConfig", Node))).


-spec extract_distribution_config(Node :: list()) -> proplist().
extract_distribution_config(Node) ->
    erlcloud_xml:decode(
      [
       {aliases, "Aliases/Items/CNAME", list},
       {comment, "Comment", optional_text},
       {default_cache_behaviour, "DefaultCacheBehavior", {single, fun extract_cache_behavior/1}},
       {http_version, "HttpVersion", optional_text},
       {enabled, "Enabled", boolean},
       {is_ipv6_enabled, "IsIPV6Enabled", boolean},
       {price_class, "PriceClass", text},
       {geo_restrictions, "Restrictions/GeoRestrictions/Items", list},
       {geo_restriction_type, "Restrictions/GeoRestrictions/RestrictionType", optional_text},
       {viewer_certificate, "ViewerCertificate", {single, fun extract_viewer_certificate/1}},
       {web_acl_id, "WebACLId", optional_text},
       {custom_error_responses, "CustomErrorResponses/Items/CustomErrorResponse", {map, fun extract_custom_error_response/1}},
       {cache_behaviours, "CacheBehaviors/Items/CacheBehavior", {map, fun extract_cache_behavior/1}},
       {origins, "Origins/Items/Origin", {map, fun extract_origin/1}}
      ], Node).


-spec extract_cache_behavior(Node :: list()) -> proplist().
extract_cache_behavior(Node) ->
    erlcloud_xml:decode(
    [
     {allowed_methods, "AllowedMethods/Items/Method", list},
     {cached_methods, "AllowedMethods/CachedMethods/Items/Method", list},
     {compress, "Compress", boolean},
     {smooth_streaming, "SmoothStreaming", boolean},
     {default_ttl, "DefaultTTL", integer},
     {max_ttl, "MaxTTL", integer},
     {min_ttl, "MinTTL", integer},
     {path_pattern, "PathPattern", optional_text},
     {target_origin_id, "TargetOriginId", text},
     {viewer_protocol_policy, "ViewerProtocolPolicy", text},
     {forwarded_query_string, "ForwardedValues/QueryString", boolean},
     {forwarded_query_string_cache_keys, "ForwardedValues/QueryStringCacheKeys/Items/Name", list},
     {forwarded_cookies_forward, "ForwardedValues/Cookies/Forward", boolean},
     {forwarded_cookies_whitelisted_names, "ForwardedValues/Cookies/WhitelistedNames/Items/Name", list},
     {forwarded_headers, "ForwardedValues/Headers/Items/Name", list},
     {trusted_signers, "TrustedSigners/Items/AwsAccountNumber", list},
     {trusted_signer_required, "TrustedSigners/Enabled", boolean}
    ], Node).


-spec extract_custom_error_response(Node :: list()) -> proplist().
extract_custom_error_response(Node) ->
    erlcloud_xml:decode(
      [
       {error_caching_min_ttl, "ErrorCachingMinTTL", integer},
       {error_code, "ErrorCode", integer},
       {response_code, "ResponseCode", text},
       {response_page_path, "ResponsePagePath", text}
      ], Node).


-spec extract_origin(Node :: list()) -> proplist().
extract_origin(Node) ->
    erlcloud_xml:decode(
      [
       {origin_id, "Id", text},
       {origin_path, "OriginPath", optional_text},
       {domain_name, "DomainName", optional_text},
       {custom_headers, "CustomHeaders/Items/OriginCustomHeader", {map, fun extract_custom_header/1}},
       {http_port, "CustomOriginConfig/HTTPPort", optional_integer},
       {https_port, "CustomOriginConfig/HTTPSPort", optional_integer},
       {origin_protocol_policy, "CustomOriginConfig/OriginProtocolPolicy", optional_text},
       {origin_ssl_protocols, "CustomOriginConfig/OriginSslProtocols/Items/SslProtocol", list},
       {origin_access_identity, "S3OriginConfig/OriginAccessIdentity", optional_text}
      ], Node).

-spec extract_custom_header(Node :: list()) -> proplist().
extract_custom_header(Node) ->
    erlcloud_xml:decode(
      [
       {name, "HeaderName", text},
       {value, "HeaderValue", text}
      ], Node).

-spec extract_viewer_certificate(Node :: list()) -> proplist().
extract_viewer_certificate(Node) ->
    erlcloud_xml:decode(
      [
       {acm_certificate_arn, "ACMCertificateArn", optional_text},
       {certificate, "Certificate", optional_text},
       {certificate_source, "CertificateSource", optional_text},
       {cloudfront_default_certificate, "CloudFrontDefaultCertificate", optional_text},
       {iam_certificate_id, "IAMCertificateId", optional_text},
       {minimum_protocol_version, "MinimumProtocolVersion", optional_text},
       {ssl_support_method, "SSLSupportMethod", optional_text}
      ], Node).


-spec new(string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey}.

-spec new(string(), string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                cloudfront_host=Host}.

-spec configure(string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey) ->
    erlcloud_config:configure(AccessKeyID, SecretAccessKey, fun new/2).

-spec configure(string(), string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host) ->
    erlcloud_config:configure(AccessKeyID, SecretAccessKey, Host, fun new/3).

-spec list_distributions() -> ok_error(proplist()).
list_distributions() ->
    list_distributions(erlcloud_aws:default_config()).


-spec list_distributions(aws_config()) -> ok_error(proplist()).
list_distributions(Config) when
      is_record(Config, aws_config) ->
    list_distributions(?MAX_RESULTS, undefined, Config).


-spec list_distributions(integer(), string()) -> ok_error(proplist(), string()).
list_distributions(MaxResults, Marker) ->
    list_distributions(MaxResults, Marker, erlcloud_aws:default_config()).

-spec list_distributions(integer(), undefined | string(), aws_config()) -> ok_error(proplist(), string()).
list_distributions(MaxResults, Marker, Config) when
      is_list(Marker) orelse Marker =:= undefined,
      is_integer(MaxResults),
      MaxResults > 0,
      MaxResults =< ?MAX_RESULTS,
      is_record(Config, aws_config) ->
    Params = lists:filter(fun ({_K, V}) -> V =/= undefined end,
                          [{"MaxItems", MaxResults}, {"Marker", Marker}]),
    case cloudfront_query(get, Config, "distribution", Params) of
        {ok, Doc} ->
            Res = [extract_distribution_summary(D) ||
                      D <- xmerl_xpath:string("/DistributionList/Items/DistributionSummary", Doc)],
            NextMarker = erlcloud_xml:get_text("NextMarker", Doc, undefined),
            {ok, Res, NextMarker};
        E ->
            E
    end.


-spec get_distribution(string()) -> ok_error(proplist()).
get_distribution(Id) when
      is_list(Id) ->
    get_distribution(Id, erlcloud_aws:default_config()).

-spec get_distribution(string(), aws_config()) -> ok_error(proplist()).
get_distribution(Id, Config) when
      is_list(Id),
      is_record(Config, aws_config) ->
    case cloudfront_query(get, Config, lists:append(["distribution/", Id]), []) of
        {ok, Doc} ->
            {ok, extract_distribution(Doc)};
        E ->
            E
    end.

cloudfront_query(Method, Config, Action, Params) ->
    cloudfront_query(Method, Config, Action, Params, ?API_VERSION).

cloudfront_query(Method, Config, Action, Params, ApiVersion) ->
    erlcloud_aws:aws_request_xml4(Method, Config#aws_config.cloudfront_host,
                                  lists:append(["/", ApiVersion, "/", Action]), 
                                  Params, "cloudfront", Config).

