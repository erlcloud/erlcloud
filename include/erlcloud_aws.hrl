-record(aws_config, {
          as_host="autoscaling.amazonaws.com"::string(),
          ec2_host="ec2.amazonaws.com"::string(),
          iam_host="iam.amazonaws.com"::string(),
          sts_host="sts.amazonaws.com"::string(),
          s3_scheme="https://"::string(),
          s3_host="s3.amazonaws.com"::string(),
          s3_port=80::non_neg_integer(),
          s3_follow_redirect=false::boolean(),
          s3_follow_redirect_count=2::non_neg_integer(),
          %% When set to 'auto' access method is chosen
          %% according to a bucket name:
          %%    * non-DNS-compliant name - 'path'
          %%    * DNS-compliant or empty name - 'vhost'
          s3_bucket_access_method=vhost::vhost|path|auto,
          s3_bucket_after_host=false::boolean(),
          sdb_host="sdb.amazonaws.com"::string(),
          elb_host="elasticloadbalancing.amazonaws.com"::string(),
          rds_host="rds.us-east-1.amazonaws.com"::string(),
          ses_host="email.us-east-1.amazonaws.com"::string(),
          sqs_host="queue.amazonaws.com"::string(),
          sqs_protocol=undefined::string()|undefined,
          sqs_port=undefined::non_neg_integer()|undefined,
          sns_scheme="http://"::string(),
          sns_host="sns.amazonaws.com"::string(),
          mturk_host="mechanicalturk.amazonaws.com"::string(),
          mon_host="monitoring.amazonaws.com"::string(),
          mon_port=undefined::non_neg_integer()|undefined,
          mon_protocol=undefined::string()|undefined,
          ddb_scheme="https://"::string(),
          ddb_host="dynamodb.us-east-1.amazonaws.com"::string(),
          ddb_port=80::non_neg_integer(),
          ddb_retry=fun erlcloud_ddb_impl:retry/1::erlcloud_ddb_impl:retry_fun(),
          ddb_streams_scheme="https://"::string(),
          ddb_streams_host="streams.dynamodb.us-east-1.amazonaws.com"::string(),
          ddb_streams_port=80::non_neg_integer(),
          route53_scheme="https://"::string(),
          route53_host="route53.amazonaws.com"::string(),
          route53_port="80"::string(),
          lambda_scheme="https://"::string(),
          lambda_host="lambda.us-east-1.amazonaws.com"::string(),
          lambda_port=443::non_neg_integer(),
          redshift_scheme="https://"::string(),
          redshift_host="redshift.us-east-1.amazonaws.com"::string(),
          redshift_port=443::non_neg_integer(),
          kinesis_scheme="https://"::string(),
          kinesis_host="kinesis.us-east-1.amazonaws.com"::string(),
          kinesis_port=80::non_neg_integer(),
          kinesis_retry=fun erlcloud_kinesis_impl:retry/2::erlcloud_kinesis_impl:retry_fun(),
          kms_scheme="https://"::string(),
          kms_host="kms.us-east-1.amazonaws.com"::string(),
          kms_port=80::non_neg_integer(),
          inspector_scheme="https://"::string(),
          inspector_host="inspector.us-west-2.amazonaws.com"::string(),
          inspector_port=80::non_neg_integer(),
          cloudtrail_scheme="https://"::string(),
          cloudtrail_host="cloudtrail.amazonaws.com"::string(),
          cloudtrail_port=80::non_neg_integer(),
          autoscaling_scheme="https://"::string(),
          autoscaling_host="autoscaling.us-east-1.amazonaws.com"::string(),
          autoscaling_port=80::non_neg_integer(),
          directconnect_scheme="https://"::string(),
          directconnect_host="directconnect.us-east-1.amazonaws.com"::string(),
          directconnect_port=80::non_neg_integer(),
          cloudsearch_scheme="https://"::string(),
          cloudsearch_host="cloudsearch.us-east-1.amazonaws.com"::string(),
          cloudsearch_port=443::non_neg_integer(),
          cloudformation_host="cloudformation.us-east-1.amazonaws.com"::string(),
          waf_scheme="https://"::string(),
          waf_host="waf.amazonaws.com"::string(),
          waf_port=443::non_neg_integer(),
          access_key_id::string()|undefined|false,
          secret_access_key::string()|undefined|false,
          security_token=undefined::string()|undefined,
          %% epoch seconds when temporary credentials will expire
          expiration=undefined :: pos_integer()|undefined,
          %% Network request timeout; if not specifed, the default timeout will be used:
          %% ddb: 1s for initial call, 10s for subsequence;
          %% s3:delete_objects_batch/{2,3}, cloudtrail: 1s;
          %% other services: 10s.
          timeout=undefined::timeout()|undefined,
          cloudtrail_raw_result=false::boolean(),
          http_client=lhttpc::erlcloud_httpc:request_fun(), %% If using hackney, ensure that it is started.
          hackney_pool=default::atom(), %% The name of the http request pool hackney should use.
          lhttpc_pool=undefined::atom(), %% The name of the http request pool lhttpc should use.
          %% Default to not retry failures (for backwards compatability).
          %% Recommended to be set to default_retry to provide recommended retry behavior.
          %% Currently only affects S3 and service modules which use erlcloud_aws
          %% for issuing HTTP request to AWS, but intent is to change other services to use this as well.
          %% If you provide a custom function be aware of this anticipated change.
          %% See erlcloud_retry for full documentation.
          retry=fun erlcloud_retry:no_retry/1::erlcloud_retry:retry_fun(),
          %% Currently matches DynamoDB retry
          %% It's likely this is too many retries for other services
          retry_num=10::non_neg_integer()
         }).
-type(aws_config() :: #aws_config{}).

-record(aws_request,
        {
          %% Provided by requesting service
          service :: atom(),
          uri :: string() | binary(),
          method :: atom(),
          request_headers :: [{string(), string()}],
          request_body :: binary(),

          %% Read from response
          attempt = 0 :: integer(),
          response_type :: ok | error | undefined,
          error_type :: aws | httpc | undefined,
          httpc_error_reason :: term() | undefined,
          response_status :: pos_integer() | undefined,
          response_status_line :: string() | undefined,
          response_headers :: [{string(), string()}] | undefined,
          response_body :: binary() | undefined,

          %% Service specific error information
          should_retry :: boolean() | undefined
        }).
-type(aws_request() :: #aws_request{}).
