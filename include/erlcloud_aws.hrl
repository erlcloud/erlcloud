-record(aws_config, {
          as_host="autoscaling.amazonaws.com"::string(),
          ec2_host="ec2.amazonaws.com"::string(),
          iam_host="iam.amazonaws.com"::string(),
          sts_host="sts.amazonaws.com"::string(),
          s3_scheme="https://"::string(),
          s3_host="s3.amazonaws.com"::string(),
          s3_port=80::non_neg_integer(),
          sdb_host="sdb.amazonaws.com"::string(),
          elb_host="elasticloadbalancing.amazonaws.com"::string(),
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
          kinesis_scheme="https://"::string(),
          kinesis_host="kinesis.us-east-1.amazonaws.com"::string(),
          kinesis_port=80::non_neg_integer(),
          kinesis_retry=fun erlcloud_kinesis_impl:retry/2::erlcloud_kinesis_impl:retry_fun(),
          cloudtrail_scheme="https://"::string(),
          cloudtrail_host="cloudtrail.amazonaws.com"::string(),
          cloudtrail_port=80::non_neg_integer(),
          access_key_id::string()|undefined|false,
          secret_access_key::string()|undefined|false,
          security_token=undefined::string()|undefined,
          timeout=10000::timeout(),
          cloudtrail_raw_result=false::boolean(),
          http_client=lhttpc::erlcloud_httpc:request_fun(), %% If using hackney, ensure that it is started.
          hackney_pool=default::atom(), %% The name of the http request pool hackney should use.
          %% Default to not retry failures (for backwards compatability).
          %% Recommended to be set to default_retry to provide recommended retry behavior.
          %% Currently only affects S3, but intent is to change other services to use this as well.
          %% If you provide a custom function be aware of this anticipated change.
          %% See erlcloud_retry for full documentation.
          retry=fun erlcloud_retry:no_retry/1::erlcloud_retry:retry_fun()
         }).
-type(aws_config() :: #aws_config{}).

-record(aws_request,
        {
          %% Provided by requesting service
          service :: s3,
          uri :: string() | binary(),
          method :: atom(),
          request_headers :: [{string(), string()}],
          request_body :: binary(),

          %% Read from response
          attempt = 0 :: integer(),
          response_type :: ok | error,
          error_type :: aws | httpc,
          httpc_error_reason :: term(),
          response_status :: pos_integer(),
          response_status_line :: string(),
          response_headers :: [{string(), string()}],
          response_body :: binary(),

          %% Service specific error information
          should_retry :: boolean()
        }).
