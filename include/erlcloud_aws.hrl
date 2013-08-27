-record(aws_config, {
          ec2_host="ec2.amazonaws.com"::string(),
          s3_scheme="https://"::string(),
          s3_host="s3.amazonaws.com"::string(),
          s3_port=80::non_neg_integer(),
          sdb_host="sdb.amazonaws.com"::string(),
          elb_host="elasticloadbalancing.amazonaws.com"::string(),
          sqs_host="queue.amazonaws.com"::string(),
          mturk_host="mechanicalturk.amazonaws.com"::string(),
          mon_host="monitoring.amazonaws.com"::string(),
          mon_port=undefined::non_neg_integer()|undefined,
          mon_protocol=undefined::string()|undefined,
          ddb_scheme="https://"::string(),
          ddb_host="dynamodb.us-east-1.amazonaws.com"::string(),
          ddb_port=80::non_neg_integer(),
          ddb_retry=fun erlcloud_ddb_impl:retry/2::erlcloud_ddb_impl:retry_fun(),
          access_key_id::string()|undefined|false,
          secret_access_key::string()|undefined|false,
          security_token=undefined::string()|undefined,
          timeout=10000::timeout()
         }).
-type(aws_config() :: #aws_config{}).
