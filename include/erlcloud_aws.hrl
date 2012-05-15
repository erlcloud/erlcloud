-record(aws_config, {
    ec2_host="ec2.amazonaws.com"::string(),
    s3_host="s3.amazonaws.com"::string(),
    s3_port=80::non_neg_integer(),
    sdb_host="sdb.amazonaws.com"::string(),
    elb_host="elasticloadbalancing.amazonaws.com"::string(),
    sqs_host="queue.amazonaws.com"::string(),
    mturk_host="mechanicalturk.amazonaws.com"::string(),
    mon_host="monitoring.amazonaws.com"::string(),
    access_key_id::string(),
    secret_access_key::string()
}).
-type(aws_config() :: #aws_config{}).

