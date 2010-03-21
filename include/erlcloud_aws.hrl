-record(aws_config, {
    ec2_host="ec2.amazonaws.com"::string(),
    s3_host="s3.amazonaws.com"::string(),
    sdb_host="sdb.amazonaws.com"::string(),
    mturk_host="mechanicalturk.amazonaws.com"::string(),
    access_key_id::string(),
    secret_access_key::string()
}).
-type(aws_config() :: #aws_config{}).

