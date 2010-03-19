-record(aws_config, {host, access_key_id, secret_access_key}).
-type(aws_config() :: #aws_config{}).

-type(proplist() :: [{atom(), term()}]).
-type(datetime() :: {{pos_integer(), 1..12, 1..31}, {0..23, 0..59, 0..60}}).
