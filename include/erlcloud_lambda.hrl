-ifndef(erlcloud_lambda_hrl).
-define(erlcloud_lambda_hrl, 0).

-record(erlcloud_lambda_code, {s3Bucket, s3Key, s3ObjectVersion, zipFile}).
-type(erlcloud_lambda_code() :: #erlcloud_lambda_code{}).

-endif.