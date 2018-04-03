%%%-------------------------------------------------------------------
%%% @author <Kirill Starikov kstarikov@alertlogic.com>
%%% @copyright (C) 2016, <Alert Logic>
%%% @doc
%%%
%%% @end
%%% Created : 15. Feb 2016 22:21
%%%-------------------------------------------------------------------
-author("<Kirill Starikov kstarikov@alertlogic.com>").

-define(S3_BUCKET_EVENT_XML_CONFIG,
    <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<NotificationConfiguration xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">
    <TopicConfiguration><Id>Coolname</Id>
    <Topic>arn:aws:sns:us-east-1:000000000000:someuser_test</Topic>
    <Event>s3:ObjectRemoved:Delete</Event>
    <Event>s3:ObjectCreated:CompleteMultipartUpload</Event>
    <Event>s3:ObjectCreated:Post</Event>
    <Filter><S3Key><FilterRule><Name>Prefix</Name><Value>images/</Value></FilterRule><FilterRule><Name>Suffix</Name><Value>jpg</Value></FilterRule></S3Key></Filter>
    </TopicConfiguration><QueueConfiguration><Id>secondSQSNotification</Id><Queue>arn:aws:sqs:us-east-1:000000000000:someuser-second-sqs-queue</Queue>
    <Event>s3:ObjectCreated:Post</Event>
    <Filter><S3Key><FilterRule><Name>Prefix</Name><Value>images/</Value></FilterRule><FilterRule><Name>Suffix</Name><Value>bmp</Value></FilterRule></S3Key></Filter>
    </QueueConfiguration><CloudFunctionConfiguration><Id>meowpeow</Id><CloudFunction>arn:aws:lambda:us-east-1:000000000000:function:s3collectTestFunction</CloudFunction>
    <Event>s3:ObjectRemoved:Delete</Event><Filter><S3Key><FilterRule><Name>Prefix</Name>
    <Value>images/</Value></FilterRule><FilterRule><Name>Suffix</Name><Value>ico</Value>
    </FilterRule></S3Key></Filter></CloudFunctionConfiguration></NotificationConfiguration>">>).

-define(S3_BUCKET_EVENT_XML_CONFIG_NO_SUFFIX,
    <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<NotificationConfiguration xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">
    <TopicConfiguration><Id>Coolname</Id>
    <Topic>arn:aws:sns:us-east-1:000000000000:someuser_test</Topic>
    <Event>s3:ObjectRemoved:Delete</Event>
    <Event>s3:ObjectCreated:CompleteMultipartUpload</Event>
    <Event>s3:ObjectCreated:Post</Event>
    <Filter><S3Key><FilterRule><Name>Prefix</Name><Value>images/</Value></FilterRule></S3Key></Filter>
    </TopicConfiguration><QueueConfiguration><Id>secondSQSNotification</Id><Queue>arn:aws:sqs:us-east-1:000000000000:someuser-second-sqs-queue</Queue>
    <Event>s3:ObjectCreated:Post</Event>
    <Filter><S3Key><FilterRule><Name>Prefix</Name><Value>images/</Value></FilterRule><FilterRule><Name>Suffix</Name><Value>bmp</Value></FilterRule></S3Key></Filter>
    </QueueConfiguration><CloudFunctionConfiguration><Id>meowpeow</Id><CloudFunction>arn:aws:lambda:us-east-1:000000000000:function:s3collectTestFunction</CloudFunction>
    <Event>s3:ObjectRemoved:Delete</Event><Filter><S3Key><FilterRule><Name>Prefix</Name>
    <Value>images/</Value></FilterRule><FilterRule><Name>Suffix</Name><Value>ico</Value>
    </FilterRule></S3Key></Filter></CloudFunctionConfiguration></NotificationConfiguration>">>).

-define(S3_BUCKET_EVENT_XML_CONFIG_NO_PREFIX,
    <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<NotificationConfiguration xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">
    <TopicConfiguration><Id>Coolname</Id>
    <Topic>arn:aws:sns:us-east-1:000000000000:someuser_test</Topic>
    <Event>s3:ObjectRemoved:Delete</Event>
    <Event>s3:ObjectCreated:CompleteMultipartUpload</Event>
    <Event>s3:ObjectCreated:Post</Event>
    <Filter><S3Key><FilterRule><Name>Suffix</Name><Value>jpg</Value></FilterRule></S3Key></Filter>
    </TopicConfiguration><QueueConfiguration><Id>secondSQSNotification</Id><Queue>arn:aws:sqs:us-east-1:000000000000:someuser-second-sqs-queue</Queue>
    <Event>s3:ObjectCreated:Post</Event>
    <Filter><S3Key><FilterRule><Name>Prefix</Name><Value>images/</Value></FilterRule><FilterRule><Name>Suffix</Name><Value>bmp</Value></FilterRule></S3Key></Filter>
    </QueueConfiguration><CloudFunctionConfiguration><Id>meowpeow</Id><CloudFunction>arn:aws:lambda:us-east-1:000000000000:function:s3collectTestFunction</CloudFunction>
    <Event>s3:ObjectRemoved:Delete</Event><Filter><S3Key><FilterRule><Name>Prefix</Name>
    <Value>images/</Value></FilterRule><FilterRule><Name>Suffix</Name><Value>ico</Value>
    </FilterRule></S3Key></Filter></CloudFunctionConfiguration></NotificationConfiguration>">>).

-define(S3_BUCKET_EVENTS_SIMPLE_XML_FORM,
    {'NotificationConfiguration',[
        {'TopicConfiguration',[
            {'Filter',[{'S3Key',[{'FilterRule',[{'Name',["Prefix"]},
                {'Value',["images/"]}]},
                {'FilterRule',[{'Name',["Suffix"]},{'Value',["jpg"]}]}]}]},
            {'Topic',["arn:aws:sns:us-east-1:000000000000:someuser_test"]},
            {'Id',["Coolname"]},
            {'Event',["s3:ObjectRemoved:Delete"]},
            {'Event',["s3:ObjectCreated:CompleteMultipartUpload"]},
            {'Event',["s3:ObjectCreated:Post"]} ]},
        {'QueueConfiguration',[
            {'Filter',[{'S3Key',[{'FilterRule',[{'Name',["Prefix"]},
                {'Value',["images/"]}]},
                {'FilterRule',[{'Name',["Suffix"]},{'Value',["bmp"]}]}]}]},
            {'Queue',["arn:aws:sqs:us-east-1:000000000000:someuser-second-sqs-queue"]},
            {'Id',["secondSQSNotification"]},
            {'Event',["s3:ObjectCreated:Post"]}]},
        {'CloudFunctionConfiguration',[
            {'Filter',[{'S3Key',[{'FilterRule',[{'Name',["Prefix"]},
                {'Value',["images/"]}]},
                {'FilterRule',[{'Name',["Suffix"]},
                    {'Value',["ico"]}]}]}]},
            {'CloudFunction',["arn:aws:lambda:us-east-1:000000000000:function:s3collectTestFunction"]},
            {'Id',["meowpeow"]},
            {'Event',["s3:ObjectRemoved:Delete"]}]}]}).

-define(S3_BUCKET_EVENTS_LIST,
    [
        [{topic_configuration, [
            {filter, [{prefix,"images/"}, {suffix,"jpg"}]},
            {topic,"arn:aws:sns:us-east-1:000000000000:someuser_test"},
            {id,"Coolname"},
            {event,["s3:ObjectRemoved:Delete",
                "s3:ObjectCreated:CompleteMultipartUpload",
                "s3:ObjectCreated:Post"]}
        ]}],
        [{queue_configuration, [
            {filter, [{prefix,"images/"}, {suffix,"bmp"}]},
            {queue,"arn:aws:sqs:us-east-1:000000000000:someuser-second-sqs-queue"},
            {id,"secondSQSNotification"},
            {event,["s3:ObjectCreated:Post"]}]}],
        [{cloud_function_configuration, [
            {filter, [{prefix,"images/"}, {suffix,"ico"}]},
            {cloud_function,"arn:aws:lambda:us-east-1:000000000000:function:s3collectTestFunction"},
            {id,"meowpeow"},
            {event,["s3:ObjectRemoved:Delete"]}]}]]).

-define(S3_BUCKET_EVENTS_LIST_NO_SUFFIX,
    [
        [{topic_configuration, [
            {filter, [{prefix,"images/"}]},
            {topic,"arn:aws:sns:us-east-1:000000000000:someuser_test"},
            {id,"Coolname"},
            {event,["s3:ObjectRemoved:Delete",
                "s3:ObjectCreated:CompleteMultipartUpload",
                "s3:ObjectCreated:Post"]}
        ]}],
        [{queue_configuration, [
            {filter, [{prefix,"images/"}, {suffix,"bmp"}]},
            {queue,"arn:aws:sqs:us-east-1:000000000000:someuser-second-sqs-queue"},
            {id,"secondSQSNotification"},
            {event,["s3:ObjectCreated:Post"]}]}],
        [{cloud_function_configuration, [
            {filter, [{prefix,"images/"}, {suffix,"ico"}]},
            {cloud_function,"arn:aws:lambda:us-east-1:000000000000:function:s3collectTestFunction"},
            {id,"meowpeow"},
            {event,["s3:ObjectRemoved:Delete"]}]}]]).

-define(S3_BUCKET_EVENTS_LIST_NO_PREFIX,
    [
        [{topic_configuration, [
            {filter, [{suffix,"jpg"}]},
            {topic,"arn:aws:sns:us-east-1:000000000000:someuser_test"},
            {id,"Coolname"},
            {event,["s3:ObjectRemoved:Delete",
                    "s3:ObjectCreated:CompleteMultipartUpload",
                    "s3:ObjectCreated:Post"]}
            ]}],
        [{queue_configuration, [
            {filter, [{prefix,"images/"}, {suffix,"bmp"}]},
            {queue,"arn:aws:sqs:us-east-1:000000000000:someuser-second-sqs-queue"},
            {id,"secondSQSNotification"},
            {event,["s3:ObjectCreated:Post"]}]}],
        [{cloud_function_configuration, [
            {filter, [{prefix,"images/"}, {suffix,"ico"}]},
            {cloud_function,"arn:aws:lambda:us-east-1:000000000000:function:s3collectTestFunction"},
            {id,"meowpeow"},
            {event,["s3:ObjectRemoved:Delete"]}]}]]).

-define(S3_BUCKET_ENCRYPTION,
    <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n
       <ServerSideEncryptionConfiguration xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">
       <Rule><ApplyServerSideEncryptionByDefault>
       <SSEAlgorithm>aws:kms</SSEAlgorithm>
       <KMSMasterKeyID>arn:aws:kms:us-east-1:1234/5678example</KMSMasterKeyID>
       </ApplyServerSideEncryptionByDefault></Rule>
       </ServerSideEncryptionConfiguration>">>
).

-define(S3_BUCKET_ENCRYPTION_NOT_FOUND,
    <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<Error>
       <Code>ServerSideEncryptionConfigurationNotFoundError</Code>
       <Message>The server side encryption configuration was not found</Message>
       <BucketName>an-tst</BucketName>
       <RequestId>07E119038B9C6DEA</RequestId>
       <HostId>M0Ku/hb8gttb6U+RBk/P7m0=</HostId>
       </Error>">>
).
