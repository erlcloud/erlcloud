erlcloud: Cloud Computing APIs For Erlang

This is version 0.4.0.  The API is subject to change.

Service APIs implemented:

- Amazon Elastic Compute Cloud (EC2)
- Amazon Simple Storage Service (S3)
- Amazon Simple Queue Service (SQS)
- Amazon SimpleDB
- Amazon Mechanical Turk
- Amazon CloudWatch (MON)

All API functions have been implemented.  Not all functions
have been thoroughly tested, so exercise care when integrating
this library into production code.  Please send bug reports and patches.

The libraries can be used two ways: either you can specify configuration
parameters in the process dictionary, or you can create a configuration object
and pass that to each request as the final parameter.

Getting started
---------------

If you're using erlcloud in your applicaiton, add it as a dependency
in your application's configuration file.  To use erlcloud in the shell,
you can ensure that all its dependencies have been started by calling:

> erlcloud:start().


Per-process configuration:

> erlcloud_ec2:configure(AccessKeyId, SecretAccessKey [, Hostname])

Hostname defaults to "ec2.amazonaws.com".

Then you can simply call, e.g. erlcloud_ec2:describe_images().

You don't need to call erlcloud_ec2:configure() if you provide your credentials
in the AMAZON_ACCESS_KEY_ID and AMAZON_SECRET_ACCESS_KEY environmental
variables.



Configuration object usage:

> EC2 = erlcloud_ec2:new(AccessKeyId, SecretAccessKey [, Hostname])

> erlcloud_ec2:describe_images(EC2).


For usage information, consult the source code and refer to the API reference at:

- http://docs.amazonwebservices.com/AWSEC2/2009-11-30/APIReference/
- http://docs.amazonwebservices.com/AmazonS3/2006-03-01/API/
- http://docs.amazonwebservices.com/AmazonSimpleDB/latest/DeveloperGuide/
