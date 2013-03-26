# erlcloud: Cloud Computing APIs For Erlang #

This is version 0.8.0.  The API is subject to change.

Service APIs implemented:

- Amazon Elastic Compute Cloud (EC2)
- Amazon Simple Storage Service (S3)
- Amazon Simple Queue Service (SQS)
- Amazon SimpleDB
- Amazon Mechanical Turk
- Amazon CloudWatch (MON)
- Amazon DynamoDB (DDB)

All API functions have been implemented.  Not all functions
have been thoroughly tested, so exercise care when integrating
this library into production code.  Please send bug reports and patches.

The libraries can be used two ways: either you can specify configuration
parameters in the process dictionary, or you can create a configuration object
and pass that to each request as the final parameter.

## Getting started ##

If you're using erlcloud in your applicaiton, add it as a dependency
in your application's configuration file.  To use erlcloud in the shell,
you can ensure that all its dependencies have been started by calling:

```
erlcloud:start().
```


Per-process configuration:

```
erlcloud_ec2:configure(AccessKeyId, SecretAccessKey [, Hostname])
```

Hostname defaults to "ec2.amazonaws.com".

Then you can simply call, e.g. `erlcloud_ec2:describe_images()`.

You don't need to call erlcloud_ec2:configure() if you provide your credentials
in the environmental variables

```
AWS_ACCESS_KEY_ID
AWS_SECRET_ACCESS_KEY
```

Configuration object usage:

```
EC2 = erlcloud_ec2:new(AccessKeyId, SecretAccessKey [, Hostname])
erlcloud_ec2:describe_images(EC2).
```

Creating an EC2 instance may look like this:
```
start_instance(Ami, KeyPair, UserData, Type, Zone) ->
    Config = #aws_config{
            access_key_id = application:get_env(aws_key),
            secret_access_key = application:get_env(aws_secret)
           },

    InstanceSpec = #ec2_instance_spec{image_id = Ami,
                                      key_name = KeyPair,
                                      instance_type = Type,
                                      availability_zone = Zone,
                                      user_data = UserData},
    erlcloud_ec2:run_instances(InstanceSpec, Config).
```

For usage information, consult the source code and refer to the API reference at:

- http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Welcome.html
- http://docs.aws.amazon.com/AmazonS3/latest/API/Welcome.html
- http://docs.amazonwebservices.com/AmazonSimpleDB/latest/DeveloperGuide/

## Roadmap ##

v0.8.0
* Existing code

v0.9.0
* Make unit tests useful and improve documentation

v1.0.0
* Refactored code for better return values (issue #7)

## Notes ##

Indentation in contributions should follow indentation style of
surrounding text. In general it follows default indentation rules of
official erlang-mode as provided by OTP team.
