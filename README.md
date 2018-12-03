# erlcloud: AWS APIs library for Erlang #

[![Build Status](https://secure.travis-ci.org/erlcloud/erlcloud.png?branch=master)](http://travis-ci.org/erlcloud/erlcloud)

This library is not developed or maintained by AWS thus lots of functionality is still missing comparing to [aws-cli](https://aws.amazon.com/cli/) or [boto](https://github.com/boto/boto).
Required functionality is being added upon request.

Service APIs implemented:
- Amazon Elastic Compute Cloud (EC2)
- Amazon EC2 Container Service (ECS)
- Amazon Simple Storage Service (S3)
- Amazon Simple Queue Service (SQS)
- Amazon SimpleDB
- Amazon Mechanical Turk
- Amazon CloudWatch (MON)
- Amazon CloudSearch
- Amazon Inspector
- Amazon Key Management Service (KMS)
- Amazon DirectConnect
- Amazon DynamoDB & DDB streams (ddb2)
- Amazon Autoscaling (AS)
- Amazon CloudTrail (CT)
- Cloud Formation (CFN)
- Config
- ElasticLoadBalancing (ELB)
- Identity and Access Management (IAM)
- Kinesis
- Glue (Catalog table, Crawlers and Job APIs support)
- Athena
- Step Functions (SF)
- CloudWatch
- MechanicalTurk
- Simple DB (SDB)
- Relational Data Service (RDS)
- Simple Email Service (SES)
- Short Token Service (STS)
- Simple Notification Service (SNS)
- Web Application Firewall (WAF)
- AWS Cost and Usage Report API
- and more to come

Majority of API functions have been implemented.
Not all functions have been thoroughly tested, so exercise care when integrating this library into production code.
Please send issues and patches.

The libraries can be used two ways:
- either you can specify configuration parameters in the process dictionary. Useful for simple tasks
- you can create a configuration object and pass that to each request as the final parameter. Useful for Cross AWS Account access

## Roadmap ##

Below is the library roadmap update along with regular features and fixes.

- 3.0.X
  - ~~Remove R16 support~~ __done__
  - Support maps

- 3.X.X
  - Fix dialyzer findings and make it mandatory for the library
  - Only SigV4 signing and generalised in one module. Keep SigV2 in SBD section only
  - No more `erlang:error()` use and use of regular tuples as error API. Breaking change.

### Major API compatibility changes between 0.13.X and 2.0.x
 - ELB APIs
 - ... list to be filled shortly

### Supported Erlang versions
At the moment we support the following OTP releases:
 - 19.3
 - 20.3
 - 21.1
 
it might still work on 17+ (primariliy due to Erlang maps) but we do not guarantee that. 

## Getting started ##
You need to clone the repository and download rebar/rebar3 (if it's not already available in your path).
```
git clone https://github.com/erlcloud/erlcloud.git
cd erlcloud
wget https://s3.amazonaws.com/rebar3/rebar3
chmod a+x rebar3
```
To compile and run erlcloud
```
make
make run
```

If you're using erlcloud in your application, add it as a dependency in your application's configuration file.
To use erlcloud in the shell, you can start it by calling:

```
application:ensure_all_started(erlcloud).
```
### Using Temporary Security Credentials

The access to AWS resource might be managed through [third-party identity provider](http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_create_for-idp.html).
The access is managed using [temporary security credentials](http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_use-resources.html).

You can provide your amazon credentials in OS environmental variables

```
export AWS_ACCESS_KEY_ID=<Your AWS Access Key>
export AWS_SECRET_ACCESS_KEY=<Your AWS Secret Access Key>
export AWS_SESSION_TOKEN=<Your AWS Security Token>
export AWS_DEFAULT_REGION=<Your region>
```
If you did not provide your amazon credentials in the OS environmental variables, then you need to provide configuration read from your profile:
```
{ok, Conf} = erlcloud_aws:profile().
erlcloud_s3:list_buckets(Conf).
```
Or you can provide them via `erlcloud` application environment variables.
```erlang
application:set_env(erlcloud, aws_access_key_id, "your key"),
application:set_env(erlcloud, aws_secret_access_key, "your secret key"),
application:set_env(erlcloud, aws_security_token, "your token"),
application:set_env(erlcloud, aws_region, "your region"),
```
### Using Access Key ###
You can provide your amazon credentials in environmental variables.
```
export AWS_ACCESS_KEY_ID=<Your AWS Access Key>
export AWS_SECRET_ACCESS_KEY=<Your AWS Secret Access Key>
```
If you did not provide your amazon credentials in the environmental variables, then you need to provide the per-process configuration:
```
erlcloud_ec2:configure(AccessKeyId, SecretAccessKey [, Hostname]).
```
Hostname defaults to non-existing `"ec2.amazonaws.com"` intentionally to avoid mix with US-East-1
Refer to [aws_config](https://github.com/erlcloud/erlcloud/blob/master/include/erlcloud_aws.hrl) for full description of all services configuration.

Configuration object usage:
```
EC2 = erlcloud_ec2:new(AccessKeyId, SecretAccessKey [, Hostname])
erlcloud_ec2:describe_images(EC2).
```

### aws_config
[aws_config](https://github.com/erlcloud/erlcloud/blob/master/include/erlcloud_aws.hrl) record contains many valuable defaults,
such as protocols and ports for AWS services. You can always redefine them by making new `#aws_config{}` record and
changing particular fields, then passing the result to any erlcloud function.
But if you want to change something in runtime this might be tedious and/or not flexible enough.

Alternative approach is to set default fields within the `app.config -> erlcloud -> aws_config` section and
rely on the config, used by all functions by default.

Example of such app.config:

```erlang
[
  {erlcloud, [
      {aws_config, [
          {s3_scheme, "http://"},
          {s3_host, "s3.example.com"}
      ]}
  ]}
].
```


### Basic use ###
Then you can start making api calls, like:
```
erlcloud_ec2:describe_images().
% list buckets of Account stored in config in process dict
% of of the account you are running in.
erlcloud_s3:list_buckets().
erlcloud_s3:list_buckets(erlcloud_aws:default_cfg()).
% List buckets on 3d Account from Conf
erlcloud_s3:list_buckets(Conf).
```

Creating an EC2 instance may look like this:
```erlang
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

For usage information, consult the source code and https://hexdocs.pm/erlcloud.
For detailed API description refer to the AWS references at:

- http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Welcome.html
- http://docs.aws.amazon.com/AmazonS3/latest/API/Welcome.html
- and other services https://aws.amazon.com/documentation/

## Notes ##

Indentation in contributions should follow indentation style of surrounding text.
In general it follows default indentation rules of official erlang-mode as provided by OTP team.

## Best Practices ##

- All interfaces should provide a method for working with non-default config.
- Public interfaces with paging logic should prefer `{ok, Results, Marker}` style to the `{{paged, Marker}, Results}` found in some modules.
In case of records output, tokens should be part of the record.
- Passing next page `NextToken`, `NextMarker` is preferred with `Opts` rather than a fun parameter like found in many modules.
- Public interfaces should normally expose proplists over records. All new modules are preferred to have both. 
- Exposed records are to be used only for complex outputs. Examples to follow: ddb2, ecs.
- Library should not expose any long running or stateful processes - no gen_servers, no caches and etc.
