-module(erlcloud_cognito_user_pools_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlcloud_aws.hrl").

%% API
-export([]).

%% define response macros
-define(EHTTPC, erlcloud_httpc).

-define(USER_POOL_ID, <<"testpool">>).
-define(CLIENT_ID,    <<"id-1">>).
-define(USERNAME,     <<"test.user">>).

-define(LIST_USERS_RESP,
    #{<<"Users">> =>
    [
        #{<<"Attributes">> =>
        [
            #{
                <<"Name">>  => <<"sub">>,
                <<"Value">> => <<"ec64aac9-78da-4c0f-870c-6389a1ef38bf">>},
            #{
                <<"Name">>  => <<"email_verified">>,
                <<"Value">> => <<"true">>},
            #{
                <<"Name">>  => <<"custom:test1">>,
                <<"Value">> => <<"test">>},
            #{
                <<"Name">>  => <<"email">>,
                <<"Value">> => <<"test@fake.email">>},
            #{
                <<"Name">>  => <<"custom:test2">>,
                <<"Value">> => <<"test">>}
        ],
            <<"Enabled">>              => true,
            <<"UserCreateDate">>       => 1632222392.623,
            <<"UserLastModifiedDate">> => 1633509297.315,
            <<"UserStatus">>           => <<"CONFIRMED">>,
            <<"Username">>             => <<"test.user1">>},
        #{<<"Attributes">> =>
        [
            #{
                <<"Name">>  => <<"sub">>,
                <<"Value">> => <<"b23652f5-2340-4137-9a5e-4b40641420eb">>},
            #{
                <<"Name">>  => <<"email_verified">>,
                <<"Value">> => <<"true">>},
            #{
                <<"Name">>  => <<"email">>,
                <<"Value">> => <<"test2@fake.email">>}
        ],
            <<"Enabled">>              => true,
            <<"UserCreateDate">>       => 1631791632.572,
            <<"UserLastModifiedDate">> => 1631791778.698,
            <<"UserStatus">>           => <<"CONFIRMED">>,
            <<"Username">>             => <<"test.user2">>}
    ]}).

-define(LIST_ALL_USERS,
    #{<<"Users">> =>
    [
        #{<<"Attributes">> =>
        [
            #{
                <<"Name">>  => <<"sub">>,
                <<"Value">> => <<"ec64aac9-78da-4c0f-870c-6389a1ef38bf">>},
            #{
                <<"Name">>  => <<"email_verified">>,
                <<"Value">> => <<"true">>},
            #{
                <<"Name">>  => <<"custom:test1">>,
                <<"Value">> => <<"test">>},
            #{
                <<"Name">>  => <<"email">>,
                <<"Value">> => <<"test@fake.email">>},
            #{
                <<"Name">>  => <<"custom:test2">>,
                <<"Value">> => <<"test">>}
        ],
            <<"Enabled">>              => true,
            <<"UserCreateDate">>       => 1632222392.623,
            <<"UserLastModifiedDate">> => 1633509297.315,
            <<"UserStatus">>           => <<"CONFIRMED">>,
            <<"Username">>             => <<"test.user1">>},
        #{<<"Attributes">> =>
        [
            #{
                <<"Name">>  => <<"sub">>,
                <<"Value">> => <<"b23652f5-2340-4137-9a5e-4b40641420eb">>},
            #{
                <<"Name">>  => <<"email_verified">>,
                <<"Value">> => <<"true">>},
            #{
                <<"Name">>  => <<"email">>,
                <<"Value">> => <<"test2@fake.email">>}
        ],
            <<"Enabled">>              => true,
            <<"UserCreateDate">>       => 1631791632.572,
            <<"UserLastModifiedDate">> => 1631791778.698,
            <<"UserStatus">>           => <<"CONFIRMED">>,
            <<"Username">>             => <<"test.user2">>},
        #{<<"Attributes">> =>
        [
            #{
                <<"Name">>  => <<"sub">>,
                <<"Value">> => <<"b23652f5-2340-4137-9a5e-4b40641420eb">>},
            #{
                <<"Name">>  => <<"email_verified">>,
                <<"Value">> => <<"true">>},
            #{
                <<"Name">>  => <<"email">>,
                <<"Value">> => <<"test3@fake.email">>}
        ],
            <<"Enabled">>              => true,
            <<"UserCreateDate">>       => 1631791632.572,
            <<"UserLastModifiedDate">> => 1631791778.698,
            <<"UserStatus">>           => <<"CONFIRMED">>,
            <<"Username">>             => <<"test.user3">>}
    ]}).

-define(ADMIN_LIST_GROUPS_FOR_USERS,
    #{<<"Groups">> =>
    [
        #{
            <<"CreationDate">>     => 1632236861.937,
            <<"GroupName">>        => <<"test">>,
            <<"LastModifiedDate">> => 1632236861.937,
            <<"UserPoolId">>       => ?USER_POOL_ID},
        #{
            <<"CreationDate">>     => 1632231111.404,
            <<"Description">>      => <<"test desc">>,
            <<"GroupName">>        => <<"test2">>,
            <<"LastModifiedDate">> => 1632231111.404,
            <<"UserPoolId">>       => ?USER_POOL_ID}
    ]}).

-define(ADMIN_GET_USER,
    #{<<"Enabled">> => true,
        <<"UserAttributes">> =>
        [
            #{
                <<"Name">>  => <<"sub">>,
                <<"Value">> => <<"ec64aac9-78da-4c0f-870c-6389a1ef38bf">>},
            #{
                <<"Name">>  => <<"email_verified">>,
                <<"Value">> => <<"true">>},
            #{
                <<"Name">>  => <<"custom:test1">>,
                <<"Value">> => <<"test">>},
            #{
                <<"Name">>  => <<"email">>,
                <<"Value">> => <<"test@fake.email">>},
            #{
                <<"Name">>  => <<"custom:test2">>,
                <<"Value">> => <<"test">>}
        ],
        <<"UserCreateDate">>       => 1632222392.623,
        <<"UserLastModifiedDate">> => 1633509297.315,
        <<"UserStatus">>           => <<"CONFIRMED">>,
        <<"Username">>             => ?USERNAME}
).

-define(ADMIN_CREATE_USER,
    #{<<"User">> =>
    #{<<"Attributes">> =>
    [
        #{
            <<"Name">>  => <<"sub">>,
            <<"Value">> => <<"1e172333-dd47-45ee-9da2-7b53d7ff3932">>}
    ],
        <<"Enabled">>              => true,
        <<"UserCreateDate">>       => 1633946606.409,
        <<"UserLastModifiedDate">> => 1633946606.409,
        <<"UserStatus">>           => <<"FORCE_CHANGE_PASSWORD">>,
        <<"Username">>             => ?USERNAME}}
).

-define(CREATE_GROUP,
    #{
        <<"Group">> =>
            #{
                <<"CreationDate">>     => 1633949776.261,
                <<"GroupName">>        => <<"test">>,
                <<"LastModifiedDate">> => 1633949776.261,
                <<"UserPoolId">>       => ?USER_POOL_ID}
    }).

-define(LIST_USER_POOLS,
    #{
        <<"NextToken">> => <<"nextpage">>,
        <<"UserPools">> =>
            [
                #{
                    <<"CreationDate">> => 1634210679.535,
                    <<"Id">> => <<"eu-west-testpool1">>,
                    <<"LambdaConfig">> => #{},
                    <<"LastModifiedDate">> => 1634210679.535,
                    <<"Name">> => ?USER_POOL_ID
                },
                #{
                    <<"CreationDate">> => 1632131813.194,
                    <<"Id">> => <<"eu-west-testpool2">>,
                    <<"LambdaConfig">> => #{},
                    <<"LastModifiedDate">> => 1634209858.344,
                    <<"Name">> => <<"TestPool2">>
                }
            ]
        }).

-define(LIST_ALL_USER_POOLS,
    #{
        <<"UserPools">> =>
        [
            #{
                <<"CreationDate">> => 1634210679.535,
                <<"Id">> => <<"eu-west-testpool1">>,
                <<"LambdaConfig">> => #{},
                <<"LastModifiedDate">> => 1634210679.535,
                <<"Name">> => ?USER_POOL_ID
            },
            #{
                <<"CreationDate">> => 1632131813.194,
                <<"Id">> => <<"eu-west-testpool2">>,
                <<"LambdaConfig">> => #{},
                <<"LastModifiedDate">> => 1634209858.344,
                <<"Name">> => <<"TestPool2">>
            },
            #{
                <<"CreationDate">> => 1634210679.535,
                <<"Id">> => <<"eu-west-testpool3">>,
                <<"LambdaConfig">> => #{},
                <<"LastModifiedDate">> => 1634210679.535,
                <<"Name">> => <<"TestPool3">>
            }
        ]
    }).

-define(DESCRIBE_POOL, #{
    <<"UserPool">> =>
    #{<<"AccountRecoverySetting">> =>
      #{<<"RecoveryMechanisms">> =>
        [#{<<"Name">> => <<"verified_email">>,<<"Priority">> => 1},
         #{<<"Name">> => <<"verified_phone_number">>,
           <<"Priority">> => 2}]},
      <<"AdminCreateUserConfig">> =>
      #{<<"AllowAdminCreateUserOnly">> => true,
        <<"InviteMessageTemplate">> =>
        #{<<"EmailMessage">> =>
          <<"Your username is {username} and temporary password is {####}. ">>,
          <<"EmailSubject">> => <<"Your temporary password">>,
          <<"SMSMessage">> =>
          <<"Your username is {username} and temporary password is {####}. ">>},
        <<"UnusedAccountValidityDays">> => 7},
      <<"Arn">> =>
      <<"arn:aws:cognito-idp:eu-west-1:87806250403242:userpool/testpool">>,
      <<"AutoVerifiedAttributes">> => [<<"email">>],
      <<"CreationDate">> => 1636620065.355,
      <<"Domain">> => <<"nxdomain3323">>,
      <<"EmailConfiguration">> =>
      #{<<"EmailSendingAccount">> => <<"COGNITO_DEFAULT">>},
      <<"EmailVerificationMessage">> =>
      <<"Your verification code is {####}. ">>,
      <<"EmailVerificationSubject">> =>
      <<"Your verification code">>,
      <<"EstimatedNumberOfUsers">> => 3,
      <<"Id">> => <<"eu-west-1_XEBAHPImu">>,
      <<"LambdaConfig">> => #{},
      <<"LastModifiedDate">> => 1636620065.355,
      <<"MfaConfiguration">> => <<"OFF">>,
      <<"Name">> => <<"test-pool">>,
      <<"Policies">> =>
      #{<<"PasswordPolicy">> =>
        #{<<"MinimumLength">> => 8,<<"RequireLowercase">> => true,
          <<"RequireNumbers">> => true,<<"RequireSymbols">> => false,
          <<"RequireUppercase">> => false,
          <<"TemporaryPasswordValidityDays">> => 7}},
      <<"SchemaAttributes">> => [
          #{<<"AttributeDataType">> => <<"String">>,
            <<"DeveloperOnlyAttribute">> => false,<<"Mutable">> => true,
            <<"Name">> => <<"custom:partner">>,<<"Required">> => false,
            <<"StringAttributeConstraints">> =>
            #{<<"MaxLength">> => <<"256">>,<<"MinLength">> => <<"1">>}},
          #{<<"AttributeDataType">> => <<"String">>,
            <<"DeveloperOnlyAttribute">> => false,<<"Mutable">> => true,
            <<"Name">> => <<"custom:organisation">>,
            <<"Required">> => false,
            <<"StringAttributeConstraints">> =>
            #{<<"MaxLength">> => <<"256">>,<<"MinLength">> => <<"1">>}},
          #{<<"AttributeDataType">> => <<"String">>,
            <<"DeveloperOnlyAttribute">> => false,<<"Mutable">> => true,
            <<"Name">> => <<"custom:role">>,<<"Required">> => false,
            <<"StringAttributeConstraints">> =>
            #{<<"MaxLength">> => <<"256">>,<<"MinLength">> => <<"1">>}},
          #{<<"AttributeDataType">> => <<"String">>,
            <<"DeveloperOnlyAttribute">> => false,<<"Mutable">> => true,
            <<"Name">> => <<"custom:viewMode">>,<<"Required">> => false,
            <<"StringAttributeConstraints">> =>
            #{<<"MaxLength">> => <<"256">>,<<"MinLength">> => <<"1">>}}
      ],
      <<"SmsAuthenticationMessage">> => <<"Your authentication code is {####}. ">>,
      <<"SmsVerificationMessage">> => <<"Your verification code is {####}. ">>,
      <<"UserPoolTags">> => #{},
      <<"UsernameAttributes">> => [<<"email">>],
      <<"UsernameConfiguration">> => #{<<"CaseSensitive">> => false},
      <<"VerificationMessageTemplate">> => #{
          <<"DefaultEmailOption">> => <<"CONFIRM_WITH_CODE">>,
          <<"EmailMessage">> =>
          <<"Your verification code is {####}. ">>,
          <<"EmailSubject">> => <<"Your verification code">>,
          <<"SmsMessage">> =>
          <<"Your verification code is {####}. ">>
      }}
}).

-define(MFA_CONFIG,
    #{
        <<"MfaConfiguration">> => <<"ON">>,
        <<"SoftwareTokenMfaConfiguration">> => #{<<"Enabled">> => true}
    }).

-define(LIST_IDENTITY_PROVIDERS,
    #{<<"Providers">> => [#{<<"CreationDate">>     => 1636620575.033,
                            <<"LastModifiedDate">> => 1636620730.0,
                            <<"ProviderName">>     => <<"test">>,
                            <<"ProviderType">>      => <<"SAML">>}],
      <<"NextToken">> => <<"nextpage">>}).

-define(LIST_ALL_IDENTITY_PROVIDERS,
    #{<<"Providers">> =>
    [
        #{
            <<"CreationDate">> => 1636620575.033,
            <<"LastModifiedDate">> => 1636620730.0,
            <<"ProviderName">> => <<"test">>,
            <<"ProviderType">> => <<"SAML">>
        },
        #{
            <<"CreationDate">> => 1636620577.033,
            <<"LastModifiedDate">> => 1636620730.0,
            <<"ProviderName">> => <<"NewOauth">>,
            <<"ProviderType">> => <<"SAML">>
        }
    ]}
    ).

-define(DESCRIBE_PROVIDER,
    #{
        <<"IdentityProvider">> =>
        #{
            <<"AttributeMapping">> =>
            #{
                <<"email">> =>
                <<"http://schemas.xmlsoap.org/ws/2005/05/identity/claims/emailaddress">>},
            <<"CreationDate">> => 1636620575.033,
            <<"IdpIdentifiers">> => [],
            <<"LastModifiedDate">> => 1636620730.0,
            <<"ProviderDetails">> =>
            #{<<"IDPSignout">> => <<"false">>,
                <<"MetadataFile">> => <<"meta xml">>,
                <<"SLORedirectBindingURI">> =>
                <<"https://login.microsoftonline.com/test/saml2">>,
                <<"SSORedirectBindingURI">> =>
                <<"https://login.microsoftonline.com/test/saml2">>},
            <<"ProviderName">> => <<"test">>,
            <<"ProviderType">> => <<"SAML">>,
            <<"UserPoolId">> => <<"us-east-2_test">>}}).

-define(DESCRIBE_USER_POOL_CLIENT,
    #{<<"UserPoolClient">> =>
      #{<<"AccessTokenValidity">> => 60,
        <<"AllowedOAuthFlows">> => [<<"code">>,<<"implicit">>],
        <<"AllowedOAuthFlowsUserPoolClient">> => true,
        <<"AllowedOAuthScopes">> =>
        [<<"aws.cognito.signin.user.admin">>,<<"email">>,
         <<"openid">>,<<"phone">>],
        <<"CallbackURLs">> => [<<"https://test.dev:30102/login">>],
        <<"ClientId">> => ?CLIENT_ID,
        <<"ClientName">> => <<"test">>,
        <<"CreationDate">> => 1635782978.411,
        <<"EnableTokenRevocation">> => true,
        <<"ExplicitAuthFlows">> =>
        [<<"ALLOW_ADMIN_USER_PASSWORD_AUTH">>,
         <<"ALLOW_CUSTOM_AUTH">>,<<"ALLOW_REFRESH_TOKEN_AUTH">>,
         <<"ALLOW_USER_SRP_AUTH">>],
        <<"IdTokenValidity">> => 60,
        <<"LastModifiedDate">> => 1636620585.842,
        <<"LogoutURLs">> => [<<"https://test.dev:30102/logout">>],
        <<"PreventUserExistenceErrors">> => <<"ENABLED">>,
        <<"ReadAttributes">> =>
        [<<"address">>,<<"birthdate">>,<<"email">>,
         <<"email_verified">>,<<"family_name">>,<<"gender">>,
         <<"given_name">>,<<"locale">>,<<"middle_name">>,<<"name">>,
         <<"nickname">>,<<"phone_number">>,
         <<"phone_number_verified">>,<<"picture">>,
         <<"preferred_username">>,<<"profile">>,<<"updated_at">>,
         <<"website">>,<<"zoneinfo">>],
        <<"RefreshTokenValidity">> => 30,
        <<"SupportedIdentityProviders">> =>
        [<<"ActualExperience">>,<<"COGNITO">>],
        <<"TokenValidityUnits">> =>
        #{<<"AccessToken">> => <<"minutes">>,
          <<"IdToken">> => <<"minutes">>,
          <<"RefreshToken">> => <<"days">>},
        <<"UserPoolId">> => <<"us-east-2_test">>,
        <<"WriteAttributes">> =>
        [<<"address">>,<<"birthdate">>,<<"email">>,
         <<"family_name">>,<<"gender">>,<<"given_name">>,
         <<"locale">>,<<"middle_name">>,<<"name">>,<<"nickname">>,
         <<"phone_number">>,<<"picture">>,<<"preferred_username">>,
         <<"profile">>,<<"updated_at">>,<<"website">>,
         <<"zoneinfo">>]}}
).

-define(LIST_USER_POOL_CLIENTS, #{
    <<"UserPoolClients">> => [
        #{<<"ClientId">>   => ?CLIENT_ID,
          <<"ClientName">> => <<"name-1">>,
          <<"UserPoolId">> => ?USER_POOL_ID}
    ]
}).

-define(LIST_ALL_USER_POOL_CLIENTS, #{
    <<"UserPoolClients">> => [
        #{<<"ClientId">>   => ?CLIENT_ID,
          <<"ClientName">> => <<"name-1">>,
          <<"UserPoolId">> => ?USER_POOL_ID},
        #{<<"ClientId">>   => ?CLIENT_ID,
          <<"ClientName">> => <<"name-1">>,
          <<"UserPoolId">> => ?USER_POOL_ID}
    ]
}).

-define(ADMIN_LIST_DEVICE,
    #{
        <<"Devices">> => [
            #{
                <<"DeviceAttributes">> => [
                    #{
                        <<"Name">> => <<"test">>,
                        <<"Value">> => <<"testvalue">>
                    }
                ],
                <<"DeviceCreateDate">> => 1635782978.411,
                <<"DeviceKey">> => <<"testKey">>,
                <<"DeviceLastAuthenticatedDate">> => 1635782978.411,
                <<"DeviceLastModifiedDate">> => 1635782978.411
            }
        ]
    }
    ).

-define(LIST_ALL_DEVICES,
    #{
        <<"Devices">> => [
            #{
                <<"DeviceAttributes">> => [
                    #{
                        <<"Name">> => <<"test">>,
                        <<"Value">> => <<"testvalue">>
                    }
                ],
                <<"DeviceCreateDate">> => 1635782978.411,
                <<"DeviceKey">> => <<"testKey">>,
                <<"DeviceLastAuthenticatedDate">> => 1635782978.411,
                <<"DeviceLastModifiedDate">> => 1635782978.411
            },
            #{
                <<"DeviceAttributes">> => [
                    #{
                        <<"Name">> => <<"test">>,
                        <<"Value">> => <<"testvalue">>
                    }
                ],
                <<"DeviceCreateDate">> => 1635782978.411,
                <<"DeviceKey">> => <<"testKey2">>,
                <<"DeviceLastAuthenticatedDate">> => 1635782978.411,
                <<"DeviceLastModifiedDate">> => 1635782978.411
            }
        ]
    }
).

-define(ADMIN_INITIATE_AUTH, #{
  <<"ChallengeName">>       => <<"SOFTWARE_TOKEN_MFA">>,
  <<"ChallengeParameters">> => #{<<"USER_ID_FOR_SRP">> => <<"id">>},
  <<"Session">>             => <<"session-token">>
}).

-define(RESPOND_TO_AUTH_CHALLENGE, #{
  <<"AuthenticationResult">> => #{
    <<"AccessToken">>       => <<"access-token">>,
    <<"ExpiresIn">>         => 1800,
    <<"IdToken">>           => <<"id-token">>,
    <<"RefreshToken">>      => <<"refresh-token">>,
    <<"TokenType">>         => <<"Bearer">>,
    <<"NewDeviceMetadata">> => #{
      <<"DeviceGroupKey">> => <<"-rowRNkJw">>,
      <<"DeviceKey">>      => <<"eu-west-1_b6657eba-2850-4a47-9357-fae69fd86b94">>
    }
  },
  <<"ChallengeParameters">> => #{}
}).

-define(CREATE_UPDATE_IDP, #{
    <<"IdentityProvider">>   => #{
      <<"AttributeMapping">> => #{
        <<"testAttr">> => <<"SAMLAttr">>
      },
      <<"CreationDate">>     => 1635782978.411,
      <<"IdpIdentifiers">>   => [ "Test IDP" ],
      <<"LastModifiedDate">> => 1635782978.411,
      <<"ProviderDetails">>  => #{
        <<"MetadataURL">> => <<"http://test-idp.com/1234">>
      },
      <<"ProviderName">>     => <<"TestIdp">>,
      <<"ProviderType">>     => <<"SAML">>,
      <<"UserPoolId">>       => <<"test">>
    }
  }).

config() ->
    #aws_config{access_key_id     = "id",
                secret_access_key = "key",
                retry             = fun erlcloud_retry:default_retry/1,
                retry_num         = 3}.

setup() ->
    erlcloud_cognito_user_pools:configure("id", "key"),
    MockedModules = [?EHTTPC, erlcloud_aws],
    meck:new(MockedModules, [passthrough]),
    meck:expect(erlcloud_aws, update_config, 1, {ok, config()}),
    meck:expect(?EHTTPC, request, 6, fun do_erlcloud_httpc_request/6),
    MockedModules.

erlcloud_cognito_user_pools_test_() ->
    {
        foreach,
        fun setup/0,
        fun meck:unload/1,
        [
            fun test_list_users/0,
            fun test_list_all_users/0,
            fun test_admin_list_groups_for_user/0,
            fun test_admin_get_user/0,
            fun test_admin_create_user/0,
            fun test_admin_delete_user/0,
            fun test_admin_add_user_to_group/0,
            fun test_admin_remove_user_from_group/0,
            fun test_create_group/0,
            fun test_delete_group/0,
            fun test_admin_reset_user_password/0,
            fun test_admin_update_user_attributes/0,
            fun test_change_password/0,
            fun test_list_user_pools/0,
            fun test_list_all_user_pools/0,
            fun test_admin_set_user_password/0,
            fun test_describe_user_pool/0,
            fun test_get_user_pool_mfa_config/0,
            fun test_list_identity_providers/0,
            fun test_list_all_identity_provider/0,
            fun test_describe_identity_provider/0,
            fun test_describe_user_pool_client/0,
            fun test_list_user_pool_clients/0,
            fun test_list_all_user_pool_clients/0,
            fun test_admin_list_devices/0,
            fun test_list_all_devices/0,
            fun test_admin_forget_device/0,
            fun test_admin_confirm_signup/0,
            fun test_admin_initiate_auth/0,
            fun test_respond_to_auth_challenge/0,
            fun test_create_identity_provider/0,
            fun test_delete_identity_provider/0,
            fun test_update_identity_provider/0,
            fun test_error_no_retry/0,
            fun test_error_retry/0
        ]
    }.

test_list_users() ->
    Request  = #{<<"UserPoolId">> => ?USER_POOL_ID},
    Expected = {ok, ?LIST_USERS_RESP},
    TestFun  = fun() -> erlcloud_cognito_user_pools:list_users(?USER_POOL_ID) end,
    do_test(Request, Expected, TestFun).

test_list_all_users() ->
    Mocked1 = ?LIST_USERS_RESP#{<<"PaginationToken">> => "1"},
    Mocked2 = #{<<"Users">> => [
        #{
            <<"Attributes">> =>
            [
                #{<<"Name">>  => <<"sub">>,
                  <<"Value">> => <<"b23652f5-2340-4137-9a5e-4b40641420eb">>},
                #{<<"Name">>  => <<"email_verified">>,
                  <<"Value">> => <<"true">>},
                #{<<"Name">>  => <<"email">>,
                  <<"Value">> => <<"test3@fake.email">>}
            ],
            <<"Enabled">>              => true,
            <<"UserCreateDate">>       => 1631791632.572,
            <<"UserLastModifiedDate">> => 1631791778.698,
            <<"UserStatus">>           => <<"CONFIRMED">>,
            <<"Username">>             => <<"test.user3">>}
    ]},
    meck:sequence(?EHTTPC, request, 6, [{ok, {{200, "OK"}, [], jsx:encode(Mocked1)}},
        {ok, {{200, "OK"}, [], jsx:encode(Mocked2)}}]),
    Expected = {ok, ?LIST_ALL_USERS},
    Response = erlcloud_cognito_user_pools:list_all_users(?USER_POOL_ID),
    ?assertEqual(Expected, Response).

test_admin_list_groups_for_user() ->
    Request = #{
        <<"Username">>   => ?USERNAME,
        <<"UserPoolId">> => ?USER_POOL_ID
    },
    Expected = {ok, ?ADMIN_LIST_GROUPS_FOR_USERS},
    TestFun = fun() ->
        erlcloud_cognito_user_pools:admin_list_groups_for_user(?USERNAME, ?USER_POOL_ID, config())
              end,
    do_test(Request, Expected, TestFun).

test_admin_get_user() ->
    Request = #{
        <<"Username">>   => ?USERNAME,
        <<"UserPoolId">> => ?USER_POOL_ID
    },
    Expected = {ok, ?ADMIN_GET_USER},
    TestFun = fun() ->
        erlcloud_cognito_user_pools:admin_get_user(?USERNAME, ?USER_POOL_ID, config())
              end,
    do_test(Request, Expected, TestFun).

test_admin_create_user() ->
    UserAttributes = #{
        <<"UserAttributes">> => [
            #{<<"Name">>  => <<"custom:test">>,
              <<"Value">> => <<"test">>},
            #{<<"Name">>  => <<"custom:test2">>,
              <<"Value">> => <<"test2">>}
        ]
    },
    Request = UserAttributes#{<<"Username">>   => ?USERNAME,
                              <<"UserPoolId">> => ?USER_POOL_ID},
    Expected = {ok, ?ADMIN_CREATE_USER},
    TestFun = fun() ->
        erlcloud_cognito_user_pools:admin_create_user(?USERNAME, ?USER_POOL_ID, UserAttributes)
              end,
    do_test(Request, Expected, TestFun).

test_admin_delete_user() ->
    Request = #{
        <<"Username">>   => ?USERNAME,
        <<"UserPoolId">> => ?USER_POOL_ID
    },
    TestFun = fun() ->
        erlcloud_cognito_user_pools:admin_delete_user(?USERNAME, ?USER_POOL_ID, config())
              end,
    do_test(Request, ok, TestFun).

test_admin_add_user_to_group() ->
    GroupName = <<"test">>,
    Request = #{
        <<"Username">>   => ?USERNAME,
        <<"UserPoolId">> => ?USER_POOL_ID,
        <<"GroupName">>  => GroupName},
    TestFun = fun() ->
        erlcloud_cognito_user_pools:admin_add_user_to_group(GroupName, ?USERNAME, ?USER_POOL_ID, config())
              end,
    do_test(Request, ok, TestFun).

test_admin_remove_user_from_group() ->
    GroupName = <<"test">>,
    Request = #{
        <<"Username">>   => ?USERNAME,
        <<"UserPoolId">> => ?USER_POOL_ID,
        <<"GroupName">>  => GroupName},
    TestFun = fun() ->
        erlcloud_cognito_user_pools:admin_remove_user_from_group(GroupName, ?USERNAME, ?USER_POOL_ID, config())
              end,
    do_test(Request, ok, TestFun).

test_create_group() ->
    GroupName = <<"test">>,
    Request = #{
        <<"UserPoolId">> => ?USER_POOL_ID,
        <<"GroupName">>  => GroupName},
    Expected = {ok, ?CREATE_GROUP},
    TestFun = fun() ->
        erlcloud_cognito_user_pools:create_group(GroupName, ?USER_POOL_ID, config())
              end,
    do_test(Request, Expected, TestFun).

test_delete_group() ->
    GroupName = <<"test">>,
    Request = #{
        <<"UserPoolId">> => ?USER_POOL_ID,
        <<"GroupName">>  => GroupName},
    TestFun = fun() ->
        erlcloud_cognito_user_pools:delete_group(GroupName, ?USER_POOL_ID, config())
              end,
    do_test(Request, ok, TestFun).

test_admin_reset_user_password() ->
    Request = #{
        <<"Username">>   => ?USERNAME,
        <<"UserPoolId">> => ?USER_POOL_ID},
    TestFun = fun() ->
      erlcloud_cognito_user_pools:admin_reset_user_password(?USERNAME, ?USER_POOL_ID)
              end,
    do_test(Request, ok, TestFun).

test_admin_update_user_attributes() ->
    Attributes = [
        #{
            <<"Name">>  => <<"custom:partner">>,
            <<"Value">> => <<"new value">>
        }
    ],
    Request = #{
        <<"UserAttributes">>  => Attributes,
        <<"UserPoolId">>      => ?USER_POOL_ID,
        <<"Username">>        => ?USERNAME},
    TestFun = fun() ->
        erlcloud_cognito_user_pools:admin_update_user_attributes(?USERNAME, ?USER_POOL_ID, Attributes)
              end,
    do_test(Request, ok, TestFun).

test_change_password() ->
    AccessToken = <<"test token">>,
    OldPass = <<"old p4ss!">>,
    NewPass = <<"new p4ss!">>,
    Request = #{
        <<"AccessToken">> => AccessToken,
        <<"PreviousPassword">> => OldPass,
        <<"ProposedPassword">> => NewPass
    },
    TestFun = fun() ->
        erlcloud_cognito_user_pools:change_password(OldPass, NewPass, AccessToken)
              end,
    do_test(Request, ok, TestFun).

test_list_user_pools() ->
    Expected = {ok, ?LIST_USER_POOLS},
    MaxResults = 60,
    Request = #{<<"MaxResults">> => MaxResults},
    TestFun = fun() -> erlcloud_cognito_user_pools:list_user_pools(MaxResults) end,
    do_test(Request, Expected, TestFun).

test_error_no_retry() ->
    erlcloud_cognito_user_pools:configure("test-access-key", "test-secret-key"),
    ErrCode = 400,
    Status = "Bad Request",
    ErrMsg = <<"Message">>,
    Operation = "ListUsers",
    Config = config(),
    Request = #{<<"UserPoolId">> => ?USER_POOL_ID},
    meck:expect(?EHTTPC, request, 6, {ok, {{ErrCode, Status}, [], ErrMsg}}),
    ?assertEqual(
        {error, {http_error, ErrCode, Status, ErrMsg, []}},
        erlcloud_cognito_user_pools:request(Config, Operation, Request)
    ).

test_list_all_user_pools() ->
    Mocked1 = ?LIST_USER_POOLS,
    Mocked2 = #{
        <<"UserPools">> =>
        [
            #{
                <<"CreationDate">> => 1634210679.535,
                <<"Id">> => <<"eu-west-testpool3">>,
                <<"LambdaConfig">> => #{},
                <<"LastModifiedDate">> => 1634210679.535,
                <<"Name">> => <<"TestPool3">>
            }
        ]},
    meck:sequence(?EHTTPC, request, 6, [{ok, {{200, "OK"}, [], jsx:encode(Mocked1)}},
        {ok, {{200, "OK"}, [], jsx:encode(Mocked2)}}]),
    Expected = {ok, ?LIST_ALL_USER_POOLS},
    Response = erlcloud_cognito_user_pools:list_all_user_pools(),
    ?assertEqual(Expected, Response).

test_admin_set_user_password() ->
    Pass = <<"test pass">>,
    Request = #{
        <<"Username">>   => ?USERNAME,
        <<"UserPoolId">> => ?USER_POOL_ID,
        <<"Password">>   => Pass,
        <<"Permanent">>  => false},
    TestFun = fun() ->
        erlcloud_cognito_user_pools:admin_set_user_password(?USERNAME, ?USER_POOL_ID, Pass)
              end,
    do_test(Request, ok, TestFun).

test_describe_user_pool() ->
    Request = #{<<"UserPoolId">> => ?USER_POOL_ID},
    Expected = {ok, ?DESCRIBE_POOL},
    TestFun = fun() -> erlcloud_cognito_user_pools:describe_user_pool(?USER_POOL_ID) end,
    do_test(Request, Expected, TestFun).

test_get_user_pool_mfa_config() ->
    Request = #{<<"UserPoolId">> => ?USER_POOL_ID},
    Expected = {ok, ?MFA_CONFIG},
    TestFun = fun() -> erlcloud_cognito_user_pools:get_user_pool_mfa_config(?USER_POOL_ID) end,
    do_test(Request, Expected, TestFun).

test_list_identity_providers() ->
    Request = #{
        <<"UserPoolId">> => ?USER_POOL_ID,
        <<"MaxResults">> => 60
    },
    Expected = {ok, ?LIST_IDENTITY_PROVIDERS},
    TestFun = fun() -> erlcloud_cognito_user_pools:list_identity_providers(?USER_POOL_ID) end,
    do_test(Request, Expected, TestFun).

test_list_all_identity_provider() ->
    Mocked1 = ?LIST_IDENTITY_PROVIDERS,
    Mocked2 = #{
        <<"Providers">> => [
            #{<<"CreationDate">>     => 1636620577.033,
              <<"LastModifiedDate">> => 1636620730.0,
              <<"ProviderName">>     => <<"NewOauth">>,
              <<"ProviderType">>     => <<"SAML">>}
        ]
    },
    meck:sequence(?EHTTPC, request, 6, [{ok, {{200, "OK"}, [], jsx:encode(Mocked1)}},
        {ok, {{200, "OK"}, [], jsx:encode(Mocked2)}}]),
    Expected = {ok, ?LIST_ALL_IDENTITY_PROVIDERS},
    Response = erlcloud_cognito_user_pools:list_all_identity_providers(?USER_POOL_ID),
    ?assertEqual(Expected, Response).

test_describe_identity_provider() ->
    ProviderName = <<"test">>,
    Request = #{
        <<"UserPoolId">>   => ?USER_POOL_ID,
        <<"ProviderName">> => ProviderName
    },
    Expected = {ok, ?DESCRIBE_PROVIDER},
    TestFun = fun() ->
        erlcloud_cognito_user_pools:describe_identity_provider(?USER_POOL_ID, ProviderName)
              end,
    do_test(Request, Expected, TestFun).

test_describe_user_pool_client() ->
    Request = #{
        <<"UserPoolId">> => ?USER_POOL_ID,
        <<"ClientId">>   => ?CLIENT_ID
    },
    Expected = {ok, ?DESCRIBE_USER_POOL_CLIENT},
    TestFun = fun() -> erlcloud_cognito_user_pools:describe_user_pool_client(?USER_POOL_ID, ?CLIENT_ID) end,
    do_test(Request, Expected, TestFun).

test_list_user_pool_clients() ->
    Request  = #{<<"UserPoolId">> => ?USER_POOL_ID,
                 <<"MaxResults">> => 60},
    TestFun  = fun() -> erlcloud_cognito_user_pools:list_user_pool_clients(?USER_POOL_ID) end,
    Expected = {ok, ?LIST_USER_POOL_CLIENTS},
    do_test(Request, Expected, TestFun).

test_list_all_user_pool_clients() ->
    Mocked1 = ?LIST_USER_POOL_CLIENTS#{<<"NextToken">> => <<"next">>},
    Mocked2 = ?LIST_USER_POOL_CLIENTS,
    meck:sequence(?EHTTPC, request, 6, [{ok, {{200, "OK"}, [], jsx:encode(Mocked1)}},
                                        {ok, {{200, "OK"}, [], jsx:encode(Mocked2)}}]),
    Expected = {ok, ?LIST_ALL_USER_POOL_CLIENTS},
    Response = erlcloud_cognito_user_pools:list_all_user_pool_clients(?USER_POOL_ID),
    ?assertEqual(Expected, Response).

test_admin_list_devices() ->
    Request = #{
        <<"UserPoolId">> => ?USER_POOL_ID,
        <<"Username">>   => ?USERNAME,
        <<"Limit">>      => 60
    },
    Expected = {ok, ?ADMIN_LIST_DEVICE},
    TestFun = fun() -> erlcloud_cognito_user_pools:admin_list_devices(?USER_POOL_ID, ?USERNAME) end,
    do_test(Request, Expected, TestFun).

test_list_all_devices() ->
    Mocked1 = ?ADMIN_LIST_DEVICE#{<<"PaginationToken">> => <<"next page">>},
    Devices = hd(maps:get(<<"Devices">>, ?ADMIN_LIST_DEVICE)),
    Mocked2 = ?ADMIN_LIST_DEVICE#{<<"Devices">> => [Devices#{<<"DeviceKey">> => <<"testKey2">>}]},
    meck:sequence(?EHTTPC, request, 6, [{ok, {{200, "OK"}, [], jsx:encode(Mocked1)}},
                                        {ok, {{200, "OK"}, [], jsx:encode(Mocked2)}}]),
    Expected = {ok, ?LIST_ALL_DEVICES},
    Response = erlcloud_cognito_user_pools:admin_list_all_devices(?USER_POOL_ID, ?USERNAME),
    ?assertEqual(Expected, Response).

test_admin_forget_device() ->
    DeviceKey = <<"test key">>,
    Request = #{
        <<"UserPoolId">> => ?USER_POOL_ID,
        <<"Username">>   => ?USERNAME,
        <<"DeviceKey">>  => DeviceKey
    },
    TestFun = fun() -> erlcloud_cognito_user_pools:admin_forget_device(?USER_POOL_ID, ?USERNAME, DeviceKey) end,
    do_test(Request, ok, TestFun).

test_admin_confirm_signup() ->
    Request = #{
        <<"UserPoolId">>     => ?USER_POOL_ID,
        <<"Username">>       => ?USERNAME,
        <<"ClientMetadata">> => #{}
    },
    TestFun = fun() -> erlcloud_cognito_user_pools:admin_confirm_signup(?USER_POOL_ID, ?USERNAME) end,
    do_test(Request, {ok, #{}}, TestFun).

test_admin_initiate_auth() ->
    AuthFlow   = <<"ADMIN_USER_PASSWORD_AUTH">>,
    AuthParams = #{<<"PASSWORD">> => <<"pass">>,
                   <<"USERNAME">> => ?USERNAME},
    Request = #{
        <<"AuthFlow">>       => AuthFlow,
        <<"AuthParameters">> => AuthParams,
        <<"ClientId">>       => ?CLIENT_ID,
        <<"UserPoolId">>     => ?USER_POOL_ID
    },
    TestFun = fun() -> erlcloud_cognito_user_pools:admin_initiate_auth(?USER_POOL_ID, ?CLIENT_ID,
                                                            AuthFlow, AuthParams) end,
    do_test(Request, {ok, ?ADMIN_INITIATE_AUTH}, TestFun).

test_respond_to_auth_challenge() ->
    ChallengeName = <<"SOFTWARE_TOKEN_MFA">>,
    ChallengeResp = #{<<"USERNAME">>                => ?USERNAME,
                      <<"SOFTWARE_TOKEN_MFA_CODE">> => <<"123456">>},
    Session = <<"session-token">>,
    Request = #{
        <<"ChallengeName">>      => ChallengeName,
        <<"ChallengeResponses">> => ChallengeResp,
        <<"ClientId">>           => ?CLIENT_ID,
        <<"Session">>            => Session
    },
    TestFun = fun() -> erlcloud_cognito_user_pools:respond_to_auth_challenge(?CLIENT_ID, ChallengeName,
                                                                  ChallengeResp, Session) end,
    do_test(Request, {ok, ?RESPOND_TO_AUTH_CHALLENGE}, TestFun).

test_create_identity_provider() ->
  UserPoolId      = <<"test">>,
  ProviderName    = <<"testIdp">>,
  ProviderType    = <<"SAML">>,
  ProviderDetails = #{
    <<"MetadataURL">> => <<"http://test-idp.com/1234">>
  },
  AttributeMapping = #{
    <<"testAttr">>    => <<"SAMLAttr">>
  },
  IdpIdentifiers   = [ "Test IDP" ],

  Request = #{
    <<"UserPoolId">>       => UserPoolId,
    <<"ProviderName">>     => ProviderName,
    <<"ProviderType">>     => ProviderType,
    <<"ProviderDetails">>  => ProviderDetails,
    <<"AttributeMapping">> => AttributeMapping,
    <<"IdpIdentifiers">>   => IdpIdentifiers
  },

  TestFun = fun() -> erlcloud_cognito_user_pools:create_identity_provider(UserPoolId, ProviderName,
                                                               ProviderType, ProviderDetails,
                                                               AttributeMapping, IdpIdentifiers) end,
  do_test(Request, {ok, ?CREATE_UPDATE_IDP}, TestFun).

test_delete_identity_provider() ->
  UserPoolId   = <<"test">>,
  ProviderName = <<"testIdp">>,

  Request = #{
    <<"UserPoolId">>   => UserPoolId,
    <<"ProviderName">> => ProviderName
  },

  TestFun = fun() -> erlcloud_cognito_user_pools:delete_identity_provider(UserPoolId, ProviderName) end,
  do_test(Request, ok, TestFun).

test_update_identity_provider() ->
  UserPoolId      = <<"test">>,
  ProviderName    = <<"testIdp">>,
  ProviderDetails = #{
    <<"MetadataURL">> => <<"http://test-idp.com/1234">>
  },
  AttributeMapping = #{
    <<"testAttr">>    => <<"SAMLAttr">>
  },
  IdpIdentifiers   = [ "Test IDP" ],

  Request = #{
    <<"UserPoolId">>       => UserPoolId,
    <<"ProviderName">>     => ProviderName,
    <<"ProviderDetails">>  => ProviderDetails,
    <<"AttributeMapping">> => AttributeMapping,
    <<"IdpIdentifiers">>   => IdpIdentifiers
  },

  TestFun = fun() -> erlcloud_cognito_user_pools:update_identity_provider(UserPoolId, ProviderName,
                                                               ProviderDetails, AttributeMapping,
                                                               IdpIdentifiers) end,
  do_test(Request, {ok, ?CREATE_UPDATE_IDP}, TestFun).

test_error_retry() ->
    erlcloud_cognito_user_pools:configure("test-access-key", "test-secret-key"),
    ErrCode1 = 500,
    ErrCode2 = 400,
    Status1 = "Internal Server Error",
    Status2 = "Bad Request",
    ErrMsg1 = <<"Message-1">>,
    ErrMsg2 = <<"Message-2">>,
    Operation = "ListUsers",
    Config = config(),
    Request = #{<<"UserPoolId">> => ?USER_POOL_ID},
    meck:sequence(?EHTTPC, request, 6,
        [{ok, {{ErrCode1, Status1}, [], ErrMsg1}},
            {ok, {{ErrCode2, Status2}, [], ErrMsg2}}]),
    ?assertEqual(
        {error, {http_error, ErrCode2, Status2, ErrMsg2, []}},
        erlcloud_cognito_user_pools:request(Config, Operation, Request)
    ).

do_test(Request, ExpectedResult, TestedFun) ->
    erlcloud_cognito_user_pools:configure("test-access-key", "test-secret-key"),
    ?assertEqual(ExpectedResult, TestedFun()),
    Encoded = jsx:encode(Request),
    ?assertMatch([{_, {?EHTTPC, request, [_, post, _, Encoded, _, _]}, _}],
                 meck:history(?EHTTPC)).

do_erlcloud_httpc_request(_, post, Headers, _, _, _) ->
    Target = proplists:get_value("x-amz-target", Headers),
    ["AWSCognitoIdentityProviderService", Operation] = string:tokens(Target, "."),
    RespBody =
        case Operation of
            "ListUsers"                 -> ?LIST_USERS_RESP;
            "AdminListGroupsForUser"    -> ?ADMIN_LIST_GROUPS_FOR_USERS;
            "AdminGetUser"              -> ?ADMIN_GET_USER;
            "AdminCreateUser"           -> ?ADMIN_CREATE_USER;
            "AdminDeleteUser"           -> #{};
            "AdminAddUserToGroup"       -> #{};
            "AdminRemoveUserFromGroup"  -> #{};
            "CreateGroup"               -> ?CREATE_GROUP;
            "DeleteGroup"               -> #{};
            "AdminResetUserPassword"    -> #{};
            "AdminUpdateUserAttributes" -> #{};
            "ChangePassword"            -> #{};
            "ListUserPools"             -> ?LIST_USER_POOLS;
            "AdminSetUserPassword"      -> #{};
            "DescribeUserPool"          -> ?DESCRIBE_POOL;
            "ListUserPoolClients"       -> ?LIST_USER_POOL_CLIENTS;
            "GetUserPoolMfaConfig"      -> ?MFA_CONFIG;
            "ListIdentityProviders"     -> ?LIST_IDENTITY_PROVIDERS;
            "DescribeIdentityProvider"  -> ?DESCRIBE_PROVIDER;
            "DescribeUserPoolClient"    -> ?DESCRIBE_USER_POOL_CLIENT;
            "AdminListDevices"          -> ?ADMIN_LIST_DEVICE;
            "AdminForgetDevice"         -> #{};
            "AdminConfirmSignUp"        -> #{};
            "AdminInitiateAuth"         -> ?ADMIN_INITIATE_AUTH;
            "RespondToAuthChallenge"    -> ?RESPOND_TO_AUTH_CHALLENGE;
            "CreateIdentityProvider"    -> ?CREATE_UPDATE_IDP;
            "DeleteIdentityProvider"    -> #{};
            "UpdateIdentityProvider"    -> ?CREATE_UPDATE_IDP
        end,
    {ok, {{200, "OK"}, [], jsx:encode(RespBody)}}.
