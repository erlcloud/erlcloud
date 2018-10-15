%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_ecs_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/erlcloud_ecs.hrl").

%% Unit tests for erlcloud_ecs.
%% These tests work by using meck to mock erlcloud_httpc. There are two classes of test: input and output.
%%
%% Input tests verify that different function args produce the desired JSON request.
%% An input test list provides a list of funs and the JSON that is expected to result.
%%
%% Output tests verify that the http response produces the correct return from the fun.
%% An output test lists provides a list of response bodies and the expected return.

%% The _ecs_test macro provides line number annotation to a test, similar to _test, but doesn't wrap in a fun
-define(_ecs_test(T), {?LINE, T}).
%% The _f macro is a terse way to wrap code in a fun. Similar to _test but doesn't annotate with a line number
-define(_f(F), fun() -> F end).

-export([validate_body/2]).

%%%===================================================================
%%% Common Test Values
%%%===================================================================
-define(NAME_PARAM, <<"TestName">>).
-define(SERVICE_NAME_PARAM, <<"sample-webapp">>).
-define(EXAMPLE_UUID, <<"709b350e-8c8e-490c-8fa3-d5fc09ac9e0b">>).
-define(TASK_DEFINITION, <<"console-sample-app-static:1">>).
-define(LIMIT_SIZE, 10).

%%%===================================================================
%%% Test entry points
%%%===================================================================
operation_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [
      fun create_cluster_input_tests/1,
      fun create_cluster_output_tests/1,
      fun create_service_input_tests/1,
      fun create_service_output_tests/1,
      fun delete_cluster_input_tests/1,
      fun delete_cluster_output_tests/1,
      fun delete_service_input_tests/1,
      fun delete_service_output_tests/1,
      fun deregister_container_instance_input_tests/1,
      fun deregister_container_instance_output_tests/1,
      fun deregister_task_definition_input_tests/1,
      fun deregister_task_definition_output_tests/1,
      fun describe_clusters_input_tests/1,
      fun describe_clusters_output_tests/1,
      fun describe_container_instances_input_tests/1,
      fun describe_container_instances_output_tests/1,
      fun describe_services_input_tests/1,
      fun describe_services_output_tests/1,
      fun describe_task_definition_input_tests/1,
      fun describe_task_definition_output_tests/1,
      fun describe_tasks_input_tests/1,
      fun describe_tasks_output_tests/1,
      fun list_clusters_input_tests/1,
      fun list_clusters_output_tests/1,
      fun list_container_instances_input_tests/1,
      fun list_container_instances_output_tests/1,
      fun list_services_input_tests/1,
      fun list_services_output_tests/1,
      fun list_task_definition_families_input_tests/1,
      fun list_task_definition_families_output_tests/1,
      fun list_task_definitions_input_tests/1,
      fun list_task_definitions_output_tests/1,
      fun list_tasks_input_tests/1,
      fun list_tasks_output_tests/1,
      fun register_task_definition_input_tests/1,
      fun register_task_definition_output_tests/1,
      fun run_task_input_tests/1,
      fun run_task_output_tests/1,
      fun start_task_input_tests/1,
      fun start_task_output_tests/1,
      fun stop_task_input_tests/1,
      fun stop_task_output_tests/1,
      fun update_container_agent_input_tests/1,
      fun update_container_agent_output_tests/1,
      fun update_service_input_tests/1,
      fun update_service_output_tests/1
     ]
    }.

start() ->
    meck:new(erlcloud_httpc),
    ok.


stop(_) ->
    meck:unload(erlcloud_httpc).

%%%===================================================================
%%% Actual test specifiers
%%%===================================================================

%% CreateCluster test based on the API examples:
%% http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_CreateCluster.html
create_cluster_input_tests(_) ->
    Tests = 
        [?_ecs_test(
            {"CreateCluster example request",
             ?_f(erlcloud_ecs:create_cluster([{cluster_name, "My-cluster"}])), "
{
    \"clusterName\": \"My-cluster\"
}"
            })
        ],

    Response = "
{    
    \"cluster\": {
        \"activeServicesCount\": 0,
        \"clusterArn\": \"arn:aws:ecs:us-east-1:012345678910:cluster/My-cluster\",
        \"clusterName\": \"My-cluster\",
        \"pendingTasksCount\": 0,
        \"registeredContainerInstancesCount\": 0,
        \"runningTasksCount\": 0,
        \"status\": \"ACTIVE\"
    }
}",
    input_tests(Response, Tests).

create_cluster_output_tests(_) ->
    Tests =
        [?_ecs_test(
            {"CreateCluster example response", "
{    
    \"cluster\": {
        \"activeServicesCount\": 0,
        \"clusterArn\": \"arn:aws:ecs:us-east-1:012345678910:cluster/My-cluster\",
        \"clusterName\": \"My-cluster\",
        \"pendingTasksCount\": 0,
        \"registeredContainerInstancesCount\": 0,
        \"runningTasksCount\": 0,
        \"status\": \"ACTIVE\"
    }
}",
            {ok,#ecs_cluster
                    {active_services_count = 0,
                     cluster_arn = <<"arn:aws:ecs:us-east-1:012345678910:cluster/My-cluster">>,
                     cluster_name = <<"My-cluster">>,
                     pending_tasks_count = 0,
                     registered_container_instances_count = 0,
                     running_tasks_count = 0,status = <<"ACTIVE">>}}})
        ],
    output_tests(?_f(erlcloud_ecs:create_cluster([{cluster_name, "My-cluster"}, {out, record}])), Tests).

%% CreateService test based on the API examples:
%% http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_CreateService.html
create_service_input_tests(_) ->
    Tests =
        [?_ecs_test(
            {"CreateService example request",
             ?_f(erlcloud_ecs:create_service("ecs-simple-service", "ecs-demo", 10)), "
{
    \"serviceName\": \"ecs-simple-service\",
    \"taskDefinition\": \"ecs-demo\",
    \"desiredCount\": 10
}"
            })
        ],

    Response = "
{
    \"service\": {
        \"clusterArn\": \"arn:aws:ecs:us-east-1:012345678910:cluster/default\",
        \"deploymentConfiguration\": {
            \"maximumPercent\": 200,
            \"minimumHealthyPercent\": 100
        },
        \"deployments\": [
          {
            \"createdAt\": 1430326887.362,
            \"desiredCount\": 10,
            \"id\": \"ecs-svc/9223370606527888445\",
            \"pendingCount\": 0,
            \"runningCount\": 0,
            \"status\": \"PRIMARY\",
            \"taskDefinition\": \"arn:aws:ecs:us-east-1:012345678910:task-definition/ecs-demo:1\",
            \"updatedAt\": 1430326887.362
          }
        ],
        \"desiredCount\": 10,
        \"events\": [],
        \"loadBalancers\": [],
        \"pendingCount\": 0,
        \"runningCount\": 0,
        \"serviceArn\": \"arn:aws:ecs:us-east-1:012345678910:service/ecs-simple-service\",
        \"serviceName\": \"ecs-simple-service\",
        \"status\": \"ACTIVE\",
        \"taskDefinition\": \"arn:aws:ecs:us-east-1:012345678910:task-definition/ecs-demo:1\"
     }
}",
    input_tests(Response, Tests).

create_service_output_tests(_) ->
    Tests =
        [
            ?_ecs_test({"CreateService example response", "
{
    \"service\": {
        \"clusterArn\": \"arn:aws:ecs:us-east-1:012345678910:cluster/default\",
        \"deploymentConfiguration\": {
            \"maximumPercent\": 200,
            \"minimumHealthyPercent\": 100
        },
        \"deployments\": [
          {
            \"createdAt\": 1430326887.362,
            \"desiredCount\": 10,
            \"id\": \"ecs-svc/9223370606527888445\",
            \"pendingCount\": 0,
            \"runningCount\": 0,
            \"status\": \"PRIMARY\",
            \"taskDefinition\": \"arn:aws:ecs:us-east-1:012345678910:task-definition/ecs-demo:1\",
            \"updatedAt\": 1430326887.362
          }
        ],
        \"desiredCount\": 10,
        \"events\": [],
        \"loadBalancers\": [],
        \"pendingCount\": 0,
        \"runningCount\": 0,
        \"serviceArn\": \"arn:aws:ecs:us-east-1:012345678910:service/ecs-simple-service\",
        \"serviceName\": \"ecs-simple-service\",
        \"status\": \"ACTIVE\",
        \"taskDefinition\": \"arn:aws:ecs:us-east-1:012345678910:task-definition/ecs-demo:1\"
     }
}",
            {ok, #ecs_service
                    {cluster_arn = <<"arn:aws:ecs:us-east-1:012345678910:cluster/default">>,
                     deployment_configuration = #ecs_deployment_configuration{
                        maximum_percent = 200,
                        minimum_healthy_percent = 100
                     },
                     deployments = [#ecs_deployment{
                        created_at = 1430326887.362,
                        desired_count = 10,
                        id = <<"ecs-svc/9223370606527888445">>,
                        pending_count = 0,
                        running_count = 0,
                        status = <<"PRIMARY">>,
                        task_definition = <<"arn:aws:ecs:us-east-1:012345678910:task-definition/ecs-demo:1">>,
                        updated_at = 1430326887.362
                     }],
                     desired_count = 10,
                     events = [],
                     load_balancers = [],
                     pending_count = 0,
                     running_count = 0,
                     service_arn = <<"arn:aws:ecs:us-east-1:012345678910:service/ecs-simple-service">>,
                     service_name = <<"ecs-simple-service">>,
                     status = <<"ACTIVE">>,
                     task_definition = <<"arn:aws:ecs:us-east-1:012345678910:task-definition/ecs-demo:1">>
                    }
            }
        })
        ],
    output_tests(?_f(erlcloud_ecs:create_service("ecs-simple-service", "ecs-demo", 10, [{out, record}])), Tests).

%% DeleteCluster test based on the API examples:
%% http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DeleteCluster.html
delete_cluster_input_tests(_) ->
    Tests = 
        [?_ecs_test(
            {"DeleteCluster example request",
             ?_f(erlcloud_ecs:delete_cluster("My-cluster")), "
{
    \"cluster\": \"My-cluster\"
}"
            })
        ],

    Response = "
{    
    \"cluster\": {
        \"activeServicesCount\": 0,
        \"clusterArn\": \"arn:aws:ecs:us-east-1:012345678910:cluster/My-cluster\",
        \"clusterName\": \"My-cluster\",
        \"pendingTasksCount\": 0,
        \"registeredContainerInstancesCount\": 0,
        \"runningTasksCount\": 0,
        \"status\": \"INACTIVE\"
    }
}",
    input_tests(Response, Tests).

delete_cluster_output_tests(_) ->
    Tests =
        [?_ecs_test(
            {"DeleteCluster example response", "
{    
    \"cluster\": {
        \"activeServicesCount\": 0,
        \"clusterArn\": \"arn:aws:ecs:us-east-1:012345678910:cluster/My-cluster\",
        \"clusterName\": \"My-cluster\",
        \"pendingTasksCount\": 0,
        \"registeredContainerInstancesCount\": 0,
        \"runningTasksCount\": 0,
        \"status\": \"INACTIVE\"
    }
}",
            {ok,#ecs_cluster
                    {active_services_count = 0,
                     cluster_arn = <<"arn:aws:ecs:us-east-1:012345678910:cluster/My-cluster">>,
                     cluster_name = <<"My-cluster">>,
                     pending_tasks_count = 0,
                     registered_container_instances_count = 0,
                     running_tasks_count = 0,
                     status = <<"INACTIVE">>}}})
        ],
    output_tests(?_f(erlcloud_ecs:delete_cluster("My-cluster", [{out, record}])), Tests).

%% DeleteService test based on the API examples:
%% http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DeleteService.html
delete_service_input_tests(_) ->
    Tests =
        [?_ecs_test(
            {"DeleteService example request",
             ?_f(erlcloud_ecs:delete_service("ecs-simple-service")), "
{
    \"service\": \"ecs-simple-service\"
}"
            })
        ],

    Response = "
{
    \"service\": {
        \"clusterArn\": \"arn:aws:ecs:us-east-1:012345678910:cluster/default\",
        \"deploymentConfiguration\": {
            \"maximumPercent\": 200,
            \"minimumHealthyPercent\": 100
        },
        \"deployments\": [
          {
            \"createdAt\": 1430326887.362,
            \"desiredCount\": 10,
            \"id\": \"ecs-svc/9223370606527888445\",
            \"pendingCount\": 0,
            \"runningCount\": 0,
            \"status\": \"PRIMARY\",
            \"taskDefinition\": \"arn:aws:ecs:us-east-1:012345678910:task-definition/ecs-demo:1\",
            \"updatedAt\": 1430326887.362
          }
        ],
        \"desiredCount\": 10,
        \"events\": [],
        \"loadBalancers\": [],
        \"pendingCount\": 0,
        \"runningCount\": 0,
        \"serviceArn\": \"arn:aws:ecs:us-east-1:012345678910:service/ecs-simple-service\",
        \"serviceName\": \"ecs-simple-service\",
        \"status\": \"DRAINING\",
        \"taskDefinition\": \"arn:aws:ecs:us-east-1:012345678910:task-definition/ecs-demo:1\"
     }
}",
    input_tests(Response, Tests).

delete_service_output_tests(_) ->
    Tests =
        [
            ?_ecs_test({"DeleteService example response", "
{
    \"service\": {
        \"clusterArn\": \"arn:aws:ecs:us-east-1:012345678910:cluster/default\",
        \"deploymentConfiguration\": {
            \"maximumPercent\": 200,
            \"minimumHealthyPercent\": 100
        },
        \"deployments\": [
          {
            \"createdAt\": 1430326887.362,
            \"desiredCount\": 10,
            \"id\": \"ecs-svc/9223370606527888445\",
            \"pendingCount\": 0,
            \"runningCount\": 0,
            \"status\": \"PRIMARY\",
            \"taskDefinition\": \"arn:aws:ecs:us-east-1:012345678910:task-definition/ecs-demo:1\",
            \"updatedAt\": 1430326887.362
          }
        ],
        \"desiredCount\": 10,
        \"events\": [],
        \"loadBalancers\": [],
        \"pendingCount\": 0,
        \"runningCount\": 0,
        \"serviceArn\": \"arn:aws:ecs:us-east-1:012345678910:service/ecs-simple-service\",
        \"serviceName\": \"ecs-simple-service\",
        \"status\": \"DRAINING\",
        \"taskDefinition\": \"arn:aws:ecs:us-east-1:012345678910:task-definition/ecs-demo:1\"
     }
}",
            {ok, #ecs_service
                    {cluster_arn = <<"arn:aws:ecs:us-east-1:012345678910:cluster/default">>,
                     deployment_configuration = #ecs_deployment_configuration{
                        maximum_percent = 200,
                        minimum_healthy_percent = 100
                     },
                     deployments = [#ecs_deployment{
                        created_at = 1430326887.362,
                        desired_count = 10,
                        id = <<"ecs-svc/9223370606527888445">>,
                        pending_count = 0,
                        running_count = 0,
                        status = <<"PRIMARY">>,
                        task_definition = <<"arn:aws:ecs:us-east-1:012345678910:task-definition/ecs-demo:1">>,
                        updated_at = 1430326887.362
                     }],
                     desired_count = 10,
                     events = [],
                     load_balancers = [],
                     pending_count = 0,
                     running_count = 0,
                     service_arn = <<"arn:aws:ecs:us-east-1:012345678910:service/ecs-simple-service">>,
                     service_name = <<"ecs-simple-service">>,
                     status = <<"DRAINING">>,
                     task_definition = <<"arn:aws:ecs:us-east-1:012345678910:task-definition/ecs-demo:1">>
                    }
            }
        })
        ],
    output_tests(?_f(erlcloud_ecs:delete_service("ecs-simple-service", [{out, record}])), Tests).


%% DeregisterContainerInstance test based on the API examples:
%% http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DeregisterContainerInstance.html
deregister_container_instance_input_tests(_) ->
    Tests =
        [?_ecs_test(
            {"DeregisterContainerInstance example request",
             ?_f(erlcloud_ecs:deregister_container_instance("c9c9a6f2-8766-464b-8805-9c57b9368fb0")), "
{
    \"containerInstance\": \"c9c9a6f2-8766-464b-8805-9c57b9368fb0\"
}"
            })
        ],
    Response = "
{
    \"containerInstance\": {
        \"agentConnected\": true,
        \"attributes\": [
          {
            \"name\": \"com.amazonaws.ecs.capability.privileged-container\"
          },
          {
            \"name\": \"com.amazonaws.ecs.capability.docker-remote-api.1.17\"
          },
          {
            \"name\": \"com.amazonaws.ecs.capability.docker-remote-api.1.18\"
          },
          {
            \"name\": \"com.amazonaws.ecs.capability.docker-remote-api.1.19\"
          },
          {
            \"name\": \"com.amazonaws.ecs.capability.logging-driver.json-file\"
          },
          {
            \"name\": \"com.amazonaws.ecs.capability.logging-driver.syslog\"
          }
        ],
        \"containerInstanceArn\": \"arn:aws:ecs:us-west-2:012345678910:container-instance/c9c9a6f2-8766-464b-8805-9c57b9368fb0\",
        \"ec2InstanceId\": \"i-0c3826c9\",
        \"pendingTasksCount\": 0,
        \"registeredResources\": [
          {
            \"doubleValue\": 0,
            \"integerValue\": 1024,
            \"longValue\": 0,
            \"name\": \"CPU\",
            \"type\": \"INTEGER\"
          },
          {
            \"doubleValue\": 0,
            \"integerValue\": 995,
            \"longValue\": 0,
            \"name\": \"MEMORY\",
            \"type\": \"INTEGER\"
          },
          {
            \"doubleValue\": 0,
            \"integerValue\": 0,
            \"longValue\": 0,
            \"name\": \"PORTS\",
            \"stringSetValue\": [
              \"22\",
              \"2376\",
              \"2375\",
              \"51678\"
            ],
            \"type\": \"STRINGSET\"
          },
          {
            \"doubleValue\": 0,
            \"integerValue\": 0,
            \"longValue\": 0,
            \"name\": \"PORTS_UDP\",
            \"stringSetValue\": [],
            \"type\": \"STRINGSET\"
          }
        ],
        \"remainingResources\": [
          {
            \"doubleValue\": 0,
            \"integerValue\": 1024,
            \"longValue\": 0,
            \"name\": \"CPU\",
            \"type\": \"INTEGER\"
          },
          {
            \"doubleValue\": 0,
            \"integerValue\": 995,
            \"longValue\": 0,
            \"name\": \"MEMORY\",
            \"type\": \"INTEGER\"
          },
          {
            \"doubleValue\": 0,
            \"integerValue\": 0,
            \"longValue\": 0,
            \"name\": \"PORTS\",
            \"stringSetValue\": [
              \"22\",
              \"2376\",
              \"2375\",
              \"51678\"
            ],
            \"type\": \"STRINGSET\"
          },
          {
            \"doubleValue\": 0,
            \"integerValue\": 0,
            \"longValue\": 0,
            \"name\": \"PORTS_UDP\",
            \"stringSetValue\": [],
            \"type\": \"STRINGSET\"
          }
        ],
        \"runningTasksCount\": 0,
        \"status\": \"INACTIVE\",
        \"versionInfo\": {
          \"agentHash\": \"b197edd\",
          \"agentVersion\": \"1.5.0\",
          \"dockerVersion\": \"DockerVersion: 1.7.1\"
        }
      }
}",
    input_tests(Response, Tests).

deregister_container_instance_output_tests(_) ->
    Tests =
        [?_ecs_test(
            {"DeregisterContainerInstance example response", "
{
    \"containerInstance\": {
        \"agentConnected\": true,
        \"attributes\": [
          {
            \"name\": \"com.amazonaws.ecs.capability.privileged-container\"
          },
          {
            \"name\": \"com.amazonaws.ecs.capability.docker-remote-api.1.17\"
          },
          {
            \"name\": \"com.amazonaws.ecs.capability.docker-remote-api.1.18\"
          },
          {
            \"name\": \"com.amazonaws.ecs.capability.docker-remote-api.1.19\"
          },
          {
            \"name\": \"com.amazonaws.ecs.capability.logging-driver.json-file\"
          },
          {
            \"name\": \"com.amazonaws.ecs.capability.logging-driver.syslog\"
          }
        ],
        \"containerInstanceArn\": \"arn:aws:ecs:us-west-2:012345678910:container-instance/c9c9a6f2-8766-464b-8805-9c57b9368fb0\",
        \"ec2InstanceId\": \"i-0c3826c9\",
        \"pendingTasksCount\": 0,
        \"registeredResources\": [
          {
            \"doubleValue\": 0,
            \"integerValue\": 1024,
            \"longValue\": 0,
            \"name\": \"CPU\",
            \"type\": \"INTEGER\"
          },
          {
            \"doubleValue\": 0,
            \"integerValue\": 995,
            \"longValue\": 0,
            \"name\": \"MEMORY\",
            \"type\": \"INTEGER\"
          },
          {
            \"doubleValue\": 0,
            \"integerValue\": 0,
            \"longValue\": 0,
            \"name\": \"PORTS\",
            \"stringSetValue\": [
              \"22\",
              \"2376\",
              \"2375\",
              \"51678\"
            ],
            \"type\": \"STRINGSET\"
          },
          {
            \"doubleValue\": 0,
            \"integerValue\": 0,
            \"longValue\": 0,
            \"name\": \"PORTS_UDP\",
            \"stringSetValue\": [],
            \"type\": \"STRINGSET\"
          }
        ],
        \"remainingResources\": [
          {
            \"doubleValue\": 0,
            \"integerValue\": 1024,
            \"longValue\": 0,
            \"name\": \"CPU\",
            \"type\": \"INTEGER\"
          },
          {
            \"doubleValue\": 0,
            \"integerValue\": 995,
            \"longValue\": 0,
            \"name\": \"MEMORY\",
            \"type\": \"INTEGER\"
          },
          {
            \"doubleValue\": 0,
            \"integerValue\": 0,
            \"longValue\": 0,
            \"name\": \"PORTS\",
            \"stringSetValue\": [
              \"22\",
              \"2376\",
              \"2375\",
              \"51678\"
            ],
            \"type\": \"STRINGSET\"
          },
          {
            \"doubleValue\": 0,
            \"integerValue\": 0,
            \"longValue\": 0,
            \"name\": \"PORTS_UDP\",
            \"stringSetValue\": [],
            \"type\": \"STRINGSET\"
          }
        ],
        \"runningTasksCount\": 0,
        \"status\": \"INACTIVE\",
        \"versionInfo\": {
          \"agentHash\": \"b197edd\",
          \"agentVersion\": \"1.5.0\",
          \"dockerVersion\": \"DockerVersion: 1.7.1\"
        }
    }
}",
            {ok, #ecs_container_instance{
                    agent_connected = true,
                    attributes = [
                        #ecs_attribute{
                            name = <<"com.amazonaws.ecs.capability.privileged-container">>
                        },
                        #ecs_attribute{
                            name = <<"com.amazonaws.ecs.capability.docker-remote-api.1.17">>
                        },
                        #ecs_attribute{
                            name = <<"com.amazonaws.ecs.capability.docker-remote-api.1.18">>
                        },
                        #ecs_attribute{
                            name = <<"com.amazonaws.ecs.capability.docker-remote-api.1.19">>
                        },
                        #ecs_attribute{
                            name = <<"com.amazonaws.ecs.capability.logging-driver.json-file">>
                        },
                        #ecs_attribute{
                            name = <<"com.amazonaws.ecs.capability.logging-driver.syslog">>
                        }
                    ],
                    container_instance_arn = <<"arn:aws:ecs:us-west-2:012345678910:container-instance/c9c9a6f2-8766-464b-8805-9c57b9368fb0">>,
                    ec2_instance_id = <<"i-0c3826c9">>,
                    pending_tasks_count = 0,
                    registered_resources = [
                        #ecs_resource{
                            double_value = 0,
                            integer_value = 1024,
                            long_value = 0,
                            name = <<"CPU">>,
                            type = <<"INTEGER">>
                        },
                        #ecs_resource{
                            double_value = 0,
                            integer_value = 995,
                            long_value = 0,
                            name = <<"MEMORY">>,
                            type = <<"INTEGER">>
                        },
                        #ecs_resource{
                            double_value = 0,
                            integer_value = 0,
                            long_value = 0,
                            name = <<"PORTS">>,
                            string_set_value = [<<"22">>, <<"2376">>, <<"2375">>, <<"51678">>],
                            type = <<"STRINGSET">>
                        },
                        #ecs_resource{
                            double_value = 0,
                            integer_value = 0,
                            long_value = 0,
                            name = <<"PORTS_UDP">>,
                            string_set_value = [],
                            type = <<"STRINGSET">>
                        }
                    ],
                    remaining_resources = [
                        #ecs_resource{
                            double_value = 0,
                            integer_value = 1024,
                            long_value = 0,
                            name = <<"CPU">>,
                            type = <<"INTEGER">>
                        },
                        #ecs_resource{
                            double_value = 0,
                            integer_value = 995,
                            long_value = 0,
                            name = <<"MEMORY">>,
                            type = <<"INTEGER">>
                        },
                        #ecs_resource{
                            double_value = 0,
                            integer_value = 0,
                            long_value = 0,
                            name = <<"PORTS">>,
                            string_set_value = [<<"22">>, <<"2376">>, <<"2375">>, <<"51678">>],
                            type = <<"STRINGSET">>
                        },
                        #ecs_resource{
                            double_value = 0,
                            integer_value = 0,
                            long_value = 0,
                            name = <<"PORTS_UDP">>,
                            string_set_value = [],
                            type = <<"STRINGSET">>
                        }
                    ],
                    running_tasks_count = 0,
                    status = <<"INACTIVE">>,
                    version_info = #ecs_version_info{
                        agent_hash = <<"b197edd">>,
                        agent_version = <<"1.5.0">>,
                        docker_version = <<"DockerVersion: 1.7.1">>
                    }
                }
            }})
        ],
    output_tests(?_f(erlcloud_ecs:deregister_container_instance("c9c9a6f2-8766-464b-8805-9c57b9368fb0", [{out, record}])), Tests).


%% DeregisterTaskDefinition test based on the API examples:
%% http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DeregisterTaskDefinition.html
deregister_task_definition_input_tests(_) ->
    Tests =
        [?_ecs_test(
            {"DeregisterTaskDefinition example request",
             ?_f(erlcloud_ecs:deregister_task_definition("cpu-wave:1")), "
{
    \"taskDefinition\": \"cpu-wave:1\"
}"
            })
        ],

    Response = "
{
    \"taskDefinition\": {
    \"containerDefinitions\": [
      {
        \"command\": [
          \"apt-get update; apt-get install stress; while true; do stress --cpu $(( RANDOM % 4 )) -t $(( RANDOM % 10 )); done\"
        ],
        \"cpu\": 50,
        \"entryPoint\": [
          \"bash\",
          \"-c\"
        ],
        \"environment\": [],
        \"essential\": true,
        \"image\": \"ubuntu\",
        \"memory\": 100,
        \"mountPoints\": [],
        \"name\": \"wave\",
        \"portMappings\": [],
        \"volumesFrom\": []
      }
    ],
    \"family\": \"cpu-wave\",
    \"revision\": 1,
    \"status\": \"INACTIVE\",
    \"taskDefinitionArn\": \"arn:aws:ecs:us-west-2:012345678910:task-definition/cpu-wave:1\",
    \"volumes\": []
  }
}",
    input_tests(Response, Tests).

deregister_task_definition_output_tests(_) ->
    Tests = 
        [
            ?_ecs_test({"DeregisterTaskDefinition example response", "
{
    \"taskDefinition\": {
    \"containerDefinitions\": [
      {
        \"command\": [
          \"apt-get update; apt-get install stress; while true; do stress --cpu $(( RANDOM % 4 )) -t $(( RANDOM % 10 )); done\"
        ],
        \"cpu\": 50,
        \"entryPoint\": [
          \"bash\",
          \"-c\"
        ],
        \"environment\": [],
        \"essential\": true,
        \"image\": \"ubuntu\",
        \"memory\": 100,
        \"mountPoints\": [],
        \"name\": \"wave\",
        \"portMappings\": [],
        \"volumesFrom\": []
      }
    ],
    \"family\": \"cpu-wave\",
    \"revision\": 1,
    \"status\": \"INACTIVE\",
    \"taskDefinitionArn\": \"arn:aws:ecs:us-west-2:012345678910:task-definition/cpu-wave:1\",
    \"volumes\": []
  }
}",
            {ok, #ecs_task_definition{
                    container_definitions = [
                        #ecs_container_definition{
                            command = [<<"apt-get update; apt-get install stress; while true; do stress --cpu $(( RANDOM % 4 )) -t $(( RANDOM % 10 )); done">>],
                            cpu = 50,
                            entry_point = [<<"bash">>, <<"-c">>],
                            environment = [],
                            essential = true,
                            image = <<"ubuntu">>,
                            memory = 100,
                            mount_points = [],
                            name = <<"wave">>,
                            port_mappings = [],
                            volumes_from = []
                        }
                    ],
                    family = <<"cpu-wave">>,
                    revision = 1,
                    status = <<"INACTIVE">>,
                    task_definition_arn = <<"arn:aws:ecs:us-west-2:012345678910:task-definition/cpu-wave:1">>,
                    volumes = []
                }
            }})
        ],
    output_tests(?_f(erlcloud_ecs:deregister_task_definition("cpu-wave:1", [{out, record}])), Tests).


%% DescribeClusters test based on the API examples:
%% http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DescribeClusters.html
describe_clusters_input_tests(_) ->
    Tests = 
        [?_ecs_test(
            {"DescribeClusters example request",
             ?_f(erlcloud_ecs:describe_clusters([{clusters, ["default"]}])), "
{
    \"clusters\": [\"default\"]
}"
            })
        ],

    Response = "
{    
    \"clusters\": [{
        \"activeServicesCount\": 1,
        \"clusterArn\": \"arn:aws:ecs:us-east-1:012345678910:cluster/default\",
        \"clusterName\": \"My-cluster\",
        \"pendingTasksCount\": 0,
        \"registeredContainerInstancesCount\": 0,
        \"runningTasksCount\": 0,
        \"status\": \"ACTIVE\"
    }],
    \"failures\": []
}",
    input_tests(Response, Tests).


describe_clusters_output_tests(_) ->
    Tests =
        [?_ecs_test(
            {"DescribeClusters example response", "
{    
    \"clusters\": [{
        \"activeServicesCount\": 1,
        \"clusterArn\": \"arn:aws:ecs:us-east-1:012345678910:cluster/default\",
        \"clusterName\": \"My-cluster\",
        \"pendingTasksCount\": 0,
        \"registeredContainerInstancesCount\": 0,
        \"runningTasksCount\": 0,
        \"status\": \"ACTIVE\"
    }],
    \"failures\": []
}",
            {ok, #ecs_describe_clusters{
                clusters = [
                    #ecs_cluster{
                        active_services_count = 1,
                        cluster_arn = <<"arn:aws:ecs:us-east-1:012345678910:cluster/default">>,
                        cluster_name = <<"My-cluster">>,
                        pending_tasks_count = 0,
                        registered_container_instances_count = 0,
                        running_tasks_count = 0,
                        status = <<"ACTIVE">>
                    }
                ],
                failures = []
            }}})
        ],
    output_tests(?_f(erlcloud_ecs:describe_clusters([{clusters, ["default"]}, {out, record}])), Tests).


%% DescribeContainerInstances test based on the API examples:
%% http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DescribeContainerInstances.html
describe_container_instances_input_tests(_) ->
    Tests = 
        [?_ecs_test(
            {"DescribeContainerInstances example request",
             ?_f(erlcloud_ecs:describe_container_instances(["f9cc75bb-0c94-46b9-bf6d-49d320bc1551"])), "
{
    \"containerInstances\": [
        \"f9cc75bb-0c94-46b9-bf6d-49d320bc1551\"
    ]
}"
            })
        ],
    Response = "
{
    \"containerInstances\": [
        {
            \"agentConnected\": true,
            \"attributes\": [
                {
                    \"name\": \"com.amazonaws.ecs.capability.privileged-container\"
                },
                {
                    \"name\": \"com.amazonaws.ecs.capability.docker-remote-api.1.17\"
                },
                {
                    \"name\": \"com.amazonaws.ecs.capability.docker-remote-api.1.18\"
                },
                {
                    \"name\": \"com.amazonaws.ecs.capability.docker-remote-api.1.19\"
                },
                {
                    \"name\": \"com.amazonaws.ecs.capability.docker-remote-api.1.20\"
                },
                {
                    \"name\": \"com.amazonaws.ecs.capability.docker-remote-api.1.21\"
                },
                {
                    \"name\": \"com.amazonaws.ecs.capability.logging-driver.json-file\"
                },
                {
                    \"name\": \"com.amazonaws.ecs.capability.logging-driver.syslog\"
                },
                {
                    \"name\": \"com.amazonaws.ecs.capability.logging-driver.awslogs\"
                },
                {
                    \"name\": \"com.amazonaws.ecs.capability.ecr-auth\"
                }
            ],
            \"containerInstanceArn\": \"arn:aws:ecs:us-west-2:012345678910:container-instance/f9cc75bb-0c94-46b9-bf6d-49d320bc1551\",
            \"ec2InstanceId\": \"i-042f39dc\",
            \"pendingTasksCount\": 0,
            \"registeredResources\": [
                {
                    \"doubleValue\": 0,
                    \"integerValue\": 1024,
                    \"longValue\": 0,
                    \"name\": \"CPU\",
                    \"type\": \"INTEGER\"
                },
                {
                    \"doubleValue\": 0,
                    \"integerValue\": 995,
                    \"longValue\": 0,
                    \"name\": \"MEMORY\",
                    \"type\": \"INTEGER\"
                },
                {
                    \"doubleValue\": 0,
                    \"integerValue\": 0,
                    \"longValue\": 0,
                    \"name\": \"PORTS\",
                    \"stringSetValue\": [
                        \"22\",
                        \"2376\",
                        \"2375\",
                        \"51678\"
                    ],
                    \"type\": \"STRINGSET\"
                },
                {
                    \"doubleValue\": 0,
                    \"integerValue\": 0,
                    \"longValue\": 0,
                    \"name\": \"PORTS_UDP\",
                    \"stringSetValue\": [],
                    \"type\": \"STRINGSET\"
                }
            ],
            \"remainingResources\": [
                {
                    \"doubleValue\": 0,
                    \"integerValue\": 1024,
                    \"longValue\": 0,
                    \"name\": \"CPU\",
                    \"type\": \"INTEGER\"
                },
                {
                    \"doubleValue\": 0,
                    \"integerValue\": 995,
                    \"longValue\": 0,
                    \"name\": \"MEMORY\",
                    \"type\": \"INTEGER\"
                },
                {
                    \"doubleValue\": 0,
                    \"integerValue\": 0,
                    \"longValue\": 0,
                    \"name\": \"PORTS\",
                    \"stringSetValue\": [
                        \"22\",
                        \"2376\",
                        \"2375\",
                        \"51678\"
                    ],
                    \"type\": \"STRINGSET\"
                },
                {
                    \"doubleValue\": 0,
                    \"integerValue\": 0,
                    \"longValue\": 0,
                    \"name\": \"PORTS_UDP\",
                    \"stringSetValue\": [],
                    \"type\": \"STRINGSET\"
                }
            ],
            \"runningTasksCount\": 0,
            \"status\": \"ACTIVE\",
            \"versionInfo\": {
                \"agentHash\": \"0931217\",
                \"agentVersion\": \"1.9.0\",
                \"dockerVersion\": \"DockerVersion: 1.9.1\"
            }
        }
    ],
    \"failures\": []
}",
    input_tests(Response, Tests).

describe_container_instances_output_tests(_) ->
    Tests = 
        [?_ecs_test(
            {"DescribeContainerInstances example response", "
{
    \"containerInstances\": [
        {
            \"agentConnected\": true,
            \"attributes\": [
                {
                    \"name\": \"com.amazonaws.ecs.capability.privileged-container\"
                },
                {
                    \"name\": \"com.amazonaws.ecs.capability.docker-remote-api.1.17\"
                },
                {
                    \"name\": \"com.amazonaws.ecs.capability.docker-remote-api.1.18\"
                },
                {
                    \"name\": \"com.amazonaws.ecs.capability.docker-remote-api.1.19\"
                },
                {
                    \"name\": \"com.amazonaws.ecs.capability.docker-remote-api.1.20\"
                },
                {
                    \"name\": \"com.amazonaws.ecs.capability.docker-remote-api.1.21\"
                },
                {
                    \"name\": \"com.amazonaws.ecs.capability.logging-driver.json-file\"
                },
                {
                    \"name\": \"com.amazonaws.ecs.capability.logging-driver.syslog\"
                },
                {
                    \"name\": \"com.amazonaws.ecs.capability.logging-driver.awslogs\"
                },
                {
                    \"name\": \"com.amazonaws.ecs.capability.ecr-auth\"
                }
            ],
            \"containerInstanceArn\": \"arn:aws:ecs:us-west-2:012345678910:container-instance/f9cc75bb-0c94-46b9-bf6d-49d320bc1551\",
            \"ec2InstanceId\": \"i-042f39dc\",
            \"pendingTasksCount\": 0,
            \"registeredResources\": [
                {
                    \"doubleValue\": 0,
                    \"integerValue\": 1024,
                    \"longValue\": 0,
                    \"name\": \"CPU\",
                    \"type\": \"INTEGER\"
                },
                {
                    \"doubleValue\": 0,
                    \"integerValue\": 995,
                    \"longValue\": 0,
                    \"name\": \"MEMORY\",
                    \"type\": \"INTEGER\"
                },
                {
                    \"doubleValue\": 0,
                    \"integerValue\": 0,
                    \"longValue\": 0,
                    \"name\": \"PORTS\",
                    \"stringSetValue\": [
                        \"22\",
                        \"2376\",
                        \"2375\",
                        \"51678\"
                    ],
                    \"type\": \"STRINGSET\"
                },
                {
                    \"doubleValue\": 0,
                    \"integerValue\": 0,
                    \"longValue\": 0,
                    \"name\": \"PORTS_UDP\",
                    \"stringSetValue\": [],
                    \"type\": \"STRINGSET\"
                }
            ],
            \"remainingResources\": [
                {
                    \"doubleValue\": 0,
                    \"integerValue\": 1024,
                    \"longValue\": 0,
                    \"name\": \"CPU\",
                    \"type\": \"INTEGER\"
                },
                {
                    \"doubleValue\": 0,
                    \"integerValue\": 995,
                    \"longValue\": 0,
                    \"name\": \"MEMORY\",
                    \"type\": \"INTEGER\"
                },
                {
                    \"doubleValue\": 0,
                    \"integerValue\": 0,
                    \"longValue\": 0,
                    \"name\": \"PORTS\",
                    \"stringSetValue\": [
                        \"22\",
                        \"2376\",
                        \"2375\",
                        \"51678\"
                    ],
                    \"type\": \"STRINGSET\"
                },
                {
                    \"doubleValue\": 0,
                    \"integerValue\": 0,
                    \"longValue\": 0,
                    \"name\": \"PORTS_UDP\",
                    \"stringSetValue\": [],
                    \"type\": \"STRINGSET\"
                }
            ],
            \"runningTasksCount\": 0,
            \"status\": \"ACTIVE\",
            \"versionInfo\": {
                \"agentHash\": \"0931217\",
                \"agentVersion\": \"1.9.0\",
                \"dockerVersion\": \"DockerVersion: 1.9.1\"
            }
        }
    ],
    \"failures\": []
}
",
            {ok, #ecs_describe_container_instances{
                container_instances = [
                    #ecs_container_instance{
                        agent_connected = true,
                        attributes = [
                            #ecs_attribute{
                                name = <<"com.amazonaws.ecs.capability.privileged-container">>
                            },
                            #ecs_attribute{
                                name = <<"com.amazonaws.ecs.capability.docker-remote-api.1.17">>
                            },
                            #ecs_attribute{
                                name = <<"com.amazonaws.ecs.capability.docker-remote-api.1.18">>
                            },
                            #ecs_attribute{
                                name = <<"com.amazonaws.ecs.capability.docker-remote-api.1.19">>
                            },
                            #ecs_attribute{
                                name = <<"com.amazonaws.ecs.capability.docker-remote-api.1.20">>
                            },
                            #ecs_attribute{
                                name = <<"com.amazonaws.ecs.capability.docker-remote-api.1.21">>
                            },
                            #ecs_attribute{
                                name = <<"com.amazonaws.ecs.capability.logging-driver.json-file">>
                            },
                            #ecs_attribute{
                                name = <<"com.amazonaws.ecs.capability.logging-driver.syslog">>
                            },
                            #ecs_attribute{
                                name = <<"com.amazonaws.ecs.capability.logging-driver.awslogs">>
                            },
                            #ecs_attribute{
                                name = <<"com.amazonaws.ecs.capability.ecr-auth">>
                            }
                        ],
                        container_instance_arn = <<"arn:aws:ecs:us-west-2:012345678910:container-instance/f9cc75bb-0c94-46b9-bf6d-49d320bc1551">>,
                        ec2_instance_id = <<"i-042f39dc">>,
                        pending_tasks_count = 0,
                        registered_resources = [
                            #ecs_resource{
                                double_value = 0,
                                integer_value = 1024,
                                long_value = 0,
                                name = <<"CPU">>,
                                type = <<"INTEGER">>
                            },
                            #ecs_resource{
                                double_value = 0,
                                integer_value = 995,
                                long_value = 0,
                                name = <<"MEMORY">>,
                                type = <<"INTEGER">>
                            },
                            #ecs_resource{
                                double_value = 0,
                                integer_value = 0,
                                long_value = 0,
                                name = <<"PORTS">>,
                                string_set_value = [<<"22">>, <<"2376">>, <<"2375">>, <<"51678">>],
                                type = <<"STRINGSET">>
                            },
                            #ecs_resource{
                                double_value = 0,
                                integer_value = 0,
                                long_value = 0,
                                name = <<"PORTS_UDP">>,
                                string_set_value = [],
                                type = <<"STRINGSET">>
                            }
                        ],
                        remaining_resources = [
                            #ecs_resource{
                                double_value = 0,
                                integer_value = 1024,
                                long_value = 0,
                                name = <<"CPU">>,
                                type = <<"INTEGER">>
                            },
                            #ecs_resource{
                                double_value = 0,
                                integer_value = 995,
                                long_value = 0,
                                name = <<"MEMORY">>,
                                type = <<"INTEGER">>
                            },
                            #ecs_resource{
                                double_value = 0,
                                integer_value = 0,
                                long_value = 0,
                                name = <<"PORTS">>,
                                string_set_value = [<<"22">>, <<"2376">>, <<"2375">>, <<"51678">>],
                                type = <<"STRINGSET">>
                            },
                            #ecs_resource{
                                double_value = 0,
                                integer_value = 0,
                                long_value = 0,
                                name = <<"PORTS_UDP">>,
                                string_set_value = [],
                                type = <<"STRINGSET">>
                            }
                        ],
                        running_tasks_count = 0,
                        status = <<"ACTIVE">>,
                        version_info = #ecs_version_info{
                            agent_hash = <<"0931217">>,
                            agent_version = <<"1.9.0">>,
                            docker_version = <<"DockerVersion: 1.9.1">>
                        }
                    }
                ],
                failures = []
            }}})
        ],
    output_tests(?_f(erlcloud_ecs:describe_container_instances("f9cc75bb-0c94-46b9-bf6d-49d320bc1551", [{out, record}])), Tests).

%% DescribeServices test based on the API examples:
%% http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DescribeServices.html
describe_services_input_tests(_) ->
    Tests = 
        [?_ecs_test(
            {"DescribeServices example request",
             ?_f(erlcloud_ecs:describe_services(["bunker-buster"], [{cluster, "telemetry"}])), "
{
    \"services\": [
        \"bunker-buster\"
      ],
      \"cluster\": \"telemetry\"
}"
            })
        ],

    Response = "
{
  \"failures\": [],
  \"services\": [
    {
      \"clusterArn\": \"arn:aws:ecs:us-west-2:012345678910:cluster/telemetry\",
      \"deploymentConfiguration\": {
          \"maximumPercent\": 200,
          \"minimumHealthyPercent\": 100
      },
      \"deployments\": [
        {
          \"createdAt\": 1432829320.611,
          \"desiredCount\": 4,
          \"id\": \"ecs-svc/9223370604025455196\",
          \"pendingCount\": 0,
          \"runningCount\": 4,
          \"status\": \"PRIMARY\",
          \"taskDefinition\": \"arn:aws:ecs:us-west-2:012345678910:task-definition/hpcc-t2-medium:1\",
          \"updatedAt\": 1432829320.611
        }
      ],
      \"desiredCount\": 4,
      \"events\": [],
      \"loadBalancers\": [],
      \"pendingCount\": 0,
      \"runningCount\": 4,
      \"serviceArn\": \"arn:aws:ecs:us-west-2:012345678910:service/bunker-buster\",
      \"serviceName\": \"bunker-buster\",
      \"status\": \"ACTIVE\",
      \"taskDefinition\": \"arn:aws:ecs:us-west-2:012345678910:task-definition/hpcc-t2-medium:1\"
    }
  ]
}
",
    input_tests(Response, Tests).


describe_services_output_tests(_) ->
    Tests =
        [?_ecs_test(
            {"DescribeServices example response", "
{
  \"failures\": [],
  \"services\": [
    {
      \"clusterArn\": \"arn:aws:ecs:us-west-2:012345678910:cluster/telemetry\",
      \"deploymentConfiguration\": {
          \"maximumPercent\": 200,
          \"minimumHealthyPercent\": 100
      },
      \"deployments\": [
        {
          \"createdAt\": 1432829320.611,
          \"desiredCount\": 4,
          \"id\": \"ecs-svc/9223370604025455196\",
          \"pendingCount\": 0,
          \"runningCount\": 4,
          \"status\": \"PRIMARY\",
          \"taskDefinition\": \"arn:aws:ecs:us-west-2:012345678910:task-definition/hpcc-t2-medium:1\",
          \"updatedAt\": 1432829320.611
        }
      ],
      \"desiredCount\": 4,
      \"events\": [],
      \"loadBalancers\": [],
      \"pendingCount\": 0,
      \"runningCount\": 4,
      \"serviceArn\": \"arn:aws:ecs:us-west-2:012345678910:service/bunker-buster\",
      \"serviceName\": \"bunker-buster\",
      \"status\": \"ACTIVE\",
      \"taskDefinition\": \"arn:aws:ecs:us-west-2:012345678910:task-definition/hpcc-t2-medium:1\"
    }
  ]
}
",
            {ok, #ecs_describe_services{
                services = [
                    #ecs_service{
                        cluster_arn = <<"arn:aws:ecs:us-west-2:012345678910:cluster/telemetry">>,
                        deployment_configuration = #ecs_deployment_configuration{
                            maximum_percent = 200,
                            minimum_healthy_percent = 100
                        },
                        deployments = [
                            #ecs_deployment{
                                created_at = 1432829320.611,
                                desired_count = 4,
                                id = <<"ecs-svc/9223370604025455196">>,
                                pending_count = 0,
                                running_count = 4,
                                status = <<"PRIMARY">>,
                                task_definition = <<"arn:aws:ecs:us-west-2:012345678910:task-definition/hpcc-t2-medium:1">>,
                                updated_at = 1432829320.611
                            }
                        ],
                        desired_count = 4,
                        events = [],
                        load_balancers = [],
                        pending_count = 0,
                        running_count = 4,
                        service_arn = <<"arn:aws:ecs:us-west-2:012345678910:service/bunker-buster">>,
                        service_name = <<"bunker-buster">>,
                        status = <<"ACTIVE">>,
                        task_definition = <<"arn:aws:ecs:us-west-2:012345678910:task-definition/hpcc-t2-medium:1">>
                    }
                ],
                failures = []
            }}})
        ],
    output_tests(?_f(erlcloud_ecs:describe_services(["bunker-buster"], [{cluster, "telemetry"}, {out, record}])), Tests).

%% DescribeTaskDefinition test based on the API examples:
%% http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DescribeTaskDefinition.html
describe_task_definition_input_tests(_) ->
    Tests = 
        [?_ecs_test(
            {"DescribeTaskDefinition example request",
             ?_f(erlcloud_ecs:describe_task_definition("hello_world:10")), "
{
  \"taskDefinition\": \"hello_world:10\"
}
"
            })
        ],

    Response = "
{
  \"taskDefinition\": {
    \"containerDefinitions\": [
      {
        \"cpu\": 10,
        \"environment\": [],
        \"essential\": true,
        \"image\": \"wordpress\",
        \"links\": [
          \"mysql\"
        ],
        \"memory\": 500,
        \"mountPoints\": [],
        \"name\": \"wordpress\",
        \"portMappings\": [
          {
            \"containerPort\": 80,
            \"hostPort\": 80
          }
        ],
        \"volumesFrom\": []
      },
      {
        \"cpu\": 10,
        \"environment\": [
          {
            \"name\": \"MYSQL_ROOT_PASSWORD\",
            \"value\": \"password\"
          }
        ],
        \"essential\": true,
        \"image\": \"mysql\",
        \"memory\": 500,
        \"mountPoints\": [],
        \"name\": \"mysql\",
        \"portMappings\": [],
        \"volumesFrom\": []
      }
    ],
    \"family\": \"hello_world\",
    \"revision\": 10,
    \"taskDefinitionArn\": \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:10\",
    \"volumes\": []
  }
}
",
    input_tests(Response, Tests).

describe_task_definition_output_tests(_) ->
    Tests = 
        [?_ecs_test(
            {"DescribeTaskDefinition example response", "
{
  \"taskDefinition\": {
    \"containerDefinitions\": [
      {
        \"cpu\": 10,
        \"environment\": [],
        \"essential\": true,
        \"image\": \"wordpress\",
        \"links\": [
          \"mysql\"
        ],
        \"memory\": 500,
        \"mountPoints\": [],
        \"name\": \"wordpress\",
        \"portMappings\": [
          {
            \"containerPort\": 80,
            \"hostPort\": 80
          }
        ],
        \"volumesFrom\": []
      },
      {
        \"cpu\": 10,
        \"environment\": [
          {
            \"name\": \"MYSQL_ROOT_PASSWORD\",
            \"value\": \"password\"
          }
        ],
        \"essential\": true,
        \"image\": \"mysql\",
        \"memory\": 500,
        \"mountPoints\": [],
        \"name\": \"mysql\",
        \"portMappings\": [],
        \"volumesFrom\": []
      }
    ],
    \"family\": \"hello_world\",
    \"revision\": 10,
    \"taskDefinitionArn\": \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:10\",
    \"volumes\": []
  }
}
",
            {ok, #ecs_task_definition{
                    container_definitions = [
                        #ecs_container_definition{
                            cpu = 10,
                            environment = [],
                            essential = true,
                            image = <<"wordpress">>,
                            links = [<<"mysql">>],
                            memory = 500,
                            mount_points = [],
                            name = <<"wordpress">>,
                            port_mappings = [
                                #ecs_port_mapping{
                                    container_port = 80,
                                    host_port = 80
                                }
                            ],
                            volumes_from = []
                        },
                        #ecs_container_definition{
                            cpu = 10,
                            environment = [
                                #ecs_attribute{
                                    name = <<"MYSQL_ROOT_PASSWORD">>,
                                    value = <<"password">>
                                }
                            ],
                            essential = true,
                            image = <<"mysql">>,
                            memory = 500,
                            mount_points = [],
                            name = <<"mysql">>,
                            port_mappings = [],
                            volumes_from = []
                        }
                    ],
                    family = <<"hello_world">>,
                    revision = 10,
                    task_definition_arn = <<"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:10">>,
                    volumes = []
                }
            }})
        ],
    output_tests(?_f(erlcloud_ecs:describe_task_definition("hello_world:10", [{out, record}])), Tests).

%% DescribeTasks test based on the API examples:
%% http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DescribeTasks.html
describe_tasks_input_tests(_) ->
    Tests = 
        [?_ecs_test(
            {"DescribeTasks example request",
             ?_f(erlcloud_ecs:describe_tasks(["c09f0188-7f87-4b0f-bfc3-16296622b6fe"])), "
{
  \"tasks\": [
    \"c09f0188-7f87-4b0f-bfc3-16296622b6fe\"
  ]
}
"
            })
        ],
    Response = "
{
  \"failures\": [],
  \"tasks\": [
    {
      \"clusterArn\": \"arn:aws:ecs:us-east-1:012345678910:cluster/default\",
      \"containerInstanceArn\": \"arn:aws:ecs:us-east-1:012345678910:container-instance/84818520-995f-4d94-9d70-7714bacc2953\",
      \"containers\": [
        {
          \"containerArn\": \"arn:aws:ecs:us-east-1:012345678910:container/76c980a8-2454-4a9c-acc4-9eb103117273\",
          \"lastStatus\": \"RUNNING\",
          \"name\": \"mysql\",
          \"networkBindings\": [],
          \"taskArn\": \"arn:aws:ecs:us-east-1:012345678910:task/c09f0188-7f87-4b0f-bfc3-16296622b6fe\"
        },
        {
          \"containerArn\": \"arn:aws:ecs:us-east-1:012345678910:container/e3c69b8f-f15e-4d33-8093-282c2d2325e9\",
          \"lastStatus\": \"RUNNING\",
          \"name\": \"wordpress\",
          \"networkBindings\": [
            {
              \"bindIP\": \"0.0.0.0\",
              \"containerPort\": 80,
              \"hostPort\": 80
            }
          ],
          \"taskArn\": \"arn:aws:ecs:us-east-1:012345678910:task/c09f0188-7f87-4b0f-bfc3-16296622b6fe\"
        }
      ],
      \"desiredStatus\": \"RUNNING\",
      \"lastStatus\": \"RUNNING\",
      \"overrides\": {
        \"containerOverrides\": [
          {
            \"name\": \"mysql\"
          },
          {
            \"name\": \"wordpress\"
          }
        ]
      },
      \"startedBy\": \"ecs-svc/9223370606521064774\",
      \"taskArn\": \"arn:aws:ecs:us-east-1:012345678910:task/c09f0188-7f87-4b0f-bfc3-16296622b6fe\",
      \"taskDefinitionArn\": \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:10\"
    }
  ]
}
",
    input_tests(Response, Tests).

describe_tasks_output_tests(_) ->
    Tests =
        [?_ecs_test(
            {"DescribeTasks example response", "
{
  \"failures\": [],
  \"tasks\": [
    {
      \"clusterArn\": \"arn:aws:ecs:us-east-1:012345678910:cluster/default\",
      \"containerInstanceArn\": \"arn:aws:ecs:us-east-1:012345678910:container-instance/84818520-995f-4d94-9d70-7714bacc2953\",
      \"containers\": [
        {
          \"containerArn\": \"arn:aws:ecs:us-east-1:012345678910:container/76c980a8-2454-4a9c-acc4-9eb103117273\",
          \"lastStatus\": \"RUNNING\",
          \"name\": \"mysql\",
          \"networkBindings\": [],
          \"taskArn\": \"arn:aws:ecs:us-east-1:012345678910:task/c09f0188-7f87-4b0f-bfc3-16296622b6fe\"
        },
        {
          \"containerArn\": \"arn:aws:ecs:us-east-1:012345678910:container/e3c69b8f-f15e-4d33-8093-282c2d2325e9\",
          \"lastStatus\": \"RUNNING\",
          \"name\": \"wordpress\",
          \"networkBindings\": [
            {
              \"bindIP\": \"0.0.0.0\",
              \"containerPort\": 80,
              \"hostPort\": 80
            }
          ],
          \"taskArn\": \"arn:aws:ecs:us-east-1:012345678910:task/c09f0188-7f87-4b0f-bfc3-16296622b6fe\"
        }
      ],
      \"desiredStatus\": \"RUNNING\",
      \"lastStatus\": \"RUNNING\",
      \"overrides\": {
        \"containerOverrides\": [
          {
            \"name\": \"mysql\"
          },
          {
            \"name\": \"wordpress\"
          }
        ]
      },
      \"startedBy\": \"ecs-svc/9223370606521064774\",
      \"taskArn\": \"arn:aws:ecs:us-east-1:012345678910:task/c09f0188-7f87-4b0f-bfc3-16296622b6fe\",
      \"taskDefinitionArn\": \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:10\"
    }
  ]
}
",
            {ok, #ecs_describe_tasks{
                failures = [],
                tasks = [
                    #ecs_task{
                        cluster_arn = <<"arn:aws:ecs:us-east-1:012345678910:cluster/default">>,
                        container_instance_arn = <<"arn:aws:ecs:us-east-1:012345678910:container-instance/84818520-995f-4d94-9d70-7714bacc2953">>,
                        containers = [
                            #ecs_container{
                                container_arn = <<"arn:aws:ecs:us-east-1:012345678910:container/76c980a8-2454-4a9c-acc4-9eb103117273">>,
                                last_status = <<"RUNNING">>,
                                name = <<"mysql">>,
                                network_bindings = [],
                                task_arn = <<"arn:aws:ecs:us-east-1:012345678910:task/c09f0188-7f87-4b0f-bfc3-16296622b6fe">>
                            },
                            #ecs_container{
                                container_arn = <<"arn:aws:ecs:us-east-1:012345678910:container/e3c69b8f-f15e-4d33-8093-282c2d2325e9">>,
                                last_status = <<"RUNNING">>,
                                name = <<"wordpress">>,
                                network_bindings = [
                                    #ecs_network_binding{
                                        bind_ip = <<"0.0.0.0">>,
                                        container_port = 80,
                                        host_port = 80
                                    }
                                ],
                                task_arn = <<"arn:aws:ecs:us-east-1:012345678910:task/c09f0188-7f87-4b0f-bfc3-16296622b6fe">>
                            }
                        ],
                        desired_status = <<"RUNNING">>,
                        last_status = <<"RUNNING">>,
                        overrides = #ecs_task_override{
                            container_overrides = [
                                #ecs_container_override{
                                    name = <<"mysql">>
                                },
                                #ecs_container_override{
                                    name = <<"wordpress">>
                                }
                            ]
                        },
                        started_by = <<"ecs-svc/9223370606521064774">>,
                        task_arn = <<"arn:aws:ecs:us-east-1:012345678910:task/c09f0188-7f87-4b0f-bfc3-16296622b6fe">>,
                        task_definition_arn = <<"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:10">>
                    }
                ]
            }}})
        ],
    output_tests(?_f(erlcloud_ecs:describe_tasks(["c09f0188-7f87-4b0f-bfc3-16296622b6fe"], [{out, record}])), Tests).

%% ListClusters test based on the API examples:
%% http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListClusters.html
list_clusters_input_tests(_) ->
    Tests = 
        [?_ecs_test(
            {"ListClusters example request",
             ?_f(erlcloud_ecs:list_clusters()), "{}"})],

    Response = "
{
  \"clusterArns\": [
    \"arn:aws:ecs:us-east-1:012345678910:cluster/My-cluster\",
    \"arn:aws:ecs:us-east-1:012345678910:cluster/default\"
  ]
}
",
    input_tests(Response, Tests).

list_clusters_output_tests(_) ->
    Tests =
        [?_ecs_test(
            {"ListClusters example response", "
{
  \"clusterArns\": [
    \"arn:aws:ecs:us-east-1:012345678910:cluster/My-cluster\",
    \"arn:aws:ecs:us-east-1:012345678910:cluster/default\"
  ]
}
",
            {ok, #ecs_list_clusters{
                cluster_arns = [
                    <<"arn:aws:ecs:us-east-1:012345678910:cluster/My-cluster">>,
                    <<"arn:aws:ecs:us-east-1:012345678910:cluster/default">>
                ]
            }}})
        ],
    output_tests(?_f(erlcloud_ecs:list_clusters([{out, record}])), Tests).

%% ListContainerInstances test based on the API examples:
%% http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListContainerInstances.html
list_container_instances_input_tests(_) ->
    Tests = 
        [?_ecs_test(
            {"ListContainerInstances example request",
             ?_f(erlcloud_ecs:list_container_instances()), "{}"})],

    Response = "
{
  \"containerInstanceArns\": [
    \"arn:aws:ecs:us-west-2:012345678910:container-instance/14e8cce9-0b16-4af4-bfac-a85f7587aa98\",
    \"arn:aws:ecs:us-west-2:012345678910:container-instance/23bbf61b-45b4-4a4f-b90c-c86290f066d6\",
    \"arn:aws:ecs:us-west-2:012345678910:container-instance/bd0abd43-94ce-4909-9750-0dcc471ca4cb\",
    \"arn:aws:ecs:us-west-2:012345678910:container-instance/c967b2ee-68ea-415b-b220-5936b26e6a04\",
    \"arn:aws:ecs:us-west-2:012345678910:container-instance/f5ec555b-8da4-48e1-8427-0e03c3674a29\"
  ]
}
",
    input_tests(Response, Tests).

list_container_instances_output_tests(_) ->
    Tests =
        [?_ecs_test(
            {"ListContainerInstances example response", "
{
  \"containerInstanceArns\": [
    \"arn:aws:ecs:us-west-2:012345678910:container-instance/14e8cce9-0b16-4af4-bfac-a85f7587aa98\",
    \"arn:aws:ecs:us-west-2:012345678910:container-instance/23bbf61b-45b4-4a4f-b90c-c86290f066d6\",
    \"arn:aws:ecs:us-west-2:012345678910:container-instance/bd0abd43-94ce-4909-9750-0dcc471ca4cb\",
    \"arn:aws:ecs:us-west-2:012345678910:container-instance/c967b2ee-68ea-415b-b220-5936b26e6a04\",
    \"arn:aws:ecs:us-west-2:012345678910:container-instance/f5ec555b-8da4-48e1-8427-0e03c3674a29\"
  ]
}
",
            {ok, #ecs_list_container_instances{
                container_instance_arns = [
                    <<"arn:aws:ecs:us-west-2:012345678910:container-instance/14e8cce9-0b16-4af4-bfac-a85f7587aa98">>,
                    <<"arn:aws:ecs:us-west-2:012345678910:container-instance/23bbf61b-45b4-4a4f-b90c-c86290f066d6">>,
                    <<"arn:aws:ecs:us-west-2:012345678910:container-instance/bd0abd43-94ce-4909-9750-0dcc471ca4cb">>,
                    <<"arn:aws:ecs:us-west-2:012345678910:container-instance/c967b2ee-68ea-415b-b220-5936b26e6a04">>,
                    <<"arn:aws:ecs:us-west-2:012345678910:container-instance/f5ec555b-8da4-48e1-8427-0e03c3674a29">>
                ]
            }}})
        ],
    output_tests(?_f(erlcloud_ecs:list_container_instances([{out, record}])), Tests).

%% ListServices test based on the API examples:
%% http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListServices.html
list_services_input_tests(_) ->
    Tests = 
        [?_ecs_test(
            {"ListServices example request",
             ?_f(erlcloud_ecs:list_services()), "{}"})],

    Response = "
{
  \"serviceArns\": [
    \"arn:aws:ecs:us-east-1:012345678910:service/hello_world\",
    \"arn:aws:ecs:us-east-1:012345678910:service/ecs-simple-service\"
  ]
}
",
    input_tests(Response, Tests).

list_services_output_tests(_) ->
    Tests =
        [?_ecs_test(
            {"ListServices example response", "
{
  \"serviceArns\": [
    \"arn:aws:ecs:us-east-1:012345678910:service/hello_world\",
    \"arn:aws:ecs:us-east-1:012345678910:service/ecs-simple-service\"
  ]
}
",
            {ok, #ecs_list_services{
                service_arns = [
                    <<"arn:aws:ecs:us-east-1:012345678910:service/hello_world">>,
                    <<"arn:aws:ecs:us-east-1:012345678910:service/ecs-simple-service">>
                ]
            }}})
        ],
    output_tests(?_f(erlcloud_ecs:list_services([{out, record}])), Tests).

%% ListTaskDefinitionFamilies test based on the API examples:
%% http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListTaskDefinitionFamilies.html
list_task_definition_families_input_tests(_) ->
    Tests = 
        [?_ecs_test(
            {"ListTaskDefinitionFamilies example request",
             ?_f(erlcloud_ecs:list_task_definition_families([{family_prefix, "hpcc"}])), "
{
    \"familyPrefix\": \"hpcc\"
}"
            })
        ],

    Response = "
{
  \"families\": [
    \"hpcc\",
    \"hpcc-t2-medium\"
  ]
}
",
    input_tests(Response, Tests).

list_task_definition_families_output_tests(_) ->
    Tests =
        [?_ecs_test(
            {"ListTaskDefinitionFamilies example response", "
{
  \"families\": [
    \"hpcc\",
    \"hpcc-t2-medium\"
  ]
}
",
            {ok, #ecs_list_task_definition_families{
                families = [
                    <<"hpcc">>,
                    <<"hpcc-t2-medium">>
                ]
            }}})
        ],
    output_tests(?_f(erlcloud_ecs:list_task_definition_families([{family_prefix, "hpcc"}, {out, record}])), Tests).

%% ListTaskDefinitions test based on the API examples:
%% http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListTaskDefinitions.html
list_task_definitions_input_tests(_) ->
    Tests = 
        [?_ecs_test(
            {"ListTaskDefinitions example response",
             ?_f(erlcloud_ecs:list_task_definitions([{family_prefix, "hello_world"}])), "
{
    \"familyPrefix\": \"hello_world\"
}"
            })
        ],

    Response = "
{
  \"taskDefinitionArns\": [
    \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:1\",
    \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:2\",
    \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:3\",
    \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:4\",
    \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:5\",
    \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:6\",
    \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:7\",
    \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:8\",
    \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:9\",
    \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:10\"
  ]
}
",
    input_tests(Response, Tests).

list_task_definitions_output_tests(_) ->
    Tests =
        [?_ecs_test(
            {"ListTaskDefinitions example response", "
{
  \"taskDefinitionArns\": [
    \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:1\",
    \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:2\",
    \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:3\",
    \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:4\",
    \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:5\",
    \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:6\",
    \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:7\",
    \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:8\",
    \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:9\",
    \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:10\"
  ]
}
",
            {ok, #ecs_list_task_definitions{
                task_definition_arns = [
                    <<"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:1">>,
                    <<"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:2">>,
                    <<"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:3">>,
                    <<"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:4">>,
                    <<"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:5">>,
                    <<"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:6">>,
                    <<"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:7">>,
                    <<"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:8">>,
                    <<"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:9">>,
                    <<"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:10">>
                ]
            }}})
        ],
    output_tests(?_f(erlcloud_ecs:list_task_definitions([{family_prefix, "hello-world"}, {out, record}])), Tests).

%% ListTasks test based on the API examples:
%% http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListTasks.html
list_tasks_input_tests(_) ->
    Tests = 
        [?_ecs_test(
            {"ListTasks example request",
             ?_f(erlcloud_ecs:list_tasks()), "{}"})],

    Response = "
{
    \"taskArns\": [
        \"arn:aws:ecs:us-east-1:012345678910:task/0b69d5c0-d655-4695-98cd-5d2d526d9d5a\",
        \"arn:aws:ecs:us-east-1:012345678910:task/51a01bdf-d00e-487e-ab14-7645330b6207\",
        \"arn:aws:ecs:us-east-1:012345678910:task/b0b28bb8-2be3-4810-b52b-88df129d893c\",
        \"arn:aws:ecs:us-east-1:012345678910:task/c09f0188-7f87-4b0f-bfc3-16296622b6fe\"
    ]
}
",
    input_tests(Response, Tests).

list_tasks_output_tests(_) ->
    Tests =
        [?_ecs_test(
            {"ListTasks example response", "
{
    \"taskArns\": [
        \"arn:aws:ecs:us-east-1:012345678910:task/0b69d5c0-d655-4695-98cd-5d2d526d9d5a\",
        \"arn:aws:ecs:us-east-1:012345678910:task/51a01bdf-d00e-487e-ab14-7645330b6207\",
        \"arn:aws:ecs:us-east-1:012345678910:task/b0b28bb8-2be3-4810-b52b-88df129d893c\",
        \"arn:aws:ecs:us-east-1:012345678910:task/c09f0188-7f87-4b0f-bfc3-16296622b6fe\"
    ]
}
",
            {ok, #ecs_list_tasks{
                task_arns = [
                    <<"arn:aws:ecs:us-east-1:012345678910:task/0b69d5c0-d655-4695-98cd-5d2d526d9d5a">>,
                    <<"arn:aws:ecs:us-east-1:012345678910:task/51a01bdf-d00e-487e-ab14-7645330b6207">>,
                    <<"arn:aws:ecs:us-east-1:012345678910:task/b0b28bb8-2be3-4810-b52b-88df129d893c">>,
                    <<"arn:aws:ecs:us-east-1:012345678910:task/c09f0188-7f87-4b0f-bfc3-16296622b6fe">>
                ]
            }}})
        ],
    output_tests(?_f(erlcloud_ecs:list_tasks([{out, record}])), Tests).

%% RegisterTaskDefinition test based on the API examples:
%% http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_RegisterTaskDefinition.html
register_task_definition_input_tests(_) ->
    Tests = 
        [?_ecs_test(
            {"RegisterTaskDefinition example request",
             ?_f(erlcloud_ecs:register_task_definition([
                                                            [
                                                                {name, "wordpress"},
                                                                {links, ["mysql"]},
                                                                {image, "wordpress"},
                                                                {essential, true},
                                                                {port_mappings, [
                                                                    {container_port, 80},
                                                                    {host_port, 80}
                                                                ]},
                                                                {memory, 500},
                                                                {cpu, 10}
                                                            ],
                                                            [
                                                                {name, "mysql"},
                                                                {image, "mysql"},
                                                                {environment, [
                                                                    {name, "MYSQL_ROOT_PASSWORD"},
                                                                    {value, "password"}
                                                                ]},
                                                                {essential, true},
                                                                {memory, 500},
                                                                {cpu, 10}
                                                            ]
                                                        ], "hello_world", [{network_mode, host}])), "
{
  \"networkMode\": \"host\",
  \"containerDefinitions\": [
    {
      \"name\": \"wordpress\",
      \"links\": [
        \"mysql\"
      ],
      \"image\": \"wordpress\",
      \"essential\": true,
      \"portMappings\": [
        {
          \"containerPort\": 80,
          \"hostPort\": 80
        }
      ],
      \"memory\": 500,
      \"cpu\": 10
    },
    {
      \"name\": \"mysql\",
      \"image\": \"mysql\",
      \"cpu\": 10,
      \"environment\": [
        {
          \"name\": \"MYSQL_ROOT_PASSWORD\",
          \"value\": \"password\"
        }
      ],
      \"memory\": 500,
      \"essential\": true
    }
  ],
  \"family\": \"hello_world\"
}
"
           })
        ],

    Response = "
{
  \"taskDefinition\": {
    \"containerDefinitions\": [
      {
        \"cpu\": 10,
        \"environment\": [],
        \"essential\": true,
        \"image\": \"wordpress\",
        \"links\": [
          \"mysql\"
        ],
        \"memory\": 500,
        \"mountPoints\": [],
        \"name\": \"wordpress\",
        \"portMappings\": [
          {
            \"containerPort\": 80,
            \"hostPort\": 80,
            \"protocol\": \"tcp\"
          }
        ],
        \"volumesFrom\": []
      },
      {
        \"cpu\": 10,
        \"environment\": [
          {
            \"name\": \"MYSQL_ROOT_PASSWORD\",
            \"value\": \"password\"
          }
        ],
        \"essential\": true,
        \"image\": \"mysql\",
        \"memory\": 500,
        \"mountPoints\": [],
        \"name\": \"mysql\",
        \"portMappings\": [],
        \"volumesFrom\": []
      }
    ],
    \"family\": \"hello_world\",
    \"networkMode\": \"host\",
    \"requiresAttributes\": [
      {
        \"name\": \"com.amazonaws.ecs.capability.docker-remote-api.1.18\"
      }
    ],
    \"revision\": 4,
    \"status\": \"ACTIVE\",
    \"taskDefinitionArn\": \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:4\",
    \"volumes\": []
  }
}
",
    input_tests(Response, Tests).

register_task_definition_output_tests(_) ->
    Tests =
        [?_ecs_test(
            {"RegisterTaskDefinition example response", "
{
  \"taskDefinition\": {
    \"containerDefinitions\": [
      {
        \"cpu\": 10,
        \"environment\": [],
        \"essential\": true,
        \"image\": \"wordpress\",
        \"links\": [
          \"mysql\"
        ],
        \"memory\": 500,
        \"mountPoints\": [],
        \"name\": \"wordpress\",
        \"portMappings\": [
          {
            \"containerPort\": 80,
            \"hostPort\": 80,
            \"protocol\": \"tcp\"
          }
        ],
        \"volumesFrom\": []
      },
      {
        \"cpu\": 10,
        \"environment\": [
          {
            \"name\": \"MYSQL_ROOT_PASSWORD\",
            \"value\": \"password\"
          }
        ],
        \"essential\": true,
        \"image\": \"mysql\",
        \"memory\": 500,
        \"mountPoints\": [],
        \"name\": \"mysql\",
        \"portMappings\": [],
        \"volumesFrom\": []
      }
    ],
    \"family\": \"hello_world\",
    \"networkMode\": \"host\",
    \"requiresAttributes\": [
      {
        \"name\": \"com.amazonaws.ecs.capability.docker-remote-api.1.18\"
      }
    ],
    \"revision\": 4,
    \"status\": \"ACTIVE\",
    \"taskDefinitionArn\": \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:4\",
    \"volumes\": []
  }
}
",
            {ok, #ecs_task_definition{
                    container_definitions = [
                        #ecs_container_definition{
                            cpu = 10,
                            environment = [],
                            essential = true,
                            image = <<"wordpress">>,
                            links = [<<"mysql">>],
                            memory = 500,
                            mount_points = [],
                            name = <<"wordpress">>,
                            port_mappings = [
                                #ecs_port_mapping{
                                    container_port = 80,
                                    host_port = 80,
                                    protocol = tcp
                                }
                            ],
                            volumes_from = []
                        },
                        #ecs_container_definition{
                            cpu = 10,
                            environment = [
                                #ecs_attribute{
                                    name = <<"MYSQL_ROOT_PASSWORD">>,
                                    value = <<"password">>
                                }
                            ],
                            essential = true,
                            image = <<"mysql">>,
                            memory = 500,
                            mount_points = [],
                            name = <<"mysql">>,
                            port_mappings = [],
                            volumes_from = []
                        }
                    ],
                    family = <<"hello_world">>,
                    network_mode = host,
                    requires_attributes = [
                        #ecs_attribute{
                            name = <<"com.amazonaws.ecs.capability.docker-remote-api.1.18">>
                        }
                    ],
                    revision = 4,
                    status = <<"ACTIVE">>,
                    task_definition_arn = <<"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:4">>,
                    volumes = []
                }
            }})
        ],
    output_tests(?_f(erlcloud_ecs:register_task_definition([
                                                            [
                                                                {name, "wordpress"},
                                                                {links, ["mysql"]},
                                                                {image, "wordpress"},
                                                                {essential, true},
                                                                {port_mappings, [
                                                                    {container_port, 80},
                                                                    {host_port, 80}
                                                                ]},
                                                                {memory, 500},
                                                                {cpu, 10}
                                                            ],
                                                            [
                                                                {name, "mysql"},
                                                                {image, "mysql"},
                                                                {environment, [
                                                                    {name, "MYSQL_ROOT_PASSWORD"},
                                                                    {value, "password"}
                                                                ]},
                                                                {essential, true},
                                                                {memory, 500},
                                                                {cpu, 10}
                                                            ]
                                                        ], "hello_world", [{network_mode, host}, {out, record}])), Tests).

%% RunTask test based on the API examples:
%% http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_RunTask.html
run_task_input_tests(_) ->
    Tests = 
        [?_ecs_test(
            {"RunTask example request",
             ?_f(erlcloud_ecs:run_task("hello_world", [{count, 1}])), "
{
  \"count\": 1,
  \"taskDefinition\": \"hello_world\"
}
"
            }),
         ?_ecs_test(
            {"RunTask example request",
             ?_f(erlcloud_ecs:run_task(
                 "hello_world",
                 [{count, 1},
                  {placement_strategy, [
                      [{field, "attribute:ecs.availability-zone"},
                       {type, "spread"}],
                      [{field, "instanceId"},
                       {type, "spread"}]
                  ]}
                 ])), "
{
  \"count\": 1,
  \"placementStrategy\": [
      {\"type\": \"spread\",
       \"field\": \"attribute:ecs.availability-zone\"},
      {\"type\": \"spread\",
       \"field\": \"instanceId\"}
   ],
  \"taskDefinition\": \"hello_world\"
}
"
            })
        ],
    Response = "
{
  \"failures\": [],
  \"tasks\": [
    {
      \"clusterArn\": \"arn:aws:ecs:us-east-1:012345678910:cluster/default\",
      \"containerInstanceArn\": \"arn:aws:ecs:us-east-1:012345678910:container-instance/cf447635-790d-477d-be24-58a9cb819d45\",
      \"containers\": [
        {
          \"containerArn\": \"arn:aws:ecs:us-east-1:012345678910:container/e1ed7aac-d9b2-4315-8726-d2432bf11868\",
          \"lastStatus\": \"PENDING\",
          \"name\": \"wordpress\",
          \"taskArn\": \"arn:aws:ecs:us-east-1:012345678910:task/d8c67b3c-ac87-4ffe-a847-4785bc3a8b55\"
        },
        {
          \"containerArn\": \"arn:aws:ecs:us-east-1:012345678910:container/4b69fabd-991d-4781-bbf9-7efa03c754aa\",
          \"lastStatus\": \"PENDING\",
          \"name\": \"mysql\",
          \"taskArn\": \"arn:aws:ecs:us-east-1:012345678910:task/d8c67b3c-ac87-4ffe-a847-4785bc3a8b55\"
        }
      ],
      \"desiredStatus\": \"RUNNING\",
      \"lastStatus\": \"PENDING\",
      \"overrides\": {
        \"containerOverrides\": [
          {
            \"name\": \"wordpress\"
          },
          {
            \"name\": \"mysql\"
          }
        ]
      },
      \"taskArn\": \"arn:aws:ecs:us-east-1:012345678910:task/d8c67b3c-ac87-4ffe-a847-4785bc3a8b55\",
      \"taskDefinitionArn\": \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:11\"
    }
  ]
}
",
    input_tests(Response, Tests).

run_task_output_tests(_) ->
    Tests =
        [?_ecs_test(
            {"RunTask example response", "
{
  \"failures\": [],
  \"tasks\": [
    {
      \"clusterArn\": \"arn:aws:ecs:us-east-1:012345678910:cluster/default\",
      \"containerInstanceArn\": \"arn:aws:ecs:us-east-1:012345678910:container-instance/cf447635-790d-477d-be24-58a9cb819d45\",
      \"containers\": [
        {
          \"containerArn\": \"arn:aws:ecs:us-east-1:012345678910:container/e1ed7aac-d9b2-4315-8726-d2432bf11868\",
          \"lastStatus\": \"PENDING\",
          \"name\": \"wordpress\",
          \"taskArn\": \"arn:aws:ecs:us-east-1:012345678910:task/d8c67b3c-ac87-4ffe-a847-4785bc3a8b55\"
        },
        {
          \"containerArn\": \"arn:aws:ecs:us-east-1:012345678910:container/4b69fabd-991d-4781-bbf9-7efa03c754aa\",
          \"lastStatus\": \"PENDING\",
          \"name\": \"mysql\",
          \"taskArn\": \"arn:aws:ecs:us-east-1:012345678910:task/d8c67b3c-ac87-4ffe-a847-4785bc3a8b55\"
        }
      ],
      \"desiredStatus\": \"RUNNING\",
      \"lastStatus\": \"PENDING\",
      \"overrides\": {
        \"containerOverrides\": [
          {
            \"name\": \"wordpress\"
          },
          {
            \"name\": \"mysql\"
          }
        ]
      },
      \"taskArn\": \"arn:aws:ecs:us-east-1:012345678910:task/d8c67b3c-ac87-4ffe-a847-4785bc3a8b55\",
      \"taskDefinitionArn\": \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:11\"
    }
  ]
}
",
            {ok, #ecs_run_task{
                failures = [],
                tasks = [
                    #ecs_task{
                        cluster_arn = <<"arn:aws:ecs:us-east-1:012345678910:cluster/default">>,
                        container_instance_arn = <<"arn:aws:ecs:us-east-1:012345678910:container-instance/cf447635-790d-477d-be24-58a9cb819d45">>,
                        containers = [
                            #ecs_container{
                                container_arn = <<"arn:aws:ecs:us-east-1:012345678910:container/e1ed7aac-d9b2-4315-8726-d2432bf11868">>,
                                last_status = <<"PENDING">>,
                                name = <<"wordpress">>,
                                task_arn = <<"arn:aws:ecs:us-east-1:012345678910:task/d8c67b3c-ac87-4ffe-a847-4785bc3a8b55">>
                            },
                            #ecs_container{
                                container_arn = <<"arn:aws:ecs:us-east-1:012345678910:container/4b69fabd-991d-4781-bbf9-7efa03c754aa">>,
                                last_status = <<"PENDING">>,
                                name = <<"mysql">>,
                                task_arn = <<"arn:aws:ecs:us-east-1:012345678910:task/d8c67b3c-ac87-4ffe-a847-4785bc3a8b55">>
                            }
                        ],
                        desired_status = <<"RUNNING">>,
                        last_status = <<"PENDING">>,
                        overrides = #ecs_task_override{
                            container_overrides = [
                                #ecs_container_override{
                                    name = <<"wordpress">>
                                },
                                #ecs_container_override{
                                    name = <<"mysql">>
                                }
                            ]
                        },
                        task_arn = <<"arn:aws:ecs:us-east-1:012345678910:task/d8c67b3c-ac87-4ffe-a847-4785bc3a8b55">>,
                        task_definition_arn = <<"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:11">>
                    }
                ]
            }}})
        ],
    output_tests(?_f(erlcloud_ecs:run_task("hello_world", [{count, 1}, {out, record}])), Tests).

%% StartTask test based on the API examples:
%% http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_StartTask.html
start_task_input_tests(_) ->
    Tests = 
        [?_ecs_test(
            {"StartTask example request",
             ?_f(erlcloud_ecs:start_task("hello_world", ["8db248d6-16a7-42b5-b9f9-43d3b1ad9430"])), "
{
  \"containerInstances\": [
    \"8db248d6-16a7-42b5-b9f9-43d3b1ad9430\"
  ],
  \"taskDefinition\": \"hello_world\"
}
"
            })
        ],

    Response = "
{
  \"failures\": [],
  \"tasks\": [
    {
      \"clusterArn\": \"arn:aws:ecs:us-east-1:012345678910:cluster/default\",
      \"containerInstanceArn\": \"arn:aws:ecs:us-east-1:012345678910:container-instance/8db248d6-16a7-42b5-b9f9-43d3b1ad9430\",
      \"containers\": [
        {
          \"containerArn\": \"arn:aws:ecs:us-east-1:012345678910:container/37234a82-77f6-41d7-b54b-591f1e278093\",
          \"lastStatus\": \"PENDING\",
          \"name\": \"wordpress\",
          \"taskArn\": \"arn:aws:ecs:us-east-1:012345678910:task/a126249b-b7e4-4b06-9d8f-1b56e75a99b5\"
        },
        {
          \"containerArn\": \"arn:aws:ecs:us-east-1:012345678910:container/05a5528c-77f6-4e5b-8f9a-2b0a1928a926\",
          \"lastStatus\": \"PENDING\",
          \"name\": \"mysql\",
          \"taskArn\": \"arn:aws:ecs:us-east-1:012345678910:task/a126249b-b7e4-4b06-9d8f-1b56e75a99b5\"
        }
      ],
      \"desiredStatus\": \"RUNNING\",
      \"lastStatus\": \"PENDING\",
      \"overrides\": {
        \"containerOverrides\": [
          {
            \"name\": \"wordpress\"
          },
          {
            \"name\": \"mysql\"
          }
        ]
      },
      \"taskArn\": \"arn:aws:ecs:us-east-1:012345678910:task/a126249b-b7e4-4b06-9d8f-1b56e75a99b5\",
      \"taskDefinitionArn\": \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:11\"
    }
  ]
}
",
    input_tests(Response, Tests).

start_task_output_tests(_) ->
    Tests =
        [?_ecs_test(
            {"StartTask example response", "
{
  \"failures\": [],
  \"tasks\": [
    {
      \"clusterArn\": \"arn:aws:ecs:us-east-1:012345678910:cluster/default\",
      \"containerInstanceArn\": \"arn:aws:ecs:us-east-1:012345678910:container-instance/8db248d6-16a7-42b5-b9f9-43d3b1ad9430\",
      \"containers\": [
        {
          \"containerArn\": \"arn:aws:ecs:us-east-1:012345678910:container/37234a82-77f6-41d7-b54b-591f1e278093\",
          \"lastStatus\": \"PENDING\",
          \"name\": \"wordpress\",
          \"taskArn\": \"arn:aws:ecs:us-east-1:012345678910:task/a126249b-b7e4-4b06-9d8f-1b56e75a99b5\"
        },
        {
          \"containerArn\": \"arn:aws:ecs:us-east-1:012345678910:container/05a5528c-77f6-4e5b-8f9a-2b0a1928a926\",
          \"lastStatus\": \"PENDING\",
          \"name\": \"mysql\",
          \"taskArn\": \"arn:aws:ecs:us-east-1:012345678910:task/a126249b-b7e4-4b06-9d8f-1b56e75a99b5\"
        }
      ],
      \"desiredStatus\": \"RUNNING\",
      \"lastStatus\": \"PENDING\",
      \"overrides\": {
        \"containerOverrides\": [
          {
            \"name\": \"wordpress\"
          },
          {
            \"name\": \"mysql\"
          }
        ]
      },
      \"taskArn\": \"arn:aws:ecs:us-east-1:012345678910:task/a126249b-b7e4-4b06-9d8f-1b56e75a99b5\",
      \"taskDefinitionArn\": \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:11\"
    }
  ]
}
",
            {ok, #ecs_start_task{
                failures = [],
                tasks = [
                    #ecs_task{
                        cluster_arn = <<"arn:aws:ecs:us-east-1:012345678910:cluster/default">>,
                        container_instance_arn = <<"arn:aws:ecs:us-east-1:012345678910:container-instance/8db248d6-16a7-42b5-b9f9-43d3b1ad9430">>,
                        containers = [
                            #ecs_container{
                                container_arn = <<"arn:aws:ecs:us-east-1:012345678910:container/37234a82-77f6-41d7-b54b-591f1e278093">>,
                                last_status = <<"PENDING">>,
                                name = <<"wordpress">>,
                                task_arn = <<"arn:aws:ecs:us-east-1:012345678910:task/a126249b-b7e4-4b06-9d8f-1b56e75a99b5">>
                            },
                            #ecs_container{
                                container_arn = <<"arn:aws:ecs:us-east-1:012345678910:container/05a5528c-77f6-4e5b-8f9a-2b0a1928a926">>,
                                last_status = <<"PENDING">>,
                                name = <<"mysql">>,
                                task_arn = <<"arn:aws:ecs:us-east-1:012345678910:task/a126249b-b7e4-4b06-9d8f-1b56e75a99b5">>
                            }
                        ],
                        desired_status = <<"RUNNING">>,
                        last_status = <<"PENDING">>,
                        overrides = #ecs_task_override{
                            container_overrides = [
                                #ecs_container_override{
                                    name = <<"wordpress">>
                                },
                                #ecs_container_override{
                                    name = <<"mysql">>
                                }
                            ]
                        },
                        task_arn = <<"arn:aws:ecs:us-east-1:012345678910:task/a126249b-b7e4-4b06-9d8f-1b56e75a99b5">>,
                        task_definition_arn = <<"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:11">>
                    }
                ]
            }}})
        ],
    output_tests(?_f(erlcloud_ecs:start_task("hello_world", ["8db248d6-16a7-42b5-b9f9-43d3b1ad9430"], [{out, record}])), Tests).

%% StopTask test based on the API examples:
%% http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_StopTask.html
stop_task_input_tests(_) ->
    Tests = 
        [?_ecs_test(
            {"StopTask example request",
             ?_f(erlcloud_ecs:stop_task("a126249b-b7e4-4b06-9d8f-1b56e75a99b5")), "
{
  \"task\": \"a126249b-b7e4-4b06-9d8f-1b56e75a99b5\"
}
"
            })
        ],
    Response = "
{
  \"task\": {
    \"clusterArn\": \"arn:aws:ecs:us-east-1:012345678910:cluster/default\",
    \"containerInstanceArn\": \"arn:aws:ecs:us-east-1:012345678910:container-instance/8db248d6-16a7-42b5-b9f9-43d3b1ad9430\",
    \"containers\": [
      {
        \"containerArn\": \"arn:aws:ecs:us-east-1:012345678910:container/05a5528c-77f6-4e5b-8f9a-2b0a1928a926\",
        \"lastStatus\": \"RUNNING\",
        \"name\": \"mysql\",
        \"networkBindings\": [],
        \"taskArn\": \"arn:aws:ecs:us-east-1:012345678910:task/a126249b-b7e4-4b06-9d8f-1b56e75a99b5\"
      },
      {
        \"containerArn\": \"arn:aws:ecs:us-east-1:012345678910:container/37234a82-77f6-41d7-b54b-591f1e278093\",
        \"lastStatus\": \"RUNNING\",
        \"name\": \"wordpress\",
        \"networkBindings\": [
          {
            \"bindIP\": \"0.0.0.0\",
            \"containerPort\": 80,
            \"hostPort\": 80
          }
        ],
        \"taskArn\": \"arn:aws:ecs:us-east-1:012345678910:task/a126249b-b7e4-4b06-9d8f-1b56e75a99b5\"
      }
    ],
    \"desiredStatus\": \"STOPPED\",
    \"lastStatus\": \"RUNNING\",
    \"overrides\": { 
      \"containerOverrides\": [
        {
          \"name\": \"mysql\"
        },
        {
          \"name\": \"wordpress\"
        }
      ]
    },
    \"taskArn\": \"arn:aws:ecs:us-east-1:012345678910:task/a126249b-b7e4-4b06-9d8f-1b56e75a99b5\",
    \"taskDefinitionArn\": \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:11\"
  }
}
",
    input_tests(Response, Tests).

stop_task_output_tests(_) ->
    Tests =
        [?_ecs_test(
            {"StopTask example response", "
{
  \"task\": {
    \"clusterArn\": \"arn:aws:ecs:us-east-1:012345678910:cluster/default\",
    \"containerInstanceArn\": \"arn:aws:ecs:us-east-1:012345678910:container-instance/8db248d6-16a7-42b5-b9f9-43d3b1ad9430\",
    \"containers\": [
      {
        \"containerArn\": \"arn:aws:ecs:us-east-1:012345678910:container/05a5528c-77f6-4e5b-8f9a-2b0a1928a926\",
        \"lastStatus\": \"RUNNING\",
        \"name\": \"mysql\",
        \"networkBindings\": [],
        \"taskArn\": \"arn:aws:ecs:us-east-1:012345678910:task/a126249b-b7e4-4b06-9d8f-1b56e75a99b5\"
      },
      {
        \"containerArn\": \"arn:aws:ecs:us-east-1:012345678910:container/37234a82-77f6-41d7-b54b-591f1e278093\",
        \"lastStatus\": \"RUNNING\",
        \"name\": \"wordpress\",
        \"networkBindings\": [
          {
            \"bindIP\": \"0.0.0.0\",
            \"containerPort\": 80,
            \"hostPort\": 80
          }
        ],
        \"taskArn\": \"arn:aws:ecs:us-east-1:012345678910:task/a126249b-b7e4-4b06-9d8f-1b56e75a99b5\"
      }
    ],
    \"desiredStatus\": \"STOPPED\",
    \"lastStatus\": \"RUNNING\",
    \"overrides\": { 
      \"containerOverrides\": [
        {
          \"name\": \"mysql\"
        },
        {
          \"name\": \"wordpress\"
        }
      ]
    },
    \"taskArn\": \"arn:aws:ecs:us-east-1:012345678910:task/a126249b-b7e4-4b06-9d8f-1b56e75a99b5\",
    \"taskDefinitionArn\": \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:11\"
  }
}
",
            {ok, #ecs_task{
                    cluster_arn = <<"arn:aws:ecs:us-east-1:012345678910:cluster/default">>,
                    container_instance_arn = <<"arn:aws:ecs:us-east-1:012345678910:container-instance/8db248d6-16a7-42b5-b9f9-43d3b1ad9430">>,
                    containers = [
                        #ecs_container{
                            container_arn = <<"arn:aws:ecs:us-east-1:012345678910:container/05a5528c-77f6-4e5b-8f9a-2b0a1928a926">>,
                            last_status = <<"RUNNING">>,
                            name = <<"mysql">>,
                            network_bindings = [],
                            task_arn = <<"arn:aws:ecs:us-east-1:012345678910:task/a126249b-b7e4-4b06-9d8f-1b56e75a99b5">>
                        },
                        #ecs_container{
                            container_arn = <<"arn:aws:ecs:us-east-1:012345678910:container/37234a82-77f6-41d7-b54b-591f1e278093">>,
                            last_status = <<"RUNNING">>,
                            name = <<"wordpress">>,
                            network_bindings = [
                                #ecs_network_binding{
                                    bind_ip = <<"0.0.0.0">>,
                                    container_port = 80,
                                    host_port = 80
                                }
                            ],
                            task_arn = <<"arn:aws:ecs:us-east-1:012345678910:task/a126249b-b7e4-4b06-9d8f-1b56e75a99b5">>
                        }
                    ],
                    desired_status = <<"STOPPED">>,
                    last_status = <<"RUNNING">>,
                    overrides = #ecs_task_override{
                        container_overrides = [
                            #ecs_container_override{
                                name = <<"mysql">>
                            },
                            #ecs_container_override{
                                name = <<"wordpress">>
                            }
                        ]
                    },
                    task_arn = <<"arn:aws:ecs:us-east-1:012345678910:task/a126249b-b7e4-4b06-9d8f-1b56e75a99b5">>,
                    task_definition_arn = <<"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:11">>
                }
            }})
        ],
    output_tests(?_f(erlcloud_ecs:stop_task("a126249b-b7e4-4b06-9d8f-1b56e75a99b5", [{out, record}])), Tests).

%% UpdateContainerAgent test based on the API examples:
%% http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_UpdateContainerAgent.html
update_container_agent_input_tests(_) ->
    Tests = 
        [?_ecs_test(
            {"UpdateContainerAgent example request",
             ?_f(erlcloud_ecs:update_container_agent("c9c9a6f2-8766-464b-8805-9c57b9368fb0", [{cluster, "update"}])), "
{
  \"cluster\": \"update\",
  \"containerInstance\": \"c9c9a6f2-8766-464b-8805-9c57b9368fb0\"
}
"
            })
        ],
    Response = "
{
    \"containerInstance\": {
        \"agentConnected\": true,
        \"agentUpdateStatus\": \"PENDING\",
        \"attributes\": [
          {
            \"name\": \"com.amazonaws.ecs.capability.privileged-container\"
          },
          {
            \"name\": \"com.amazonaws.ecs.capability.docker-remote-api.1.17\"
          },
          {
            \"name\": \"com.amazonaws.ecs.capability.docker-remote-api.1.18\"
          },
          {
            \"name\": \"com.amazonaws.ecs.capability.docker-remote-api.1.19\"
          },
          {
            \"name\": \"com.amazonaws.ecs.capability.logging-driver.json-file\"
          },
          {
            \"name\": \"com.amazonaws.ecs.capability.logging-driver.syslog\"
          }
        ],
        \"containerInstanceArn\": \"arn:aws:ecs:us-west-2:012345678910:container-instance/c9c9a6f2-8766-464b-8805-9c57b9368fb0\",
        \"ec2InstanceId\": \"i-0c3826c9\",
        \"pendingTasksCount\": 0,
        \"registeredResources\": [
          {
            \"doubleValue\": 0,
            \"integerValue\": 1024,
            \"longValue\": 0,
            \"name\": \"CPU\",
            \"type\": \"INTEGER\"
          },
          {
            \"doubleValue\": 0,
            \"integerValue\": 995,
            \"longValue\": 0,
            \"name\": \"MEMORY\",
            \"type\": \"INTEGER\"
          },
          {
            \"doubleValue\": 0,
            \"integerValue\": 0,
            \"longValue\": 0,
            \"name\": \"PORTS\",
            \"stringSetValue\": [
              \"22\",
              \"2376\",
              \"2375\",
              \"51678\"
            ],
            \"type\": \"STRINGSET\"
          },
          {
            \"doubleValue\": 0,
            \"integerValue\": 0,
            \"longValue\": 0,
            \"name\": \"PORTS_UDP\",
            \"stringSetValue\": [],
            \"type\": \"STRINGSET\"
          }
        ],
        \"remainingResources\": [
          {
            \"doubleValue\": 0,
            \"integerValue\": 1024,
            \"longValue\": 0,
            \"name\": \"CPU\",
            \"type\": \"INTEGER\"
          },
          {
            \"doubleValue\": 0,
            \"integerValue\": 995,
            \"longValue\": 0,
            \"name\": \"MEMORY\",
            \"type\": \"INTEGER\"
          },
          {
            \"doubleValue\": 0,
            \"integerValue\": 0,
            \"longValue\": 0,
            \"name\": \"PORTS\",
            \"stringSetValue\": [
              \"22\",
              \"2376\",
              \"2375\",
              \"51678\"
            ],
            \"type\": \"STRINGSET\"
          },
          {
            \"doubleValue\": 0,
            \"integerValue\": 0,
            \"longValue\": 0,
            \"name\": \"PORTS_UDP\",
            \"stringSetValue\": [],
            \"type\": \"STRINGSET\"
          }
        ],
        \"runningTasksCount\": 0,
        \"status\": \"INACTIVE\",
        \"versionInfo\": {
          \"agentHash\": \"b197edd\",
          \"agentVersion\": \"1.5.0\",
          \"dockerVersion\": \"DockerVersion: 1.7.1\"
        }
      }
}",
    input_tests(Response, Tests).

update_container_agent_output_tests(_) ->
    Tests =
        [?_ecs_test(
            {"UpdateContainerAgent example response", "
{
    \"containerInstance\": {
        \"agentConnected\": true,
        \"agentUpdateStatus\": \"PENDING\",
        \"attributes\": [
          {
            \"name\": \"com.amazonaws.ecs.capability.privileged-container\"
          },
          {
            \"name\": \"com.amazonaws.ecs.capability.docker-remote-api.1.17\"
          },
          {
            \"name\": \"com.amazonaws.ecs.capability.docker-remote-api.1.18\"
          },
          {
            \"name\": \"com.amazonaws.ecs.capability.docker-remote-api.1.19\"
          },
          {
            \"name\": \"com.amazonaws.ecs.capability.logging-driver.json-file\"
          },
          {
            \"name\": \"com.amazonaws.ecs.capability.logging-driver.syslog\"
          }
        ],
        \"containerInstanceArn\": \"arn:aws:ecs:us-west-2:012345678910:container-instance/c9c9a6f2-8766-464b-8805-9c57b9368fb0\",
        \"ec2InstanceId\": \"i-0c3826c9\",
        \"pendingTasksCount\": 0,
        \"registeredResources\": [
          {
            \"doubleValue\": 0,
            \"integerValue\": 1024,
            \"longValue\": 0,
            \"name\": \"CPU\",
            \"type\": \"INTEGER\"
          },
          {
            \"doubleValue\": 0,
            \"integerValue\": 995,
            \"longValue\": 0,
            \"name\": \"MEMORY\",
            \"type\": \"INTEGER\"
          },
          {
            \"doubleValue\": 0,
            \"integerValue\": 0,
            \"longValue\": 0,
            \"name\": \"PORTS\",
            \"stringSetValue\": [
              \"22\",
              \"2376\",
              \"2375\",
              \"51678\"
            ],
            \"type\": \"STRINGSET\"
          },
          {
            \"doubleValue\": 0,
            \"integerValue\": 0,
            \"longValue\": 0,
            \"name\": \"PORTS_UDP\",
            \"stringSetValue\": [],
            \"type\": \"STRINGSET\"
          }
        ],
        \"remainingResources\": [
          {
            \"doubleValue\": 0,
            \"integerValue\": 1024,
            \"longValue\": 0,
            \"name\": \"CPU\",
            \"type\": \"INTEGER\"
          },
          {
            \"doubleValue\": 0,
            \"integerValue\": 995,
            \"longValue\": 0,
            \"name\": \"MEMORY\",
            \"type\": \"INTEGER\"
          },
          {
            \"doubleValue\": 0,
            \"integerValue\": 0,
            \"longValue\": 0,
            \"name\": \"PORTS\",
            \"stringSetValue\": [
              \"22\",
              \"2376\",
              \"2375\",
              \"51678\"
            ],
            \"type\": \"STRINGSET\"
          },
          {
            \"doubleValue\": 0,
            \"integerValue\": 0,
            \"longValue\": 0,
            \"name\": \"PORTS_UDP\",
            \"stringSetValue\": [],
            \"type\": \"STRINGSET\"
          }
        ],
        \"runningTasksCount\": 0,
        \"status\": \"INACTIVE\",
        \"versionInfo\": {
          \"agentHash\": \"b197edd\",
          \"agentVersion\": \"1.5.0\",
          \"dockerVersion\": \"DockerVersion: 1.7.1\"
        }
      }
}
",
            {ok, #ecs_container_instance{
                    agent_connected = true,
                    agent_update_status = <<"PENDING">>,
                    attributes = [
                        #ecs_attribute{
                            name = <<"com.amazonaws.ecs.capability.privileged-container">>
                        },
                        #ecs_attribute{
                            name = <<"com.amazonaws.ecs.capability.docker-remote-api.1.17">>
                        },
                        #ecs_attribute{
                            name = <<"com.amazonaws.ecs.capability.docker-remote-api.1.18">>
                        },
                        #ecs_attribute{
                            name = <<"com.amazonaws.ecs.capability.docker-remote-api.1.19">>
                        },
                        #ecs_attribute{
                            name = <<"com.amazonaws.ecs.capability.logging-driver.json-file">>
                        },
                        #ecs_attribute{
                            name = <<"com.amazonaws.ecs.capability.logging-driver.syslog">>
                        }
                    ],
                    container_instance_arn = <<"arn:aws:ecs:us-west-2:012345678910:container-instance/c9c9a6f2-8766-464b-8805-9c57b9368fb0">>,
                    ec2_instance_id = <<"i-0c3826c9">>,
                    pending_tasks_count = 0,
                    registered_resources = [
                        #ecs_resource{
                            double_value = 0,
                            integer_value = 1024,
                            long_value = 0,
                            name = <<"CPU">>,
                            type = <<"INTEGER">>
                        },
                        #ecs_resource{
                            double_value = 0,
                            integer_value = 995,
                            long_value = 0,
                            name = <<"MEMORY">>,
                            type = <<"INTEGER">>
                        },
                        #ecs_resource{
                            double_value = 0,
                            integer_value = 0,
                            long_value = 0,
                            name = <<"PORTS">>,
                            string_set_value = [<<"22">>, <<"2376">>, <<"2375">>, <<"51678">>],
                            type = <<"STRINGSET">>
                        },
                        #ecs_resource{
                            double_value = 0,
                            integer_value = 0,
                            long_value = 0,
                            name = <<"PORTS_UDP">>,
                            string_set_value = [],
                            type = <<"STRINGSET">>
                        }
                    ],
                    remaining_resources = [
                        #ecs_resource{
                            double_value = 0,
                            integer_value = 1024,
                            long_value = 0,
                            name = <<"CPU">>,
                            type = <<"INTEGER">>
                        },
                        #ecs_resource{
                            double_value = 0,
                            integer_value = 995,
                            long_value = 0,
                            name = <<"MEMORY">>,
                            type = <<"INTEGER">>
                        },
                        #ecs_resource{
                            double_value = 0,
                            integer_value = 0,
                            long_value = 0,
                            name = <<"PORTS">>,
                            string_set_value = [<<"22">>, <<"2376">>, <<"2375">>, <<"51678">>],
                            type = <<"STRINGSET">>
                        },
                        #ecs_resource{
                            double_value = 0,
                            integer_value = 0,
                            long_value = 0,
                            name = <<"PORTS_UDP">>,
                            string_set_value = [],
                            type = <<"STRINGSET">>
                        }
                    ],
                    running_tasks_count = 0,
                    status = <<"INACTIVE">>,
                    version_info = #ecs_version_info{
                        agent_hash = <<"b197edd">>,
                        agent_version = <<"1.5.0">>,
                        docker_version = <<"DockerVersion: 1.7.1">>
                    }
                }
            }})
        ],
    output_tests(?_f(erlcloud_ecs:update_container_agent("c9c9a6f2-8766-464b-8805-9c57b9368fb0", [{cluster, "update"}, {out, record}])), Tests).

%% UpdateService test based on the API examples:
%% http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_UpdateService.html
update_service_input_tests(_) ->
    Tests = 
        [?_ecs_test(
            {"UpdateService example request",
             ?_f(erlcloud_ecs:update_service("hello_world", [{desired_count, 3}])), "
{
    \"service\": \"hello_world\",
    \"desiredCount\": 3
}
"
            })
        ],
    Response = "
{
  \"service\": {
    \"clusterArn\": \"arn:aws:ecs:us-east-1:012345678910:cluster/default\",
    \"deploymentConfiguration\": {
        \"maximumPercent\": 200,
        \"minimumHealthyPercent\": 100
    },
    \"deployments\": [
      {
        \"createdAt\": 1430333711.033,
        \"desiredCount\": 3,
        \"id\": \"ecs-svc/9223370606521064774\",
        \"pendingCount\": 0,
        \"runningCount\": 0,
        \"status\": \"PRIMARY\",
        \"taskDefinition\": \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:10\",
        \"updatedAt\": 1430336267.173
      }
    ],
    \"desiredCount\": 3,
    \"events\": [],
    \"loadBalancers\": [],
    \"pendingCount\": 0,
    \"runningCount\": 0,
    \"serviceArn\": \"arn:aws:ecs:us-east-1:012345678910:service/hello_world\",
    \"serviceName\": \"hello_world\",
    \"status\": \"ACTIVE\",
    \"taskDefinition\": \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:10\"
  }
}
",
    input_tests(Response, Tests).

update_service_output_tests(_) ->
    Tests =
        [?_ecs_test(
            {"UpdateService example response", "
{
  \"service\": {
    \"clusterArn\": \"arn:aws:ecs:us-east-1:012345678910:cluster/default\",
    \"deploymentConfiguration\": {
        \"maximumPercent\": 200,
        \"minimumHealthyPercent\": 100
    },
    \"deployments\": [
      {
        \"createdAt\": 1430333711.033,
        \"desiredCount\": 3,
        \"id\": \"ecs-svc/9223370606521064774\",
        \"pendingCount\": 0,
        \"runningCount\": 0,
        \"status\": \"PRIMARY\",
        \"taskDefinition\": \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:10\",
        \"updatedAt\": 1430336267.173
      }
    ],
    \"desiredCount\": 3,
    \"events\": [],
    \"loadBalancers\": [],
    \"pendingCount\": 0,
    \"runningCount\": 0,
    \"serviceArn\": \"arn:aws:ecs:us-east-1:012345678910:service/hello_world\",
    \"serviceName\": \"hello_world\",
    \"status\": \"ACTIVE\",
    \"taskDefinition\": \"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:10\"
  }
}
",
            {ok, #ecs_service{
                    cluster_arn = <<"arn:aws:ecs:us-east-1:012345678910:cluster/default">>,
                    deployment_configuration = #ecs_deployment_configuration{
                        maximum_percent = 200,
                        minimum_healthy_percent = 100
                    },
                    deployments = [
                        #ecs_deployment{
                            created_at = 1430333711.033,
                            desired_count = 3,
                            id = <<"ecs-svc/9223370606521064774">>,
                            pending_count = 0,
                            running_count = 0,
                            status = <<"PRIMARY">>,
                            task_definition = <<"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:10">>,
                            updated_at = 1430336267.173
                        }
                    ],
                    desired_count = 3,
                    events = [],
                    load_balancers = [],
                    pending_count = 0,
                    running_count = 0,
                    service_arn = <<"arn:aws:ecs:us-east-1:012345678910:service/hello_world">>,
                    service_name = <<"hello_world">>,
                    status = <<"ACTIVE">>,
                    task_definition = <<"arn:aws:ecs:us-east-1:012345678910:task-definition/hello_world:10">>
                }
            }})
        ],
    output_tests(?_f(erlcloud_ecs:update_service("hello_world", [{desired_count, 3}, {out, record}])), Tests).

%%%===================================================================
%%% Input test helpers
%%%===================================================================

-type expected_body() :: string().

sort_json([{_, _} | _] = Json) ->
    %% Value is an object
    SortedChildren = [{K, sort_json(V)} || {K,V} <- Json],
    lists:keysort(1, SortedChildren);
sort_json([_|_] = Json) ->
    %% Value is an array
    [sort_json(I) || I <- Json];
sort_json(V) ->
    V.

%% verifies that the parameters in the body match the expected parameters
-spec validate_body(binary(), expected_body()) -> ok.
validate_body(Body, Expected) ->
    Want = sort_json(jsx:decode(list_to_binary(Expected))),
    Actual = sort_json(jsx:decode(Body)),
    case Want =:= Actual of
        true -> ok;
        false ->
            ?debugFmt("~nEXPECTED~n~p~nACTUAL~n~p~n", [Want, Actual])
    end,
    ?assertEqual(Want, Actual).


%% returns the mock of the erlcloud_httpc function input tests expect to be called.
%% Validates the request body and responds with the provided response.
-spec input_expect(string(), expected_body()) -> fun().
input_expect(Response, Expected) ->
    fun(_Url, post, _Headers, Body, _Timeout, _Config) ->
            validate_body(Body, Expected),
            {ok, {{200, "OK"}, [], list_to_binary(Response)}}
    end.


%% input_test converts an input_test specifier into an eunit test generator
-type input_test_spec() :: {pos_integer(), {fun(), expected_body()} | {string(), fun(), expected_body()}}.
-spec input_test(string(), input_test_spec()) -> tuple().
input_test(Response, {Line, {Description, Fun, Expected}})
  when is_list(Description) ->
    {Description,
     {Line,
      fun() ->
              meck:expect(erlcloud_httpc, request, input_expect(Response, Expected)),
              erlcloud_ecs:configure(string:copies("A", 20), string:copies("a", 40)),
              Fun()
      end}}.


%% input_tests converts a list of input_test specifiers into an eunit test generator
-spec input_tests(string(), [input_test_spec()]) -> [tuple()].
input_tests(Response, Tests) ->
    [input_test(Response, Test) || Test <- Tests].

%%%===================================================================
%%% Output test helpers
%%%===================================================================

%% returns the mock of the erlcloud_httpc function output tests expect to be called.
-spec output_expect(string()) -> fun().
output_expect(Response) ->
    fun(_Url, post, _Headers, _Body, _Timeout, _Config) ->
            {ok, {{200, "OK"}, [], Response}}
    end.

%% output_test converts an output_test specifier into an eunit test generator
-type output_test_spec() :: {pos_integer(), {string(), term()} | {string(), string(), term()}}.
-spec output_test(fun(), output_test_spec()) -> tuple().
output_test(Fun, {Line, {Description, Response, Result}}) ->
    {Description,
     {Line,
      fun() ->
              meck:expect(erlcloud_httpc, request, output_expect(list_to_binary(Response))),
              erlcloud_ecs:configure(string:copies("A", 20), string:copies("a", 40)),
              Actual = Fun(),
              case Result =:= Actual of
                  true -> ok;
                  false ->
                      ?debugFmt("~nEXPECTED~n~p~nACTUAL~n~p~n", [Result, Actual])
              end,
              ?assertEqual(Result, Actual)
      end}}.


%% output_tests converts a list of output_test specifiers into an eunit test generator
-spec output_tests(fun(), [output_test_spec()]) -> [term()].
output_tests(Fun, Tests) ->
    [output_test(Fun, Test) || Test <- Tests].
