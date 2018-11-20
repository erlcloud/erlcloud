%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(erlcloud_elb_tests).
-include_lib("eunit/include/eunit.hrl").


%%%===================================================================
%%% Test entry points
%%%===================================================================

start() ->
    meck:new(erlcloud_aws, [passthrough]),
    ok.


stop(_) ->
    meck:unload(erlcloud_aws).


elb_tags_test_() ->
    {foreach,
        fun start/0,
        fun stop/1,
        [
            {"Request describe_tags.",
                fun() ->
                    meck:expect(erlcloud_aws, aws_request_xml4, fun(_, _, _, _, _, _) ->
                        {ok, describe_tags_response_xmerl()} end),
                    LoadBalancerName = "vvorobyov-classic",
                    Resp = erlcloud_elb:describe_tags(
                        [LoadBalancerName],
                        erlcloud_aws:default_config()),
                    ?assertMatch(
                        {ok, [
                            [
                                {load_balancer_name, LoadBalancerName},
                                {tags, [[{value, _}, {key, _}], [{value, _}, {key, _}]]}
                            ]
                        ]},
                        Resp
                    )
                end}
        ]
    }.


%%%===================================================================
%%% Helpers
%%%===================================================================

describe_tags_response_xmerl() ->
    XML = "<DescribeTagsResponse xmlns=\"http://elasticloadbalancing.amazonaws.com/doc/2012-06-01/\">
  <DescribeTagsResult>
    <TagDescriptions>
      <member>
        <LoadBalancerName>vvorobyov-classic</LoadBalancerName>
        <Tags>
          <member>
            <Value>tag-value-2</Value>
            <Key>tag-key-2</Key>
          </member>
          <member>
            <Value>tag-value-2</Value>
            <Key>tag-key-1</Key>
          </member>
        </Tags>
      </member>
    </TagDescriptions>
  </DescribeTagsResult>
  <ResponseMetadata>
    <RequestId>75bbeca1-e357-11e8-b2f4-735be4940a86</RequestId>
  </ResponseMetadata>
</DescribeTagsResponse>",
    element(1, xmerl_scan:string(XML)).
