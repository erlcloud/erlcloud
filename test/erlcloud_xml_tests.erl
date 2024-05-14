-module(erlcloud_xml_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud.hrl").

start() ->
    ok.

stop(_) ->
    ok.

xml_test_() ->
    {foreach, 
    fun start/0, 
    fun stop/1, 
    [
        fun parse_xml_test/0
    ]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 

parse_xml_test() ->
    Xml = 
    "<DescribeInstancesResponse xmlns=\"http://ec2.amazonaws.com/doc/2016-11-15/\">
        <requestId>8f7724cf-496f-496e-8fe3-example</requestId>
        <reservationSet>
            <item>
                <reservationId>r-1234567890abcdef0</reservationId>
                <ownerId>123456789012</ownerId>
                <groupSet/>
                <instancesSet>
                    <item>
                        <instanceId>i-1234567890abcdef0</instanceId>
                        <imageId>ami-bff32ccc</imageId>
                    </item>
                </instancesSet>
            </item>
            <item>
                <reservationId>r-abc</reservationId>
                <ownerId>123456789012</ownerId>
                <groupSet/>
                <instancesSet>
                    <item>
                        <instanceId>i-abc</instanceId>
                        <imageId>ami-bff32ccc</imageId>
                    </item>
                </instancesSet>
            </item>
        </reservationSet>
    </DescribeInstancesResponse>", 
    XmerlRaw = xmerl_scan:string(Xml), % Parse into xmerl structure
    Xmerl = element(1,XmerlRaw),
    Result = erlcloud_xml:xml_to_map(Xmerl),
    Expected = {ok,#{'DescribeInstancesResponse' =>
                  #{requestId => <<"8f7724cf-496f-496e-8fe3-example">>,
                    reservationSet =>
                        [#{ownerId => <<"123456789012">>,groupSet => [],
                           reservationId => <<"r-1234567890abcdef0">>,
                           instancesSet =>
                               [#{instanceId => <<"i-1234567890abcdef0">>,
                                  imageId => <<"ami-bff32ccc">>}]},
                         #{ownerId => <<"123456789012">>,groupSet => [],
                           reservationId => <<"r-abc">>,
                           instancesSet =>
                               [#{instanceId => <<"i-abc">>,
                                  imageId => <<"ami-bff32ccc">>}]}]}}},
    ?assertEqual(Expected, Result).
