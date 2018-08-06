-module(erlcloud_route53_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlcloud_aws.hrl").

route53_test_() ->
    {foreach,
     fun setup/0,
     fun meck:unload/1,
     [fun describe_zone_tests/1,
      fun describe_zones_tests/1,
      fun describe_resource_set_tests/1,
      fun describe_delegation_set_tests/1,
      fun describe_all_tests/1
     ]
    }.

mocks() ->
    [mocked_zone(), mocked_zone1(), mocked_zone2(),
     mocked_zones1(), mocked_zones2(), mocked_zones3(),
     mocked_resource_set1(), mocked_resource_set2(), mocked_resource_set3(),
     mocked_resource_set4(),
     mocked_delegation_set()].

setup() ->
    meck:new(ECA = erlcloud_aws, [non_strict]),
    meck:expect(erlcloud_aws, default_config, 0, #aws_config{}),
    meck:expect(erlcloud_aws, aws_request_xml4, mocks()),
    [ECA].

mocked_zone() ->
    {[get, '_', "/2013-04-01/hostedzone/Z1D633PJN98FT9",
      [{"Action", "GetHostedZone"},
       {"Version", '_'}], '_', '_'],
     make_response("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<GetHostedZoneResponse xmlns=\"https://route53.amazonaws.com/doc/2013-04-01/\">
   <HostedZone>
      <Id>/hostedzone/Z1D633PJN98FT9</Id>
      <Name>example.com.</Name>
      <CallerReference>2014-11-01T11:22:14Z</CallerReference>
      <Config>
         <Comment>This is my first hosted zone.</Comment>
         <PrivateZone>false</PrivateZone>
      </Config>
      <ResourceRecordSetCount>17</ResourceRecordSetCount>
   </HostedZone>
   <DelegationSet>
      <NameServers>
         <NameServer>ns-2048.awsdns-64.com</NameServer>
         <NameServer>ns-2049.awsdns-65.net</NameServer>
         <NameServer>ns-2050.awsdns-66.org</NameServer>
         <NameServer>ns-2051.awsdns-67.co.uk</NameServer>
      </NameServers>
   </DelegationSet>
</GetHostedZoneResponse>")}.

mocked_zone1() ->
    {[get, '_', "/2013-04-01/hostedzone/Z1D633PJN98FT0",
      [{"Action", "GetHostedZone"},
       {"Version", '_'}], '_', '_'],
     make_response("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<GetHostedZoneResponse xmlns=\"https://route53.amazonaws.com/doc/2013-04-01/\">
   <HostedZone>
      <Id>/hostedzone/Z1D633PJN98FT0</Id>
      <Name>example.com.</Name>
      <CallerReference>myUniqueIdentifier</CallerReference>
      <Config>
         <Comment>This is my first hosted zone.</Comment>
         <PrivateZone>false</PrivateZone>
      </Config>
      <ResourceRecordSetCount>17</ResourceRecordSetCount>
   </HostedZone>
   <DelegationSet>
      <Id>NU241VPSAMPLE</Id>
      <CallerReference>2014-10-01T11:22:14Z</CallerReference>
      <NameServers>
         <NameServer>ns-2048.awsdns-64.com</NameServer>
         <NameServer>ns-2049.awsdns-65.net</NameServer>
         <NameServer>ns-2050.awsdns-66.org</NameServer>
         <NameServer>ns-2051.awsdns-67.co.uk</NameServer>
      </NameServers>
   </DelegationSet>
</GetHostedZoneResponse>")}.

mocked_zone2() ->
    {[get, '_', "/2013-04-01/hostedzone/Z1D633PJN98FT1",
      [{"Action", "GetHostedZone"},
       {"Version", '_'}], '_', '_'],
     make_response("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<GetHostedZoneResponse xmlns=\"https://route53.amazonaws.com/doc/2013-04-01/\">
   <HostedZone>
      <Id>/hostedzone/Z1D633PJN98FT1</Id>
      <Name>example.com.</Name>
      <CallerReference>myUniqueIdentifier</CallerReference>
      <Config>
         <Comment>This is my first hosted zone.</Comment>
         <PrivateZone>false</PrivateZone>
      </Config>
      <ResourceRecordSetCount>17</ResourceRecordSetCount>
   </HostedZone>
   <VPCs>
      <VPC>
         <VPCRegion>us-east-1</VPCRegion>
         <VPCId>vpc-1a2b3c4d</VPCId>
      </VPC>
   </VPCs>
</GetHostedZoneResponse>")}.

mocked_delegation_set() ->
    {[get, '_', "/2013-04-01/delegationset",
      [{"Action", "ListReusableDelegationSets"},
       {"Version", '_'}], '_', '_'],
         make_response("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<ListReusableDelegationSetsResponse xmlns=\"https://route53.amazonaws.com/doc/2013-04-01/\">
   <DelegationSets>
      <DelegationSet>
         <Id>/delegationset/N1PA6795SAMPLE</Id>
         <CallerReference>unique value 1</CallerReference>
         <NameServers>
            <NameServer>ns-2042.awsdns-64.com</NameServer>
            <NameServer>ns-2043.awsdns-65.net</NameServer>
            <NameServer>ns-2044.awsdns-66.org</NameServer>
            <NameServer>ns-2045.awsdns-67.co.uk</NameServer>
         </NameServers>
      </DelegationSet>
      <DelegationSet>
         <Id>/delegationset/N1PA6796SAMPLE</Id>
         <CallerReference>unique value 2</CallerReference>
         <NameServers>
            <NameServer>ns-2046.awsdns-68.com</NameServer>
            <NameServer>ns-2047.awsdns-69.net</NameServer>
            <NameServer>ns-2048.awsdns-70.org</NameServer>
            <NameServer>ns-2049.awsdns-71.co.uk</NameServer>
         </NameServers>
      </DelegationSet>
   </DelegationSets>
   <IsTruncated>true</IsTruncated>
   <NextMarker>N1PA6797SAMPLE</NextMarker>
   <MaxItems>2</MaxItems>
</ListReusableDelegationSetsResponse>")}.

mocked_zones1() ->
    {[get, '_', "/2013-04-01/hostedzone", [{"Action", "ListHostedZones"},
                                           {"Version", '_'}], '_', '_'],
     make_response("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<ListHostedZonesResponse xmlns=\"https://route53.amazonaws.com/doc/2013-04-01/\">
   <HostedZones>
      <HostedZone>
         <Id>/hostedzone/Z111111QQQQQQQ</Id>
         <Name>example.com.</Name>
         <CallerReference>MyUniqueIdentifier1</CallerReference>
         <Config>
            <Comment>This is my first hosted zone.</Comment>
            <PrivateZone>false</PrivateZone>
         </Config>
         <ResourceRecordSetCount>42</ResourceRecordSetCount>
      </HostedZone>
   </HostedZones>
   <IsTruncated>true</IsTruncated>
   <NextMarker>Z222222VVVVVVV</NextMarker>
   <MaxItems>1</MaxItems>
</ListHostedZonesResponse>
")}.

mocked_zones2() ->
    {[get, '_', "/2013-04-01/hostedzone", [{"Action", "ListHostedZones"},
                                           {"Version", '_'},
                                           {"maxitems", 100},
                                           {"marker", "Z222222VVVVVVV"}],
      '_', '_'],
     make_response("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<ListHostedZonesResponse xmlns=\"https://route53.amazonaws.com/doc/2013-04-01/\">
   <HostedZones>
      <HostedZone>
         <Id>/hostedzone/Z222222VVVVVVV</Id>
         <Name>example2.com.</Name>
         <CallerReference>MyUniqueIdentifier2</CallerReference>
         <Config>
            <Comment>This is my second hosted zone.</Comment>
            <PrivateZone>false</PrivateZone>
         </Config>
         <ResourceRecordSetCount>17</ResourceRecordSetCount>
      </HostedZone>
      <HostedZone>
         <Id>/hostedzone/Z2682N5HXP0BZ4</Id>
         <Name>example3.com.</Name>
         <CallerReference>MyUniqueIdentifier3</CallerReference>
         <Config>
            <Comment>This is my third hosted zone.</Comment>
            <PrivateZone>false</PrivateZone>
         </Config>
         <ResourceRecordSetCount>117</ResourceRecordSetCount>
      </HostedZone>
   </HostedZones>
   <Marker>Z222222VVVVVVV</Marker>
   <IsTruncated>true</IsTruncated>
   <NextMarker>Z333333YYYYYYY</NextMarker>
   <MaxItems>2</MaxItems>
</ListHostedZonesResponse>")}.

mocked_zones3() ->
    {[get, '_', "/2013-04-01/hostedzone", [{"Action", "ListHostedZones"},
                      {"Version", '_'},
                      {"maxitems", 9},
                      {"DelegationSetId","NZ8X2CISAMPLE"}], '_', '_'],
     make_response("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<ListHostedZonesResponse xmlns=\"https://route53.amazonaws.com/doc/2013-04-01/\">
   <HostedZones>
      <HostedZone>
         <Id>/hostedzone/Z1D633PJN98FT9</Id>
         <Name>example1.com.</Name>
         <CallerReference>2014-10-01T11:22:14Z</CallerReference>
         <Config>
            <Comment>Delegation set id NZ8X2CISAMPLE</Comment>
         </Config>
         <ResourceRecordSetCount>4</ResourceRecordSetCount>
      </HostedZone>
      <HostedZone>
         <Id>/hostedzone/Z1I149ULENZ2PP</Id>
         <Name>example2.com.</Name>
         <CallerReference>2014-11-02T12:33:15Z</CallerReference>
         <Config>
            <Comment>Delegation set id NZ8X2CISAMPLE</Comment>
         </Config>
         <ResourceRecordSetCount>6</ResourceRecordSetCount>
      </HostedZone>
   </HostedZones>
   <IsTruncated>false</IsTruncated>
   <MaxItems>100</MaxItems>
</ListHostedZonesResponse>")}.

mocked_resource_set1() ->
    {[get, '_', "/2013-04-01/hostedzone/TESTID1/rrset",
      [{"Action", "ListResourceRecordSets"},
       {"Version", '_'}], '_', '_'],
     make_response("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<ListResourceRecordSetsResponse xmlns=\"https://route53.amazonaws.com/doc/2013-04-01/\">
   <ResourceRecordSets>
      <ResourceRecordSet>
         <Name>example.com.</Name>
         <Type>SOA</Type>
         <TTL>900</TTL>
         <ResourceRecords>
            <ResourceRecord>
               <Value>ns-2048.awsdns-64.net. hostmaster.awsdns.com. 1 7200 900 1209600 86400</Value>
            </ResourceRecord>
         </ResourceRecords>
      </ResourceRecordSet>
      <ResourceRecordSet>
         <Name>Alias</Name>
         <Type>TXT</Type>
         <AliasTarget>
            <HostedZoneId>HOSTEDZONE</HostedZoneId>
            <DNSName>DNS NAME</DNSName>
            <EvaluateTargetHealth>true</EvaluateTargetHealth>
         </AliasTarget>
         <HealthCheckId>HEALTHID</HealthCheckId>
      </ResourceRecordSet>
        <ResourceRecordSet>
         <Name>GEO</Name>
         <Type>MX</Type>
         <SetIdentifier>SETID</SetIdentifier>
         <GeoLocation>
            <ContinentCode>EU</ContinentCode>
            <CountryCode>GB</CountryCode>
            <SubdivisionCode>Wales</SubdivisionCode>
         </GeoLocation>
         <TTL>234</TTL>
         <ResourceRecords>
            <ResourceRecord>
               <Value>TEST GEO RECORD</Value>
            </ResourceRecord>
         </ResourceRecords>
      </ResourceRecordSet>
   </ResourceRecordSets>
   <IsTruncated>true</IsTruncated>
   <MaxItems>1</MaxItems>
   <NextRecordName>testdoc2.example.com</NextRecordName>
   <NextRecordType>NS</NextRecordType>
</ListResourceRecordSetsResponse>")}.

mocked_resource_set2() ->
    {[get, '_', "/2013-04-01/hostedzone/TESTID/rrset",
      [{"Action", "ListResourceRecordSets"},
       {"Version", '_'},
       {"name", "example.com."},
       {"type", "NS"}], '_', '_'],
     make_response("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<ListResourceRecordSetsResponse xmlns=\"https://route53.amazonaws.com/doc/2013-04-01/\">
   <ResourceRecordSets>
      <ResourceRecordSet>
         <Name>example.com.</Name>
         <Type>NS</Type>
         <TTL>172800</TTL>
         <ResourceRecords>
            <ResourceRecord>
               <Value>ns-2048.awsdns-64.com.</Value>
            </ResourceRecord>
            <ResourceRecord>
               <Value>ns-2049.awsdns-65.net.</Value>
            </ResourceRecord>
            <ResourceRecord>
               <Value>ns-2050.awsdns-66.org.</Value>
            </ResourceRecord>
            <ResourceRecord>
               <Value>ns-2051.awsdns-67.co.uk.</Value>
            </ResourceRecord>
         </ResourceRecords>
      </ResourceRecordSet>
   </ResourceRecordSets>
   <IsTruncated>false</IsTruncated>
   <MaxItems>10</MaxItems>
</ListResourceRecordSetsResponse>")}.

mocked_resource_set3() ->
    {[get, '_', "/2013-04-01/hostedzone/TESTID/rrset",
      [{"Action", "ListResourceRecordSets"},
       {"Version", '_'},
       {"name", "example1.com."},
       {"type", "NS"}], '_', '_'],
     make_response("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<ListResourceRecordSetsResponse xmlns=\"https://route53.amazonaws.com/doc/2013-04-01/\">
   <ResourceRecordSets>
      <ResourceRecordSet>
         <Name>example.com.</Name>
         <Type>NS</Type>
         <TTL>172800</TTL>
         <SetIdentifier>SETID</SetIdentifier>
         <ResourceRecords>
            <ResourceRecord>
               <Value>ns-2048.awsdns-64.com.</Value>
            </ResourceRecord>
            <ResourceRecord>
               <Value>ns-2049.awsdns-65.net.</Value>
            </ResourceRecord>
            <ResourceRecord>
               <Value>ns-2050.awsdns-66.org.</Value>
            </ResourceRecord>
            <ResourceRecord>
               <Value>ns-2051.awsdns-67.co.uk.</Value>
            </ResourceRecord>
         </ResourceRecords>
      </ResourceRecordSet>
   </ResourceRecordSets>
   <IsTruncated>true</IsTruncated>
   <MaxItems>10</MaxItems>
   <NextRecordIdentifier>NEXTSETID</NextRecordIdentifier>
</ListResourceRecordSetsResponse>")}.

mocked_resource_set4() ->
    {[get, '_', "/2013-04-01/hostedzone/TESTID/rrset",
      [{"Action", "ListResourceRecordSets"},
       {"Version", '_'},
       {"name", "example2.com."},
       {"type", "NS"}], '_', '_'],
     make_response("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<ListResourceRecordSetsResponse xmlns=\"https://route53.amazonaws.com/doc/2013-04-01/\">
   <ResourceRecordSets>
      <ResourceRecordSet>
         <Name>example.com.</Name>
         <Type>NS</Type>
         <TTL>172800</TTL>
         <SetIdentifier>SETID</SetIdentifier>
         <ResourceRecords>
            <ResourceRecord>
               <Value>ns-2048.awsdns-64.com.</Value>
            </ResourceRecord>
            <ResourceRecord>
               <Value>ns-2049.awsdns-65.net.</Value>
            </ResourceRecord>
            <ResourceRecord>
               <Value>ns-2050.awsdns-66.org.</Value>
            </ResourceRecord>
            <ResourceRecord>
               <Value>ns-2051.awsdns-67.co.uk.</Value>
            </ResourceRecord>
         </ResourceRecords>
      </ResourceRecordSet>
   </ResourceRecordSets>
   <IsTruncated>true</IsTruncated>
   <MaxItems>10</MaxItems>
   <NextRecordName>testdoc3.example.com</NextRecordName>
   <NextRecordType>A</NextRecordType>
   <NextRecordIdentifier>NEXTSETID</NextRecordIdentifier>
</ListResourceRecordSetsResponse>")}.

make_response(Xml) ->
    {ok, element(1, xmerl_scan:string(Xml))}.

describe_all_tests(_) ->
    [
     fun() ->
             Fun = fun(Option, _Config) ->
                           {value, {"End", End}} =  lists:keysearch("End", 1,
                                                                    Option),
                           case lists:keysearch("marker", 1, Option) of
                               {value, {"marker", End}} ->
                                   {ok, [End]};
                               {value, {"marker", Marker}} ->
                                   {ok, [Marker], Marker + 1};
                               false ->
                                   {ok, [1], 2}
                           end
                   end,
             Fun2 = fun(1, Option, _Config) ->
                            Fun(Option, _Config)
                    end,
             ?assertMatch({ok, [1, 2, 3, 4, 5, 6]},
                          erlcloud_route53:describe_all(
                            Fun, [{"End", 6}], #aws_config{}, [])),
             ?assertMatch({ok, [1, 2, 3, 4, 5, 6]},
                          erlcloud_route53:describe_all(
                            Fun2, [1], [{"End", 6}], #aws_config{}, []))
     end,
     fun() ->
             Fun = fun(Option, _Config) ->
                           {value, {"End", End}} =
                               lists:keysearch("End", 1, Option),
                           case lists:keysearch("name", 1, Option) of
                               {value, {"name", End}} ->
                                   {value, {"type", "string"}} =
                                       lists:keysearch("type", 1, Option),
                                   {ok, [End]};
                               {value, {"name", Marker}} ->
                                   {value, {"type", "string"}} =
                                       lists:keysearch("type", 1, Option),
                                   {ok, [Marker], {Marker + 1, "string"}};
                               false ->
                                   {ok, [1], {2, "string"}}
                           end
                   end,
             ?assertMatch({ok, [1, 2, 3, 4, 5, 6]},
                          erlcloud_route53:describe_all(
                            Fun, [{"End", 6}], #aws_config{}, []))
     end
    ].

describe_delegation_set_tests(_) ->
    [
     fun() ->
             Result = erlcloud_route53:describe_delegation_sets(),
             Expected = {ok,
                         [
                          [{id,"/delegationset/N1PA6795SAMPLE"},
                           {caller_reference,"unique value 1"},
                           {name_servers,["ns-2042.awsdns-64.com",
                                          "ns-2043.awsdns-65.net",
                                          "ns-2044.awsdns-66.org",
                                          "ns-2045.awsdns-67.co.uk"]}],
                          [{id,"/delegationset/N1PA6796SAMPLE"},
                           {caller_reference,"unique value 2"},
                           {name_servers,["ns-2046.awsdns-68.com",
                                          "ns-2047.awsdns-69.net",
                                          "ns-2048.awsdns-70.org",
                                          "ns-2049.awsdns-71.co.uk"]}]],
                        "N1PA6797SAMPLE"},
             ?assertEqual(Expected, Result)
     end
    ].

describe_resource_set_tests(_) ->
    [
     fun() ->
             Result = erlcloud_route53:describe_resource_sets("TESTID1"),
             Expected =     {ok,
                             [[{name,"example.com."},
                               {type,"SOA"},
                               {ttl,900},
                               {resource_records,
                                ["ns-2048.awsdns-64.net. hostmaster.awsdns.com."
                                 " 1 7200 900 1209600 86400"]}],
                              [{name,"Alias"},
                               {type,"TXT"},
                               {health_check_id,"HEALTHID"},
                               {alias_target,[{hosted_zone_id,"HOSTEDZONE"},
                                              {dns_name,"DNS NAME"},
                                              {evaluate_target_health,true}]}],
                              [{name,"GEO"},
                               {type,"MX"},
                               {set_identifier,"SETID"},
                               {ttl,234},
                               {geo_location,[{continent_code,"EU"},
                                              {country_code,"GB"},
                                              {subdivision_code,"Wales"}]},
                               {resource_records,["TEST GEO RECORD"]}]],
                             {"testdoc2.example.com","NS"}},
             ?assertEqual(Expected, Result)
     end,
     fun() ->
             Result = erlcloud_route53:describe_resource_sets("TESTID",
                        [{"name", "example.com."},
                         {"type", "NS"}], #aws_config{}),
             Expected = {ok,
                         [[{name,"example.com."},
                           {type,"NS"},
                           {ttl,172800},
                           {resource_records,["ns-2048.awsdns-64.com.",
                                              "ns-2049.awsdns-65.net.",
                                              "ns-2050.awsdns-66.org.",
                                              "ns-2051.awsdns-67.co.uk."]}]]},
             ?assertEqual(Expected, Result)
     end,
     fun() ->
             Result = erlcloud_route53:describe_resource_sets(
                        "TESTID", [{"name", "example1.com."}, {"type", "NS"}],
                        #aws_config{}),
             Expected = {ok,
                         [[{name,"example.com."},
                           {type,"NS"},
                           {set_identifier,"SETID"},
                           {ttl,172800},
                           {resource_records,["ns-2048.awsdns-64.com.",
                                              "ns-2049.awsdns-65.net.",
                                              "ns-2050.awsdns-66.org.",
                                              "ns-2051.awsdns-67.co.uk."]}]],
                         "NEXTSETID"},
              ?assertEqual(Expected, Result)
     end,
     fun() ->
             Result = erlcloud_route53:describe_resource_sets(
                        "TESTID", [{"name", "example2.com."}, {"type", "NS"}],
                        #aws_config{}),
             Expected = {ok,
                         [[{name,"example.com."},
                           {type,"NS"},
                           {set_identifier,"SETID"},
                           {ttl,172800},
                           {resource_records,["ns-2048.awsdns-64.com.",
                                              "ns-2049.awsdns-65.net.",
                                              "ns-2050.awsdns-66.org.",
                                              "ns-2051.awsdns-67.co.uk."]}]],
                         {"testdoc3.example.com", "A", "NEXTSETID"}},
              ?assertEqual(Expected, Result)
     end

    ].

describe_zone_tests(_) ->
    [
     fun() ->
             Result = erlcloud_route53:describe_zone("Z1D633PJN98FT9"),
             Expected = {ok,
                         [{zone_id,"/hostedzone/Z1D633PJN98FT9"},
                          {name,"example.com."},
                          {private,false},
                          {resourceRecordSetCount,17},
                          {delegation_set,
                           [{name_servers,["ns-2048.awsdns-64.com",
                                           "ns-2049.awsdns-65.net",
                                           "ns-2050.awsdns-66.org",
                                           "ns-2051.awsdns-67.co.uk"]}]}]},
             ?assertEqual(Expected, Result)
     end,
     fun() ->
             Result = erlcloud_route53:describe_zone("Z1D633PJN98FT0"),
             Expected = {ok,
                         [{zone_id,"/hostedzone/Z1D633PJN98FT0"},
                          {name,"example.com."},
                          {private,false},
                          {resourceRecordSetCount,17},
                          {delegation_set,
                           [{id,"NU241VPSAMPLE"},
                            {caller_reference,"2014-10-01T11:22:14Z"},
                            {name_servers,["ns-2048.awsdns-64.com",
                                           "ns-2049.awsdns-65.net",
                                           "ns-2050.awsdns-66.org",
                                           "ns-2051.awsdns-67.co.uk"]}
                           ]}]},
             ?assertEqual(Expected, Result)
     end,
     fun() ->
             Result = erlcloud_route53:describe_zone("Z1D633PJN98FT1"),
             Expected = {ok,
                         [{zone_id,"/hostedzone/Z1D633PJN98FT1"},
                          {name,"example.com."},
                          {private,false},
                          {resourceRecordSetCount,17},
                          {vpcs,[
                                 [{vpc_id,"vpc-1a2b3c4d"},
                                  {vpc_region,"us-east-1"}]
                                ]}
                         ]},
             ?assertEqual(Expected, Result)
     end
    ].

describe_zones_tests(_) ->
    [
     fun() ->
             Result = erlcloud_route53:describe_zones(),
             Expected = {ok,
                         [[{zone_id,"/hostedzone/Z111111QQQQQQQ"},
                           {name,"example.com."},
                           {private,false},
                           {resourceRecordSetCount,42}]],
                        "Z222222VVVVVVV"},
             ?assertEqual(Expected, Result)
     end,
     fun() ->
             Result = erlcloud_route53:describe_zones(
                        [{"maxitems", 100},
                         {"marker", "Z222222VVVVVVV"}],
                        #aws_config{}),
             Expected = {ok,
                         [[{zone_id,"/hostedzone/Z222222VVVVVVV"},
                           {name,"example2.com."},
                           {private,false},
                           {resourceRecordSetCount,17}],
                          [{zone_id,"/hostedzone/Z2682N5HXP0BZ4"},
                           {name,"example3.com."},
                           {private,false},
                           {resourceRecordSetCount,117}]
                         ], "Z333333YYYYYYY"},
             ?assertEqual(Expected, Result)
     end,
     fun() ->
             Result = erlcloud_route53:describe_zones(
                        [{"maxitems", 9},
                         {"DelegationSetId", "NZ8X2CISAMPLE"}],
                        #aws_config{}),
             Expected = {ok,
                         [[{zone_id,"/hostedzone/Z1D633PJN98FT9"},
                           {name,"example1.com."},
                           {resourceRecordSetCount,4}],
                          [{zone_id,"/hostedzone/Z1I149ULENZ2PP"},
                           {name,"example2.com."},
                           {resourceRecordSetCount,6}]]},
             ?assertEqual(Expected, Result)
     end
    ].
