{application, erlcloud,
 [{description, "Erlang cloud computing library"},
  {vsn, "0.2.0"},
  {modules, [erlcloud, erlcloud_aws, erlcloud_ec2, erlcloud_sdb, erlcloud_xml]},
  {registered, []},
  {applications, [stdlib, kernel, sasl, crypto, ssl, inets]},
  {env, []}
 ]
}.
