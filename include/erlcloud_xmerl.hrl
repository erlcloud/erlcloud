-include_lib("xmerl/include/xmerl.hrl").

-type(xmerl_xpath_doc_nodes() :: #xmlElement{} | #xmlAttribute{} | #xmlText{} | #xmlPI{} | #xmlComment{} | #xmlNsNode{}).
-type(xmerl_xpath_node_entity() :: #xmlDocument{} | xmerl_xpath_doc_nodes()).
-type(xmerl_xpath_doc_entity() :: #xmlDocument{} | [xmerl_xpath_doc_nodes()]).
