%% @author Ransom Richardson <ransom@ransomr.net>
%% @doc
%%
%% HTTP client abstraction for erlcloud. Simplifies changing http clients.
%% API matches lhttpc, except Config is passed instead of options for
%% future cusomizability.
%%
%% @end

-module(erlcloud_httpc).

-export([request/6]).

%% @doc According to https://github.com/esl/lhttpc/blob/8c98e36c26ab841b3b6bbbec906e4a77ccb25d4c/src/lhttpc.erl#L436-438
%%      To keep backwards compatibility, we need to trap exits here, otherwise
%%      the process running this command would die in case of error instead of
%%      just returning an error
request(URL, Method, Hdrs, Body, Timeout, _Config) ->
    OldTrapExit = process_flag(trap_exit, true),
    try lhttpc:request(URL, Method, Hdrs, Body, Timeout, [])
    after true = process_flag(trap_exit, OldTrapExit)
    end.
