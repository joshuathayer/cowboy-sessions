-module(sessions_test).

-include_lib("eunit/include/eunit.hrl").

-export([send/2, test_request_with_session/1, test_request_without_session/1]).

test_request_with_session(OnResponse) ->
  cowboy_req:new(undefined,    %% socket
                 ?MODULE,      %% transport, must be a module
                 undefined,    %% peer
                 <<>>,         %% method
                 <<>>,         %% path
                 <<>>,         %% query
                 list_to_atom("HTTP/1.1"),   %% version
                 [{<<"cookie">>, <<"SESSION_ID=decafbad">>}],           %% headers
                 <<>>,         %% host
                 undefined,    %% port
                 <<>>,         %% buffer
                 true,         %% cankeepalive
                 false,        %% compress
                 OnResponse    %% on_response
                ).

test_request_without_session(OnResponse) ->
    cowboy_req:new(undefined,    %% socket
                   ?MODULE,      %% transport, must be a module
                   undefined,    %% peer
                   <<>>,         %% method
                   <<>>,         %% path
                   <<>>,         %% query
                   list_to_atom("HTTP/1.1"),   %% version
                   [],           %% headers
                   <<>>,         %% host
                   undefined,    %% port
                   <<>>,         %% buffer
                   true,         %% cankeepalive
                   false,        %% compress
                   OnResponse    %% on_response
                  ).

test_request_without_session_with_path(OnResponse) ->
    cowboy_req:new(undefined,    %% socket
                   ?MODULE,      %% transport, must be a module
                   undefined,    %% peer
                   <<>>,         %% method
                   <<"/testpath">>,         %% path
                   <<>>,         %% query
                   list_to_atom("HTTP/1.1"),   %% version
                   [],           %% headers
                   <<>>,         %% host
                   undefined,    %% port
                   <<>>,         %% buffer
                   true,         %% cankeepalive
                   false,        %% compress
                   OnResponse    %% on_response
                  ).

%% a request like we'd see on cookiecheck from a browser that supports cookies
test_cookiecheck_request_success(OnResponse) ->
    cowboy_req:new(undefined,    %% socket
                   ?MODULE,      %% transport, must be a module
                   undefined,    %% peer
                   <<>>,         %% method
                   <<"/cookiecheck">>,         %% path
                   <<"path=%2Ftestpath">>,     %% query
                   list_to_atom("HTTP/1.1"),   %% version
                   [{<<"cookie">>, <<"SESSION_ID=decafbad">>}],           %% headers
                   <<>>,         %% host
                   undefined,    %% port
                   <<>>,         %% buffer
                   true,         %% cankeepalive
                   false,        %% compress
                   OnResponse    %% on_response
                  ).

test_cookiecheck_request_failure(OnResponse) ->
    cowboy_req:new(undefined,    %% socket
                   ?MODULE,      %% transport, must be a module
                   undefined,    %% peer
                   <<>>,         %% method
                   <<"/cookiecheck">>,         %% path
                   <<"path=%2Ftestpath">>,     %% query
                   list_to_atom("HTTP/1.1"),   %% version
                   [],           %% headers
                   <<>>,         %% host
                   undefined,    %% port
                   <<>>,         %% buffer
                   true,         %% cankeepalive
                   false,        %% compress
                   OnResponse    %% on_response
                  ).



send(_Socket, _Data) ->
    true.

%% mic check
reverse_test() -> lists:reverse([1,2,3]).
length_test() -> ?assert(length([1,2,3]) =:= 3).

request_exists_test() ->
    TestReq = test_request_without_session(undefined),
    ?assert(erlang:is_record(TestReq, list_to_atom("http_req"))).

request_with_session_test() ->
    TestReq = test_request_with_session(undefined),
    ?assert(erlang:is_record(TestReq, list_to_atom("http_req"))),

    {V, TestReq2}  = cowboy_req:version(TestReq),
    ?assert(V =:= erlang:list_to_atom("HTTP/1.1")),
        
    {SessionID, _} = cowboy_req:cookie(<<"SESSION_ID">>, TestReq2),
    ?assert(SessionID =:= <<"decafbad">>).

get_session_test() ->
    TestReq = test_request_with_session(undefined),
    {SessionID, _} = cowboy_sessions:session_id(TestReq),
    ?assert(SessionID =:= <<"decafbad">>).

meta_test() ->
    %% starting with a session with a key but no session data
    TestReq = test_request_with_session(undefined),
    TestReq2 = cowboy_sessions:update_session(TestReq, test_key, test_value),
    
    {TestValue, TestReq3} = cowboy_sessions:session_data(TestReq2, test_key),
    ?assert(TestValue =:= test_value),
    
    %% assert that we can add new session data now that we have any
    TestReq4 = cowboy_sessions:update_session(TestReq3, test_key2, test_value2),
    {TestValue2, TestReq5} = cowboy_sessions:session_data(TestReq4, test_key2),
    ?assert(TestValue2 =:= test_value2),

    %% assert that we can update existing values
    TestReq6 = cowboy_sessions:update_session(TestReq5, test_key2, test_value3),
    {TestValue3, _TestReq7} = cowboy_sessions:session_data(TestReq6, test_key2),
    ?assert(TestValue3 =:= test_value3).

redirect_test() ->
    cowboy_clock:init([]),
    TestReq = test_request_without_session_with_path(
                fun (_Status, Headers, _Body, Req) ->
                        Loc = proplists:get_value(<<"Location">>, Headers),
                        ?assert(Loc =:= <<"/cookiecheck?path=%2Ftestpath">>),
                        Req
                end),
    cowboy_sessions:redirect_session(TestReq, []),
    true.

cookiecheck_test() ->
    %% cowboy_clock:init([]),
    TestReq = test_cookiecheck_request_success(
                fun (Status, _Headers, _Body, Req) ->
                        ?assert(Status =:= 302),
                        Req
                end),
    _TestReq2 = cowboy_sessions:handle_cookiecheck(TestReq),
    true.
