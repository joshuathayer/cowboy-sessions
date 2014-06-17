-module(sessions_test).

-include_lib("eunit/include/eunit.hrl").

-export([send/2, test_request_with_session/1, test_request_without_session/1]).

test_request_path_session(Path, Session, OnResponse) ->
    Headers = case Session of
                  [] -> [];
                  _ -> Session2 = list_to_binary(io_lib:format("SESSION_ID=~s", [Session])),
                       [{<<"cookie">>, Session2}]
              end,

    cowboy_req:new(undefined,    %% socket
                   ?MODULE,      %% transport, must be a module
                   undefined,    %% peer
                   <<>>,         %% method
                   Path,         %% path
                   <<>>,         %% query
                   list_to_atom("HTTP/1.1"),   %% version
                   Headers,      %% headers
                   <<>>,         %% host
                   undefined,    %% port
                   <<>>,         %% buffer
                   true,         %% cankeepalive
                   false,        %% compress
                   OnResponse    %% on_response
                  ).

test_request_with_session(OnResponse) ->
    test_request_path_session(<<>>, "decafbad", OnResponse).

test_request_without_session(OnResponse) ->
    test_request_path_session(<<>>, "", OnResponse).

test_request_without_session_with_path(OnResponse) ->
    test_request_path_session(<<"/testpath">>, "", OnResponse).

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

redirect_2_test() ->
    TestReq = test_request_path_session(
                <<"/">>,
                "",
                fun (Status, Headers, _Body, Req) ->
                        ?assert(Status =:= 302),
                        Loc = proplists:get_value(<<"Location">>, Headers),
                        ?assert(Loc =:= <<"/cookiecheck?path=%2F">>),
                        Req
                end),
    cowboy_sessions:redirect_session(TestReq, []),
    true.

redirect_3_test() ->
    %% test case where don't have a path to redirect to...
    TestReq = test_request_path_session(
                <<>>,
                "",
                fun (Status, Headers, _Body, Req) ->
                        ?assert(Status =:= 302),
                        Loc = proplists:get_value(<<"Location">>, Headers),
                        ?assert(Loc =:= <<"/cookiecheck?path=%2F">>),
                        Req
                end),
    cowboy_sessions:redirect_session(TestReq, []),
    true.


cookiecheck_test() ->
    TestReq = test_cookiecheck_request_success(
                fun (Status, _Headers, _Body, Req) ->
                        ?assert(Status =:= 302),
                        Req
                end),
    cowboy_sessions:handle_cookiecheck(TestReq),

    TestReq2 = test_cookiecheck_request_failure(
                fun (Status, _Headers, _Body, Req) ->
                        ?assert(Status =:= 403),
                        Req
                end),
    cowboy_sessions:handle_cookiecheck(TestReq2),
    true.

undefined_session_storage_test() ->
    %% altering the universe... this should not be here.
    cowboy_sessions:start_ets(),
    register(ebb_session_service, spawn(cowboy_sessions, ets_session_process, [])),
    
    %% this request has no session ID, so there's nothing to recover from the DB.
    %% so, put_session_on_request won't do anything since there's no session to put
    %% on the request record.
    TestReq = test_request_without_session(undefined),
    {SessionID, TestReq2}  = cowboy_sessions:put_session_on_request(TestReq),
    {SessionID2, _TestReq3} = cowboy_sessions:session_id(TestReq2),
    ?assert(SessionID2 =:= SessionID),
    ?assert(SessionID2 =:= undefined).

defined_session_storage_test() ->
    %% cowboy_sessions:start_ets(),
    %% register(ebb_session_service, spawn(cowboy_sessions, ets_session_process, [])),

    %% first we assert the session does not exist in storage
    Session = cowboy_sessions:get_from_storage(<<"deadbeef">>),
    ?assert(Session =:= undefined),

    %% now we put_session_on_request, which should stick an empty session proplist
    %% on the request record, and presist it to storage
    TestReq = test_request_path_session(<<"/testpath">>, "deadbeef", undefined),
    {SessionID, TestReq2}   = cowboy_sessions:put_session_on_request(TestReq),
    {SessionID2, _TestReq3} = cowboy_sessions:session_id(TestReq2),
    ?assert(SessionID2 =:= SessionID),
    ?assert(SessionID2 =:= <<"deadbeef">>),
    
    %% we should be able to grab the session from storage now- it'll be empty,
    %% but it won't be undefined.
    Session2 = cowboy_sessions:get_from_storage(<<"deadbeef">>),
    ?assert(Session2 =:= []).
       
session_on_request_test() ->
    %% this request simulates the browser's request afer getting the cookiecheck 302
    Req2 = test_cookiecheck_request_success(
             fun (Status, Headers, _Body, Resp) ->
                     Loc     = proplists:get_value(<<"Location">>, Headers),
                     ?assert(Status =:= 302),
                     ?assert(Loc =:= <<"/testpath">>),
                     %% test that session is in db
                     Resp
             end
            ),
    cowboy_sessions:on_request(Req2),
    true.

on_response_test() ->
    TestReq = test_request_path_session(<<"/testpath">>, "d34db33f", undefined),
    TestReq2 = cowboy_sessions:update_session(TestReq, test_key, test_value),
    _TestReq3 = cowboy_sessions:on_response(undefined, undefined, undefined, TestReq2),
    Session = cowboy_sessions:get_from_storage(<<"d34db33f">>),
    ?assert(Session =:= [{test_key, test_value}]).
