-module(cowboy_sessions).

-export([session_id/1, set_session_id/1, on_request/1, on_response/4, update_session/3, session_data/2,
         init/3, terminate/3, redirect_session/2, handle_cookiecheck/1]).

-include_lib("eunit/include/eunit.hrl").


%% -- used internally to communicate with session storage process
get_from_storage(SessionID) ->
    ebb_session_service ! {self(), get, SessionID},
    receive
        {ok, Session} ->
            Session;
        _ ->
            undefined
    end.

set_in_storage(SessionID, Val) ->
    ebb_session_service ! {self(), set, SessionID, Val},
    receive
        ok ->
            Val;
        _ ->
            undefined
    end.

%% -- external API
session_id(Req) ->
    {SessionID, Req2} = cowboy_req:cookie(<<"SESSION_ID">>, Req),
    {SessionID, Req2}.

set_session_id(Req) ->
    ?debugHere,
    SessionID = generate_session_id(),
    ?debugHere,
    Req2 = cowboy_req:set_resp_cookie(<<"SESSION_ID">>, SessionID,
                                      [{path, <<"/">>}], Req),
    ?debugHere,
    {SessionID, Req2}.

update_session(Req, Key, Value) ->
    {Session, Req2}   = cowboy_req:meta(session, Req, []),
    Session2 = case lists:keyfind(Key, 1, Session) of
                   false ->
                       lists:append(Session, [{Key, Value}]);
                   _ ->
                       lists:keyreplace(Key, 1, Session, {Key, Value})
               end,
    Req3 = cowboy_req:set_meta(session, Session2, Req2),
    Req4 = cowboy_req:set_meta(session_changed, true, Req3),
    Req4.

session_data(Req, Key) ->
    {Session, Req2} = cowboy_req:meta(session, Req, []),
    Value = proplists:get_value(Key, Session, undefined),
    {Value, Req2}.

redirect_session(Req, State) ->
    {_SessionID, Req2} = set_session_id(Req),
    {Path, Req3}       = cowboy_req:path(Req2),
    Path2              = http_uri:encode(binary_to_list(Path)),
    Path3              = io_lib:format("/cookiecheck?path=~s", [Path2]),
    Path4              = list_to_binary(Path3),
    Req4               = cowboy_req:set_resp_header(<<"Location">>, Path4, Req3),
    {ok, Req5}         = cowboy_req:reply(302, [], "", Req4),
    
    {ok, Req5, State}.

%% -- interface with cowboy
init(_Transport, Req, _Opts) ->
    {ok, Req, {}}.

terminate(_Reason, _Req, _State) ->
    ok.

on_request(Req) ->
    %% if the browser has given us a session id, then put a session object on the
    %% request (and ensure it's in the database).
    {_SessionID, Req2} = put_session_on_request(Req),

    %% if this request is to /cookiecheck, we handle it ourselves. Since that includes
    %% sending a response to the browser, cowboy will not route the request once
    %% we return here (https://github.com/extend/cowboy/blob/master/guide/hooks.md)
    {Path, Req3} = cowboy_req:path(Req2),
    Req4 = case Path of
               <<"cookiecheck">> ->
                   handle_cookiecheck(Req3);
               _ ->
                   Req3
           end,
    Req4.

put_session_on_request(Req) ->
    {SessionID, Req2} = session_id(Req),

    Req3 = case SessionID of
               undefined ->
                   %% no session id, thus no session. we return the unchanged request
                   Req2;
               _ ->
                   Session = get_from_storage(SessionID),
                   Session2 = case Session of
                                  undefined ->
                                      %% no session in storage for this ID, let's store an empty one
                                      set_in_storage(SessionID, []);
                                  _ ->
                                      Session
                              end,
                   
                   cowboy_req:set_meta(session, Session2, Req2)
           end,

    {SessionID, Req3}.
    
    
handle_cookiecheck(Req) ->
    {SessionID, Req2} = session_id(Req),
    {ok, Req3} = case SessionID of
                     undefined ->
                         cookies_unsupported(Req2);
                     _ ->
                         cookies_supported(Req2)
                 end,
    Req3.

on_response(_Status, _Headers, _Body, Req) ->
    {SessionID, Req2} = session_id(Req),
    {Changed, Req3}   = cowboy_req:meta(session_changed, Req2, undefined),
    {Session, Req4}   = cowboy_req:meta(session,         Req3, undefined),
    
    case Changed of
        undefined ->
            Session;
        _ ->
            set_in_storage(SessionID, Session)
    end,
    
    Req4.

%% responses
%% we've tested the browser via redirect loop, and cookies are supported.
%% redirect back to original path.
cookies_supported(Req) ->
    {Path, Req2} = cowboy_req:qs_val(<<"path">>, Req),
    Req3         = cowboy_req:set_resp_header(<<"Location">>, Path, Req2),
    {ok, Req4}   = cowboy_req:reply(302, [], <<>>, Req3),
    {ok, Req4}.

%% browser does not support cookies. throw them a 403 forbidden.
cookies_unsupported(Req) ->
    {ok, Req2}   = cowboy_req:reply(403, [], <<>>, Req),
    {ok, Req2}.

%% session key generation
generate_session_id() ->
    %% checksum on this would be nice
    Now = {_, _, Micro} = now(),
    Nowish = calendar:now_to_universal_time(Now),
    Nowsecs = calendar:datetime_to_gregorian_seconds(Nowish),
    Then = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Prefix = io_lib:format("~14.16.0b", [(Nowsecs - Then) * 1000000 + Micro]),
    list_to_binary(Prefix ++ to_hex(crypto:rand_bytes(9))).

to_hex([]) ->
    [];
to_hex(Bin) when is_binary(Bin) ->
    to_hex(binary_to_list(Bin));
to_hex([H|T]) ->
    [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

to_digit(N) when N < 10 -> $0 + N;
to_digit(N) -> $a + N-10.
