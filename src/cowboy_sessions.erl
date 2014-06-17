-module(cowboy_sessions).

-export([session_id/1, set_session_id/1, on_request/1, on_response/4, update_session/3,
         session_data/2, init/3, terminate/3, redirect_session/2,
         start_ets/0, ets_session_process/0]).

-include_lib("eunit/include/eunit.hrl").

-ifdef(EUNIT).
-export([put_session_on_request/1, get_from_storage/1,
         handle_cookiecheck/1]).
-endif.

%% TODO: make session IDs smarter (with checksum, eg)
-type sessionid() :: { binary() }.

%% -- used internally to communicate with session storage process
-spec get_from_storage(sessionid() | undefined) -> undefined | list().
get_from_storage(undefined) ->
    undefined;
get_from_storage(SessionID) ->
    Ref = make_ref(),
    ebb_session_service ! {{self(), Ref}, get, SessionID},
    receive
        {{_Pid, Ref}, ok, Session} ->
            Session;
        {{_Pid, Ref}, _} ->
            undefined
    end.

-spec set_in_storage(sessionid() | undefined, list()) -> undefined | list().
set_in_storage(undefined, _Val) ->
    undefined;
set_in_storage(SessionID, Val) ->
    Ref = make_ref(),
    ebb_session_service ! {{self(), Ref}, set, SessionID, Val},
    receive
        {{_Pid, Ref}, ok} ->
            Val;
        {{_Pid, Ref}, _} ->
            undefined
    end.

%% -- external API
-spec session_id(cowboy_req:req()) -> {sessionid() | undefined, cowboy_req:req()}.
session_id(Req) ->
    {SessionID, Req2} = cowboy_req:cookie(<<"SESSION_ID">>, Req),
    {SessionID, Req2}.

-spec set_session_id(cowboy_req:req()) -> {binary(), cowboy_req:req()}.
set_session_id(Req) ->
    SessionID = generate_session_id(),
    Req2 = cowboy_req:set_resp_cookie(<<"SESSION_ID">>, SessionID,
                                      [{path, <<"/">>}], Req),
    {SessionID, Req2}.

-spec update_session(cowboy_req:req(), atom(), any()) -> cowboy_req:req().
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

-spec session_data(cowboy_req:req(), atom()) -> {any(), cowboy_req:req()}.
session_data(Req, Key) ->
    {Session, Req2} = cowboy_req:meta(session, Req, []),
    Value = proplists:get_value(Key, Session, undefined),
    {Value, Req2}.

-spec redirect_session(cowboy_req:req(), State) -> {ok, cowboy_req:req(), State}.
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

-spec(on_request(cowboy_req:req()) -> cowboy_req:req()).
on_request(Req) ->
    %% if the browser has given us a session id, then put a session object on the
    %% request (and ensure it's in the database).
    {_SessionID, Req2} = put_session_on_request(Req),

    %% if this request is to /cookiecheck, we handle it ourselves. Since that includes
    %% sending a response to the browser, cowboy will not route the request once
    %% we return here (https://github.com/extend/cowboy/blob/master/guide/hooks.md)
    {Path, Req3} = cowboy_req:path(Req2),
    Req4 = case Path of
               <<"/cookiecheck">> ->
                   handle_cookiecheck(Req3);
               _ ->
                   Req3
           end,
    Req4.

-spec(put_session_on_request(cowboy_req:req()) -> {undefined | sessionid(), cowboy_req:req()}).
put_session_on_request(Req) ->
    {SessionID, Req2} = session_id(Req),
    Session = get_from_storage(SessionID),
    
    Req3 = case {SessionID, Session} of
               {undefined, _Session} ->
                   Req2;
               {SessionID2, undefined} ->
                   set_in_storage(SessionID2, []),
                   cowboy_req:set_meta(session, [], Req2);

               {_SessionID2, Session2} ->
                   cowboy_req:set_meta(session, Session2, Req2)
           end,

    {SessionID, Req3}.
      
-spec(handle_cookiecheck(cowboy_req:req()) -> cowboy_req:req()).
handle_cookiecheck(Req) ->
    {SessionID, Req2} = session_id(Req),
    {ok, Req3} = case SessionID of
                     undefined ->
                         cookies_unsupported(Req2);
                     _ ->
                         cookies_supported(Req2)
                 end,
    Req3.

-spec(on_response(any(), any(), any(), cowboy_req:req()) -> cowboy_req:req()).
on_response(_Status, _Headers, _Body, Req) ->
    {SessionID, Req2} = session_id(Req),
    {Changed, Req3}   = cowboy_req:meta(session_changed, Req2, undefined),
    {Session, Req4}   = cowboy_req:meta(session,         Req3, undefined),

    case {SessionID, Changed} of
        {undefined, _} ->
            Req4;
        {_, undefined} ->
            Req4;
        {SessionID2, _} ->
            set_in_storage(SessionID2, Session),
            Req4
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
-spec(generate_session_id() -> binary()).
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

%% an example session storage process. consider not using this in production.
start_ets() ->
    ?MODULE = ets:new(?MODULE, [public, set, named_table]),
    {ok, ?MODULE}.

ets_session_process() ->
    receive
        {{From, Ref}, set, SessionID, Value} ->
            Sid2 = list_to_atom(binary_to_list(SessionID)),
            ets:insert(?MODULE, {Sid2, Value}),
            From ! {{self(), Ref}, ok},
            ets_session_process();

        {{From, Ref}, get, SessionID} ->
            Sid2 = list_to_atom(binary_to_list(SessionID)),
            Val = case ets:lookup(?MODULE, Sid2) of
                      [] ->
                          undefined;
                      Session ->
                          proplists:get_value(Sid2, Session)
                  end,
            From ! {{self(), Ref}, ok, Val},
            ets_session_process()
    end.
