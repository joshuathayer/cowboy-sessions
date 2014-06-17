%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(toppage_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
    {ok, Req, undefined}.

inner_handle(Req, State) ->

    {Count, Req2} = cowboy_sessions:session_data(Req, count),
    
    Count2 = case Count of
                 undefined -> 1;
                 _ -> Count + 1
             end,
    
    Req3 = cowboy_sessions:update_session(Req2, count, Count2),
    
    Hello = io_lib:format("Hello, your visit count is: ~p.", [Count2]),
    
    {ok, Req4} = cowboy_req:reply(200, [
                                        {<<"content-type">>, <<"text/plain">>}
                                       ], Hello, Req3),
    {ok, Req4, State}.

handle(Req, State) ->
    {SessionID, Req2} = cowboy_sessions:session_id(Req),

    case SessionID of
        undefined ->
            cowboy_sessions:redirect_session(Req2, State);
        _ ->
            inner_handle(Req2, State)
    end.
            
terminate(_Reason, _Req, _State) ->
    ok.
