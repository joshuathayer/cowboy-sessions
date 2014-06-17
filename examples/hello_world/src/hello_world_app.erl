%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(hello_world_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1, ets_session_process/0]).

%% API.

%% an example session storage process.
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

start(_Type, _Args) ->
    register(ebb_session_service, spawn(?MODULE, ets_session_process, [])),

    {ok, Module}  = start_ets(),

    Dispatch = cowboy_router:compile(
                 [
                  {'_', [
                         {"/", toppage_handler, []}
                        ]}
                 ]),

    ProtoOpts = [{env, [{dispatch, Dispatch}]},
                 {onrequest,  fun cowboy_sessions:on_request/1},
                 {onresponse, fun cowboy_sessions:on_response/4}],
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
                                ProtoOpts),
    hello_world_sup:start_link().

stop(_State) ->
    ok.
