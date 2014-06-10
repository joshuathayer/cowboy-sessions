-module(cowboy_authentication).

%% Standard callbacks.
-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([as_json/2]).
-export([is_authorized/2]).
-export([modify_session/2]).
-export([get_authenticated_user/1]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    %% io:format(standard_error, "In allowed_methods with ~p.~n", [State]),
    {[<<"PUT">>, <<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    %% io:format(standard_error, "In content_types_provided with ~p.~n", [State]),
    {[
      {{<<"application">>, <<"json">>, []}, as_json}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, modify_session}],
     Req, State}.

as_json(Req, State) ->
    %% io:format(standard_error, "In as_json with ~p.~n", [State]),
    %% we would like to get the current authenticated sessions for this user...

    %% {AuthenticatedUser, Req2} = cowboy_sessions:session_data(Req, auth_state),
    %% ebb_user_service ! {self(), authenticated_sessions, AuthenticatedUser},
    %% receive
    %%     {ok, Sessions} ->
    %%         {jiffy:encode(Sessions), Req2, State}
    %% end.

    {jiffy:encode({[{this_is_a, test}]}), Req, State}.

get_authenticated_user(Req) ->
    {AuthenticatedState, Req1} = cowboy_sessions:session_data(Req, auth_state),
    {AuthenticatedUser, Req2} = cowboy_sessions:session_data(Req1, auth_state),
    case AuthenticatedState of
        authenticated ->
            {AuthenticatedUser, Req2};
        _ ->
            {undefined, Req2}
    end.

%% part of the cowboy REST api
is_authorized(Req, State) ->
    %% get session from req, look up session in storage. we might be already
    %% authenticated on this session, in which case we should just return OK
    {SessionID, Req1} = cowboy_sessions:get_session(Req),

    {AuthenticatedState, Req2} = cowboy_sessions:session_data(Req1, auth_state),
    {AuthenticatedUser, Req3} = cwoboy_sessions:session_data(Req2, auth_state),

    case {AuthenticatedState, AuthenticatedUser} of
        {authenticated, _} ->
            {true, Req3, [{user, AuthenticatedUser}]};
        {dethenticated, _} ->
            {false, Req3, State};
        {_, _} ->
            %% we are not in an already-authenticated session
            AuthHeader = cowboy_req:parse_header(<<"authorization">>, Req3, undefined),
            io:format(standard_error, "Auth header is parsed as ~p.~n", [AuthHeader]),
            
            case {SessionID, AuthHeader} of
                {undefined, _} ->
                    %% having a valid session is a precondition of being authenticated
                    io:format(standard_error, "User has no session on this request, cannot authorize", []),
                    {ok, Res} = cowboy_req:reply(412, Req3),
                    {halt, Res, State};

                {_, {ok, {<<"basic">>, {User, Pass}}, Req4}} ->
                    io:format(standard_error, "I would like to authenticate ~p ~p.~n", [User, Pass]),
                    Where = whereis(ebb_user_service),
                    io:format(standard_error, "Whereis is ~p.~n", [Where]),
                    ebb_user_service ! {self(), authenticate, User, Pass},
                    receive
                        ok -> {true, Req4, [{user, User}]};
                        not_found ->
                            %% user not found, login failed, 403...
                            {ok, Res} = cowboy_req:reply(403, Req4),
                            {halt, Res, State};
                        Unknown ->
                            io:format(standard_error, "Unknown data returned from user process: ~p.~n", [Unknown])
                    end;

                {_, {ok, undefined, Req4}} ->
                    %% session not authenticated, no basic-auth header
                    io:format(standard_error, "No basic-auth header and no session, this is an error.~n", []),
                    {{false, <<"Basic realm=\"ebb\"">>}, Req4, State}
            end
    end.

modify_session(Req, State) ->
    {SessionID, Req1} = cowboy_sessions:get_session(Req),
    {ok, Params, Req2} = cowboy_req:body_qs(Req1),

    case {lists:keyfind(user, 1, State),
          lists:keyfind(<<"target_session">>, 1, Params),
          lists:keyfind(<<"auth_state">>, 1, Params)} of

        {false, _, _ } ->
            {false, Req2, State};
        {_, false, _} ->
            {false, Req2, State};
        {_, _, false} ->
            {false, Req2, State};
        {{_, User}, {_, TargetSessionID}, {_, AuthState}} ->
 
            io:format(standard_error, "In modify_session, Session is ~p while TargetSession is ~p.~n", [SessionID, TargetSessionID]),
            AuthState2 = case AuthState of
                             <<"authenticated">>   -> authenticated;
                             <<"deauthenticated">> -> deauthenticated;
                             _                     -> undefined
                         end,

            %% for now, we only allow modifying the current state. eventually,
            %% we should be able to look up sessions by user, and modify any
            %% user-owned session. Also, any non-valid session state is considered
            %% an error.
            case {SessionID =:= TargetSessionID, AuthState2} of
                {_, undefined} ->
                    {false, Req2, State};
                {true, _} ->
                    Req3 = cowboy_sessions:update_session(Req2, auth_state, AuthState2),
                    Req4 = cowboy_sessions:update_session(Req3, auth_user, User),
                    {true, Req4, State};
                {false, _}  ->
                    {false, Req2, State}
            end
    end.
