cowboy-sessions
===============

A library to provide sessions to a [cowboy](https://github.com/extend/cowboy) application.

Example
-------

Adding a visit counter to cowboy's [hello_world example handler](https://github.com/extend/cowboy/blob/master/examples/hello_world/src/toppage_handler.erl):

```erlang
handle(Req, State) ->
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
```

API
---

### session_id
    -spec session_id(cowboy_req:req() -> {binary() | undefined, cowboy_req:req()})

Given a cowboy request record, return the session ID (a binary string) on the request if present, or the atom `undefined` if the request doesn't have a session cookie.

### update_session
    -spec update_session(cowboy_req:req(), atom(), any()) -> cowboy_req:req()

Given a cowboy request, a key (as an atom) and a value, replace the key with the provided value on the session on the request. Returns the updated request record.

### session_data
    -spec session_data(cowboy_req:req(), atom()) -> {any(), cowboy_req:req()}.

Given a cowboy request and a key, return the data associated with the key in the session on the request (or the atom `undefined` if the key isn't found), and the updated request record.

### redirect_session
    -spec redirect_session(cowboy_req:req(), State) -> {ok, cowboy_req:req(), State}.

Create a response for redirecting the user through a cookie-detection loop. Used to ensure the browser supports cookies, as described elsewhere.

Session Storage
---------------

Cowboy-sessions is storage agnostic- it depends on a registered process to handle storing and retrieving session data. The library ships with an example backed by ets, which should only be used for illustration.

The storage process should be registered as `ebb_session_service`, and should implement the following API.

### Session Storage API

Two message types will be sent to the storage process: a "get" and a "set" message.

#### set

The `set` message is a 4-tuple- the first element is a tuple of the sending process ID and a reference, which will be sent back to the caller. The second element is the atom `set`. The third elelement is a binary string which should be used as the key for storing the fourth element, which will be a property list.

The storage implementation should store the Value, then send to the caller a 2-tuple, as shown.

```
{{From, Ref}, set, SessionID, Value} -> {{self(), Ref}, ok}
```

#### get

The `get` message is a 3-tuple. The first element is a tuple of the sending process ID and a reference, which will be sent back to the caller. The second element is the atom `get`. The third element is the SessionID, a binary string which should be used to fetch the stored session data.

The implementation should send a 3-tuple back to the caller, as shown. If session data cannot be found, `Val` may be the atom `undefined`.

```
{{From, Ref}, get, SessionID} -> {{self(), Ref}, ok, Val}
```


Cowboy Configuration
--------------------

This library makes use of [cowboy's request hooks](https://github.com/extend/cowboy/blob/master/guide/hooks.md) to examine and set cookie headers. Your application needs to be configured to integrate the library: its `ProtoOpts` needs to hook in a couple of functions, as shown:

````erlang
ProtoOpts = [...,
             {onrequest,  fun cowboy_sessions:on_request/1},
             {onresponse, fun cowboy_sessions:on_response/4}],
````

Redirects and Cookie Detection
------------------------------

Cowboy_sessions operates on browser cookies. If the user has disabled cookies in her browser, sessions will not work. The library implements a scheme for detecting cookie support in the browser when a cookie is not yet present.

The `redirect_session` function sets a cookie on the provided request and redirects through a special endpoint which tries to read the newly-set cookie. If the cookie is present, it redirects back to the page on the initial request, now with a valid session.

If the session cookie is not present upon redirect, the user is given a [403](http://en.wikipedia.org/wiki/HTTP_403) response.

So, the `redirect_session` function should be used only to protect resources which absolutely need a session present: probably in conjunction with other libraries which support authentication, for example.

Here's the hello world example again, protected by `redirect_session`. A user without a session who hits the handle function (at `/hello`, say) will be redirected through a cookie-detection loop, ending up back at `/hello` if her browser supports cookies. She'll have a session ID and will be given a greeting. If her browser does not support cookies, the user will receive a 302 error.

```erlang
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
```
