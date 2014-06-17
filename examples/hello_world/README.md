Hello world example
===================

To try this example, you need GNU `make` and `git` in your PATH.

To build the example, run the following command:

``` bash
$ make
```

To start the release in the foreground:

``` bash
$ ./_rel/bin/hello_world_example console
```

Then point your browser at [http://localhost:8080](http://localhost:8080).

Example output
--------------

``` bash
$ curl -i http://localhost:8080
HTTP/1.1 302 Found
connection: keep-alive
server: Cowboy
date: Tue, 17 Jun 2014 22:19:42 GMT
content-length: 0
set-cookie: SESSION_ID=04fc0f8a5b8e52ef298ef6bc7ad9bca4; Version=1; Path=/
Location: /cookiecheck?path=%2F

$ curl -i -b SESSION_ID=04fc0f8a5b8e52ef298ef6bc7ad9bca4 http://localhost:8080/cookiecheck?path=%2F
HTTP/1.1 302 Found
connection: keep-alive
server: Cowboy
date: Tue, 17 Jun 2014 22:20:02 GMT
content-length: 0
Location: /

$ curl -i -b SESSION_ID=04fc0f8a5b8e52ef298ef6bc7ad9bca4 http://localhost:8080/
HTTP/1.1 200 OK
connection: keep-alive
server: Cowboy
date: Tue, 17 Jun 2014 22:20:11 GMT
content-length: 30
content-type: text/plain

Hello, your visit count is: 1.

$ curl -i -b SESSION_ID=04fc0f8a5b8e52ef298ef6bc7ad9bca4 http://localhost:8080/
HTTP/1.1 200 OK
connection: keep-alive
server: Cowboy
date: Tue, 17 Jun 2014 22:20:14 GMT
content-length: 30
content-type: text/plain

Hello, your visit count is: 2.
```
