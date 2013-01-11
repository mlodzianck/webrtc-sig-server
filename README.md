webrtc-sig-server
=================

Signalling server for web-rtc-app written in Erlang.

It is intened to work with gwt-webrtc-demo application. It is based on YAWS (http://yaws.hyber.org/) HTTP serwer and
uses websockets as a transport layer for realtime messages.

Compile and run:

./rebar get-deps
./rebar compile
./run.sh
next in erlang shell
application:start(wrtc_demo_app).
