-module(httpc_util).
-include_lib("eunit/include/eunit.hrl").
-export([build_json_request/3]).


build_json_request(Type, URI, JSON) ->
    Body = mochijson2:encode(JSON),
    erlang:iolist_to_binary([
        lists:concat([Type, " ", URI, " HTTP/1.1\r\n"
            "Content-Length: ", erlang:iolist_size(Body), "\r\n"
            "Content-Type: application/json\r\n\r\n"
        ]),
        Body
    ]).

build_request_test_() ->
    [
     ?_assertMatch(
        <<"GET http://localhost/ HTTP/1.1\r\nContent-Length: 2\r\nContent-Type: application/json\r\n\r\n{}">>,
        build_json_request("GET", "http://localhost/", {struct, []})),

     ?_assertMatch(
        <<"GET http://localhost/ HTTP/1.1\r\nContent-Length: 13\r\nContent-Type: application/json\r\n\r\n{\"foo\":\"bar\"}">>,
        build_json_request("GET", "http://localhost/", {struct, [{foo, <<"bar">>}]})),

     ?_assertMatch(
        <<"GET http://localhost/ HTTP/1.1\r\nContent-Length: 18\r\nContent-Type: application/json\r\n\r\n{\"values\":[1,2,3]}">>,
        build_json_request("GET", "http://localhost/", {struct, [{values, [1,2,3]}]}))


    ].
