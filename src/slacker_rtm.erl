-module(slacker_rtm).

-include("spec.hrl").

-export([start/1, connect/1]).

%% @doc Start a Real Time Messaging API session.
-spec start(Token :: string()) -> http_response().
start(Token) ->
    slacker_request:send("rtm.start", [{"token", Token}], []).

%% @doc Start a Real Time Messaging API session.
-spec connect(Token :: string()) -> http_response().
connect(Token) ->
    slacker_request:send("rtm.connect", [{"token", Token}], []).
