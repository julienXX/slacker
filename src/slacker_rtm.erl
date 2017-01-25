-module(slacker_rtm).

-include("spec.hrl").

-export([start/1]).

%% @doc Start a Real Time Messaging API session.
-spec start(Token :: string()) -> http_response().
start(Token) ->
    slacker_request:send("rtm.start", [{"token", Token}], []).
