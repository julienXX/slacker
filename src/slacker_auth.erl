-module(slacker_auth).

-include("spec.hrl").
-export([test/1]).


%% @doc Checks authentication and tells you who you are.
-spec test(Token :: string()) -> http_response().
test(Token) ->
    slacker_request:send("auth.test", [{"token", Token}]).
