-module(slacker_api).

-include("spec.hrl").
-export([test/1, test/2]).


%% @doc Checks API calling code
-spec test(Token :: string()) -> http_response().
test(Token) ->
    slacker_request:send("api.test", [{"token", Token}]).

-spec test(Token :: string(), Error :: string()) -> http_response().
test(Token, Error) ->
    slacker_request:send("api.test", [{"token", Token},{"error", Error}]).
