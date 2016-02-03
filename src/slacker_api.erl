-module(slacker_api).

-include("spec.hrl").
-export([test/2]).


%% @doc Checks API calling code
%%
%% Options can be:
%% error: error response to return
%% foo: example property to return
%%
-spec test(Token :: string(), Options :: list()) -> http_response().
test(Token, Options) ->
    slacker_request:send("api.test", [{"token", Token}], Options).
