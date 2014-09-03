-module(users).

-include("spec.hrl").
-export([list/1]).

%% @doc List all users in the team.
-spec list(Token :: string()) -> http_response().
list(Token) ->
    request:send("users.list", [{"token", Token}]).
