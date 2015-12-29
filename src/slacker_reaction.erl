-module(slacker_reaction).

-include("spec.hrl").

-export([add/3, get/2, list/2, remove/3]).


%% @doc Add a reaction for an item at a given timestamp.
-spec add(Token :: string(), Name :: string(), Timestamp :: string()) -> http_response().
add(Token, Name, Timestamp) ->
    slacker_request:send("reactions.add", [{"token", Token},{"name", Name},{"timestamp", Timestamp}]).

%% @doc List all reactions at a given timestamp.
-spec get(Token :: string(), Timestamp :: string()) -> http_response().
get(Token, Timestamp) ->
    slacker_request:send("reactions.get", [{"token", Token},{"timestamp", Timestamp}]).

%% @doc returns a list of all items reacted to by a use.
-spec list(Token :: string(), User :: string()) -> http_response().
list(Token, User) ->
    slacker_request:send("reactions.list", [{"token", Token},{"user", User}]).

%% @doc Removes a reaction at a given timestamp.
-spec remove(Token :: string(), Name :: string(), Timestamp :: string()) -> http_response().
remove(Token, Name, Timestamp) ->
    slacker_request:send("reactions.remove", [{"token", Token},{"name", Name},{"timestamp", Timestamp}]).
