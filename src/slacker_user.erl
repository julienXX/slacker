-module(slacker_user).

-include("spec.hrl").

-export([list/1, info/2, active/1]).


%% @doc List all users in the team.
-spec list(Token :: string()) -> http_response().
list(Token) ->
    slacker_request:send("users.list", [{"token", Token}]).

%% @doc Gets information about a user.
-spec info(Token :: string(), User :: string()) -> http_response().
info(Token, User) ->
    slacker_request:send("users.info", [{"token", Token},{"user", User}]).

%% @doc Marks a user as active.
-spec active(Token :: string()) -> http_response().
active(Token) ->
    slacker_request:send("users.setActive", [{"token", Token}]).
