-module(slacker_user).

-include("spec.hrl").

-export([get_presence/2, info/2, list/2, set_active/1, set_presence/2]).


%% @doc Gets user presence information.
-spec get_presence(Token :: string(), User :: string()) -> http_response().
get_presence(Token, User) ->
    slacker_request:send("users.getPresence", [{"token", Token},{"user", User}]).

%% @doc List all users in the team.
%%
%% Options can be:
%% presence: include presence data in the output
%%
-spec list(Token :: string(), Options :: list()) -> http_response().
list(Token, Options) ->
    slacker_request:send("users.list", [{"token", Token}], Options).

%% @doc Gets information about a user.
-spec info(Token :: string(), User :: string()) -> http_response().
info(Token, User) ->
    slacker_request:send("users.info", [{"token", Token},{"user", User}]).

%% @doc Marks a user as active.
-spec set_active(Token :: string()) -> http_response().
set_active(Token) ->
    slacker_request:send("users.setActive", [{"token", Token}]).

%% @doc Sets user presence information.
-spec set_presence(Token :: string(), Presence :: string()) -> http_response().
set_presence(Token, Presence) ->
    slacker_request:send("users.setPresence", [{"token", Token},{"presence", Presence}]).
