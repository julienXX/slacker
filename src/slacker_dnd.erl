-module(slacker_dnd).

-include("spec.hrl").

-export([end_dnd/1, end_snooze/1, set_snooze/2, info/3, team_info/3]).


%% @doc Ends the user's currently scheduled Do Not Disturb session immediately.
-spec end_dnd(Token :: string()) -> http_response().
end_dnd(Token) ->
    slacker_request:send("dnd.endDnd", [{"token", Token}]).

%% @doc Ends the current user's snooze mode immediately.
-spec end_snooze(Token :: string()) -> http_response().
end_snooze(Token) ->
    slacker_request:send("dnd.endSnooze", [{"token", Token}]).

%% @doc Ends the current user's snooze mode immediately.
-spec set_snooze(Token :: string(), NumMinutes :: integer()) -> http_response().
set_snooze(Token, NumMinutes) ->
    slacker_request:send("dnd.endSnooze", [{"token", Token},{"num_minutes", NumMinutes}]).

%% @doc Provides information about a user's current Do Not Disturb settings.
%%
%% Options can be:
%% user: user to fetch status for (default: current user)
%%
-spec info(Token :: string(), User :: string(), Options :: list()) -> http_response().
info(Token, User, Options) ->
    slacker_request:send("dnd.info", [{"token", Token},{"user", User}], Options).

%% @doc Provides information about the current Do Not Disturb settings for users of a Slack team.
%%
%% Options can be:
%% users: users to fetch status for
%%
-spec team_info(Token :: string(), Users :: [string()], Options :: list()) -> http_response().
team_info(Token, Users, Options) ->
    slacker_request:send("dnd.info", [{"token", Token},{"users", Users}], Options).
