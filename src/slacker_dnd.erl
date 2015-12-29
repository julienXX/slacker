-module(slacker_dnd).

-include("spec.hrl").

-export([end_dnd/1]).
-export([end_snooze/1, set_snooze/2]).
-export([info/2, team_info/2]).

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
-spec info(Token :: string(), User :: string()) -> http_response().
info(Token, User) ->
    slacker_request:send("dnd.info", [{"token", Token},{"user", User}]).

%% @doc Provides information about the current Do Not Disturb settings for users of a Slack team.
-spec team_info(Token :: string(), Users :: [string()]) -> http_response().
team_info(Token, Users) ->
    slacker_request:send("dnd.info", [{"token", Token},{"users", Users}]).
