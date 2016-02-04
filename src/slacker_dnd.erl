-module(slacker_dnd).

-include("spec.hrl").

-export([end_dnd/1, end_snooze/1, set_snooze/2, info/2, team_info/2]).


%% @doc Ends the user's currently scheduled Do Not Disturb session immediately.
-spec end_dnd(Token :: string()) -> http_response().
end_dnd(Token) ->
    slacker_request:send("dnd.endDnd", [{"token", Token}]).

%% @doc Ends the current user's snooze mode immediately.
-spec end_snooze(Token :: string()) -> http_response().
end_snooze(Token) ->
    slacker_request:send("dnd.endSnooze", [{"token", Token}]).

%% @doc Adjusts the snooze duration.
-spec set_snooze(Token :: string(), NumMinutes :: integer()) -> http_response().
set_snooze(Token, NumMinutes) ->
    slacker_request:send("dnd.endSnooze", [{"token", Token},{"num_minutes", NumMinutes}]).

%% @doc Provides information about a user's current Do Not Disturb settings.
%%
%% Options can be:
%% user: user to fetch status for (default: current user)
%%
-spec info(Token :: string(), Options :: list()) -> http_response().
info(Token, Options) ->
    slacker_request:send("dnd.info", [{"token", Token}], Options).

%% @doc Provides information about the current Do Not Disturb settings for users of a Slack team.
%%
%% Options can be:
%% users: users to fetch status for
%%
-spec team_info(Token :: string(), Options :: list()) -> http_response().
team_info(Token, Options) ->
    slacker_request:send("dnd.info", [{"token", Token}], Options).
