-module(group).

-include("spec.hrl").

-export([history/2, list/1]).

%% @doc Fetch history of messages and events from a given private group.
-spec history(Token :: string(), Channel :: string()) -> http_response().
history(Token, Channel) ->
    request:slack("groups.history", [{"token", Token},{"channel", Channel}]).

%% @doc List of groups in the team that the calling user has access to.
-spec list(Token :: string()) -> http_response().
list(Token) ->
    request:slack("groups.list", [{"token", Token}]).
