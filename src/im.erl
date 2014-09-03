-module(im).

-include("spec.hrl").

-export([history/2, list/1]).

%% @doc Fetch history of messages and events from a given direct message channel.
-spec history(Token :: string(), Channel :: string()) -> http_response().
history(Token, Channel) ->
    request:slack("im.history", [{"token", Token},{"channel", Channel}]).

%% @doc List of im channels the user has.
-spec list(Token :: string()) -> http_response().
list(Token) ->
    request:slack("im.list", [{"token", Token}]).
