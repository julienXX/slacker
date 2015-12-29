-module(slacker_pin).

-include("spec.hrl").

-export([add/2, list/2, remove/3]).


%% @doc Pin an item.
-spec add(Token :: string(), Channel :: string()) -> http_response().
add(Token, Channel) ->
    slacker_request:send("pins.add", [{"token", Token},{"channel", Channel}]).

%% @doc List items pinned in a channel.
-spec list(Token :: string(), Channel :: string()) -> http_response().
list(Token, Channel) ->
    slacker_request:send("pins.list", [{"token", Token},{"channel", Channel}]).

%% @doc Un-pins an item at a given timestamp.
-spec remove(Token :: string(), Channel :: string(), Timestamp :: string()) -> http_response().
remove(Token, Channel, Timestamp) ->
    slacker_request:send("pins.remove", [{"token", Token},{"channel", Channel},{"timestamp", Timestamp}]).
