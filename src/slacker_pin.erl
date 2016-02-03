-module(slacker_pin).

-include("spec.hrl").

-export([add/3, list/2, remove/3]).


%% @doc Pins an item to a channel.
%%
%% Options can be:
%% file: file to pin
%% file_comment: file comment to pin
%% timestamp: timestamp of the message to pin
%%
-spec add(Token :: string(), Channel :: string(), Options :: list()) -> http_response().
add(Token, Channel, Options) ->
    slacker_request:send("pins.add", [{"token", Token},{"channel", Channel}], Options).

%% @doc Lists the items pinned to a channel.
-spec list(Token :: string(), Channel :: string()) -> http_response().
list(Token, Channel) ->
    slacker_request:send("pins.list", [{"token", Token},{"channel", Channel}]).

%% @doc Un-pin an item from a channel.
%%
%% Options can be:
%% file: file to un-pin
%% file_comment: file comment to un-pin
%% timestamp: timestamp of the message to un-pin
%%
-spec remove(Token :: string(), Channel :: string(), Options :: list()) -> http_response().
remove(Token, Channel, Options) ->
    slacker_request:send("pins.remove", [{"token", Token},{"channel", Channel}], Options).
