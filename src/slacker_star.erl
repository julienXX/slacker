-module(slacker_star).

-include("spec.hrl").

-export([add/2, list/3, remove/2]).


%% @doc Add a star to an item.
%%
%% Options can be:
%% file: file to add star to
%% file_comment: file comment to add star to
%% channel: channel to add star to
%% timestamp: timestamp of the message to add star to
%%
-spec add(Token :: string(), Options :: list()) -> http_response().
add(Token, Options) ->
    slacker_request:send("pins.add", [{"token", Token}], Options).

%% @doc Lists the items starred by a user.
%%
%% Options can be:
%% user: show star from a user (default: current user)
%% count: number of items to return per page (default: 100)
%% page: page number of results to return (default: 1)
%%
-spec list(Token :: string(), Channel :: string(), Options :: list()) -> http_response().
list(Token, Channel, Options) ->
    slacker_request:send("stars.list", [{"token", Token},{"channel", Channel}], Options).

%% @doc Removes a star from an item.
%%
%% Options can be:
%% file: file to remove star from
%% file_comment: file comment to remove star from
%% channel: channel to remove star from
%% timestamp: timestamp of the message to remove star from
%%
-spec remove(Token :: string(), Options :: list()) -> http_response().
remove(Token, Options) ->
    slacker_request:send("stars.remove", [{"token", Token}], Options).
