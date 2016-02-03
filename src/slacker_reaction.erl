-module(slacker_reaction).

-include("spec.hrl").

-export([add/3, get/2, list/2, remove/3]).


%% @doc Add a reaction for an item at a given timestamp.
%%
%% Options can be:
%% file: file to add reaction to
%% file_comment: file comment to add reaction to
%% channel: channel where the message to add reaction to was posted
%% timestamp: timestamp of the message to add reaction to
%%
-spec add(Token :: string(), Name :: string(), Options :: list()) -> http_response().
add(Token, Name, Options) ->
    slacker_request:send("reactions.add", [{"token", Token},{"name", Name}], Options).

%% @doc List all reactions at a given timestamp.
%%
%% Options can be:
%% file: file to get reaction for
%% file_comment: file comment to get reaction for
%% channel: channel where the message to add reaction to was posted
%% timestamp: timestamp of the message to add reaction to
%% full: if true always return the complete reaction list
%%
-spec get(Token :: string(), Options :: list()) -> http_response().
get(Token, Options) ->
    slacker_request:send("reactions.get", [{"token", Token}], Options).

%% @doc returns a list of all items reacted to by a use.
%%
%% Options can be:
%% user: show reactions made by this user (default: current user)
%% full: if true always return the complete reaction list
%% count: number of items to return per page (default: 100)
%% page: page number of results to return (default: 1)
%%
-spec list(Token :: string(), Options :: list()) -> http_response().
list(Token, Options) ->
    slacker_request:send("reactions.list", [{"token", Token}], Options).

%% @doc Removes a reaction at a given timestamp.
%%
%% Options can be:
%% file: file to remove reaction from
%% file_comment: file comment to remove reaction from
%% channel: channel where the message to remove reaction from was posted
%% timestamp: timestamp of the message to remove reaction from
%%
-spec remove(Token :: string(), Name :: string(), Options :: list()) -> http_response().
remove(Token, Name, Options) ->
    slacker_request:send("reactions.remove", [{"token", Token},{"name", Name}], Options).
