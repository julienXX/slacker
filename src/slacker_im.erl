-module(slacker_im).

-include("spec.hrl").

-export([close/2, history/3, list/1, mark/3, open/2]).


%% @doc Close a direct message channel.
-spec close(Token :: string(), Channel :: string()) -> http_response().
close(Token, Channel) ->
    slacker_request:send("im.close", [{"token", Token},{"channel", Channel}]).

%% @doc Fetch history of messages and events from a given direct message channel.
%%
%% Options can be:
%% latest: end of time range of messages to include in results
%% oldest: start of time range of messages to include in results
%% inclusive: include messages with latest or oldest timestamp in results (default: 0)
%% count: number of messages to return, between 1 and 1000 (default: 100)
%% unreads: include unread_count_display in the output (default: 0)
%%
-spec history(Token :: string(), Channel :: string(), Options :: list()) -> http_response().
history(Token, Channel, Options) ->
    slacker_request:send("im.history", [{"token", Token},{"channel", Channel}], Options).

%% @doc List of im channels the user has.
-spec list(Token :: string()) -> http_response().
list(Token) ->
    slacker_request:send("im.list", [{"token", Token}]).

%% @doc Moves the read cursor in a direct message channel.
-spec mark(Token :: string(), Channel :: string(), Timestamp :: string()) -> http_response().
mark(Token, Channel, Timestamp) ->
    slacker_request:send("im.history", [{"token", Token},{"channel", Channel},{"ts", Timestamp}]).

%% @doc Opens a direct message channel.
-spec open(Token :: string(), User :: string()) -> http_response().
open(Token, User) ->
    slacker_request:send("im.open", [{"token", Token},{"user", User}]).
