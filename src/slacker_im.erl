-module(slacker_im).

-include("spec.hrl").

-export([close/2, history/2, list/1, mark/3, open/2]).


%% @doc Close a direct message channel.
-spec close(Token :: string(), Channel :: string()) -> http_response().
close(Token, Channel) ->
    slacker_request:send("im.close", [{"token", Token},{"channel", Channel}]).

%% @doc Fetch history of messages and events from a given direct message channel.
-spec history(Token :: string(), Channel :: string()) -> http_response().
history(Token, Channel) ->
    slacker_request:send("im.history", [{"token", Token},{"channel", Channel}]).

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
