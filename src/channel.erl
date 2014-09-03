-module(channel).

-include("spec.hrl").

-export([info/2, join/2, leave/2, history/2,
         mark/3, invite/3, list/1]).

%% @doc Returns information about a team channel.
-spec info(Token :: string(), Channel :: string()) -> http_response().
info(Token, Channel) ->
    request:slack("channels.info", [{"token", Token},{"channel", Channel}]).

%% @doc Join a channel. If the channel does not exist, it is created.
-spec join(Token :: string(), Channel :: string()) -> http_response().
join(Token, Channel) ->
    request:slack("channels.join", [{"token", Token},{"channel", Channel}]).

%% @doc Leave a channel.
-spec leave(Token :: string(), Channel :: string()) -> http_response().
leave(Token, Channel) ->
    request:slack("channels.leave", [{"token", Token},{"channel", Channel}]).

%% @doc Fetch history of messages and events from a given channel.
-spec history(Token :: string(), Channel :: string()) -> http_response().
history(Token, Channel) ->
    request:slack("channels.history", [{"token", Token},{"channel", Channel}]).

%% @doc Set read cursor in a channel.
-spec mark(Token :: string(), Channel :: string(), Timestamp :: string()) -> http_response().
mark(Token, Channel, Timestamp) ->
    request:slack("channels.mark", [{"token", Token},{"channel", Channel},{"ts", Timestamp}]).

%% @doc Set read cursor in a channel.
-spec invite(Token :: string(), Channel :: string(), User :: string()) -> http_response().
invite(Token, Channel, User) ->
    request:slack("channels.invite", [{"token", Token},{"channel", Channel},{"user", User}]).

%% @doc List of all channels in the team.
-spec list(Token :: string()) -> http_response().
list(Token) ->
    request:slack("channels.list", [{"token", Token}]).
