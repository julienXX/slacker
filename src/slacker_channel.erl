-module(slacker_channel).

-include("spec.hrl").

-export([archive/2, create/2, history/3, info/2, invite/3,
         join/2, kick/3, leave/2, list/2, mark/3, rename/3,
         set_purpose/3, set_topic/3, unarchive/2]).


%% @doc Archives a channel.
-spec archive(Token :: string(), Channel :: string()) -> http_response().
archive(Token, Channel) ->
    slacker_request:send("channels.archive", [{"token", Token},{"channel", Channel}]).

%% @doc Creates a channel.
-spec create(Token :: string(), Name :: string()) -> http_response().
create(Token, Name) ->
    slacker_request:send("channels.create", [{"token", Token},{"name", Name}]).

%% @doc Fetches history of messages and events from a channel.
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
    slacker_request:send("channels.history", [{"token", Token},{"channel", Channel}], Options).

%% @doc Returns information about a team channel.
-spec info(Token :: string(), Channel :: string()) -> http_response().
info(Token, Channel) ->
    slacker_request:send("channels.info", [{"token", Token},{"channel", Channel}]).

%% @doc Invites a user to a channel.
-spec invite(Token :: string(), Channel :: string(), User :: string()) -> http_response().
invite(Token, Channel, User) ->
    slacker_request:send("channels.invite", [{"token", Token},{"channel", Channel},{"user", User}]).

%% @doc Join a channel. If the channel does not exist, it is created.
-spec join(Token :: string(), Channel :: string()) -> http_response().
join(Token, Channel) ->
    slacker_request:send("channels.join", [{"token", Token},{"channel", Channel}]).

%% @doc Removes a user from a channel.
-spec kick(Token :: string(), Channel :: string(), User :: string()) -> http_response().
kick(Token, Channel, User) ->
    slacker_request:send("channels.kick", [{"token", Token},{"channel", Channel},{"user", User}]).

%% @doc Leave a channel.
-spec leave(Token :: string(), Channel :: string()) -> http_response().
leave(Token, Channel) ->
    slacker_request:send("channels.leave", [{"token", Token},{"channel", Channel}]).

%% @doc List of all channels in the team.
%%
%% Options can be:
%% exclude_archived: do not return archived channels (default: 0)
%%
-spec list(Token :: string(), Options :: list()) -> http_response().
list(Token, Options) ->
    slacker_request:send("channels.list", [{"token", Token}], Options).

%% @doc Set read cursor in a channel.
-spec mark(Token :: string(), Channel :: string(), Timestamp :: string()) -> http_response().
mark(Token, Channel, Timestamp) ->
    slacker_request:send("channels.mark", [{"token", Token},{"channel", Channel},{"ts", Timestamp}]).

%% @doc Rename a channel.
-spec rename(Token :: string(), Channel :: string(), Name :: string()) -> http_response().
rename(Token, Channel, Name) ->
    slacker_request:send("channels.rename", [{"token", Token},{"channel", Channel},{"name", Name}]).

%% @doc Sets the purpose for a channel.
-spec set_purpose(Token :: string(), Channel :: string(), Purpose :: string()) -> http_response().
set_purpose(Token, Channel, Purpose) ->
    slacker_request:send("channels.setPurpose", [{"token", Token},{"channel", Channel},{"purpose", Purpose}]).

%% @doc Sets the topic for a channel.
-spec set_topic(Token :: string(), Channel :: string(), Topic :: string()) -> http_response().
set_topic(Token, Channel, Topic) ->
    slacker_request:send("channels.setTopic", [{"token", Token},{"channel", Channel},{"topic", Topic}]).

%% @doc Unarchives a channel.
-spec unarchive(Token :: string(), Channel :: string()) -> http_response().
unarchive(Token, Channel) ->
    slacker_request:send("channels.unarchive", [{"token", Token},{"channel", Channel}]).
