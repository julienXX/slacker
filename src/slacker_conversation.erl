-module(slacker_conversation).

-include("spec.hrl").

-export([archive/2, create/2, history/3, info/2, invite/3,
         join/2, kick/3, leave/2, list/2, mark/3, rename/3,
         set_purpose/3, set_topic/3, unarchive/2]).


%% @doc Archives a conversation.
-spec archive(Token :: string(), Channel :: string()) -> http_response().
archive(Token, Channel) ->
    slacker_request:send("conversations.archive", [{"token", Token},{"channel", Channel}]).

%% @doc Creates a conversation.
-spec create(Token :: string(), Name :: string()) -> http_response().
create(Token, Name) ->
    slacker_request:send("conversations.create", [{"token", Token},{"name", Name}]).

%% @doc Fetches history of messages and events from a conversation.
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
    slacker_request:send("conversations.history", [{"token", Token},{"channel", Channel}], Options).

%% @doc Returns information about a team conversation.
-spec info(Token :: string(), Channel :: string()) -> http_response().
info(Token, Channel) ->
    slacker_request:send("conversations.info", [{"token", Token},{"channel", Channel}]).

%% @doc Invites a user to a conversation.
-spec invite(Token :: string(), Channel :: string(), User :: string()) -> http_response().
invite(Token, Channel, User) ->
    slacker_request:send("conversations.invite", [{"token", Token},{"channel", Channel},{"user", User}]).

%% @doc Join a conversation. If the conversation does not exist, it is created.
-spec join(Token :: string(), Channel :: string()) -> http_response().
join(Token, Channel) ->
    slacker_request:send("conversations.join", [{"token", Token},{"channel", Channel}]).

%% @doc Removes a user from a conversation.
-spec kick(Token :: string(), Channel :: string(), User :: string()) -> http_response().
kick(Token, Channel, User) ->
    slacker_request:send("conversations.kick", [{"token", Token},{"channel", Channel},{"user", User}]).

%% @doc Leave a conversation.
-spec leave(Token :: string(), Channel :: string()) -> http_response().
leave(Token, Channel) ->
    slacker_request:send("conversations.leave", [{"token", Token},{"channel", Channel}]).

%% @doc List of all conversations in the team.
%%
%% Options can be:
%% exclude_archived: do not return archived conversations (default: 0)
%%
-spec list(Token :: string(), Options :: list()) -> http_response().
list(Token, Options) ->
    slacker_request:send("conversations.list", [{"token", Token}], Options).

%% @doc Set read cursor in a conversation.
-spec mark(Token :: string(), Channel :: string(), Timestamp :: string()) -> http_response().
mark(Token, Channel, Timestamp) ->
    slacker_request:send("conversations.mark", [{"token", Token},{"channel", Channel},{"ts", Timestamp}]).

%% @doc Rename a conversation.
-spec rename(Token :: string(), Channel :: string(), Name :: string()) -> http_response().
rename(Token, Channel, Name) ->
    slacker_request:send("conversations.rename", [{"token", Token},{"channel", Channel},{"name", Name}]).

%% @doc Sets the purpose for a conversation.
-spec set_purpose(Token :: string(), Channel :: string(), Purpose :: string()) -> http_response().
set_purpose(Token, Channel, Purpose) ->
    slacker_request:send("conversations.setPurpose", [{"token", Token},{"channel", Channel},{"purpose", Purpose}]).

%% @doc Sets the topic for a conversation.
-spec set_topic(Token :: string(), Channel :: string(), Topic :: string()) -> http_response().
set_topic(Token, Channel, Topic) ->
    slacker_request:send("conversations.setTopic", [{"token", Token},{"channel", Channel},{"topic", Topic}]).

%% @doc Unarchives a conversation.
-spec unarchive(Token :: string(), Channel :: string()) -> http_response().
unarchive(Token, Channel) ->
    slacker_request:send("conversations.unarchive", [{"token", Token},{"channel", Channel}]).
