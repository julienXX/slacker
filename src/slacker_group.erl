-module(slacker_group).

-include("spec.hrl").

-export([archive/2, close/2, create/2, create_child/2, history/3,
         info/2, invite/3, kick/3, leave/2, list/2, mark/3, open/2,
         rename/3, set_purpose/3, set_topic/3, unarchive/2]).


%% @doc Archives a private group.
-spec archive(Token :: string(), Channel :: string()) -> http_response().
archive(Token, Channel) ->
    slacker_request:send("groups.archive", [{"token", Token},{"channel", Channel}]).

%% @doc Closes a private group.
-spec close(Token :: string(), Channel :: string()) -> http_response().
close(Token, Channel) ->
    slacker_request:send("groups.close", [{"token", Token},{"channel", Channel}]).

%% @doc Creates a private group.
-spec create(Token :: string(), Name :: string()) -> http_response().
create(Token, Name) ->
    slacker_request:send("groups.create", [{"token", Token},{"name", Name}]).

%% @doc Clones and archives a private group.
-spec create_child(Token :: string(), Channel :: string()) -> http_response().
create_child(Token, Channel) ->
    slacker_request:send("groups.createChild", [{"token", Token},{"channel", Channel}]).

%% @doc Fetch history of messages and events from a given private group.
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
    slacker_request:send("groups.history", [{"token", Token},{"channel", Channel}], Options).

%% @doc Gets information about a private group.
-spec info(Token :: string(), Channel :: string()) -> http_response().
info(Token, Channel) ->
    slacker_request:send("groups.info", [{"token", Token},{"channel", Channel}]).

%% @doc Invites a user to a private group.
-spec invite(Token :: string(), Channel :: string(), User :: string()) -> http_response().
invite(Token, Channel, User) ->
    slacker_request:send("groups.invite", [{"token", Token},{"channel", Channel},{"user", User}]).

%% @doc Removes a user from a private group.
-spec kick(Token :: string(), Channel :: string(), User :: string()) -> http_response().
kick(Token, Channel, User) ->
    slacker_request:send("groups.kick", [{"token", Token},{"channel", Channel},{"user", User}]).

%% @doc Leaves a private group.
-spec leave(Token :: string(), Channel :: string()) -> http_response().
leave(Token, Channel) ->
    slacker_request:send("groups.leave", [{"token", Token},{"channel", Channel}]).

%% @doc List of groups in the team that the calling user has access to.
%%
%% Options can be:
%% exclude_archived: do not return archived private channels (default: 0)
%%
-spec list(Token :: string(), Options :: list()) -> http_response().
list(Token, Options) ->
    slacker_request:send("groups.list", [{"token", Token}], Options).

%% @doc Sets the read cursor in a private group.
-spec mark(Token :: string(), Channel :: string(), Timestamp :: string()) -> http_response().
mark(Token, Channel, Timestamp) ->
    slacker_request:send("groups.leave", [{"token", Token},{"channel", Channel},{"ts", Timestamp}]).

%% @doc Opens a private group.
-spec open(Token :: string(), Channel :: string()) -> http_response().
open(Token, Channel) ->
    slacker_request:send("groups.open", [{"token", Token},{"channel", Channel}]).

%% @doc Rename a group.
-spec rename(Token :: string(), Channel :: string(), Name :: string()) -> http_response().
rename(Token, Channel, Name) ->
    slacker_request:send("groups.rename", [{"token", Token},{"channel", Channel},{"name", Name}]).

%% @doc Sets the purpose for a private group.
-spec set_purpose(Token :: string(), Channel :: string(), Purpose :: string()) -> http_response().
set_purpose(Token, Channel, Purpose) ->
    slacker_request:send("groups.setPurpose", [{"token", Token},{"channel", Channel},{"purpose", Purpose}]).

%% @doc Sets the topic for a private group.
-spec set_topic(Token :: string(), Channel :: string(), Topic :: string()) -> http_response().
set_topic(Token, Channel, Topic) ->
    slacker_request:send("groups.setTopic", [{"token", Token},{"channel", Channel},{"topic", Topic}]).

%% @doc Unarchives a private group.
-spec unarchive(Token :: string(), Channel :: string()) -> http_response().
unarchive(Token, Channel) ->
    slacker_request:send("groups.unarchive", [{"token", Token},{"channel", Channel}]).
