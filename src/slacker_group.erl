-module(slacker_group).

-include("spec.hrl").

-export([history/2, list/1, create/2, create_child/2,
         invite/3, kick/3, leave/2, mark/3, rename/3,
         set_purpose/3, set_topic/3]).


%% @doc Fetch history of messages and events from a given private group.
-spec history(Token :: string(), Channel :: string()) -> http_response().
history(Token, Channel) ->
    slacker_request:send("groups.history", [{"token", Token},{"channel", Channel}]).

%% @doc List of groups in the team that the calling user has access to.
-spec list(Token :: string()) -> http_response().
list(Token) ->
    slacker_request:send("groups.list", [{"token", Token}]).

%% @doc Creates a private group.
-spec create(Token :: string(), Name :: string()) -> http_response().
create(Token, Name) ->
    slacker_request:send("groups.create", [{"token", Token},{"name", Name}]).

%% @doc Clones and archives a private group.
-spec create_child(Token :: string(), Channel :: string()) -> http_response().
create_child(Token, Channel) ->
    slacker_request:send("groups.createChild", [{"token", Token},{"channel", Channel}]).

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

%% @doc Sets the read cursor in a private group.
-spec mark(Token :: string(), Channel :: string(), Timestamp :: string()) -> http_response().
mark(Token, Channel, Timestamp) ->
    slacker_request:send("groups.leave", [{"token", Token},{"channel", Channel},{"ts", Timestamp}]).

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
