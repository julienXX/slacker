%%%----------------------------------------------------------------------------
%%% @author Julien Blanchard <julien@sideburns.eu>
%%% @doc
%%% Erlang library for Slack API - http://api.slack.com/
%%% @end
%%%----------------------------------------------------------------------------

-module(slacker).

-export([start/0, stop/0]).
-export([auth_test/1,
         users_list/1, users_info/2, users_active/1,
         channels_info/2, channels_join/2, channels_leave/2, channels_history/2,
         channels_mark/3, channels_invite/3, channels_list/1, channels_kick/3,
         channels_rename/3, channels_purpose/3, channels_topic/3,
         chat_update/4, chat_delete/3,
         files_upload/1, files_list/1, files_info/2,
         im_history/2, im_list/1, im_mark/3,
         groups_history/2, groups_list/1, groups_create/2, groups_create_child/2,
         groups_invite/3, groups_kick/3, groups_leave/2, groups_mark/3, groups_rename/3,
         groups_purpose/3, groups_topic/3,
         search_all/2, search_files/2, search_messages/2,
         post_message/3,
         stars_list/1,
         emoji_list/1,
         presence_set/2]).

-type headers() :: list({string(), any()}).
-type json_term() :: list({binary(), json_term()})
    | list(json_term())
    | true
    | false
    | null
    | integer()
    | float()
    | binary().
-type http_response() :: {ok, Status :: integer(), Headers :: headers(), Body :: json_term()}.

-define(DEPS, [crypto, asn1, public_key, ssl, inets, slacker]).
-define(API_URL, "https://slack.com/api/").

-spec start() -> ok.
start() ->
    [application:start(A) || A <- ?DEPS],
    ok.

-spec stop() -> ok.
stop() ->
    [application:stop(A) || A <- ?DEPS],
    ok.

%%% Slack API

%% @doc Checks authentication and tells you who you are.
-spec auth_test(Token :: string()) -> http_response().
auth_test(Token) ->
    slack_request("auth.test", [{"token", Token}]).

%% @doc List all users in the team.
-spec users_list(Token :: string()) -> http_response().
users_list(Token) ->
    slack_request("users.list", [{"token", Token}]).

%% @doc Gets information about a user.
-spec users_info(Token :: string(), User :: string()) -> http_response().
users_info(Token, User) ->
    slack_request("users.info", [{"token", Token},{"user", User}]).

%% @doc Marks a user as active.
-spec users_active(Token :: string()) -> http_response().
users_active(Token) ->
    slack_request("users.setActive", [{"token", Token}]).

%% @doc Returns information about a team channel.
-spec channels_info(Token :: string(), Channel :: string()) -> http_response().
channels_info(Token, Channel) ->
    slack_request("channels.info", [{"token", Token},{"channel", Channel}]).

%% @doc Join a channel. If the channel does not exist, it is created.
-spec channels_join(Token :: string(), Channel :: string()) -> http_response().
channels_join(Token, Channel) ->
    slack_request("channels.join", [{"token", Token},{"channel", Channel}]).

%% @doc Leave a channel.
-spec channels_leave(Token :: string(), Channel :: string()) -> http_response().
channels_leave(Token, Channel) ->
    slack_request("channels.leave", [{"token", Token},{"channel", Channel}]).

%% @doc Fetch history of messages and events from a given channel.
-spec channels_history(Token :: string(), Channel :: string()) -> http_response().
channels_history(Token, Channel) ->
    slack_request("channels.history", [{"token", Token},{"channel", Channel}]).

%% @doc Set read cursor in a channel.
-spec channels_mark(Token :: string(), Channel :: string(), Timestamp :: string()) -> http_response().
channels_mark(Token, Channel, Timestamp) ->
    slack_request("channels.mark", [{"token", Token},{"channel", Channel},{"ts", Timestamp}]).

%% @doc Set read cursor in a channel.
-spec channels_invite(Token :: string(), Channel :: string(), User :: string()) -> http_response().
channels_invite(Token, Channel, User) ->
    slack_request("channels.invite", [{"token", Token},{"channel", Channel},{"user", User}]).

%% @doc List of all channels in the team.
-spec channels_list(Token :: string()) -> http_response().
channels_list(Token) ->
    slack_request("channels.list", [{"token", Token}]).

%% @doc Removes a user from a channel.
-spec channels_kick(Token :: string(), Channel :: string(), User :: string()) -> http_response().
channels_kick(Token, Channel, User) ->
    slack_request("channels.kick", [{"token", Token},{"channel", Channel},{"user", User}]).

%% @doc Rename a channel.
-spec channels_rename(Token :: string(), Channel :: string(), Name :: string()) -> http_response().
channels_rename(Token, Channel, Name) ->
    slack_request("channels.rename", [{"token", Token},{"channel", Channel},{"name", Name}]).

%% @doc Sets the purpose for a channel.
-spec channels_purpose(Token :: string(), Channel :: string(), Purpose :: string()) -> http_response().
channels_purpose(Token, Channel, Purpose) ->
    slack_request("channels.setPurpose", [{"token", Token},{"channel", Channel},{"purpose", Purpose}]).

%% @doc Sets the topic for a channel.
-spec channels_topic(Token :: string(), Channel :: string(), Topic :: string()) -> http_response().
channels_topic(Token, Channel, Topic) ->
    slack_request("channels.setTopic", [{"token", Token},{"channel", Channel},{"topic", Topic}]).

%% @doc Upload or create a file.
-spec files_upload(Token :: string()) -> http_response().
files_upload(Token) ->
    erlang:error(not_implemented_error).

%% @doc List & filter team files.
-spec files_list(Token :: string()) -> http_response().
files_list(Token) ->
    slack_request("files.list", [{"token", Token}]).

%% @doc Returns information about a file in your team.
-spec files_info(Token :: string(), File :: string()) -> http_response().
files_info(Token, File) ->
    slack_request("files.info", [{"token", Token},{"file", File}]).

%% @doc Fetch history of messages and events from a given direct message channel.
-spec im_history(Token :: string(), Channel :: string()) -> http_response().
im_history(Token, Channel) ->
    slack_request("im.history", [{"token", Token},{"channel", Channel}]).

%% @doc List of im channels the user has.
-spec im_list(Token :: string()) -> http_response().
im_list(Token) ->
    slack_request("im.list", [{"token", Token}]).

%% @doc Moves the read cursor in a direct message channel.
-spec im_mark(Token :: string(), Channel :: string(), Timestamp :: string()) -> http_response().
im_mark(Token, Channel, Timestamp) ->
    slack_request("im.mark", [{"token", Token},{"channel", Channel},{"ts", Timestamp}]).

%% @doc Fetch history of messages and events from a given private group.
-spec groups_history(Token :: string(), Channel :: string()) -> http_response().
groups_history(Token, Channel) ->
    slack_request("groups.history", [{"token", Token},{"channel", Channel}]).

%% @doc List of groups in the team that the calling user has access to.
-spec groups_list(Token :: string()) -> http_response().
groups_list(Token) ->
    slack_request("groups.list", [{"token", Token}]).

%% @doc Creates a private group.
-spec groups_create(Token :: string(), Name :: string()) -> http_response().
groups_create(Token, Name) ->
    slack_request("groups.create", [{"token", Token},{"name", Name}]).

%% @doc Clones and archives a private group.
-spec groups_create_child(Token :: string(), Channel :: string()) -> http_response().
groups_create_child(Token, Channel) ->
    slack_request("groups.createChild", [{"token", Token},{"channel", Channel}]).

%% @doc Invites a user to a private group.
-spec groups_invite(Token :: string(), Channel :: string(), User :: string()) -> http_response().
groups_invite(Token, Channel, User) ->
    slack_request("groups.invite", [{"token", Token},{"channel", Channel},{"user", User}]).

%% @doc Removes a user from a private group.
-spec groups_kick(Token :: string(), Channel :: string(), User :: string()) -> http_response().
groups_kick(Token, Channel, User) ->
    slack_request("groups.kick", [{"token", Token},{"channel", Channel},{"user", User}]).

%% @doc Leaves a private group.
-spec groups_leave(Token :: string(), Channel :: string()) -> http_response().
groups_leave(Token, Channel) ->
    slack_request("groups.leave", [{"token", Token},{"channel", Channel}]).

%% @doc Sets the read cursor in a private group.
-spec groups_mark(Token :: string(), Channel :: string(), Timestamp :: string()) -> http_response().
groups_mark(Token, Channel, Timestamp) ->
    slack_request("groups.leave", [{"token", Token},{"channel", Channel},{"ts", Timestamp}]).

%% @doc Rename a group.
-spec groups_rename(Token :: string(), Channel :: string(), Name :: string()) -> http_response().
groups_rename(Token, Channel, Name) ->
    slack_request("groups.rename", [{"token", Token},{"channel", Channel},{"name", Name}]).

%% @doc Sets the purpose for a private group.
-spec groups_purpose(Token :: string(), Channel :: string(), Purpose :: string()) -> http_response().
groups_purpose(Token, Channel, Purpose) ->
    slack_request("groups.setPurpose", [{"token", Token},{"channel", Channel},{"purpose", Purpose}]).

%% @doc Sets the topic for a private group.
-spec groups_topic(Token :: string(), Channel :: string(), Topic :: string()) -> http_response().
groups_topic(Token, Channel, Topic) ->
    slack_request("groups.setTopic", [{"token", Token},{"channel", Channel},{"topic", Topic}]).

%% @doc Search for messages and files matching a query.
-spec search_all(Token :: string(), Query :: string()) -> http_response().
search_all(Token, Query) ->
    slack_request("search.all", [{"token", Token},{"query", Query}]).

%% @doc Search for files matching a query.
-spec search_files(Token :: string(), Query :: string()) -> http_response().
search_files(Token, Query) ->
    slack_request("search.files", [{"token", Token},{"query", Query}]).

%% @doc Search for messages matching a query.
-spec search_messages(Token :: string(), Query :: string()) -> http_response().
search_messages(Token, Query) ->
    slack_request("search.messages", [{"token", Token},{"query", Query}]).

%% @doc Updates a message in a channel.
-spec chat_update(Token :: string(), TimeStamp :: string(), Channel :: string(), Text :: string()) -> http_response().
chat_update(Token, TimeStamp, Channel, Text) ->
    slack_request("chat.delete", [{"token", Token},{"ts", TimeStamp},{"channel", Channel},{"text", Text}]).

%% @doc Deletes a message from a channel.
-spec chat_delete(Token :: string(), TimeStamp :: string(), Channel :: string()) -> http_response().
chat_delete(Token, TimeStamp, Channel) ->
    slack_request("chat.delete", [{"token", Token},{"ts", TimeStamp},{"channel", Channel}]).

%% @doc Post a message.
-spec post_message(Token :: string(), Channel :: string(), Message :: string()) -> http_response().
post_message(Token, Channel, Message) ->
    slack_request("chat.postMessage", [{"token", Token},{"channel", Channel},{"text", Message}]).

%% @doc Lists the items starred by a user.
-spec stars_list(Token :: string()) -> http_response().
stars_list(Token) ->
    slack_request("stars.list", [{"token", Token}]).

%% @doc Lists the custom emoji for a team.
-spec emoji_list(Token :: string()) -> http_response().
emoji_list(Token) ->
    slack_request("emoji.list", [{"token", Token}]).

%% @doc Manually set user presence.
-spec presence_set(Token :: string(), Presence :: string()) -> http_response().
presence_set(Token, Presence) ->
    slack_request("presence.set", [{"token", Token},{"presence", Presence}]).


%%% Internals

%% @doc Sends a request to Slack API
slack_request(Endpoint, Params) ->
    URL = restc:construct_url(?API_URL, Endpoint, Params),
    restc:request(get, URL).
