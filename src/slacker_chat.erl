-module(slacker_chat).

-include("spec.hrl").

-export([update/4, delete/3]).
-export([post_message/3, post_message/4, post_message/5]).
-export([post_rich_message/5]).

%% @doc Updates a message in a channel.
-spec update(Token :: string(), TimeStamp :: string(), Channel :: string(), Text :: string()) -> http_response().
update(Token, TimeStamp, Channel, Text) ->
    slacker_request:send("chat.delete", [{"token", Token},{"ts", TimeStamp},{"channel", Channel},{"text", Text}]).

%% @doc Deletes a message from a channel.
-spec delete(Token :: string(), TimeStamp :: string(), Channel :: string()) -> http_response().
delete(Token, TimeStamp, Channel) ->
    slacker_request:send("chat.delete", [{"token", Token},{"ts", TimeStamp},{"channel", Channel}]).

%% @doc Post a message.
-spec post_message(Token :: string(), Channel :: string(), Message :: string()) -> http_response().
post_message(Token, Channel, Message) ->
    slacker_request:send("chat.postMessage", [{"token", Token},{"channel", Channel},{"text", Message}]).

%% @doc Post a message as a specific user.
-spec post_message(Token :: string(), Channel :: string(), Message :: string(), Usrname :: string()) -> http_response().
post_message(Token, Channel, Message, Username) ->
    slacker_request:send("chat.postMessage", [{"token", Token},{"channel", Channel}, {"username", Username},{"text", Message}]).


%% @doc Post a message as a specific user and custom icon.
-spec post_message(Token :: string(), Channel :: string(), Message :: string(), Username :: string(), IconUrl :: string()) -> http_response().
post_message(Token, Channel, Message, Username, IconUrl) ->
    slacker_request:send("chat.postMessage", [{"token", Token},{"channel", Channel}, {"username", Username}, {"icon_url", IconUrl},{"text", Message}]).


%% @doc Post a message as a specific user and custom icon and attachment.
-spec post_rich_message(Token :: string(), Channel :: string(), Username :: string(), IconUrl :: string(), Attachment :: any()) -> http_response().
post_rich_message(Token, Channel, Username, IconUrl, Attachment) ->
    slacker_request:send("chat.postMessage", [{"token", Token},{"channel", Channel},
                                              {"username", Username}, {"icon_url", IconUrl},{"attachments", [Attachment]}]).
