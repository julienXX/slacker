-module(slacker_chat).

-include("spec.hrl").

-export([delete/3, post_message/4, update/5]).


%% @doc Deletes a message from a channel.
-spec delete(Token :: string(), Timestamp :: string(), Channel :: string()) -> http_response().
delete(Token, Timestamp, Channel) ->
    slacker_request:send("chat.delete", [{"token", Token},{"ts", Timestamp},{"channel", Channel}]).

%% @doc Post a message.
%%
%% Options can be:
%% username: name of the bot
%% as_user: pass true to post the message as the authed user, instead of as a bot
%% parse: see https://api.slack.com/docs/formatting
%% link_names: find and link channel names and usernames
%% attachments: structured message attachments
%% unfurl_links: true to enable unfurling of primarily text-based content
%% unfurl_media: false to disable unfurling of media content
%% icon_url: URL to an image to use as the icon for this message
%% icon_emoji: emoji to use as the icon for this message
%%
-spec post_message(Token :: string(), Channel :: string(), Message :: string(), Options :: list()) -> http_response().
post_message(Token, Channel, Message, Options) ->
    slacker_request:send("chat.postMessage", [{"token", Token},{"channel", Channel},{"text", Message}], Options).

%% @doc Updates a message in a channel.
%%
%% Options can be:
%% attachments: structured message attachments
%% parse: see https://api.slack.com/docs/formatting
%% link_names: find and link channel names and usernames
%%
-spec update(Token :: string(), Timestamp :: string(), Channel :: string(), Text :: string(), Options :: list()) -> http_response().
update(Token, Timestamp, Channel, Text, Options) ->
    slacker_request:send("chat.delete", [{"token", Token},{"ts", Timestamp},{"channel", Channel},{"text", Text}], Options).
