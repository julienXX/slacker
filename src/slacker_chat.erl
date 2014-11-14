-module(slacker_chat).

-include("spec.hrl").

-export([update/4, delete/3, post_message/3]).

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
