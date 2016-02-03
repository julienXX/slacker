-module(slacker_emoji).

-include("spec.hrl").

-export([emoji_list/1]).


%% @doc Lists the custom emoji for a team.
-spec emoji_list(Token :: string()) -> http_response().
emoji_list(Token) ->
    slacker_request:send("emoji.list", [{"token", Token}]).
