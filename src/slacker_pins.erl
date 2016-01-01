-module(slacker_pins).

-include("spec.hrl").

-export([add/5, list/2, remove/5]).

%% @doc Pin an item (file, file comment, channel message, or group message) to a particular channel.
-spec add(Token :: string(), Channel :: string(), File :: string(), 
          FileComment :: string(), TimeStamp :: string()) -> http_response().
add(Token, Channel, File, FileComment, TimeStamp) ->
    Data = [{"token", Token}, {"channel", Channel}, {"file", File},
            {"file_comment", FileComment}, {"timestamp", TimeStamp}
           ],
    slacker_request:send("pins.add", Data).

%% @doc This method lists the items pinned to a channel.
-spec list(Token :: string(), Channel :: string()) -> http_response().
list(Token, Channel) ->
    slacker_request:send("pins.list", [{"token", Token}, {"channel", Channel}]).

%% @doc Un-pin an item (file, file comment, channel message, or group message) from a channel.
-spec remove(Token :: string(), Channel :: string(), File :: string(),
            FileComment :: string(), TimeStamp :: string()) -> http_response().
remove(Token, Channel, File, FileComment, TimeStamp) ->
    Data = [{"token", Token}, {"channel", Channel}, {"file", File},
            {"file_comment", FileComment}, {"timestamp", TimeStamp}],
    slacker_request:send("pins.remove", Data).
