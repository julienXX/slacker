-module(file).

-include("spec.hrl").

-export([upload/1, list/1, info/2]).

%% @doc Upload or create a file.
-spec upload(Token :: string()) -> http_response().
upload(Token) ->
    erlang:error(not_implemented_error).

%% @doc List & filter team files.
-spec list(Token :: string()) -> http_response().
list(Token) ->
    request:slack("files.list", [{"token", Token}]).

%% @doc Returns information about a file in your team.
-spec info(Token :: string(), File :: string()) -> http_response().
info(Token, File) ->
    request:slack("files.info", [{"token", Token},{"file", File}]).
