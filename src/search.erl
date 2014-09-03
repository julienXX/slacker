-module(search).

-include("spec.hrl").

-export([all/2, files/2, messages/2]).

%% @doc Search for messages and files matching a query.
-spec all(Token :: string(), Query :: string()) -> http_response().
all(Token, Query) ->
    request:slack("search.all", [{"token", Token},{"query", Query}]).

%% @doc Search for files matching a query.
-spec files(Token :: string(), Query :: string()) -> http_response().
files(Token, Query) ->
    request:slack("search.files", [{"token", Token},{"query", Query}]).

%% @doc Search for messages matching a query.
-spec messages(Token :: string(), Query :: string()) -> http_response().
messages(Token, Query) ->
    request:slack("search.messages", [{"token", Token},{"query", Query}]).
