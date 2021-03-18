-module(slacker_file).

-include("spec.hrl").

-export([info/3, list/2, upload/1]).


%% @doc Returns information about a file in your team.
%%
%% Options can be:
%% count: number of items to return per page (default: 100)
%% page: page number of results to return (default: 1)
%%
-spec info(Token :: string(), File :: string(), Options :: list()) -> http_response().
info(Token, File, Options) ->
    slacker_request:send("files.info", [{"token", Token},{"file", File}], Options).

%% @doc List and filter team files.
%%
%% Options can be:
%% user: filter files created by a single user
%% channel: filter files appearing in a specific channel
%% ts_from: filter files created after this timestamp (default: now)
%% ts_to: filter files created before this timestamp (default: now)
%% types: filter files by type (default: all)
%% count: number of items to return per page (default: 100)
%% page: page number of results to return (default: 1)
%%
-spec list(Token :: string(), Options :: list()) -> http_response().
list(Token, Options) ->
    slacker_request:send("files.list", [{"token", Token}], Options).

%% @doc Upload or create a file.
-spec upload(Token :: string()) -> http_response().
upload(_) ->
    erlang:error(not_implemented_error).
