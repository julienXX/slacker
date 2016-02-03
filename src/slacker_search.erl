-module(slacker_search).

-include("spec.hrl").

-export([all/3, files/3, messages/3]).


%% @doc Search for messages and files matching a query.
%%
%% Options can be:
%% sort: return matches sorted by either score or timestamp (default: score)
%% sort_dir: asc or desc (default: desc)
%% highlight: pass a value of 1 to enable query highlight markers
%% count: number of items to return per page (default: 20)
%% page: page number of results to return (default: 1)
%%
-spec all(Token :: string(), Query :: string(), Options :: list()) -> http_response().
all(Token, Query, Options) ->
    slacker_request:send("search.all", [{"token", Token},{"query", Query}], Options).

%% @doc Search for files matching a query.
%%
%% Options can be:
%% sort: return matches sorted by either score or timestamp (default: score)
%% sort_dir: asc or desc (default: desc)
%% highlight: pass a value of 1 to enable query highlight markers
%% count: number of items to return per page (default: 20)
%% page: page number of results to return (default: 1)
%%
-spec files(Token :: string(), Query :: string(), Options :: list()) -> http_response().
files(Token, Query, Options) ->
    slacker_request:send("search.files", [{"token", Token},{"query", Query}], Options).

%% @doc Search for messages matching a query.
%%
%% Options can be:
%% sort: return matches sorted by either score or timestamp (default: score)
%% sort_dir: asc or desc (default: desc)
%% highlight: pass a value of 1 to enable query highlight markers
%% count: number of items to return per page (default: 20)
%% page: page number of results to return (default: 1)
%%
-spec messages(Token :: string(), Query :: string(), Options :: list()) -> http_response().
messages(Token, Query, Options) ->
    slacker_request:send("search.messages", [{"token", Token},{"query", Query}], Options).
