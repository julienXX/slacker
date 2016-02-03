-module(slacker_usergroup).

-include("spec.hrl").

-export([create/3, disable/3, enable/3, list/2, update/3, list_users/3, update_users/4]).


%% @doc Create a user group.
%%
%% Options can be:
%% handle: mention handle
%% description: short description
%% channels: comma separated string of encoded channel IDs for which the user group uses as a default
%% include_count: include the number of users in each user group
%%
-spec create(Token :: string(), Name :: string(), Options :: list()) -> http_response().
create(Token, Name, Options) ->
    slacker_request:send("usergroups.create", [{"token", Token},{"name", Name}], Options).

%% @doc Disable a user group.
%%
%% Options can be:
%% include_count: include the number of users in each user group
%%
-spec disable(Token :: string(), Usergroup :: string(), Options :: list()) -> http_response().
disable(Token, Usergroup, Options) ->
    slacker_request:send("usergroups.disable", [{"token", Token},{"usergroup", Usergroup}], Options).

%% @doc Enable a disabled user group.
%%
%% Options can be:
%% include_count: include the number of users in each user group
%%
-spec enable(Token :: string(), Usergroup :: string(), Options :: list()) -> http_response().
enable(Token, Usergroup, Options) ->
    slacker_request:send("usergroups.enable", [{"token", Token},{"usergroup", Usergroup}], Options).

%% @doc List user groups.
%%
%% Options can be:
%% include_disabled: include disabled user groups
%% include_count: include the number of users in each user group
%% include_users: include the list of users in each user group
%%
-spec list(Token :: string(), Options :: list()) -> http_response().
list(Token, Options) ->
    slacker_request:send("usergroups.list", [{"token", Token}], Options).

%% @doc Update a user group.
%%
%% Options can be:
%% name: user group name
%% handle: mention handle
%% description: short description
%% channels: comma separated string of encoded channel IDs for which the user group uses as a default
%% include_count: include the number of users in each user group
%%
-spec update(Token :: string(), Usergroup :: string(), Options :: list()) -> http_response().
update(Token, Usergroup, Options) ->
    slacker_request:send("usergroups.update", [{"token", Token},{"usergroup", Usergroup}], Options).

%% @doc List users in a user group.
%%
%% Options can be:
%% include_disabled: include the disabled user groups
%%
-spec list_users(Token :: string(), Usergroup :: string(), Options :: list()) -> http_response().
list_users(Token, Usergroup, Options) ->
    slacker_request:send("usergroups.users.list", [{"token", Token},{"usergroup", Usergroup}], Options).

%% @doc Update the list of users.
%%
%% Options can be:
%% include_count: include the number of users in the user group
%%
-spec update_users(Token :: string(), Usergroup :: string(), Users :: [string()], Options :: list()) -> http_response().
update_users(Token, Usergroup, Users, Options) ->
    slacker_request:send("usergroups.users.update", [{"token", Token},{"usergroup", Usergroup},{"users", Users}], Options).
