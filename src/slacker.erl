-module(slacker).

-export([
         users_list/1,
         channels_history/1, channels_mark/1, channels_list/1,
         files_upload/1, files_list/1,
         im_history/1, im_list/1,
         groups_history/1, groups_list/1,
         search_all/1, search_files/1, search_messages/1,
         post_message/1
]).


%% API

%%----------------------------------------------------------------------
%% Function: users_list/1
%% Purpose:  List all users in the team
%% Args:     Token is your token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason} (if the process is dead)
%%----------------------------------------------------------------------
users_list(Token) ->
    slack_request(Token, "users.list").

channels_history(Token) ->
    %% Fetch history of messages and events from a given channel
    slack_request(Token, "channels.history").

channels_mark(Token) ->
    %% Set read cursor in a channel
    slack_request(Token, "channels.mark").

channels_list(Token) ->
    %% List of all channels in the team.
    slack_request(Token, "channels.list").

files_upload(Token) ->
    %% Upload or create a file
    slack_request(Token, "files.upload").

files_list(Token) ->
    %% List & filter team files
    slack_request(Token, "files.list").

im_history(Token) ->
    %% Fetch history of messages and events from a given direct message channel
    slack_request(Token, "im.history").

im_list(Token) ->
    %% List of im channels the user has.
    slack_request(Token, "im.list").

groups_history(Token) ->
    %% Fetch history of messages and events from a given private group
    slack_request(Token, "groups.history").

groups_list(Token) ->
    %% List of groups in the team that the calling user has access to.
    slack_request(Token, "groups.list").

search_all(Token) ->
    %% Search for messages and files matching a query
    slack_request(Token, "search.all").

search_files(Token) ->
    %% Search for files matching a query
    slack_request(Token, "search.files").

search_messages(Token) ->
    %% Search for messages matching a query
    slack_request(Token, "search.messages").

post_message(Token) ->
    %% Post a message
    slack_request(Token, "chat.postMessage").

%% Internals

slack_request(Token, Endpoint) ->
    Base_URL = "https://slack.com/api/",
    URL = restc:construct_url(Base_URL, Endpoint, [{"token", Token}]),
    restc:request(get, URL).

ok() ->
    ok.
