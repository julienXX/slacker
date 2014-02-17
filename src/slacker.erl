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

%%----------------------------------------------------------------------
%% Function: channels_history/1
%% Purpose:  Fetch history of messages and events from a given channel
%% Args:     Token is your token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason} (if the process is dead)
%%----------------------------------------------------------------------
channels_history(Token) ->
    slack_request(Token, "channels.history").

%%----------------------------------------------------------------------
%% Function: channels_mark/1
%% Purpose:  Set read cursor in a channel
%% Args:     Token is your token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason} (if the process is dead)
%%----------------------------------------------------------------------
channels_mark(Token) ->
    slack_request(Token, "channels.mark").

%%----------------------------------------------------------------------
%% Function: channels_list/1
%% Purpose:  List of all channels in the team.
%% Args:     Token is your token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason} (if the process is dead)
%%----------------------------------------------------------------------
channels_list(Token) ->
    slack_request(Token, "channels.list").

%%----------------------------------------------------------------------
%% Function: files_upload/1
%% Purpose:  Upload or create a file
%% Args:     Token is your token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason} (if the process is dead)
%%----------------------------------------------------------------------
files_upload(Token) ->
    slack_request(Token, "files.upload").

%%----------------------------------------------------------------------
%% Function: files_list/1
%% Purpose:  List & filter team files
%% Args:     Token is your token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason} (if the process is dead)
%%----------------------------------------------------------------------
files_list(Token) ->
    slack_request(Token, "files.list").

%%----------------------------------------------------------------------
%% Function: im_history/1
%% Purpose:  Fetch history of messages and events from a given direct message channel
%% Args:     Token is your token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason} (if the process is dead)
%%----------------------------------------------------------------------
im_history(Token) ->
    slack_request(Token, "im.history").

%%----------------------------------------------------------------------
%% Function: im_list/1
%% Purpose:  List of im channels the user has.
%% Args:     Token is your token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason} (if the process is dead)
%%----------------------------------------------------------------------
im_list(Token) ->
    slack_request(Token, "im.list").

%%----------------------------------------------------------------------
%% Function: groups_history/1
%% Purpose:  Fetch history of messages and events from a given private group
%% Args:     Token is your token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason} (if the process is dead)
%%----------------------------------------------------------------------
groups_history(Token) ->
    slack_request(Token, "groups.history").

%%----------------------------------------------------------------------
%% Function: groups_list/1
%% Purpose:  List of groups in the team that the calling user has access to.
%% Args:     Token is your token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason} (if the process is dead)
%%----------------------------------------------------------------------
groups_list(Token) ->
    slack_request(Token, "groups.list").

%%----------------------------------------------------------------------
%% Function: search_all/1
%% Purpose:  Search for messages and files matching a query
%% Args:     Token is your token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason} (if the process is dead)
%%----------------------------------------------------------------------
search_all(Token) ->
    slack_request(Token, "search.all").

%%----------------------------------------------------------------------
%% Function: search_files/1
%% Purpose:  Search for files matching a query
%% Args:     Token is your token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason} (if the process is dead)
%%----------------------------------------------------------------------
search_files(Token) ->
    slack_request(Token, "search.files").

%%----------------------------------------------------------------------
%% Function: search_messages/1
%% Purpose:  Search for messages matching a query
%% Args:     Token is your token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason} (if the process is dead)
%%----------------------------------------------------------------------
search_messages(Token) ->
    slack_request(Token, "search.messages").

%%----------------------------------------------------------------------
%% Function: post_message/1
%% Purpose:  Post a message
%% Args:     Token is your token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason} (if the process is dead)
%%----------------------------------------------------------------------
post_message(Token) ->
    slack_request(Token, "chat.postMessage").

%% Internals

slack_request(Token, Endpoint) ->
    Base_URL = "https://slack.com/api/",
    URL = restc:construct_url(Base_URL, Endpoint, [{"token", Token}]),
    restc:request(get, URL).

ok() ->
    ok.
