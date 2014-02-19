-module(slacker).

-export([
         init/0,
         users_list/1,
         channels_history/2, channels_mark/3, channels_list/1,
         files_upload/1, files_list/1,
         im_history/2, im_list/1,
         groups_history/2, groups_list/1,
         search_all/1, search_files/1, search_messages/1,
         post_message/1
]).


%% API

init() ->
    application:start(inets),
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl).


%% Slack API

%%----------------------------------------------------------------------
%% Function: users_list/1
%% Purpose:  List all users in the team
%% Args:     Token is your token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason}
%%----------------------------------------------------------------------
users_list(Token) ->
    slack_request(Token, "users.list").

%%----------------------------------------------------------------------
%% Function: channels_history/1
%% Purpose:  Fetch history of messages and events from a given channel
%% Args:     Token, Channel ID
%% Returns:  A list of {Status, Body}
%%           or {error, Reason}
%%----------------------------------------------------------------------
channels_history(Token, Channel) ->
    slack_request(Token, Channel, "channels.history").

%%----------------------------------------------------------------------
%% Function: channels_mark/1
%% Purpose:  Set read cursor in a channel
%% Args:     Token, Channel ID, Timestamp of the most recently seen message
%% Returns:  A list of {Status, Body}
%%           or {error, Reason}
%%----------------------------------------------------------------------
channels_mark(Token, Channel, Timestamp) ->
    slack_request(Token, Channel, Timestamp, "channels.mark").

%%----------------------------------------------------------------------
%% Function: channels_list/1
%% Purpose:  List of all channels in the team.
%% Args:     Token is your token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason}
%%----------------------------------------------------------------------
channels_list(Token) ->
    slack_request(Token, "channels.list").

%%----------------------------------------------------------------------
%% Function: files_upload/1
%% Purpose:  Upload or create a file
%% Args:     Token is your token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason}
%%----------------------------------------------------------------------
files_upload(Token) ->
    erlang:error(not_implemented_error).

%%----------------------------------------------------------------------
%% Function: files_list/1
%% Purpose:  List & filter team files
%% Args:     Token is your token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason}
%%----------------------------------------------------------------------
files_list(Token) ->
    slack_request(Token, "files.list").

%%----------------------------------------------------------------------
%% Function: im_history/1
%% Purpose:  Fetch history of messages and events from a given direct message channel
%% Args:     Token, Channel ID
%% Returns:  A list of {Status, Body}
%%           or {error, Reason}
%%----------------------------------------------------------------------
im_history(Token, Channel) ->
    slack_request(Token, Channel, "im.history").

%%----------------------------------------------------------------------
%% Function: im_list/1
%% Purpose:  List of im channels the user has.
%% Args:     Token is your token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason}
%%----------------------------------------------------------------------
im_list(Token) ->
    slack_request(Token, "im.list").

%%----------------------------------------------------------------------
%% Function: groups_history/1
%% Purpose:  Fetch history of messages and events from a given private group
%% Args:     Token, Channel ID
%% Returns:  A list of {Status, Body}
%%           or {error, Reason}
%%----------------------------------------------------------------------
groups_history(Token, Channel) ->
    slack_request(Token, Channel, "groups.history").

%%----------------------------------------------------------------------
%% Function: groups_list/1
%% Purpose:  List of groups in the team that the calling user has access to.
%% Args:     Token is your token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason}
%%----------------------------------------------------------------------
groups_list(Token) ->
    slack_request(Token, "groups.list").

%%----------------------------------------------------------------------
%% Function: search_all/1
%% Purpose:  Search for messages and files matching a query
%% Args:     Token is your token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason}
%%----------------------------------------------------------------------
search_all(Token) ->
    slack_request(Token, "search.all").

%%----------------------------------------------------------------------
%% Function: search_files/1
%% Purpose:  Search for files matching a query
%% Args:     Token is your token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason}
%%----------------------------------------------------------------------
search_files(Token) ->
    slack_request(Token, "search.files").

%%----------------------------------------------------------------------
%% Function: search_messages/1
%% Purpose:  Search for messages matching a query
%% Args:     Token is your token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason}
%%----------------------------------------------------------------------
search_messages(Token) ->
    slack_request(Token, "search.messages").

%%----------------------------------------------------------------------
%% Function: post_message/1
%% Purpose:  Post a message
%% Args:     Token is your token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason}
%%----------------------------------------------------------------------
post_message(Token) ->
    slack_request(Token, "chat.postMessage").

%% Internals

slack_request(Token, Endpoint) ->
    Base_URL = "https://slack.com/api/",
    URL = restc:construct_url(Base_URL, Endpoint, [{"token", Token}]),
    restc:request(get, URL).

slack_request(Token, Channel, Endpoint) ->
    Base_URL = "https://slack.com/api/",
    URL = restc:construct_url(Base_URL, Endpoint, [{"token", Token},{"channel", Channel}]),
    restc:request(get, URL).

slack_request(Token, Channel, Timestamp, Endpoint) ->
    Base_URL = "https://slack.com/api/",
    URL = restc:construct_url(Base_URL, Endpoint, [{"token", Token},{"channel", Channel},{"ts", Timestamp}]),
    restc:request(get, URL).

ok() ->
    ok.
