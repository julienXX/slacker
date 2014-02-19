-module(slacker).

-export([
         init/0,
         users_list/1,
         channels_history/2, channels_mark/3, channels_list/1,
         files_upload/1, files_list/1,
         im_history/2, im_list/1,
         groups_history/2, groups_list/1,
         search_all/2, search_files/2, search_messages/2,
         post_message/3
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
%% Args:     Token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason}
%%----------------------------------------------------------------------
users_list(Token) ->
    slack_request("users.list", [{"token", Token}]).

%%----------------------------------------------------------------------
%% Function: channels_history/2
%% Purpose:  Fetch history of messages and events from a given channel
%% Args:     Token, Channel ID
%% Returns:  A list of {Status, Body}
%%           or {error, Reason}
%%----------------------------------------------------------------------
channels_history(Token, Channel) ->
    slack_request("channels.history", [{"token", Token},{"channel", Channel}]).

%%----------------------------------------------------------------------
%% Function: channels_mark/3
%% Purpose:  Set read cursor in a channel
%% Args:     Token, Channel ID, Timestamp of the most recently seen message
%% Returns:  A list of {Status, Body}
%%           or {error, Reason}
%%----------------------------------------------------------------------
channels_mark(Token, Channel, Timestamp) ->
    slack_request("channels.mark", [{"token", Token},{"channel", Channel},{"ts", Timestamp}]).

%%----------------------------------------------------------------------
%% Function: channels_list/1
%% Purpose:  List of all channels in the team.
%% Args:     Token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason}
%%----------------------------------------------------------------------
channels_list(Token) ->
    slack_request("channels.list", [{"token", Token}]).

%%----------------------------------------------------------------------
%% Function: files_upload/1
%% Purpose:  Upload or create a file
%% Args:     Token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason}
%%----------------------------------------------------------------------
files_upload(Token) ->
    erlang:error(not_implemented_error).

%%----------------------------------------------------------------------
%% Function: files_list/1
%% Purpose:  List & filter team files
%% Args:     Token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason}
%%----------------------------------------------------------------------
files_list(Token) ->
    slack_request("files.list", [{"token", Token}]).

%%----------------------------------------------------------------------
%% Function: im_history/2
%% Purpose:  Fetch history of messages and events from a given direct message channel
%% Args:     Token, Channel ID
%% Returns:  A list of {Status, Body}
%%           or {error, Reason}
%%----------------------------------------------------------------------
im_history(Token, Channel) ->
    slack_request("im.history", [{"token", Token},{"channel", Channel}]).

%%----------------------------------------------------------------------
%% Function: im_list/1
%% Purpose:  List of im channels the user has.
%% Args:     Token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason}
%%----------------------------------------------------------------------
im_list(Token) ->
    slack_request("im.list", [{"token", Token}]).

%%----------------------------------------------------------------------
%% Function: groups_history/2
%% Purpose:  Fetch history of messages and events from a given private group
%% Args:     Token, Channel ID
%% Returns:  A list of {Status, Body}
%%           or {error, Reason}
%%----------------------------------------------------------------------
groups_history(Token, Channel) ->
    slack_request("groups.history", [{"token", Token},{"channel", Channel}]).

%%----------------------------------------------------------------------
%% Function: groups_list/1
%% Purpose:  List of groups in the team that the calling user has access to.
%% Args:     Token
%% Returns:  A list of {Status, Body}
%%           or {error, Reason}
%%----------------------------------------------------------------------
groups_list(Token) ->
    slack_request("groups.list", [{"token", Token}]).

%%----------------------------------------------------------------------
%% Function: search_all/2
%% Purpose:  Search for messages and files matching a query
%% Args:     Token, Query
%% Returns:  A list of {Status, Body}
%%           or {error, Reason}
%%----------------------------------------------------------------------
search_all(Token, Query) ->
    slack_request("search.all", [{"token", Token},{"query", Query}]).

%%----------------------------------------------------------------------
%% Function: search_files/2
%% Purpose:  Search for files matching a query
%% Args:     Token, Query
%% Returns:  A list of {Status, Body}
%%           or {error, Reason}
%%----------------------------------------------------------------------
search_files(Token, Query) ->
    slack_request("search.files", [{"token", Token},{"query", Query}]).

%%----------------------------------------------------------------------
%% Function: search_messages/2
%% Purpose:  Search for messages matching a query
%% Args:     Token, Query
%% Returns:  A list of {Status, Body}
%%           or {error, Reason}
%%----------------------------------------------------------------------
search_messages(Token, Query) ->
    slack_request("search.messages", [{"token", Token},{"query", Query}]).

%%----------------------------------------------------------------------
%% Function: post_message/3
%% Purpose:  Post a message
%% Args:     Token, Channel ID, Message to post
%% Returns:  A list of {Status, Body}
%%           or {error, Reason}
%%----------------------------------------------------------------------
post_message(Token, Channel, Message) ->
    slack_request("chat.postMessage", [{"token", Token},{"channel", Channel},{"text", Message}]).


%% Internals

slack_request(Endpoint, Params) ->
    Base_URL = "https://slack.com/api/",
    URL = restc:construct_url(Base_URL, Endpoint, Params),
    restc:request(get, URL).

ok() ->
    ok.
