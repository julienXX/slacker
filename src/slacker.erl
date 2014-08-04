%%%----------------------------------------------------------------------------
%%% @author Julien Blanchard <julien@sideburns.eu>
%%% @doc
%%% Erlang library for Slack API - http://api.slack.com/
%%% @end
%%%----------------------------------------------------------------------------

-module(slacker).

-export([start/0, stop/0]).
-export([auth_test/1,
         users_list/1,
         channels_history/2, channels_mark/3, channels_list/1, channels_info/2,
         files_upload/1, files_list/1, files_info/2,
         im_history/2, im_list/1,
         groups_history/2, groups_list/1,
         search_all/2, search_files/2, search_messages/2,
         post_message/3,
         stars_list/1,
         emoji_list/1]).

-type headers() :: list({string(), any()}).
-type json_term() :: list({binary(), json_term()})
    | list(json_term())
    | true
    | false
    | null
    | integer()
    | float()
    | binary().
-type http_response() :: {ok, Status :: integer(), Headers :: headers(), Body :: json_term()}.

-define(DEPS, [crypto, asn1, public_key, ssl, inets, slacker]).
-define(API_URL, "https://slack.com/api/").

-spec start() -> ok.
start() ->
    [application:start(A) || A <- ?DEPS],
    ok.

-spec stop() -> ok.
stop() ->
    [application:stop(A) || A <- ?DEPS],
    ok.

%%% Slack API

%% @doc Checks authentication and tells you who you are
-spec auth_test(Token :: string()) -> http_response().
auth_test(Token) ->
    slack_request("auth.test", [{"token", Token}]).

%% @doc List all users in the team
-spec users_list(Token :: string()) -> http_response().
users_list(Token) ->
    slack_request("users.list", [{"token", Token}]).

%% @doc Fetch history of messages and events from a given channel
-spec channels_history(Token :: string(), Channel :: string()) -> http_response().
channels_history(Token, Channel) ->
    slack_request("channels.history", [{"token", Token},{"channel", Channel}]).

%% @doc Set read cursor in a channel
-spec channels_mark(Token :: string(), Channel :: string(), Timestamp :: integer()) -> http_response().
channels_mark(Token, Channel, Timestamp) ->
    slack_request("channels.mark", [{"token", Token},{"channel", Channel},{"ts", Timestamp}]).

%% @doc List of all channels in the team.
-spec channels_list(Token :: string()) -> http_response().
channels_list(Token) ->
    slack_request("channels.list", [{"token", Token}]).

%% @doc Returns information about a team channel
-spec channels_info(Token :: string(), Channel :: string()) -> http_response().
channels_info(Token, Channel) ->
    slack_request("channels.info", [{"token", Token},{"channel", Channel}]).

%% @doc Upload or create a file
-spec files_upload(Token :: string()) -> http_response().
files_upload(Token) ->
    erlang:error(not_implemented_error).

%% @doc List & filter team files
-spec files_list(Token :: string()) -> http_response().
files_list(Token) ->
    slack_request("files.list", [{"token", Token}]).

%% @doc Returns information about a file in your team
-spec files_info(Token :: string(), File :: string()) -> http_response().
files_info(Token, File) ->
    slack_request("files.info", [{"token", Token},{"file", File}]).

%% @doc Fetch history of messages and events from a given direct message channel
-spec im_history(Token :: string(), Channel :: string()) -> http_response().
im_history(Token, Channel) ->
    slack_request("im.history", [{"token", Token},{"channel", Channel}]).

%% @doc List of im channels the user has.
-spec im_list(Token :: string()) -> http_response().
im_list(Token) ->
    slack_request("im.list", [{"token", Token}]).

%% @doc Fetch history of messages and events from a given private group
-spec groups_history(Token :: string(), Channel :: string()) -> http_response().
groups_history(Token, Channel) ->
    slack_request("groups.history", [{"token", Token},{"channel", Channel}]).

%% @doc List of groups in the team that the calling user has access to.
-spec groups_list(Token :: string()) -> http_response().
groups_list(Token) ->
    slack_request("groups.list", [{"token", Token}]).

%% @doc Search for messages and files matching a query
-spec search_all(Token :: string(), Query :: string()) -> http_response().
search_all(Token, Query) ->
    slack_request("search.all", [{"token", Token},{"query", Query}]).

%% @doc Search for files matching a query
-spec search_files(Token :: string(), Query :: string()) -> http_response().
search_files(Token, Query) ->
    slack_request("search.files", [{"token", Token},{"query", Query}]).

%% @doc Search for messages matching a query
-spec search_messages(Token :: string(), Query :: string()) -> http_response().
search_messages(Token, Query) ->
    slack_request("search.messages", [{"token", Token},{"query", Query}]).

%% @doc Post a message
-spec post_message(Token :: string(), Channel :: string(), Message :: string()) -> http_response().
post_message(Token, Channel, Message) ->
    slack_request("chat.postMessage", [{"token", Token},{"channel", Channel},{"text", Message}]).

%% @doc Lists the items starred by a user
-spec stars_list(Token :: string()) -> http_response().
stars_list(Token) ->
    slack_request("stars.list", [{"token", Token}]).

%% @doc Lists the custom emoji for a team
-spec emoji_list(Token :: string()) -> http_response().
emoji_list(Token) ->
    slack_request("emoji.list", [{"token", Token}]).


%%% Internals

%% @doc Sends a request to Slack API
slack_request(Endpoint, Params) ->
    URL = restc:construct_url(?API_URL, Endpoint, Params),
    restc:request(get, URL).
