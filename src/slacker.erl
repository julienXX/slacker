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

-type json_term() :: list({binary(), json_term()})
    | list(json_term())
    | true
    | false
    | null
    | integer()
    | float()
    | binary().
-type http_response() :: {ok, Data :: json_term(), Meta :: json_term()} | {error, Reason :: term()}.

-define(API_URL, "https://slack.com/api/").


%% API

init() ->
    application:start(inets),
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl).


%% Slack API

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

%% @doc Upload or create a file
-spec files_upload(Token :: string()) -> http_response().
files_upload(Token) ->
    erlang:error(not_implemented_error).

%% @doc List & filter team files
-spec files_list(Token :: string()) -> http_response().
files_list(Token) ->
    slack_request("files.list", [{"token", Token}]).

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


%% Internals

slack_request(Endpoint, Params) ->
    URL = restc:construct_url(?API_URL, Endpoint, Params),
    restc:request(get, URL).

ok() ->
    ok.
