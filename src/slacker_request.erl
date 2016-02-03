-module(slacker_request).

-export([send/2, send/3]).

-define(API_URL, "https://slack.com/api/").


%% @doc Sends a request to Slack API
send(Endpoint, Params) ->
    send(Endpoint, Params, []).

%% @doc Sends a request with optional parameters to Slack API
send(Endpoint, Params, Options) ->
    URL = restc:construct_url(?API_URL, Endpoint, lists:append(Params, Options)),
    restc:request(get, URL).
