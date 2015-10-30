-module(slacker_request).

-export([send/2, send_post/4]).

-define(API_URL, "https://slack.com/api/").


%% @doc Sends a request to Slack API
send(Endpoint, Params) ->
    URL = restc:construct_url(?API_URL, Endpoint, Params),
    restc:request(get, URL).

send_post(Endpoint, Params, Headers, Data) ->
    URL = restc:construct_url(?API_URL, Endpoint, Params),
    restc:request(post, json, URL, [], Headers, Data).
