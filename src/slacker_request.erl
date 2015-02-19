-module(slacker_request).

-export([send/2]).

-define(API_URL, "https://slack.com/api/").


%% @doc Sends a request to Slack API
send(Endpoint, Params) ->
    URL = restc:construct_url(?API_URL, Endpoint, Params),
    error_logger:info_msg("restc: url=~s", [URL]),
    restc:request(get, URL).
