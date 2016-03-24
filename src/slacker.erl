%%%----------------------------------------------------------------------------
%%% @author Julien Blanchard <julien@sideburns.eu>
%%% @doc
%%% Erlang library for Slack API - http://api.slack.com/
%%% @end
%%%----------------------------------------------------------------------------

-module(slacker).

-export([start/0, stop/0]).

-define(DEPS, [crypto, asn1, public_key, ssl, inets, idna, hackney, restc, slacker]).

-spec start() -> ok.
start() ->
    [application:start(A) || A <- ?DEPS],
    ok.

-spec stop() -> ok.
stop() ->
    [application:stop(A) || A <- ?DEPS],
    ok.
