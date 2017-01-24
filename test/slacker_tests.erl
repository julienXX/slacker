-module(slacker_tests).
-export([]).
-include_lib("eunit/include/eunit.hrl").

slacker_test_() ->
    {setup,
     fun() -> application:ensure_all_started(slacker) end,
     fun(_) -> slacker:stop() end,
     [
      {timeout, 100, {"Slack itself is up and running",
                      fun test_slack_is_up_and_running/0}},
      {timeout, 100, {"Unauthorized retrieve call", fun test_unauth_get/0}},
      {timeout, 100, {"Retrieve users", fun test_users_list/0}}
     ]
    }.


% This should be the first test. It uses the lower-level slacker API and the
% Slack test API, so that we don't need an API token and get an error closer
% to the source of the problem. If this one fails, everything else using the
% network will fail.
test_slack_is_up_and_running() ->
    {_Ok, _Status, _Headers, Body} = slacker_request:send("api.test", []),
    ?assertEqual(true, get_val(<<"ok">>, Body)).

test_unauth_get() ->
    Token = "bad_token",
    {_Ok, _Status, _Headers, Body} = slacker_user:list(Token, []),
    ?assertEqual(false, get_val(<<"ok">>, Body)),
    ?assertEqual(<<"invalid_auth">>, get_val(<<"error">>, Body)).


test_users_list() ->
    {ok, Token} = read_token(),
    {_Ok, _Status, _Headers, Body} = slacker_user:list(Token, []),
    ?assertEqual(true, get_val(<<"ok">>, Body)).

%%% Simple tests

user_get_presence_test() ->
    slacker:start(),
    {ok, Token} = read_token(),
    {_, _, _, Body} = slacker_user:get_presence(Token, "ipinak"),
    ?assertEqual(false, get_val(<<"ok">>, Body)).

%%% Internal functionality

read_token() ->
    case file:consult("../token.txt") of
        {ok, [Token]} -> {ok, Token};
        Error -> {error, {cannot_read_token, Error}}
    end.

get_val(Key, PL) ->
    proplists:get_value(Key, PL).
