-module(slacker_tests).
-export([]).
-include_lib("eunit/include/eunit.hrl").


slacker_test_() ->
    {setup,
     fun() -> slacker:start() end,
     fun(_) -> slacker:stop() end,
     [
      {timeout, 100, {"Unauthorized retrieve call", fun unauth_get_test/0}},
      {timeout, 100, {"Retrieve users", fun users_list_test/0}}
     ]
    }.


unauth_get_test() ->
    slacker:start(),
    Token = "bad_token",
    {_Ok, _Status, _Headers, Body} = slacker_user:list(Token),
    ?assertEqual(false, get_val(<<"ok">>, Body)),
    ?assertEqual(<<"invalid_auth">>, get_val(<<"error">>, Body)).


users_list_test() ->
    slacker:start(),
    Token = read_token(),
    {_Ok, _Status, _Headers, Body} = slacker_user:list(Token),
    ?assertEqual(true, get_val(<<"ok">>, Body)).

application_start_works_test() ->
    slacker:start(),
    Token = read_token(),
    {_, _, _, Body} = slacker_user:get_presence(Token, "ipinak"),
    ?assertEqual(false, get_val(<<"ok">>, Body)).

%%% Internal functionality

read_token() ->
    case file:consult("../token.txt") of
        {ok,[Token]} -> Token;
        Error -> throw(Error)
    end.

get_val(Key, PL) ->
    proplists:get_value(Key, PL).
