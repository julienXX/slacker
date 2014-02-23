-module(slacker_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


test_unauth_get() ->
    Token = read_token(),
    {_, _, _, Body} = slacker:get_users(Token),
    ?assertEqual(error, Body).


%%% Internal functionality

read_token() ->
    case file:consult("../token.txt") of
        {ok,[Keys]} -> Keys;
        _ -> throw("Unable to read token from token.txt file!")
    end.
