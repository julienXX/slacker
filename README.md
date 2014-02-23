slacker
=======

[Slack](http://slack.com) API client in Erlang.

To use slacker, you need a team token that you can grab [here](https://api.slack.com/#auth).

## Fetch dependencies

    $ rebar get-deps

## Compile

    $ rebar compile

## Quick start

    $ erl -pa ebin deps/*/ebin
    $ Token = "your team token".
    $ slacker:init().

## TODO
- files.upload API endpoint is not implemented
- add specs
