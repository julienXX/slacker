slacker
=======

Erlang [Slack](http://slack.com) REST API wrapper.

To use slacker, you need a bearer token; see the [Slack documentation](https://api.slack.com/web#authentication) for details.
 
Warning: a token is equivalent to a username (for a given team) plus a password. Manage and store it securely.

## Installation

Slacker is built with [rebar3](https://www.rebar3.org/). We provide a `make` wrapper for convenience.

To build the application simply run `make all`. This will build the .beam, .app
files and the documentation.

To generate the documentation, run `make doc`.

Or add it to your `rebar.config`

```
{deps, [
    ....
    {slacker, {git, "git://github.com/julienXX/slacker.git", {branch, "master"}}}
]}.
```

Note that the way dependencies are handled has changed from rebar2 to rebar3. Read [rebar3 Upgrading dependencies](https://www.rebar3.org/docs/dependencies#section-upgrading-dependencies) for details.

## Testing

The tests need connectivity to slack.com and the majority of them also need a bearer token.

Get a token following the instructions at the beginning of this document, then create file `priv/token.txt` containing your token. Use `priv/token.txt.example` as a template.

To run the tests use `make test`.

## Quick start
```shell
λ rebar shell
```
```
Eshell V8.2  (abort with ^G)
1> application:ensure_all_started(slacker).
2> Token = "your team token".
2> Options = [].
4> {ok, Status, Headers, Body} = slacker_user:list(Token, Options).
```

## Message formatting

Slack messages are JSON structures that follow a specific [formatting](https://api.slack.com/docs/attachments). After creating a message you pass it
to `slacker_chat:post_rich_message/5` as an `attachment` parameter.

Simple message with a bot icon and a colored left border:
```erlang
Msg = slacker_rich_messages:format(<<"Hello">>, <<"Hello World!">>, <<"#df4f18">>).
```

Simple message with a table, a bot icon and a colored left border:
```erlang
Fields = [[{title, "Col"}, {value, 1}]].
Msg = slacker_rich_messages:format_table(<<"Hello">>, <<"Hello World!">>, Fields, <<"#df4f18">>).
```

## TODO
- files.upload API endpoint is not implemented

## Contribute
- Fork slacker
- Write some new features or fix bug
- Test it
- Pull request

## LICENSE
License in LICENSE file

## More info
More info at julien@sideburns.eu
