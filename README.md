slacker
=======

Erlang [Slack](http://slack.com) REST API wrapper.

To use slacker, you need a team token that you can grab [here](https://api.slack.com/#auth).

## Fetch dependencies
```shell
    $ rebar get-deps
```
## Compile
```shell
    $ rebar compile
```
or
```shell
    $ make all
```
## Quick start
```shell
    $ erl -pa ebin deps/*/ebin
    $ Token = "your team token".
    $ slacker:init().
```
## TODO
- files.upload API endpoint is not implemented
- add specs

## Contribute
- Fork slacker
- Write some new features or fix bug
- Test it
- Pull request

## LICENSE
License in LICENSE file

## More info
More info at julien@sideburns.eu
