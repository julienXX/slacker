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
```
```
Eshell V5.10.4  (abort with ^G)
1> inets:start().
2> ssl:start().
3> Token = "your team token".
4> {Ok, Status, Headers, Body} = slacker:users_list(Token).
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
