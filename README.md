slacker
=======

Erlang [Slack](http://slack.com) REST API wrapper.

To use slacker, you need a team token that you can grab [here](https://api.slack.com/#auth).

## Installation

To build the application simply run 'make all'. This should build .beam, .app
files and documentation.

To run tests run 'make test'.
To generate doc, run 'make doc'.

Or add it to your rebar config

```
{deps, [
    ....
    {slacker, ".*", {git, "git://github.com/julienXX/slacker.git", {branch, "master"}}}
]}.
```
## Quick start
```shell
λ rebar shell
```
```
Eshell V6.2  (abort with ^G)
1> slacker:start().
2> Token = "your team token".
3> {Ok, Status, Headers, Body} = slacker_user:list(Token).
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
