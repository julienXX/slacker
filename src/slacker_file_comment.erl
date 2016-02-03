-module(slacker_file_comment).

-include("spec.hrl").

-export([add/3, delete/3, edit/4]).


%% @doc Add a comment to an existing file.
-spec add(Token :: string(), File :: string(), Comment :: string()) -> http_response().
add(Token, File, Comment) ->
    slacker_request:send("files.comments.add", [{"token", Token},{"file", File},{"comment", Comment}]).

%% @doc Delete an existing comment on a file.
-spec delete(Token :: string(), File :: string(), CommentId :: string()) -> http_response().
delete(Token, File, CommentId) ->
    slacker_request:send("files.comments.delete", [{"token", Token},{"file", File},{"id", CommentId}]).

%% @doc Edit an existing comment on a file.
-spec edit(Token :: string(), File :: string(), CommentId :: string(), Comment :: string()) -> http_response().
edit(Token, File, CommentId, Comment) ->
    slacker_request:send("files.comments.edit", [{"token", Token},{"file", File},{"id", CommentId},{"comment", Comment}]).
