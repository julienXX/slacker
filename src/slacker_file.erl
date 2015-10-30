-module(slacker_file).

-include("spec.hrl").

-export([list/1, info/2]).
-export([upload/3, upload/4, upload/5, upload/6, upload/7, upload/8]).

-type csv() :: string().

%% @doc List & filter team files.
-spec list(Token :: string()) -> http_response().
list(Token) ->
    slacker_request:send("files.list", [{"token", Token}]).

%% @doc Returns information about a file in your team.
-spec info(Token :: string(), File :: string()) -> http_response().
info(Token, File) ->
    slacker_request:send("files.info", [{"token", Token},{"file", File}]).

%% @doc Upload of create a file. 
-spec upload(Token :: string(), File :: binary(), 
             Content :: binary()) -> http_response().
upload(Token, File, Content) ->
    slacker_request:send_post("files.upload", [{"token", Token}, 
                                               {"file", File}, 
                                               {"content", Content}]).

-spec upload(Token :: string(), File :: binary(), Content :: binary(), 
             FileType :: string()) -> http_response().
upload(Token, File, Content, FileType) ->
    slacker_request:send_post("files.upload", [{"token", Token}, 
                                               {"file", File}, 
                                               {"content", Content},
                                               {"filetype", FileType}]).

-spec upload(Token :: string(), File :: binary(), Content :: binary(), 
             FileType :: string(), Filename :: string()) -> http_response().
upload(Token, File, Content, FileType, Filename) ->
    slacker_request:send_post("files.upload", [{"token", Token}, 
                                               {"file", File}, 
                                               {"content", Content}, 
                                               {"filetype", FileType},
                                               {"filename", Filename}]).

-spec upload(Token :: string(), File :: binary(), Content :: binary(), 
             FileType :: string(), Filename :: string(), 
             Title :: string()) -> http_response().
upload(Token, File, Content, FileType, Filename, Title) ->
    slacker_request:send_post("files.upload", [{"token", Token}, 
                                               {"file", File}, 
                                               {"content", Content}, 
                                               {"filetype", FileType}, 
                                               {"filename", Filename}, 
                                               {"title", Title}]).

-spec upload(Token :: string(), File :: binary(), Content :: binary(), 
             FileType :: string(), Filename :: string(), Title :: string(), 
             InitComment :: string()) -> http_response().
upload(Token, File, Content, FileType, Filename, Title, InitComment) ->
    slacker_request:send_post("files.upload", [{"token", Token}, 
                                               {"file", File},
                                               {"content", Content}, 
                                               {"filetype", FileType}, 
                                               {"filename", Filename}, 
                                               {"title", Title}, 
                                               {"initial_comment", InitComment}]).

-spec upload(Token :: string(), File :: binary(), Content :: binary(), 
             FileType :: string(), Filename :: string(), Title :: string(), 
             InitComment :: string(), Channels :: csv()) -> http_response().
upload(Token, File, Content, FileType, Filename, Title, InitComment, Channels) ->
    slacker_request:send_post("files.upload", [{"token", Token}, 
                                               {"file", File}, 
                                               {"content", Content}, 
                                               {"filetype", FileType}, 
                                               {"filename", Filename}, 
                                               {"title", Title}, 
                                               {"initial_comment", InitComment}, 
                                               {"channels", Channels}]).

