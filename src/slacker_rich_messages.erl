-module(slacker_rich_messages).

-include("spec.hrl").

-export([format/3, format/4, format/5]).
-export([format_table/4, format_table/5]).

%% @doc Creates rich attachment message
-spec format(binary(), binary(), binary()) -> binary().
format(Title, Message, Color) ->
    to_json([{<<"fallback">>, Message},
             {<<"title">>,    Title},
             {<<"text">>,     Message},
             {<<"color">>,    Color}]).

%% @doc Creates rich attachment message with icon
-spec format(binary(), binary(), binary(), binary()) -> binary().
format(Title, Message, Color, IconUrl) ->
    to_json([{<<"fallback">>, Message},
             {<<"title">>,    Title},
             {<<"text">>,     Message},
             {<<"icon_url">>, IconUrl},
             {<<"color">>,    Color}]).

%% @doc Creates rich attachment message with icon and image
-spec format(binary(), binary(), binary(), binary(), binary()) -> binary().
format(Title, Message, Color, URL, IconUrl) ->
    to_json([{<<"fallback">>,  Message},
             {<<"title">>,     Title},
             {<<"text">>,      Message},
             {<<"icon_url">>,  IconUrl},
             {<<"image_url">>, URL},
             {<<"color">>,     Color}]).

%% @doc Creates rich attachment message in table format
-spec format_table(binary(), binary(), fields(), binary()) -> binary().
format_table(Title, Message, Fields, Color) ->
    to_json([{<<"fallback">>, Message},
             {<<"title">>,    Title},
             {<<"text">>,     Message},
             {<<"fields">>,   lists:map(fun format_field/1, Fields)},
             {<<"color">>,    Color}]).

%% @doc Creates rich attachment message in table format with icon
-spec format_table(binary(), binary(), fields(), binary(), binary()) -> binary().
format_table(Title, Message, Fields, Color, IconUrl) ->
    to_json([{<<"fallback">>,  Message},
             {<<"title">>,     Title},
             {<<"text">>,      Message},
             {<<"fields">>,    lists:map(fun format_field/1, Fields)},
             {<<"icon_url">>,  IconUrl},
             {<<"color">>,     Color}]).


%%% Internal functionality

format_field(Field) ->
    [{<<"title">>, proplists:get_value(title, Field)},
     {<<"value">>, proplists:get_value(value, Field)},
     {<<"short">>, true}].

to_json(Struct) ->
    jsx:encode([Struct]).
