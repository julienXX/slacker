-type headers() :: list({string(), any()}).
-type json_term() :: list({binary(), json_term()})
    | list(json_term())
    | true
    | false
    | null
    | integer()
    | float()
    | binary().
-type http_response() :: {ok, Status :: integer(), Headers :: headers(), Body :: json_term()}.

-type field_title() :: atom() | title | value.
-type field()       :: {field_title(), binary()}.
-type fields()      :: list(list(field())).
