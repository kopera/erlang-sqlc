%% @private
-module(sqlc_scanner).
-export([
    string/1,
    format_error/1
]).

-record(state, {
    line = 1,
    column = 1,
    token_start = {1, 1},
    tokens = []
}).

-define(is_whitespace(C), (is_integer(C) andalso (
           C =:= $\s
    orelse C =:= $\t
    orelse C =:= $\n
    orelse C =:= $\r
))).

-define(is_identifier_start(C), (is_integer(C) andalso (C >= $a andalso C =< $z))).
-define(is_identifier_cont(C), (?is_identifier_start(C) orelse (C =:= $_))).


-spec string(string()) -> {ok, [token()]} | {error, error_info()}.
-type token() :: {token_category(), location(), string()}.
-type token_category() ::
    '(' | ')' | '[' | ']' | '?' | ':' | ';' | query | mutation | returns | as | identifier | parameter | fragment.
-type location() :: {Line :: pos_integer(), Column :: pos_integer()}.
-type error() :: {location(), sqlc_scanner, error_info()}.
-type error_info() ::
      {invalid_character, char()}
    | {incomplete_statement, Start :: location()}
    | invalid_indentation.
string(Input) ->
    case scan(Input, #state{}) of
        {ok, #state{tokens = Tokens}} ->
            {ok, Tokens};
        {error, _} = Error ->
            Error
    end.


-spec format_error(error_info()) -> string().
format_error({invalid_character, Character}) ->
    lists:flatten(io_lib:format("illegal character ~ts", [[Character]]));
format_error({incomplete_statement, {Line, Column}}) ->
    lists:flatten(io_lib:format("incomplete SQL statement started at line: ~B column: ~B, did you forget a `;`?", [Line, Column]));
format_error(invalid_indentation) ->
    "bad indentation in SQL statement".


%% @private
scan([C | _] = Input, State) when ?is_whitespace(C) ->
    skip_whitespace(Input, State, fun scan/2);
scan([$( | Rest], State) ->
    scan(Rest, token_end('(', "(", column_inc(token_start(State))));
scan([$) | Rest], State) ->
    scan(Rest, token_end(')', ")", column_inc(token_start(State))));
scan([$[ | Rest], State) ->
    scan(Rest, token_end('[', "[", column_inc(token_start(State))));
scan([$] | Rest], State) ->
    scan(Rest, token_end(']', "]", column_inc(token_start(State))));
scan([$? | Rest], State) ->
    scan(Rest, token_end('?', "?", column_inc(token_start(State))));
scan([$, | Rest], State) ->
    scan(Rest, token_end(',', ",", column_inc(token_start(State))));
scan([$: | Rest], State) ->
    scan_parameter(Rest, [], column_inc(token_start(State)), fun scan/2);
scan([$-, $- | Rest], State) ->
    skip_line_comment(Rest, columns_inc(2, token_start(State)));
scan([C | Rest], State) when ?is_identifier_start(C) ->
    scan_identifier(Rest, [C], column_inc(token_start(State)));
scan([C | _Rest], State) ->
    {error, make_error({invalid_character, C}, State)};
scan([], #state{tokens = Tokens} = State) ->
    {ok, State#state{tokens = lists:reverse(Tokens)}}.


%% @private
-spec scan_parameter(Input, Acc, State, Cont) -> Result when
    Input :: string(),
    Acc :: string(),
    State :: #state{},
    Cont :: fun((Input, State) -> Result),
    Result :: {ok, [token()]} | {error, error()}.
scan_parameter([C | Rest], [], State, Cont) when ?is_identifier_start(C) ->
    scan_parameter(Rest, [C], column_inc(State), Cont);
scan_parameter([C | Rest], Acc, State, Cont) when ?is_identifier_cont(C) ->
    scan_parameter(Rest, [C | Acc], column_inc(State), Cont);
scan_parameter([C | _Rest], [], State, _Cont) ->
    {error, make_error({invalid_character, C}, State)};
scan_parameter(Rest, Acc, State, Cont) ->
    Cont(Rest, token_end(parameter, lists:reverse(Acc), State)).


%% @private
-spec scan_identifier(Input, Acc, State) -> Result when
    Input :: string(),
    Acc :: string(),
    State :: #state{},
    Result :: {ok, [token()]} | {error, error()}.
scan_identifier([C | Rest], Acc, State) when ?is_identifier_cont(C) ->
    scan_identifier(Rest, [C | Acc], column_inc(State));
scan_identifier(Rest, Acc, State) ->
    case lists:reverse(Acc) of
        "query" = Symbol -> scan(Rest, token_end(query, Symbol, State));
        "mutation" = Symbol -> scan(Rest, token_end(mutation, Symbol, State));
        "returns" = Symbol -> scan(Rest, token_end(returns, Symbol, State));
        "as" = Symbol -> scan_statement(Rest, token_start(token_end(as, Symbol, State)));
        Symbol -> scan(Rest, token_end(identifier, Symbol, State))
    end.

%% @private
scan_statement([C | Rest], State) when C =:= $\s; C =:= $\t ->
    scan_statement(Rest, column_inc(State));
scan_statement([$\r, $\n | Rest], State) ->
    {Indent, _} = string:take(Rest, [$\t, $\s]),
    scan_statement_line_start(Indent, Rest, [], token_start(string:length(Indent), line_inc(State)), fun scan_statement_line/4);
scan_statement([$\n | Rest], State) ->
    {Indent, _} = string:take(Rest, [$\t, $\s]),
    scan_statement_line_start(Indent, Rest, [], token_start(string:length(Indent), line_inc(State)), fun scan_statement_line/4);
scan_statement([C | _Rest], State) ->
    {error, make_error({invalid_character, C}, State)};
scan_statement([], #state{token_start = Start} = State) ->
    {error, make_error({incomplete_statement, Start}, State)}.


%% @private
scan_statement_line_start(Indent, [$\r, $\n | Rest], Acc, State, Cont) -> % Unindented empty line
    scan_statement_line_start(Indent, Rest, [$\r, $\n | Acc], line_inc(State), Cont);
scan_statement_line_start(Indent, [$\n | Rest], Acc, State, Cont) -> % Unindented empty line
    scan_statement_line_start(Indent, Rest, [$\n | Acc], line_inc(State), Cont);
scan_statement_line_start(Indent, Input, Acc, State, Cont) ->
    case string:prefix(Input, Indent) of
        nomatch ->
            {error, make_error(invalid_indentation, State)};
        Rest ->
            Cont(Indent, Rest, Acc, columns_inc(string:length(Indent), State))
    end.


%% @private
scan_statement_line(Indent, [$\r, $\n | Rest], Acc, State) ->
    scan_statement_line_start(Indent, Rest, [$\r, $\n | Acc], line_inc(State), fun scan_statement_line/4);
scan_statement_line(Indent, [$\n | Rest], Acc, State) ->
    scan_statement_line_start(Indent, Rest, [$\n | Acc], line_inc(State), fun scan_statement_line/4);
scan_statement_line(Indent, [$-, $- | Rest], Acc, State) ->
    scan_statement_line_comment(Indent, Rest, [$-, $- | Acc], columns_inc(2, State));
scan_statement_line(_Indent, [$; | Rest], Acc, State) ->
    Symbol = lists:reverse(Acc),
    scan(Rest, token_end(';', ";", token_start(token_end(fragment, Symbol, column_inc(State)))));
scan_statement_line(Indent, [$' | Rest], Acc, State) ->
    scan_statement_quoted(Indent, $', Rest, [$' | Acc], column_inc(State));
scan_statement_line(Indent, [$" | Rest], Acc, State) ->
    scan_statement_quoted(Indent, $", Rest, [$" | Acc], column_inc(State));
scan_statement_line(Indent, [$:, C | Rest], Acc, State) when ?is_identifier_start(C) ->
    Symbol = lists:reverse(Acc),
    scan_parameter([C | Rest], [], column_inc(token_start(token_end(fragment, Symbol, State))), fun (Rest1, State1) ->
        scan_statement_line(Indent, Rest1, [], token_start(State1))
    end);
scan_statement_line(Indent, [C | Rest], Acc, State) ->
    scan_statement_line(Indent, Rest, [C | Acc], column_inc(State));
scan_statement_line(_Indent, [], _Acc, #state{token_start = Start} = State) ->
    {error, make_error({incomplete_statement, Start}, State)}.


%% @private
scan_statement_line_comment(Indent, [$\r, $\n | Rest], Acc, State) ->
    scan_statement_line(Indent, Rest, [$\r, $\n | Acc], line_inc(State));
scan_statement_line_comment(Indent, [$\n | Rest], Acc, State) ->
    scan_statement_line(Indent, Rest, [$\n | Acc], line_inc(State));
scan_statement_line_comment(Indent, [C | Rest], Acc, State) ->
    scan_statement_line_comment(Indent, Rest, [C | Acc], column_inc(State));
scan_statement_line_comment(Indent, [], Acc, State) ->
    scan_statement_line(Indent, [], Acc, column_inc(State)).


%% @private
scan_statement_quoted(Indent, Q, [$\r, $\n | Rest], Acc, State) ->
    scan_statement_line_start(Indent, Rest, [$\r, $\n | Acc], line_inc(State), fun (_Indent, Rest1, Acc1, State1) ->
        scan_statement_quoted(Indent, Q, Rest1, Acc1, State1)
    end);
scan_statement_quoted(Indent, Q, [$\n | Rest], Acc, State) ->
    scan_statement_line_start(Indent, Rest, [$\n | Acc], line_inc(State), fun (_Indent, Rest1, Acc1, State1) ->
        scan_statement_quoted(Indent, Q, Rest1, Acc1, State1)
    end);
scan_statement_quoted(Indent, Q, [Q, Q | Rest], Acc, State) ->
    scan_statement_quoted(Indent, Q, Rest, [Q, Q | Acc], columns_inc(2, State));
scan_statement_quoted(Indent, Q, [Q | Rest], Acc, State) ->
    scan_statement_line(Indent, Rest, [Q | Acc], column_inc(State));
scan_statement_quoted(Indent, Q, [C | Rest], Acc, State) ->
    scan_statement_quoted(Indent, Q, Rest, [C | Acc], column_inc(State));
scan_statement_quoted(Indent, _, [], Acc, State) ->
    scan_statement_line(Indent, [], Acc, State).


%% @private
skip_whitespace([$\n | Rest], State, Cont) ->
    skip_whitespace(Rest, line_inc(State), Cont);
skip_whitespace([C | Rest], State, Cont) when ?is_whitespace(C) ->
    skip_whitespace(Rest, column_inc(State), Cont);
skip_whitespace(Rest, State, Cont) ->
    Cont(Rest, State).


%% @private
skip_line_comment([$\n | Rest], State) ->
    scan(Rest, line_inc(State));
skip_line_comment([_ | Rest], State) ->
    skip_line_comment(Rest, column_inc(State));
skip_line_comment([] = Rest, State) ->
    scan(Rest, State).


%% @private
make_error(Error, _State = #state{line = Line, column = Column, tokens = _Tokens}) ->
    {{Line, Column}, ?MODULE, Error}.


%% @private
token_start(State) ->
    token_start(0, State).

%% @private
token_start(ColumnAdj, State = #state{line = Line, column = Column}) ->
    State#state{token_start = {Line, Column + ColumnAdj}}.


%% @private
token_end(fragment, [], State) ->
    State;
token_end(Category, Symbol, State = #state{token_start = Start, tokens = Tokens}) ->
    Token = {Category, Start, Symbol},
    State#state{tokens = [Token | Tokens]}.


%% @private
line_inc(State = #state{line = Line}) ->
    State#state{line = Line + 1, column = 1}.


%% @private
column_inc(State) ->
    columns_inc(1, State).

%% @private
columns_inc(Add, State = #state{column = Column}) when is_integer(Add), Add > 0 ->
    State#state{column = Column + Add}.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

test_data() ->
    "
        query test() returns user as
            -- ignored
            -- ignore quoted '
            -- ignore quoted \"
            -- ignore parameter :test
            select true;
    ".

string_test_() ->
    [
        ?_assertMatch({ok, [
            {query, _, "query"},
            {identifier, _,"test"},
            {'(', _,"("},
            {')', _,")"},
            {returns, _,"returns"},
            {identifier, _,"user"},
            {as, _,"as"},
            {fragment, _, "-- ignored\n            -- ignore quoted '\n            -- ignore quoted \"\n            -- ignore parameter :test\n            select true"},
            {';', _,";"}
        ]}, string(test_data()))
    ].


-endif.
