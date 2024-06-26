Header "%% @private".

Nonterminals
    definitions
    definition
    request
    request_params
    request_param
    request_result
    request_statement
    identifier_chain
    type.

Terminals
    '(' ')'
    '[' ']'
    ',' ';' '.'
    query
    mutation
    returns
    as
    identifier
    parameter
    fragment.

Rootsymbol definitions.

% ==============================================================================
% Rules
% ==============================================================================

% Definitions ------------------------------------------------------------------

definitions -> definition definitions   : ['$1' | '$2'].
definitions -> '$empty'                 : [].

definition -> request                   : '$1'.

% Request ----------------------------------------------------------------------

request -> query identifier '(' request_params ')' returns request_result as request_statement ';' :
    {query, as_atom('$2'), #{
        parameters => '$4',
        returns => '$7',
        statement => '$9'
    }}.
request -> mutation identifier '(' request_params ')' returns request_result as request_statement ';' :
    {mutation, as_atom('$2'), #{
        parameters => '$4',
        returns => '$7',
        statement => '$9'
    }}.
request -> mutation identifier '(' request_params ')' as request_statement ';' :
    {mutation, as_atom('$2'), #{
        parameters => '$4',
        statement => '$7'
    }}.

% Request Parameters -----------------------------------------------------------

request_params -> request_param ',' request_params  : ['$1' | '$3'].
request_params -> request_param                     : ['$1'].
request_params -> '$empty'                          : [].

request_param -> parameter type                     : {as_atom('$1'), '$2'}.

% Request Result ---------------------------------------------------------------

request_result -> type                              : '$1'.


% Request Statement ------------------------------------------------------------

request_statement -> fragment request_statement     : [as_binary_string('$1') | '$2'].
request_statement -> parameter request_statement    : [{parameter, as_atom('$1')} | '$2'].
request_statement -> fragment                       : [as_binary_string('$1')].
request_statement -> parameter                      : [{parameter, as_atom('$1')}].

% Common -----------------------------------------------------------------------

type -> identifier_chain '[' ']'                    : <<('$1')/binary, "[]">>.
type -> identifier_chain                            : '$1'.

identifier_chain -> identifier                      : as_binary_string('$1').
identifier_chain -> identifier '.' identifier_chain : <<(as_binary_string('$1'))/binary, ".", ('$3')/binary>>.

Erlang code.

-spec as_atom({Type, Location, Value}) -> atom() when
    Type :: atom(),
    Location :: {pos_integer(), pos_integer()},
    Value :: string().
as_atom(Token) ->
    list_to_atom(value_of(Token)).

-spec as_binary_string({Type, Location, Value}) -> binary() when
    Type :: atom(),
    Location :: {pos_integer(), pos_integer()},
    Value :: string().
as_binary_string(Token) ->
    unicode:characters_to_binary(value_of(Token)).

-spec value_of({Type, Location, Value}) -> Value when
    Type :: atom(),
    Location :: {pos_integer(), pos_integer()},
    Value :: string().
value_of({_Type, _Location, Value}) ->
    Value.
