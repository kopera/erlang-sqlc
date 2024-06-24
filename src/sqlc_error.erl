-module(sqlc_error).
-export([
    new/2
]).
-export_type([
    t/0,
    type/0
]).

-type t() :: #{
    type := syntax | type(),
    message := string(),
    line => pos_integer(),
    column => pos_integer()
}.
-type type() ::
      {duplicate_request, RequestName :: atom()}
    | {duplicate_parameter, RequestName :: atom(), ParameterName :: atom()}
    | {missing_parameters, [ParameterName :: atom()]}
    | {unused_parameters, [ParameterName :: atom()]}
    | {io, term()}.


%% @private
-spec new(type(), string()) -> t().
new(Type, Message) ->
    #{
        type => Type,
        message => Message
    }.