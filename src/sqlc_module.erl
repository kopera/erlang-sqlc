% Based on the `merl_build.erl` module included in OTP with the following
% License notice:
% 
% ---------------------------------------------------------------------
% Licensed under the Apache License, Version 2.0 (the "License"); you may
% not use this file except in compliance with the License. You may obtain
% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
% 
% @author Richard Carlsson 
% @copyright 2012 Richard Carlsson <carlsson.richard@gmail.com>
% ---------------------------------------------------------------------
-module(sqlc_module).
-export([
    new/1,
    add_requests/2,
    add_request/2
]).
-export([
    to_ast/1,
    to_erl/1
]).
-export_type([
    t/0
]).

-include_lib("syntax_tools/include/merl.hrl").

-record(codegen, {
    module :: atom(),
    exports = [] :: [{atom(), integer()}],
    functions = [] :: [{atom(), nonempty_list(erl_syntax:syntaxTree())}]
}).
-type t() :: #codegen{}.


%% @private
-spec new(atom()) -> #codegen{}.
new(ModuleName) when is_atom(ModuleName) ->
    #codegen{
        module = ModuleName
    }.


%% @private
-spec add_requests([Request], Codegen) -> Codegen when
    Request :: request(),
    Codegen :: #codegen{}.
add_requests([], Codegen) ->
    Codegen;
add_requests([Request | Requests], Codegen) ->
    add_requests(Requests, add_request(Request, Codegen)).


%% @private
-spec add_request(Request, Codegen) -> Codegen when
    Request :: request(),
    Codegen :: #codegen{}.
-type request() :: {query | mutation, request_name(), request_info()}.
-type request_name() :: atom().
-type request_info() :: #{
    return => return_type(),
    parameters := [{atom(), parameter_type()}],
    statement := [unicode:unicode_binary() | {parameter, atom()}]
}.
-type return_type() :: {array, atom()} | atom().
-type parameter_type() :: {array, atom()} | atom().
add_request({RequestType, RequestName, #{parameters := RequestParameters, statement := RequestStatement}}, #codegen{module = ModuleName} = Codegen) ->
    Parameters = [begin
        {ParameterName, #{
            key => erl_syntax:tuple([erl_syntax:atom(ModuleName), erl_syntax:atom(RequestName), erl_syntax:atom(ParameterName)]),
            type => ParameterType,
            variable => erl_syntax:variable(pascal_case(atom_to_list(ParameterName))),
            cast => case ParameterType of
                {array, ParameterSubType} -> erl_syntax:string(atom_to_list(ParameterSubType) ++ "[]");
                ParameterType -> erl_syntax:string(atom_to_list(ParameterType))
            end
        }}
    end || {ParameterName, ParameterType} <- RequestParameters],
    ParametersExpr = gen_request_parameters_expr(Parameters),
    StatementExpr = gen_request_statement_expr(Parameters, RequestStatement),
    Clause = ?Q([
        "(_@ParametersExpr) ->",
        "   #{type => '@RequestType@', statement => _@StatementExpr}"
    ]),
    add_exported_function(RequestName, lists:flatten([Clause]), Codegen).


%% @private
-spec gen_request_parameters_expr(Parameters) -> erl_syntax:syntaxTree() when
    Parameters :: [{atom(), parameter_info()}].
-type parameter_info() :: #{
    type := parameter_type(),
    key := erl_syntax:syntaxTree(),
    variable := erl_syntax:syntaxTree(),
    cast := erl_syntax:syntaxTree()
}.
gen_request_parameters_expr(Parameters) ->
    erl_syntax:map_expr([begin
        Key = erl_syntax:atom(ParameterName),
        erl_syntax:map_field_exact(Key, ParameterVariable)
    end || {ParameterName, #{variable := ParameterVariable}} <- Parameters]).


%% @private
gen_request_statement_expr(ParametersList, RequestStatement) ->
    Parameters = maps:from_list(ParametersList),
    %% eqwalizer:ignore
    erl_syntax:list(lists:flatmap(fun
        (Fragment) when is_binary(Fragment) ->
            [
                %% eqwalizer:ignore
                erl_syntax:binary([erl_syntax:binary_field(erl_syntax:string(unicode:characters_to_list(Fragment)))])
            ];
        ({parameter, ParameterName}) ->
            #{ParameterName := #{
                key := ParameterKey,
                variable := ParameterVariable,
                cast := ParameterCast
            }} = Parameters,
            [
                ?Q("<<\"(\">>"),
                ?Q("{parameter, #{key => _@ParameterKey, value => _@ParameterVariable}}"),
                ?Q("<<\"::\", _@ParameterCast>>"),
                % erl_syntax:binary([
                %     erl_syntax:binary_field(erl_syntax:string("::")),
                %     erl_syntax:binary_field(ParameterCast)
                % ]),
                ?Q("<<\")\">>")
            ]
    end, RequestStatement)).


%% @private
-spec pascal_case(string()) -> string().
pascal_case(String) ->
    %% eqwalizer:ignore
    lists:flatten(lists:join("", [string:titlecase(Word) || Word <- string:split(String, "_")])).


%% @private
-spec add_exported_function(Name, Clauses, Codegen) -> Codegen when
    Name :: atom(),
    Clauses :: nonempty_list(erl_syntax:syntaxTree()),
    Codegen :: #codegen{}.
add_exported_function(Name, Clauses, #codegen{} = Codegen) when is_atom(Name) ->
    add_function(true, Name, Clauses, Codegen).


%% @private
% -spec add_local_function(Name, Clauses, Codegen) -> Codegen when
%     Name :: atom(),
%     Clauses :: nonempty_list(erl_syntax:syntaxTree()),
%     Codegen :: #codegen{}.
% add_local_function(Name, Clauses, #codegen{} = Codegen) when is_atom(Name) ->
%     add_function(false, Name, Clauses, Codegen).


-spec add_function(Exported, Name, Clauses, Codegen) -> Codegen when
    Exported :: boolean(),
    Name :: atom(),
    Clauses :: nonempty_list(erl_syntax:syntaxTree()),
    Codegen :: #codegen{}.
add_function(Exported, Name, [FirstClause | _] = Clauses, #codegen{} = Codegen) ->
    Arity = length(erl_syntax:clause_patterns(FirstClause)),
    NewExports = case Exported of
        true -> [{Name, Arity} | Codegen#codegen.exports];
        false -> Codegen#codegen.exports
    end,
    Codegen#codegen{
        exports = NewExports,
        functions = [{Name, Clauses} | Codegen#codegen.functions]
    }.


-spec to_ast(t()) -> erl_syntax:syntaxTree().
to_ast(Codegen) ->
    erl_syntax:form_list(to_forms(Codegen)).


-spec to_erl(t()) -> unicode:chardata().
to_erl(Codegen) ->
    erl_prettypr:format(to_ast(Codegen), [
        {paper, 120},
        {ribbon, 80}
    ]).


-spec to_forms(t()) -> [erl_syntax:syntaxTree()].
to_forms(#codegen{module = ModuleName} = Codegen) ->
    Exports = lists:reverse([begin
        erl_syntax:arity_qualifier(erl_syntax:atom(ExportName), erl_syntax:integer(ExportArity))
    end || {ExportName, ExportArity} <- Codegen#codegen.exports]),
    Functions = lists:reverse([begin
        erl_syntax:function(erl_syntax:atom(FunctionName), FunctionClauses)
    end || {FunctionName, FunctionClauses} <- Codegen#codegen.functions]),
    %% eqwalizer:ignore
    merl:tree(lists:flatten(?Q([
        "%% @private",
        "-module('@ModuleName@').",
        "-export(['@_Exports'/1]).",
        "",
        "'@_Functions'() -> [].",
        ""
    ]))).