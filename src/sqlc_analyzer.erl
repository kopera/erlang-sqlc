%% @private
-module(sqlc_analyzer).
-feature(maybe_expr, enable).
-export([
    verify/1
]).


-spec verify(sqlc_definitions:t()) -> ok | {error, sqlc_error:t()}.
verify(Definitions) ->
    maybe
        ok ?= check_duplicate_requests(Definitions, ordsets:new()),
        ok ?= check_requests(Definitions)
    else
        {error, Error} ->
            {error, sqlc_error:new(Error, lists:flatten(format_error(Error)))}
    end.


format_error({duplicate_request, RequestName}) ->
    io_lib:format("duplicate request `~s`", [RequestName]);
format_error({duplicate_parameter, RequestName, Name}) ->
    io_lib:format("duplicate parameter `~s` in request `~s`", [Name, RequestName]);
format_error({missing_parameters, Missing}) ->
    io_lib:format("unknown parameters `~p` used in request definition", [Missing]);
format_error({unused_parameters, Unused}) ->
    io_lib:format("unused parameters `~p`", [Unused]).


check_duplicate_requests([{_, RequestName, _} | Requests], Seen) ->
    case ordsets:is_element(RequestName, Seen) of
        false -> check_duplicate_requests(Requests, Seen);
        true -> {error, {duplicate_request, RequestName}}
    end;
check_duplicate_requests([], _Seen) ->
    ok.


check_requests([]) ->
    ok;
check_requests([Request | Requests]) ->
    case check_request(Request) of
        ok -> check_requests(Requests);
        {error, _} = Error -> Error
    end.


check_request({_, RequestName, #{parameters := Parameters}} = Request) ->
    maybe
        ok ?= check_duplicate_parameters(RequestName, Parameters, ordsets:new()),
        ok ?= check_statement_parameters(Request)
    end.

check_duplicate_parameters(RequestName, [{Name, _Type} | Parameters], Seen) ->
    case ordsets:is_element(Name, Seen) of
        false -> check_duplicate_parameters(RequestName, Parameters, Seen);
        true -> {error, {duplicate_parameter, RequestName, Name}}
    end;
check_duplicate_parameters(_RequestName, [], _Seen) ->
    ok.


check_statement_parameters({_, RequestName, #{parameters := Parameters, statement := Statement}}) ->
    RequestParameterNames = ordsets:from_list([Name || {Name, _Type} <- Parameters]),
    StatementParameterNames = ordsets:from_list([Name || {parameter, Name} <- Statement]),
    maybe
        {missing, []} ?= {missing, ordsets:subtract(StatementParameterNames, RequestParameterNames)},
        {unused, []} ?= {unused, ordsets:subtract(RequestParameterNames, StatementParameterNames)},
        ok
    else
        {missing, Missing} ->
            {error, {missing_parameters, RequestName, Missing}};
        {unused, Unused} ->
            {error, {unused_parameters, RequestName, Unused}}
    end.