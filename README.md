# sqlc

SQL compiler for Erlang

## Build

    $ rebar3 compile

## Example

    ```erlang
    1> {ok, Module} = sqlc:string(example, "
        query by_id(:id uuid) returns user as
            select user_id as id, name, avatar_url, created_at
                from example.user
                where user_id = :id;
    ").
    {ok,{codegen,example,
                [{by_id,1}],
                [{by_id,[{tree,clause,
                                {attr,87,[],none},
                                {clause,[{tree,map_expr,
                                            {attr,0,[],none},
                                            {map_expr,none,[{tree,map_field_exact,{attr,...},{...}}]}}],
                                        none,
                                        [{tree,map_expr,
                                            {attr,88,[],none},
                                            {map_expr,none,
                                                        [{tree,map_field_assoc,...},{tree,...}]}}]}}]}]}}
    2> io:format("~s~n", [sqlc_module:to_erl(Module)]).
    %% @private
    -module(example).

    -export([by_id/1]).

    by_id(#{id := Id}) ->
        #{type => query,
        statement =>
            [<<"select user_id as id, name, avatar_url, created_at\n "
                "   from example.user\n    where user_id = ">>,
            <<"(">>,
            {parameter, #{key => {example, by_id, id}, value => Id}},
            <<"::", "uuid">>,
            <<")">>]}.
    ok
    ```
