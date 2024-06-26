%% @private
-module(sqlc_definitions).
-export([
    from_string/1
]).
-export_type([
    t/0,
    request/0
]).

-type t() :: [request()].
-type request() :: {query | mutation, request_name(), request_info()}.
-type request_name() :: atom().
-type request_info() :: #{
    return => return_type(),
    parameters := [{atom(), parameter_type()}],
    statement := [unicode:unicode_binary() | {parameter, atom()}]
}.
-type return_type() :: unicode:unicode_binary().
-type parameter_type() :: unicode:unicode_binary().


-spec from_string(unicode:chardata()) -> {ok, t()} | {error, sqlc_error:t()}.
from_string(Input) ->
    case unicode:characters_to_list(Input) of
        String when is_list(String) ->
            case sqlc_scanner:string(String) of
                {ok, Tokens} ->
                    case sqlc_parser:parse(Tokens) of
                        {ok, Parsed} ->
                            % eqwalizer:ignore
                            {ok, Parsed};
                        {error, {{Line, Column}, _, Details}} when is_integer(Line), is_integer(Column) ->
                            {error, #{
                                type => syntax,
                                message => lists:flatten(sqlc_parser:format_error(Details)),
                                line => Line,
                                column => Column
                            }}
                    end;
                {error, {{Line, Column}, _, Details}} ->
                    {error, #{
                        type => syntax,
                        message => sqlc_scanner:format_error(Details),
                        line => Line,
                        column => Column
                    }}
            end;
        _ ->
            erlang:error(badarg, [Input])
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

test_data() ->
    <<"
        query by_id(:id uuid) returns user as
            select user_id as id, name, avatar_url, created_at
                from example.user
                where user_id = :id;

        query list(:filter jsonb) returns user[] as
            select user_id as id, name, avatar_url, created_at
                from example.user
                where (:filter['name'] is null or name = :filter['name'])
                    and (:filter['avatar_url'] is null or avatar_url = :filter['avatar_url'])
                    and (:filter['created_at.after'] is null or created_at >= :filter['created_at.after'])
                    and (:filter['created_at.before'] is null or created_at >= :filter['created_at.before']);

        mutation create(:name text, :avatar_url text, :account_type text, :account_id text) returns user as
            with account as (
                insert into example.user_account (type, user_account_id, user_id)
                values (:account_type, :account_id, gen_random_uuid())
                on conflict do nothing
                returning user_id
            )
            insert into example.user (user_id, name, avatar_url)
                select user_id, :name, :avatar_url from account
                returning user_id as id, name, avatar_url, created_at;

        mutation update(:id uuid, :updates jsonb) returns user as
            update example.user
                set name = coalesce(:updates['name'], user.name)
                    , avatar_url = coalesce(:updates['avatar_url'], user.avatar_url)
                where user_id = :id
                returning user_id as id, name, avatar_url, created_at;

        mutation delete(:id uuid) as
            delete from example.user
                where user_id = :id;
    ">>.

from_string_test_() ->
    [
        ?_assertMatch({ok, [
            {query, by_id, #{}},
            {query, list, #{}},
            {mutation, create, #{}},
            {mutation, update, #{}},
            {mutation, delete, #{}}
        ]}, from_string(test_data()))
    ].

-endif.
