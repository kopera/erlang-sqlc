-module(sqlc).
-feature(maybe_expr, enable).
-export([
    file/1,
    string/2
]).


-spec file(file:filename_all()) -> {ok, sqlc_module:t()} | {error, sqlc_error:t()}.
file(Path) ->
    ModuleName = case filename:basename(Path, ".sql") of
        Basename when is_binary(Basename) -> binary_to_atom(Basename);
        Basename when is_list(Basename) -> list_to_atom(Basename)
    end,
    case file:read_file(Path) of
        {ok, Input} ->
            string(ModuleName, Input);
        {error, Error} ->
            {error, sqlc_error:new({io, Error}, file:format_error(Error))}
    end.


-spec string(module(), unicode:chardata()) -> {ok, sqlc_module:t()} | {error, sqlc_error:t()}.
string(ModuleName, Input) when is_atom(ModuleName) ->
    case sqlc_definitions:from_string(Input) of
        {ok, Requests} ->
            case sqlc_analyzer:verify(Requests) of
                ok ->
                    {ok, sqlc_module:add_requests(Requests, sqlc_module:new(ModuleName))};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
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

string_test_() ->
    [
        ?_assertMatch({ok, _}, string(sqlc_example, test_data()))
    ].


-endif.
