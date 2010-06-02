-module(platformer_db).
-export([delete/1, find/1, read_all/1, reset/0, write/1]).

-include_lib("stdlib/include/qlc.hrl").

-include("platformer.hrl").

delete(Oid) ->
    F = fun() ->
		mnesia:delete(Oid)
	end,
    mnesia:transaction(F).

find(Q) ->
    F = fun() ->
		qlc:e(Q)
	end,
    transaction(F).

read_all(Table) ->
    Q = qlc:q([X || X <- mnesia:table(Table)]),
    find(Q).

reset() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    
    mnesia:create_table(user, [{disc_copies, [node()]}, {attributes, record_info(fields, user)}]),
    mnesia:create_table(server, [{disc_copies, [node()]}, {attributes, record_info(fields, server)}]),
    mnesia:create_table(query_token, [{disc_copies, [node()]}, {attributes, record_info(fields, query_token)}]),
    server_resource:load_preconfigured(),
    {ok}.

transaction(F) ->
    case mnesia:transaction(F) of
	{atomic, Result} ->
	    Result;
	{aborted, _Reason} ->
	    []
    end.

write(Rec) ->
    F = fun() ->
		mnesia:write(Rec)
	end,
    mnesia:transaction(F).
