-module(platformer.core.db).
-export([check_tables/0, delete/1, find/1, read_all/1, reset/0, write/1]).

-import(log4erl).
-import(mnesia).
-import(qlc).

-include_lib("stdlib/include/qlc.hrl").
-include("platformer.hrl").

%% This checks that the tables are present.  It doesn't check their
%% structure (for that we need to do real release management).  If
%% the tables aren't there, it calls reset.
check_tables() ->
    case mnesia:system_info(tables) of
        [query_token, pfnode, pfuser, schema] ->
            log4erl:debug("Database tables are present."),
            ok;
        _ ->
            log4erl:debug("Database tables are not all present."),
            reset()
    end.
                                                   

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
    log4erl:warn("Resetting the database (any existing data will be lost)."),
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    
    mnesia:create_table(pfuser, [{disc_copies, [node()]}, {attributes, record_info(fields, pfuser)}]),
    mnesia:create_table(pfnode, [{disc_copies, [node()]}, {attributes, record_info(fields, pfnode)}]),
    mnesia:create_table(query_token, [{disc_copies, [node()]}, {attributes, record_info(fields, query_token)}]),
    node:load_preconfigured(),
    ok.

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
