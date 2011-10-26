%% @doc Represents a memo about a user of Platformer.
%%
%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.

-module(platformer_user).
-behaviour(platformer_memo).

%% Exports required by platformer_memo.
-export([create/2, delete/2, exists/1, exists/2, get/1, is_valid_id/1, to_json/1]).

%% Other exports specific to platformer_user.
-export([create/1]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("jsonerl.hrl").
-include_lib("platformer.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Create a brand new user.  Return an id and a path.
%%
%% @spec create(envelope()) -> {Userid::string(), Path::string()}
create(#envelope{} = Envelope) ->
    platformer_memo:create("user", Envelope).

%% @doc Create a local record of a user that already exists somewhere else.
%%
%% @spec create(binary(), envelope()) -> {Id::string(), Path::string()}
create(Id, #envelope{source=Source} = Envelope) ->
    User = #platformer_user{id=Id, status=active, last_modified=platformer_util:now_int(), source=Source},
    platformer_memo:create("user", Id, User, Envelope).

%% @doc Mark a local record of a user as deleted.
%%
%% @spec delete(binary(), envelope()) -> ok | {error, Error}
delete(Id, #envelope{} = Envelope) ->
    platformer_memo:delete("user", Id, Envelope).

%% @doc Check whether there is a local record for a user with the given id.
%%  No attempt is made to check remove servers (For that, use {@link exists/2}.)
%%
%% @spec exists(binary()) -> {bool(), active | deleted}
exists(Id) ->
    platformer_memo:exists("user", Id).
    
%% @spec exists(binary(), envelope()) -> {bool(), active | deleted}
exists(Id, #envelope{} = Envelope) ->
    platformer_memo:exists("user", Id, Envelope).
    
%% @doc Get a user by id.
%%
%% @spec get(binary()) -> platformer_user()
get(Id) ->
    platformer_memo:get("user", Id).

%% @doc Is the given id valid for a user?
%%
%% @spec is_valid_id(binary()) -> bool()
is_valid_id(Id) ->
    platformer_memo:is_valid_id("user", Id).

%% @doc Produce a json representation of the user with the given id.
%%
%% @spec to_json(binary()) -> string()
to_json(Id) when is_binary(Id) ->
    list_to_binary(jsonerl:encode({{user, {{id, Id}}}})).

%%
%% TESTS
%%

% a valid id for a user
valid_id_00_test() ->
    ?assert(is_valid_id("platformer_user_06000740-0d04-4f44-bfb4-bcf406b3ddd6")).

% invalid id (type is misspelled)
invalid_id_00_test() ->
    ?assertNot(is_valid_id("platformer_esur_06000740-0d04-4f44-bfb4-bcf406b3ddd6")).

% valid json representation of a user
valid_json_00_test() ->
    ?assertEqual(<<"{\"user\":{\"id\":\"platformer_user_2a00109d-7f43-4225-a94c-78e3316fe018\"}}">>,
                 to_json(<<"platformer_user_2a00109d-7f43-4225-a94c-78e3316fe018">>)).
