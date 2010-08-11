%% @doc Represents a memo about a user of Platformer.
%%
%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.

-module(platformer_user).
-behaviour(platformer_memo).

%% Exports required by platformer_memo.
-export([create/1, create/2, delete/2, exists/1, exists/2, get/1, is_valid_id/1, to_json/1]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("jsonerl.hrl").
-include_lib("platformer.hrl").

%% @doc Create a brand new user.  Return an id and a path.
%%
%% @spec create(envelope()) -> {Userid::string(), Path::string()}
create(#envelope{} = Envelope) ->
    platformer_memo:create("user", Envelope).

%% @doc Create a local record of a user that already exists somewhere else.
%%
%% @spec create(binary(), envelope()) -> {Userid::string(), Path::string()}
create(Id, #envelope{source=Source} = Envelope) when is_binary(Id) ->
    User = #platformer_user{id=Id, status=active, last_modified=platformer_util:now_int(), source=Source},
    platformer_memo:create("user", Id, User, Envelope).

%% @doc Mark a local record of a user as deleted.
%%
%% @spec delete(binary(), envelope()) -> ok | {error, Error}
delete(Id, #envelope{} = Envelope) when is_binary(Id) ->
    platformer_memo:delete("user", Id, Envelope).

%% @doc Get a user by id.
%%
%% @spec get(binary()) -> platformer_user()
get(Id) when is_binary(Id) ->
    platformer_memo:get("user", Id).

%% @doc Check whether there is a local record for a user with the given id.
%%  No attempt is made to check remove servers (For that, use {@link exists/2}.)
%%
%% @spec exists(binary()) -> {bool(), active | deleted}
exists(Id) when is_binary(Id) ->
    platformer_memo:exists("user", Id).
    
%% @spec exists(binary(), envelope()) -> {bool(), active | deleted}
exists(Id, #envelope{} = Envelope) when is_binary(Id) ->
    platformer_memo:exists("user", Id, Envelope).
    

%% @doc Is the given id valid for a user?
%%
%% @spec is_valid_id(binary()) -> bool()
is_valid_id(Id) when is_binary(Id) ->
    is_valid_id(binary_to_list(Id));
is_valid_id(Id) when is_list(Id) ->
    string:left(Id, 16) =:= "platformer_user_"
        andalso
        platformer_util:is_valid_uuid(string:substr(Id, 17)).

%% @doc Produce a json representation of the user with the given id.
%%
%% @spec to_json(binary()) -> string()
to_json(Id) when is_binary(Id) ->
    jsonerl:encode({{user, {{id, Id}}}}).
