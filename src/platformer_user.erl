%% @doc Represents a user in Platformer.  (Functions here are
%% independent of any data storage mechanism.)
%%
%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.

-module(platformer_user).
-behaviour(platformer_element).

% Exports required by platformer_element.
-export([get_id/1, get_path/1]).

-include_lib("platformer.hrl").

%% @doc Rather redundant; just returns the id of a given user record.
%% @spec get_id(platformer_user()) -> string()
get_id(#platformer_user{id=Id})  when Id =/= undefined ->
    binary_to_list(Id).

%% @doc Returns the path that should be used (appended to hostname for URI)
%% for referring to this user.
%%
%% @spec get_path(platformer_user()) -> string()
get_path(#platformer_node{id=Id}) when Id =/= undefined ->
    "/user/" ++ binary_to_list(Id).
