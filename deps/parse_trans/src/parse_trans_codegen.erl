%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.0, (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.erlang.org/license/EPL1_0.txt
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Original Code is exprecs-0.2.
%%%
%%% The Initial Developer of the Original Code is Ericsson AB.
%%% Portions created by Ericsson are Copyright (C), 2006, Ericsson AB.
%%% All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.

%%%-------------------------------------------------------------------
%%% File    : parse_trans_codegen.erl
%%% @author  : Ulf Wiger <ulf.wiger@erlang-solutions.com>
%%% @end
%%% Description : 
%%%-------------------------------------------------------------------

%%% @doc Parse transform for code generation pseduo functions
%%% 
%%% <p>...</p>
%%%
%%% @end

-module(parse_trans_codegen).

-export([parse_transform/2]).


%% @spec (Forms, Options) -> NewForms
%%
%% @doc
%% Searches for calls to `codegen:gen_function(Name, Fun)' and 
%% substitutes the abstract code for a function with name `Name'
%% and the same behaviour as `Fun'.
%% 
%% It is possible to do some limited expansion (importing a value
%% bound at compile-time), using the construct <code>{'$var', V}</code>, where
%% `V' is a bound variable in the scope of the call to `gen_function/2'.
%%
%% Example:
%% <pre>
%% gen(Name, X) ->
%%    codegen:gen_function(Name, fun(L) -> lists:member({'$var',X}, L) end).
%% </pre>
%%
%% After transformation, calling `gen(contains_17, 17)' will yield the 
%% abstract form corresponding to:
%% <pre>
%% contains_17(L) ->
%%    lists:member(17, L).
%% </pre>
%% @end
%%
parse_transform(Forms, Options) ->
    {NewForms, _} =
	parse_trans:depth_first(fun xform_fun/4, [], Forms, Options),
    parse_trans:revert(NewForms).

xform_fun(application, Form, _Ctxt, Acc) ->
    MFA = erl_syntax_lib:analyze_application(Form),
    case MFA of
	{codegen, {gen_function, 2}} ->
	    [NameF, FunF] =
		erl_syntax:application_arguments(Form),
	    Clauses = erl_syntax:fun_expr_clauses(FunF),
	    ClauseForms = parse_trans:revert(Clauses),
	    Arity = get_arity(ClauseForms),
	    Abstract = erl_parse:abstract(ClauseForms),
	    NewClauses = substitute(Abstract),
	    NewForm = {tuple,1,[{atom,1,function},
                                {integer, 1, erl_syntax:get_pos(Form)},
				NameF,
				{integer,1, Arity},
				NewClauses]},
	    {NewForm, Acc};
	_ ->
	    {Form, Acc}
    end;
xform_fun(_, Form, _Ctxt, Acc) ->
    {Form, Acc}.

substitute({tuple,L0,[{atom,_,tuple},
		      {integer,_,L},
		      {cons,_,
		       {tuple,_,[{atom,_,atom},{integer,_,_},{atom,_,'$var'}]},
		       {cons,_,
			{tuple,_,[{atom,_,var},{integer,_,_},{atom,_,V}]},
			{nil,_}}}]}) ->
    {call, L0, {remote,L0,{atom,L0,erl_parse},
			   {atom,L0,abstract}},
     [{var, L0, V}, {integer, L0, L}]};
substitute([]) ->
    [];
substitute([H|T]) ->
    [substitute(H) | substitute(T)];
substitute(T) when is_tuple(T) ->
    list_to_tuple(substitute(tuple_to_list(T)));
substitute(X) ->
    X.






get_arity(Clauses) ->
    Ays = [length(P) || {clause, _, P, _, _} <- Clauses],
    case lists:usort(Ays) of
	[Ay] ->
	    Ay;
	Other ->
	    erlang:error(ambiguous, Other)
    end.
