-module(es_eval).

-include_lib("eunit/include/eunit.hrl").

-export([es_eval/2]).

%% APIs
es_eval(String, Bindings) ->
    {ok, Tokens, _} = erl_scan:string(String),
    {ok, Es} = erl_parse:parse_exprs(replace_op(Tokens, Bindings)),
    {value, V, _} = erl_eval:exprs(Es, Bindings),
    V.

%% Internal Helper Functions
replace_op([], _) -> [];
replace_op([Var={var, N, A}|Toks], Bindings) ->
    Sa = atom_to_list(A),
    L = string:len(Sa),
    case (string:sub_string(Sa, 1, 1) =:= "_" andalso string:sub_string(Sa, L, L) =:= "_")
    of
	true ->
	    {ok, Val} = orddict:find(A, Bindings),
	    [{Val, N}|replace_op(Toks, Bindings)];
	false ->
	    [Var|replace_op(Toks, Bindings)]
    end;
replace_op([T|Toks], Bindings) ->
    [T|replace_op(Toks, Bindings)].

%%Unit Test
es_eval_test() ->
    B = erl_eval:add_binding('X', 3,
			     erl_eval:add_binding('Y', 4, [])),
    B1 = orddict:store('_op1_', '>',
		       orddict:store('_op2_', '==', B)),
    true = es_eval("(X _op1_ Y) _op2_ false.", B1).
