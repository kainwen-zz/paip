-module(sym_math_pm).

-include("types.hrl").

-export([pm/3, check_guard/2]).

-type pm_result() :: fail | {ok, bindings()}.
-spec pm(Pattern::exp(), Guard::string(), Exp::exp()) -> pm_result().
pm(Pattern, Guard, Exp) ->    
    case pattern_match(Pattern, Exp, []) of
	{ok, B} ->
	    check_guard(Guard, B);
	fail ->
	    fail
    end.

-spec pattern_match(Pattern::exp(),
		    Exp::exp(), Bindings::bindings()) -> pm_result().
pattern_match({number, N}, {number, N}, B) -> {ok, B};
pattern_match({symbol, S}, Exp, B) ->
    case lists:keysearch(S, 1, B) of
	false ->
	    NB = [{S, Exp}|B],
	    {ok, NB};
	{value, {S, Exp}} ->
	    {ok, B};
	_ ->
	    fail
    end;
pattern_match({binop_exp, Op, PtE1, PtE2}, {binop_exp, Op, E1, E2}, B) ->
    case pm_helper(PtE1, E1, B) of
	fail ->
	    fail;
	{ok, NB} ->
	    pm_helper(PtE2, E2, NB)
    end;
pattern_match({negative_exp, PtE}, {negative_exp, E}, B) ->
    pm_helper(PtE, E, B);
pattern_match({Tag, PtE, {symbol, S1}},
	      {Tag, E, {symbol, S2}}, B) when Tag =:= diff_exp;
					      Tag =:= int_exp ->
    case pm_helper(PtE, E, B) of
	fail ->
	    fail;
	{ok, NB} ->
	    C1 = sym_math_util:contain_symbol(PtE, {symbol, S1}),
	    C2 = sym_math_util:contain_symbol(E, {symbol, S2}),
	    case {C1, C2} of
		{true, true} ->
		    case lists:keysearch(S1, 1, NB) of
			{value, {S1, {symbol, S2}}} ->
			    {ok, NB};
			_ ->
			    fail
		    end;
		{false, false} ->
		    {ok, NB};
		_ ->
		    fail
	    end
    end;
pattern_match({Tag, PtE}, {Tag, E}, B) when Tag =:= log_exp;
					    Tag =:= exp_exp;
					    Tag =:= sin_exp;
					    Tag =:= cos_exp ->
    pm_helper(PtE, E, B);
pattern_match(_, _, _) -> fail.

-spec pm_helper(exp(), exp(), bindings()) -> pm_result().
pm_helper(PtE, E, B) ->
    case pattern_match(PtE, E, B) of
	fail ->
	    fail;
	{ok, NB} ->
	    {ok, NB}
    end.

-spec check_guard(string(), bindings()) -> pm_result().
check_guard(Gd, Bindings) ->
    NB = [{list_to_atom(string:uppercase(atom_to_list(Key))),
	   case Val of {number, N} -> N ; _ -> Val end}
	  || {Key, Val} <- Bindings],
    {ok, Tokens, _} = erl_scan:string(Gd),
    {ok, Es} = erl_parse:parse_exprs(Tokens),
    {value, V, _} = erl_eval:exprs(Es, NB),
    case V of
	true ->
	    {ok, Bindings};
	false ->
	    fail
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

pm_test() ->
    Pt1 = sym_math_parse:scan_and_parse("int{x*y, x}"),
    E1 = sym_math_parse:scan_and_parse("int{x*y, y}"),
    ?assert(fail =:= pm(Pt1, "true.", E1)),
    
    E2 = sym_math_parse:scan_and_parse("int{a*y, a}"),
    ?assert(pm(Pt1, "true.", E2) =:= {ok,[{y,{symbol,y}},{x,{symbol,a}}]}).
-endif.
