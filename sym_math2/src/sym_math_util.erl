-module(sym_math_util).

-include("types.hrl").

-export([is_just_simple_arith/1, eval/1, contain/2, contain_intexp/1, linear/2]).

-spec is_just_simple_arith(exp()) -> boolean().
is_just_simple_arith({const, _}) -> true;
is_just_simple_arith({number, _}) -> true;
is_just_simple_arith({symbol, _}) -> false;
is_just_simple_arith({binop_exp, _, E1, E2}) ->
    is_just_simple_arith(E1) andalso is_just_simple_arith(E2);
is_just_simple_arith({negative_exp, E}) ->
    is_just_simple_arith(E);
is_just_simple_arith({diff_exp, _, _}) -> false;
is_just_simple_arith({int_exp, _, _}) -> false;
is_just_simple_arith({ExpTag, E}) when ExpTag =:= log_exp;
				       ExpTag =:= exp_exp;
				       ExpTag =:= sin_exp;
				       ExpTag =:= cos_exp ->
    is_just_simple_arith(E).

-spec eval(exp()) -> fail | {number, number()}.
eval(Exp) ->
    case is_just_simple_arith(Exp) of
	true ->
	    eval_internal(Exp);
	false ->
	    fail
    end.

eval_internal({const, pi}) -> {number, 3.1415926};
eval_internal(N={number, _}) -> N;
eval_internal({binop_exp, Op, E1, E2}) -> 
    Func = fetch_func(Op),
    {number, N1} = eval(E1),
    {number, N2} = eval(E2),
    {number, Func(N1, N2)};
eval_internal({ExpTag, E}) when ExpTag =:= negative_exp;
				ExpTag =:= log_exp;
				ExpTag =:= exp_exp;
				ExpTag =:= sin_exp;
				ExpTag =:= cos_exp ->
    Func = fetch_func(ExpTag),
    {number, N} = eval(E),
    {number, Func(N)}.

fetch_func(Tag) ->
    FuncLib = [
	       {'+', fun (A, B) -> A + B end},
	       {'-', fun (A, B) -> A - B end},
	       {'*', fun (A, B) -> A * B end},
	       {'/', fun (A, B) -> A / B end},
	       {'^', fun (A, B) -> pow(A, B) end},
	       {negative_exp, fun (X) -> -X end},
	       {log_exp, fun (X) -> math:log(X) end},
	       {exp_exp, fun (X) -> math:exp(X) end},
	       {sin_exp, fun (X) -> math:sin(X) end},
	       {cos_exp, fun (X) -> math:cos(X) end}
	      ],
    {Tag, Func} = lists:keyfind(Tag, 1, FuncLib),
    Func.

contain(E, E) -> true;
contain({Tag, _}, _E) when Tag =:= number;
			  Tag =:= symbol;
			  Tag =:= const ->
    false;
contain({binop_exp, _Op, E1, E2}, E) ->
    contain(E1, E) orelse contain(E2, E);
contain({Tag, E1}, E) when Tag =:= negative_exp;
			   Tag =:= log_exp;     
			   Tag =:= exp_exp;
			   Tag =:= sin_exp;
			   Tag =:= cos_exp ->
    contain(E1, E);
contain(_, _) -> false.

contain_intexp({Tag, _}) when Tag =:= number;
			      Tag =:= symbol;
			      Tag =:= const ->
    false;
contain_intexp({binop_exp, _, E1, E2}) ->
    lists:any(fun contain_intexp/1, [E1, E2]);
contain_intexp({int_exp, _, _}) -> true;
contain_intexp({diff_exp, E1, E2}) ->
    lists:any(fun contain_intexp/1, [E1, E2]);
contain_intexp({Tag, E}) when Tag =:= negative_exp;
			      Tag =:= log_exp;     
			      Tag =:= exp_exp;
			      Tag =:= sin_exp;
			      Tag =:= cos_exp ->
    contain_intexp(E).
    

pow(A, B) when is_integer(A), is_integer(B) ->
    case B of
	0 ->
	    1;
	_ ->
	    A * pow(A, B-1)
    end;
pow(A, B) ->
    math:pow(A, B).    



% linear(x^2, x^2/2)
strip_const({binop_exp, Op, E1, {number, _}}) when Op =:= '+';
						   Op =:= '-' ->
    strip_const(E1);
strip_const({binop_exp, '/', E1, {number, N}}) ->
    {binop_exp, '*', {number, 1/N}, E1};
strip_const(E) -> E.

linear(E1, E2) ->
    linear_internal(strip_const(E1),
		    strip_const(E2)).

linear_internal(E, E) -> {ok, {number, 1}};
linear_internal({binop_exp, '*', {number, N}, E},
		{binop_exp, '*', {number, M}, E}) ->
    {ok, {number, M/N}};
linear_internal(E, {binop_exp, '*', {number, M}, E}) ->
    {ok, {number, M}};
linear_internal({binop_exp, '*', {number, N}, E}, E) ->
    {ok, {number, 1/N}};
linear_internal(_, _) -> fail.
        

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

eval_test() ->
    ExpAns = [
	      {"3*2+2*3", {number, 12}},
	      {"3*(2+2)*3", {number, 36}},
	      {"3+5^2", {number,28}},
	      {"log{2.718}", {number,0.999896315728952}},
	      {"log{exp{1}}", {number,1.0}}
	     ],
    ok = lists:foreach(fun ({E, A}) ->
			       ?assert(
				  sym_math_util:eval(
				    sym_math_parse:scan_and_parse(E)) =:= A
				 )
		       end,
		       ExpAns).

contain_test() ->
    ?assertNot(contain({symbol, a}, {symbol, b})).

-endif.
