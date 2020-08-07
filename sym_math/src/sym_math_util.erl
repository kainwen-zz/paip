-module(sym_math_util).

-export([can_complete_eval/1, eval/1]).

-include_lib("eunit/include/eunit.hrl").

-export([print_exp/1]).

print_exp(Exp) ->
    string:join(print_exp_internal(Exp), " ").

print_exp_internal({integer, Int}) ->
    [integer_to_list(Int)];
print_exp_internal({symbol, S}) ->
    [atom_to_list(S)];
print_exp_internal({negation_exp, Exp}) ->
    ["-"] ++ print_exp_internal(Exp);
print_exp_internal({binop_exp, Op, Left, Right}) ->
    print_exp_internal(Left) ++ [atom_to_list(Op)] ++ print_exp_internal(Right);
print_exp_internal({exp_term, Exp}) ->
    ["("] ++ print_exp_internal(Exp) ++ [")"].

eval(Exp={integer, _N}) -> Exp;
eval({binop_exp, Op, Left, Right}) ->
    {integer, L} = eval(Left),
    {integer, R} = eval(Right),
    Func = fetch_binop_func(Op),
    V = Func(L, R),
    {integer, V};
eval({negation_exp, E}) ->
    {integer, V} = eval(E),
    {integer, -V};
eval({exp_term, E}) ->
    eval(E).

fetch_binop_func('+') ->
    fun (A, B) -> A + B end;
fetch_binop_func('-') ->
    fun (A, B) -> A - B end;
fetch_binop_func('*') ->
    fun (A, B) -> A * B end;
fetch_binop_func('/') ->
    fun (A, B) -> A / B end;
fetch_binop_func('^') ->
    fun (A, B) -> pow(A, B) end.

pow(_A, 0) -> 1;
pow(A, B) when B > 0 ->
    pow(A, B-1) * A.
    

can_complete_eval({symbol, _}) -> false;
can_complete_eval({integer, _}) -> true;
can_complete_eval({binop_exp, _, Left, Right}) ->
    can_complete_eval(Left) andalso
	can_complete_eval(Right);
can_complete_eval({exp_term, Exp}) ->
    can_complete_eval(Exp).

%%Eunit
print_exp_test() ->
    E1 = sym_math_parse:scan_and_parse("(a+1-3/y)/2+a*b*3-2*j"),
    ?assert(print_exp(E1) =:= "( a + 1 - 3 / y ) / 2 + a * b * 3 - 2 * j"),
    E2 = sym_math_parse:scan_and_parse("4*a^3"),
    ?assert(print_exp(E2) =:= "4 * a ^ 3"),
    E3 = sym_math_parse:scan_and_parse("--4*a^3"),
    ?assert(print_exp(E3) =:= "- - 4 * a ^ 3").
