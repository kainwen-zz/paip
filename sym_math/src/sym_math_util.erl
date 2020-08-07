-module(sym_math_util).

-export([can_complete_eval/1]).

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

can_complete_eval(Exp) ->
    not do_not_contain_symbol(Exp).

do_not_contain_symbol({symbol, _}) -> false;
do_not_contain_symbol({integer, _}) -> false;
do_not_contain_symbol({binop_exp, _, Left, Right}) ->
    do_not_contain_symbol(Left) andalso
	do_not_contain_symbol(Right);
do_not_contain_symbol({exp_term, Exp}) ->
    do_not_contain_symbol(Exp).

%%Eunit
print_exp_test() ->
    E1 = sym_math_parse:scan_and_parse("(a+1-3/y)/2+a*b*3-2*j"),
    ?assert(print_exp(E1) =:= "( a + 1 - 3 / y ) / 2 + a * b * 3 - 2 * j"),
    E2 = sym_math_parse:scan_and_parse("4*a^3"),
    ?assert(print_exp(E2) =:= "4 * a ^ 3"),
    E3 = sym_math_parse:scan_and_parse("--4*a^3"),
    ?assert(print_exp(E3) =:= "- - 4 * a ^ 3").
