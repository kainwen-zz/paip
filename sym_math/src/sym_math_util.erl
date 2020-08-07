-module(sym_math_util).

-include_lib("eunit/include/eunit.hrl").

-export([print_exp/1]).

print_exp(Exp) ->
    string:join(print_exp_internal(Exp), " ").

print_exp_internal({integer, _, Int}) ->
    [integer_to_list(Int)];
print_exp_internal({symbol, _, S}) ->
    [atom_to_list(S)];
print_exp_internal({binop_exp, Op, Left, Right}) ->
    print_exp_internal(Left) ++ [atom_to_list(Op)] ++ print_exp_internal(Right);
print_exp_internal({exp_term, Exp}) ->
    ["("] ++ print_exp_internal(Exp) ++ [")"].

%%Eunit
print_exp_test() ->
    E = sym_math_parse:scan_and_parse("(a+1-3/y)/2+a*b*3-2*j"),
    ?assert(print_exp(E) =:= "( a + 1 - 3 / y ) / 2 + a * b * 3 - 2 * j").
