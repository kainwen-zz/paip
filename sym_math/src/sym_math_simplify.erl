-module(sym_math_simplify).

-export([simplify/1]).

-include_lib("eunit/include/eunit.hrl").

simplify(Exp) ->
    Rules = sym_math_rules:simplification_rules(),
    simplify(Exp, Rules).

simplify(Exp={integer, _Int}, _Rules) -> Exp;
simplify(Exp={symbol, _S}, _Rules) -> Exp;
simplify({binop_exp, Op, Left, Right}, Rules) ->
    NL = simplify(Left, Rules),
    NR = simplify(Right, Rules),
    NewExp = {binop_exp, Op, NL, NR},
    simplify_common(NewExp, Rules);
simplify({negation_exp, E}, Rules) ->
    NewExp = {negation_exp, simplify(E, Rules)},
    simplify_common(NewExp, Rules);
simplify({exp_term, E}, Rules) ->
    NewExp = {exp_term, simplify(E, Rules)},
    simplify_common(NewExp, Rules).

simplify_common(NewExp, Rules) ->
    case sym_math_util:can_complete_eval(NewExp) of
	true ->
	    sym_math_util:eval(NewExp);
	false ->
	    case sym_math_rules:pick_rule(NewExp, Rules) of
		{ok, Response} ->
		    Response;
		fail ->
		    NewExp
	    end
    end.

%% Eunit
simplify_test() ->
    ?assert(sym_math_util:print_exp(sym_math_simplify:simplify(sym_math_parse:scan_and_parse("2 + 2"))) =:= "4"),
    ?assert(sym_math_util:print_exp(sym_math_simplify:simplify(sym_math_parse:scan_and_parse("5 * 20 + 30 + 7"))) =:= "137"),
    ?assert(sym_math_util:print_exp(sym_math_simplify:simplify(sym_math_parse:scan_and_parse("5 * x - (4 + 1) * x"))) =:= "0"),
    ?assert(sym_math_util:print_exp(sym_math_simplify:simplify(sym_math_parse:scan_and_parse("y / z * (5 * x - (4 + 1) * x)"))) =:= "0"),
    ?assert(sym_math_util:print_exp(sym_math_simplify:simplify(sym_math_parse:scan_and_parse("(4-3) * x + (y / y - 1) * z"))) =:= "x"),
    ?assert(sym_math_util:print_exp(sym_math_simplify:simplify(sym_math_parse:scan_and_parse("3 * 2 * X"))) =:= "6 * X").
