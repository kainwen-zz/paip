-module(sym_math_rules).

-include("types.hrl").

-export([parse/1, load_rule_from_file/1,
	 check_and_apply_rule/2, pick_one_rule_and_apply/2]).

-spec parse(string()) -> rule().
parse(S) ->
    {Rule, Guard} = case string:find(S, "with") of
			nomatch ->
			    {S, "true."};
			_ ->
			    [A, B] = string:split(S, "with"),
			    {string:strip(A, both), string:strip(B, both)}
		    end,
    [A1, B1] = string:split(Rule, "="),
    Pt = sym_math_parse:scan_and_parse(A1),
    Res = sym_math_parse:scan_and_parse(B1),
    {Pt, Res, Guard}.

-spec load_rule_from_file(string()) -> [rule()].
load_rule_from_file(Fn) ->
    {ok, Data} = file:read_file(Fn),
    Code = binary_to_list(Data),
    [parse(string:strip(Line, both))
     || Line <- string:split(Code, "\n", all),
	length(string:strip(Line, both)) > 0].

-type check_apply_rule_result() :: fail | {ok, exp()}.
-spec pick_one_rule_and_apply(exp(), [rule()]) -> check_apply_rule_result().
pick_one_rule_and_apply(_Exp, []) -> fail;
pick_one_rule_and_apply(Exp, [Rule|Rules]) -> 
    case check_and_apply_rule(Rule, Exp) of
	fail ->
	    pick_one_rule_and_apply(Exp, Rules);
	E ->
	    E
    end.

%% Match A rule to an Exp

-spec check_and_apply_rule(rule(), exp()) -> check_apply_rule_result().
check_and_apply_rule({Pattern, Response, Guard}, Exp) ->
    case sym_math_pm:pm(Pattern, Guard, Exp) of
	fail ->
	    fail;
	{ok, Bindings} ->
	    {ok, apply_rule(Response, Bindings)};
	{partok, Bindings, AA} ->
	    {ok, {binop_exp, '*', apply_rule(Response, Bindings), AA}}
    end.

-spec apply_rule(exp(), bindings()) -> exp().
apply_rule(N={number, _}, _B) -> N;
apply_rule(C={const, _}, _B) -> C;
apply_rule(Sym={symbol, S}, B) ->
    case lists:keysearch(S, 1, B) of
	false ->
	    Sym;
	{value, {S, Val}} ->
	    Val
    end;
apply_rule({binop_exp, Op, E1, E2}, B) ->
    {binop_exp, Op, apply_rule(E1, B), apply_rule(E2, B)};
apply_rule({Tag, E}, B) when Tag =:= negative_exp;
			     Tag =:= log_exp;
			     Tag =:= exp_exp;
			     Tag =:= sin_exp;
			     Tag =:= cos_exp ->
    {Tag, apply_rule(E, B)};
apply_rule({Tag, E, S}, B) when Tag =:= int_exp; 
				Tag =:= diff_exp ->
    {Tag, apply_rule(E, B), apply_rule(S, B)}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

rule_test() ->
    Rule1 = "s * n = n * s with is_number(N) and not is_number(S).",
    ?assert(sym_math_rules:parse("s * n = n * s with is_number(N) and not is_number(S).") =:=
		{{binop_exp,'*',{symbol,s},{symbol,n}},
		 {binop_exp,'*',{symbol,n},{symbol,s}},
		 "is_number(N) and not is_number(S)."}),

    E1 = sym_math_parse:scan_and_parse("x * 9"),
    ?assert(sym_math_rules:check_and_apply_rule(sym_math_rules:parse(Rule1), E1) =:=
		{ok,{binop_exp,'*',{number,9},{symbol,x}}}).
-endif.
