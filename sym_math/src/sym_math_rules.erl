-module(sym_math_rules).

-export([empty_rule/0, add_rule/3, add_rules/2,
	 simplification_rules/0, pick_rule/2]).

empty_rule() ->
    orddict:new().

add_rule(Pattern, Response, Rules) ->
    orddict:store(Pattern, Response, Rules).

add_rules([], Rules) -> Rules;
add_rules([{P, R}|PRs], Rules) -> 
    NewRules = orddict:store(P, R, Rules),
    add_rules(PRs, NewRules).

simplification_rules() ->
    RuleString = "
     (x + 0  = x)
     (0 + x  = x)
     (x + x  = 2 * x)
     (x - 0  = x)
     (0 - x  = - x)
     (x - x  = 0)
     (- - x  = x)
     (x * 1  = x)
     (1 * x  = x)
     (x * 0  = 0)
     (0 * x  = 0)
     (x * x  = x ^ 2)
     (x / 0  = undefined)
     (0 / x  = 0)
     (x / 1  = x)
     (x / x  = 1)
     (0 ^ 0  = undefined)
     (x ^ 0  = 1)
     (0 ^ x  = 0)
     (1 ^ x  = 1)
     (x ^ 1  = x)
     (x ^ (-1) = 1 / x)
     (x * (y / x) = y)
     ((y / x) * x = y)
     ((y * x) / x = y)
     ((x * y) / x = y)
     (x + (- x) = 0)
     ((- x) + x = 0)
     (x + y - x = y)
    ", 
    Lines = [string:sub_string(string:strip(Line, both), 2,
			       length(string:strip(Line, both)) - 1)
	     ||
	     Line <-string:split(RuleString, "\n", all),
		length(string:strip(Line, both)) > 0],
    add_rules([transform_to_rule(L)
	       || L <- Lines],
	      empty_rule()).
    
transform_to_rule(L) ->
    [Left, Right] = string:split(L, "="),
    Pattern = sym_math_parse:scan_and_parse(Left),
    Response = sym_math_parse:scan_and_parse(Right),
    {Pattern, Response}.

pick_rule(_Exp, []) -> fail;
pick_rule(Exp, [{Pattern, Response}|Rules]) ->
    case pattern_match(Pattern, Exp) of
	{ok, Bindings} ->
	    {ok, apply_rule(Response, Bindings)};
	fail ->
	    pick_rule(Exp, Rules)
    end.

pattern_match(Pt, Exp) ->
    pattern_match(Pt, Exp, orddict:new()).

pattern_match({integer, Int}, {integer, Int}, Bindings) -> {ok, Bindings};
pattern_match({symbol, X}, Exp, Bindings) ->
    case orddict:find(X, Bindings) of
	error ->
	    {ok, orddict:store(X, Exp, Bindings)};
	{ok, Exp} ->
	    {ok, Bindings};
	_ ->
	    fail
    end;
pattern_match({binop_exp, Op, X, Y}, {binop_exp, Op, Left, Right}, Bindings) ->
    handle_pattern_list([X, Y], [Left, Right], Bindings);
pattern_match({negation_exp, X}, {negation_exp, E}, Bindings) ->
    handle_pattern_list([X], [E], Bindings);
pattern_match(_, _, _) -> fail.

handle_pattern_list([], [], B) -> {ok, B};
handle_pattern_list([], _, _) -> fail;
handle_pattern_list(_, [], _) -> fail;
handle_pattern_list([X|Xs], [E|Es], B) ->
    case pattern_match(X, E, B) of
	fail ->
	    fail;
	{ok, B1} ->
	    handle_pattern_list(Xs, Es, B1)
    end.

apply_rule(Exp={integer, _}, _Bindings) -> Exp;
apply_rule({symbol, S}, Bindings) ->
    {ok, E} = orddict:find(S, Bindings),
    E;
apply_rule({binop_exp, Op, X, Y}, Bindings) ->
    {binop_exp, Op,
     apply_rule(X, Bindings),
     apply_rule(Y, Bindings)};
apply_rule({negation_exp, E}, Bindings) ->
    {negation_exp, apply_rule(E, Bindings)}.
