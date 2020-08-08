-module(sym_math_rules).

-export([empty_rule/0, add_rule/4, add_rules/2, pm/3,
	 simplification_rules/0, pick_rule/2]).

empty_rule() ->
    [].

add_rule(Pattern, Response, G, Rules) ->
    [{Pattern, Response, G}|Rules].

add_rules([], Rules) -> Rules;
add_rules([{P, R, G}|PRs], Rules) -> 
    NewRules = add_rule(P, R, G, Rules),
    add_rules(PRs, NewRules).

simplification_rules() ->
    RuleString = "
     x + 0  = x
     0 + x  = x
     x + x  = 2 * x
     x - 0  = x
     0 - x  = - x
     x - x  = 0
     - - x  = x
     x * 1  = x
     1 * x  = x
     x * 0  = 0
     0 * x  = 0
     x * x  = x ^ 2
     x / 0  = undefined
     0 / x  = 0
     x / 1  = x
     x / x  = 1
     0 ^ 0  = undefined
     x ^ 0  = 1
     0 ^ x  = 0
     1 ^ x  = 1
     x ^ 1  = x
     x ^ (-1) = 1 / x
     x * (y / x) = y
     (y / x) * x = y
     (y * x) / x = y
     (x * y) / x = y
     x + (- x) = 0
     (- x) + x = 0
     x + y - x = y
     s * n = n * s :with is_number(N) and not is_number(S)
     n * (m * x) = (n * m) * x :with is_number(N) and is_number(M)
     x * (n * y) = n * (x * y) :with is_number(N)
     (n * x) * y = n * (x * y) :with is_number(N)
     n * x * y = n * (x * y) :with is_number(N)
     n + s = s + n :with is_number(N) and not is_number(S)
     (x + m) + n = x + n + m :with is_number(M) and is_number(N)
     x + (y + n) = (x + y) + n :with is_number(N)
     (x + n) + y = (x + y) + n :with is_number(N)
    ", 
    Lines = [string:strip(Line, both)
	     ||
	     Line <-string:split(RuleString, "\n", all),
		length(string:strip(Line, both)) > 0],
    add_rules([transform_to_rule(L)
	       || L <- Lines],
	      empty_rule()).
    
transform_to_rule(L) ->
    {PtStr, GdStr} = case string:find(L, ":with") of
			 nomatch ->
			     {L, "true."};
			 _ ->
			     [A, B] = string:split(L, ":with"),
			     {string:strip(A, both),
			      string:join([string:strip(B, both), "."], "")}
		     end,
    [Left, Right] = string:split(PtStr, "="),
    Pattern = sym_math_parse:scan_and_parse(Left),
    Response = sym_math_parse:scan_and_parse(Right),
    {Pattern, Response, GdStr}.

pick_rule(_Exp, []) -> fail;
pick_rule(Exp, [{Pattern, Response, GdStr}|Rules]) ->
    case pm(Pattern, GdStr, Exp) of
	{ok, Bindings} ->
	    {ok, apply_rule(Response, Bindings)};
	fail ->
	    pick_rule(Exp, Rules)
    end.

pm(Pt, GdStr, Exp) ->
    case pattern_match(Pt, Exp, orddict:new()) of
	fail ->
	    fail;
	{ok, Bindings} ->
	    case eval_under_bindings(GdStr, Bindings) of
		true ->
		    {ok, Bindings};
		false ->
		    fail
	    end
    end.	

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
pattern_match({exp_term, E1}, {exp_term, E2}, Bindings) ->
    pattern_match(E1, E2, Bindings);
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

eval_under_bindings(GdStr, Bindings) ->
    L = orddict:to_list(Bindings),
    NL = [{list_to_atom(string:to_upper(atom_to_list(K))),
	   case V of
	       {integer, Int} ->
		   Int;
	       _ ->
		   V
	   end}
	  || {K, V} <- L],
    NB = orddict:from_list(NL),
    es_eval:es_eval(GdStr, NB).

apply_rule(Exp={integer, _}, _Bindings) -> Exp;
apply_rule({symbol, S}, Bindings) ->
    {ok, E} = orddict:find(S, Bindings),
    E;
apply_rule({binop_exp, Op, X, Y}, Bindings) ->
    {binop_exp, Op,
     apply_rule(X, Bindings),
     apply_rule(Y, Bindings)};
apply_rule({negation_exp, E}, Bindings) ->
    {negation_exp, apply_rule(E, Bindings)};
apply_rule({exp_term, E}, Bindings) ->
    apply_rule(E, Bindings).

