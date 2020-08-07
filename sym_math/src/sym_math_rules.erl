-module(sym_math_rules).

-export([empty_rule/0, add_rule/3, add_rules/2, simplification_rules/0]).

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
