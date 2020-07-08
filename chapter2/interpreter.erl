-module(interpreter).

-export([init/0, generate1/1]).

init() ->
    rule_engine:start(),
    Rules = [
	     {sentence, [[noun_phrase, verb_phrase]]},
	     {noun_phrase, [[article, adj_star, noun, pp_star], [name], [pronoun]]},
	     {verb_phrase, [[verb, noun_phrase, pp_star]]},
	     {pp_star, [[], [pp, pp_star]]},
	     {adj_star, [[], [adj, adj_star]]},
	     {pp, [[prep, noun_phrase]]},
	     {prep, [to, in, by, with, on]},
	     {adj, [big, little, blue, green, adiabatic]},
	     {name, [pat, kim, lee, terry, robin]},
	     {article, [the, a]},
	     {noun, [man, ball, woman, table]},
	     {verb, [hit, took, saw, liked]},
	     {pronoun, [he, she, it, these, those, that]}
	    ],
    rule_engine:add(Rules).

generate1(Tag) ->
    Rule = rule_engine:fetch(Tag),
    case is_atom(Rule) of
	true ->
	    case rule_engine:member(Rule) of
		false ->
		    [Rule];
		true ->
		    generate1(Rule)
	    end;
	false ->
	    lists:flatmap(fun generate1/1, Rule)
    end.	    
    
