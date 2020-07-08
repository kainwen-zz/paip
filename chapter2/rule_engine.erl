-module(rule_engine).

-export([start/0, add/1, member/1, fetch/1]).

start() ->
    ets:new(?MODULE, [named_table]).

add(Rules) ->
    ok = lists:foreach(fun (Rule) ->
			       true = ets:insert(?MODULE, Rule)
		       end,
		       Rules).

member(Tag) ->
    ets:member(?MODULE, Tag).

fetch(Tag) ->    
    case ets:lookup(?MODULE, Tag) of
	[] ->
	    erlang:error({Tag, "rule not found!"});
	[{Tag, [Rule]}] -> Rule;
	[{Tag, Rules}] when is_list(Rules) ->
	    random_pick_one_element(Rules);
	R ->
	    erlang:error({R, "rule format not correct!"})
    end.

random_pick_one_element(L) ->
    N = rand:uniform(length(L)),
    lists:nth(N, L).
