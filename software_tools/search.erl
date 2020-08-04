-module(search).

-export([dfs/3, bfs/3]).

tree_search([], _DoWeFinish, _Successors, _Combiner) -> fail;
tree_search([S|Rest], DoWeFinish, Successors, Combiner) ->
    io:format("~~searching: ~p\n", [[S|Rest]]),
    case DoWeFinish(S) of
	true ->
	    S;
	false ->
	    Succ = Successors(S),
	    States = Combiner(Succ, Rest),
	    tree_search(States, DoWeFinish, Successors, Combiner)
    end.

dfs(State, DoWeFinish, Successors) ->
    tree_search([State],
		DoWeFinish,
		Successors,
		fun (Succ, Rest) ->
			Succ ++ Rest
		end).

bfs(State, DoWeFinish, Successors) ->
    tree_search([State],
		DoWeFinish,
		Successors,
		fun (Succ, Rest) ->
			Rest ++ Succ
		end).
