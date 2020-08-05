-module(search).

-export([dfs/3, bfs/3, best_first_search/4, beam_search/5]).

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

sorter(CostFn) ->
    fun (Succ, Rest) ->
	    lists:sort(fun (A, B) ->
			       CostFn(A) < CostFn(B)
		       end,
		       Succ ++ Rest)
    end.

best_first_search(State, DoWeFinish, Successors, CostFn) ->
    tree_search([State], DoWeFinish, Successors, sorter(CostFn)).

beam_search(State, DoWeFinish, Successors, CostFn, BeamWidth) ->
    tree_search([State], DoWeFinish, Successors,
		fun (Succ, Rest) ->
			Sorted = lists:sort(fun (A, B) ->
						    CostFn(A) < CostFn(B)
					    end,
					    Succ ++ Rest),
			lists:sublist(Sorted, BeamWidth)
		end).
