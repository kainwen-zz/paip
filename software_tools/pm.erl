-module(pm).

-include_lib("eunit/include/eunit.hrl").

-export([pattern_match/2]).

pattern_match(Pattern, Input) ->
    pattern_match(Pattern, Input, orddict:new()).

pattern_match([], [], B) -> {ok, B};
pattern_match([], _, _B) -> error;
pattern_match([Pt={simple_var, _}|Pts], Input, B) -> 
    handle_simple_var(Pt, Input, Pts, B);
pattern_match([Pt|Pts], Input, B) when is_atom(Pt); is_integer(Pt) -> 
    handle_const(Pt, Input, Pts, B);
pattern_match([Pt={segment, _}|Pts], Input, B) -> 
    handle_segment(Pt, Input, Pts, B);
pattern_match([Pt={assert, _}|Pts], Input, B) -> 
    handle_assert(Pt, Input, Pts, B);
 pattern_match([Pt={guarded, _}|Pts], Input, B) -> 
     handle_guarded(Pt, Input, Pts, B);
pattern_match([Pt={n_choose_1, _}|Pts], Input, B) -> 
    handle_choose(Pt, Input, Pts, B);
pattern_match([Pt={except, _}|Pts], Input, B) -> 
    handle_except(Pt, Input, Pts, B).

handle_simple_var({simple_var, _Var}, [], _Pts, _B) -> error;
handle_simple_var({simple_var, Var}, Input=[I|RemInput], Pts, B) ->
    case orddict:find(Var, B) of
	error ->
	    NB = orddict:store(Var, [I], B),
	    pattern_match(Pts, RemInput, NB);
	{ok, Val} ->
	    case consume(Val, Input) of
		error ->
		    error;
		{ok, R2} ->
		    pattern_match(Pts, R2, B)
	    end
    end.

handle_const(Const, [Const|Input], Pts, B) ->
    pattern_match(Pts, Input, B);
handle_const(_, _, _, _) -> error.

handle_segment({segment, {Strategy, Var}}, Input, Pts, B) ->
     case orddict:find(Var, B) of
	 error ->
	     {StartPos, EndPos} = get_start_end_by_strategy(Strategy, length(Input)),
	     segment_try(Var, StartPos, EndPos, Input, Pts, B);
	 {ok, Val} ->
	     R = consume(Val, Input),
	     pattern_match(Pts, R, B)
     end.

handle_assert({assert, Code}, Input, Pts, B) ->
    NB = reform_bindings_for_eval(B),
    case es_eval:es_eval(Code, NB) of
	true ->
	    pattern_match(Pts, Input, B);
	false ->
	    error
    end.

handle_guarded({guarded, {_Var, _Guards}}, [], _Pts, _B) -> error;
handle_guarded({guarded, {Var, Guards}}, [I|Is], Pts, B) ->
    NB = orddict:store(Var, I, []),
    case lists:all(fun (G) ->
			   es_eval:es_eval(G, NB)
		   end,
		   Guards) of
	true ->
	    NNB = orddict:store(Var, [I], B),
	    pattern_match(Pts, Is, NNB);
	false ->
	    error
    end.
 
handle_choose({n_choose_1, []}, _Input, _Pts, _B) -> error;
handle_choose({n_choose_1, _}, [], _Pts, _B) -> error;
handle_choose({n_choose_1, [C|Cs]}, Input=[I|Is], Pts, B) ->
    case C =:= I of
	true ->
	    case pattern_match(Pts, Is, B) of
		{ok, NB} ->
		    {ok, NB};
		error ->
		    handle_choose({n_choose_1, Cs}, Input, Pts, B)
	    end;
	false ->
	    handle_choose({n_choose_1, Cs}, Input, Pts, B)
    end.    

handle_except({except, _Var}, [], _Pts, _B) -> error;
handle_except({except, Var}, [I|Is], Pts, B) ->
    case orddict:find(Var, B) of
	error ->
	    error;
	{ok, [I]} ->
	    error;
	_ ->
	    pattern_match(Pts, Is, B)
    end.

segment_try(_Var, Pos, EndPos, _Input, _Pts, _B) when Pos > EndPos -> error;
segment_try(Var, Pos, EndPos, Input, Pts, B) ->
    TryMatch = lists:sublist(Input, Pos),
    RemInput = lists:nthtail(length(TryMatch), Input),
    NB = orddict:store(Var, TryMatch, B),
    case pattern_match(Pts, RemInput, NB) of
	error ->
	    segment_try(Var, Pos+1, EndPos, Input, Pts, B);
	{ok, NNB} ->
	    {ok, NNB}
    end.	    

get_start_end_by_strategy(S, L) ->
    D = [{star, {0, L}},
	 {question, {0, 1}},
	 {plus, {1, L}}],
    {value, {S, R}} =  lists:keysearch(S, 1, D),
    R.

consume([], Input) -> {ok, Input};
consume(_Vals, []) -> error;
consume([Val|Vals], [Val|Input]) ->
    consume(Vals, Input);
consume(_, _) -> error.

reform_bindings_for_eval(B) ->
    L = orddict:to_list(B),
    NL = [case Val of
	      [I] ->
		  {Key, I};
	      _ ->
		  {Key, Val}
	  end
	  || {Key, Val} <- L],
    orddict:from_list(NL).

pm_test() ->
    %(pat-match '(x = (?is ?n numberp)) '(x = 34)) => ((?n . 34))
    %(pat-match '(x = (?is ?n numberp)) '(x = x)) => NIL
    Pt1 = ['X', '=', {guarded, {'N', ["is_number(N)."]}}],
    In1 = ['X', '=', 34],
    {ok,[{'N',[34]}]} = pm:pattern_match(Pt1, In1),
    In2 = ['X', '=', 'X'],
    error = pm:pattern_match(Pt1, In2),
    %(pat-match '(?x (?or < = >) ?y) '(3 < 4)) => ((?Y . 4) (?X . 3))
    Pt3 = [{simple_var, 'X'}, {n_choose_1, ['<', '=', '>']}, {simple_var, 'Y'}],
    In3 = [3, '<', 4],
    {ok,[{'X',[3]},{'Y',[4]}]} = pm:pattern_match(Pt3, In3),
    %(pat-match '(x = (?and (?is ?n numberp) (?is ?n oddp))) '(x = 3)) => ((?N . 3))
    Pt4 = ['X', '=', {guarded, {'N', ["is_number(N).", "(N rem 2) == 1."]}}],
    In4 = ['X', '=', 3],
    {ok,[{'N',[3]}]} = pm:pattern_match(Pt4, In4),
    %(pat-match '(?x /= (?not ?x)) '(3 /= 4)) => ((?X . 3))
    Pt5 = [{simple_var, 'X'}, '/=', {except, 'X'}],
    In5 = [3, '/=', 4],
    {ok,[{'X',[3]}]} = pm:pattern_match(Pt5, In5),
    %(pat-match '(?x > ?y (?if (> ?x ?y))) '(4 > 3)) => ((?Y . 3) (?X . 4))
    Pt6 = [{simple_var, 'X'}, '>', {simple_var, 'Y'}, {assert, "X > Y."}],
    In6 = [4, '>', 3],
    {ok,[{'X',[4]},{'Y',[3]}]} = pm:pattern_match(Pt6, In6),
    %(pat-match '(a (?* ?x) d) '(a b c d)) => ((?X B C))
    Pt7 = ['a', {segment, {star, 'X'}}, 'd'],
    In7 = [a, b, c, d],
    {ok,[{'X',[b,c]}]} = pm:pattern_match(Pt7, In7),
    %(pat-match '(a (?* ?x) (?* ?y) d) '(a b c d))=> ((?Y B C) (?X))
    Pt8 = ['a', {segment, {star, 'X'}}, {segment, {star, 'Y'}}, d],
    In8 = [a, b, c, d],
    {ok,[{'X',[]},{'Y',[b,c]}]} = pm:pattern_match(Pt8, In8),
    %(pat-match  '(a (?* ?x) (?* ?y) ?x ?y)  '(a b c d (b c) (d))) => ((?Y D) (?X B C))
    Pt9 = [a, {segment, {star, 'X'}}, {segment, {star, 'Y'}}, {simple_var, 'X'}, {simple_var, 'Y'}],
    In9 = [a, b, c, d, b, c, d],
    {ok,[{'X',[]},{'Y',[b,c,d]}]} = pm:pattern_match(Pt9, In9),
    %(pat-match  '(?x ?op ?y is ?z (?if (eql (?op ?x ?y) ?z))) '(3 + 4 is 7)) => ((?Z . 7) (?Y . 4) (?OP . +) (?X . 3))
    Pt10 = [{simple_var, 'X'}, {simple_var, '_op_'}, {simple_var, 'Y'}, 'is',
	    {simple_var, 'Z'}, {assert, "(X _op_ Y) == Z."}],
    In10 = [3, '+', 4, 'is', 7],
    {ok,[{'X',[3]},{'Y',[4]},{'Z',[7]},{'_op_',['+']}]} = pm:pattern_match(Pt10, In10),
    % (pat-match  '(?x ?op ?y (?if (?op ?x ?y))) '(3 > 4)) => NIL
    Pt11 = [{simple_var, 'X'}, {simple_var, '_op_'}, {simple_var, 'Y'},
	    {assert, "X _op_ Y."}],
    In11 = [3, '>', 4],
    error = pm:pattern_match(Pt11, In11).
