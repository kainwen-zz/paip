-module(unification).

-export([unify/2, unifier/2]).

-include("types.hrl").

-type unify_result() :: fail | eqclass:ecs().
-spec unify(clause(), clause()) -> unify_result().
unify({relation, R1, Args1},
      {relation, R2, Args2}) when R1 =:= R2, length(Args1) =:= length(Args2) ->
    ArgPair = lists:zip(Args1, Args2),
    lists:foldl(fun ({A1, A2}, ECs) ->
			case ECs of
			    fail ->
				fail;
			    _ ->
				Ec = eqclass:build_ec(A1, A2),
				eqclass:merge_ecs([Ec|ECs])
			end
		end,
		eqclass:empty_ecs(),
		ArgPair);
unify(_, _) -> fail.

unifier({relation, R1, Args1}, {relation, R2, Args2}) ->
    ECs = unify({relation, R1, Args1}, {relation, R2, Args2}),
    case ECs of
	fail ->
	    fail;
	_ ->
	    {relation, R1, [subsis(ECs, A) || A <- Args1]}
    end.

subsis(_ECs, A={Tag, _}) when Tag =:= number;
			      Tag =:= atom ->
    A;
subsis([], {var, Var}) ->
    erlang:error({not_found, Var});
subsis([{C, M}|ECs], {var, Var}) ->
    case sets:is_element(Var, M) of
	true ->
	    case C of
		not_set ->
		    {var, Var};
		_ ->
		    C
	    end;
	false ->
	    subsis(ECs, {var, Var})
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

unify_test() ->
    %(unify '(?x + 1) '(2 + ?y))=> ((?Y . 1) (?X . 2))
    {[{fact, R1}], []} = prolog_parser:scan_and_parse("add(X, 1)."),
    {[{fact, R2}], []} = prolog_parser:scan_and_parse("add(2, Y)."),
    ?assert(unification:unifier(R1, R2) =:=
		{relation,add,[{number,2},{number,1}]}),
    %(unify '(f ?x) '(f ?y))=> ((?X . ?Y))
    {[{fact, R3}], []} = prolog_parser:scan_and_parse("f(X)."),
    {[{fact, R4}], []} = prolog_parser:scan_and_parse("f(Y)."),
    ?assert(unification:unifier(R3, R4) =:= {relation,f,[{var,'X'}]}),
    
    %(unify '(?a + ?a = 0) '(?x + ?y = ?y))
    {[{fact, R5}], []} = prolog_parser:scan_and_parse("add(A, A, 0)."),
    {[{fact, R6}], []} = prolog_parser:scan_and_parse("add(X, Y, Y)."),
    ?assert(unification:unifier(R5, R6) =:= {relation,add,[{number,0},{number,0},{number,0}]}),
    
    %(unifier '(?a + ?a = 2) '(?x + ?y = ?y))=> (2 + 2 = 2)
    {[{fact, R7}], []} = prolog_parser:scan_and_parse("add(A, A, 2)."),
    {[{fact, R8}], []} = prolog_parser:scan_and_parse("add(X, Y, Y)."),
    ?assert(unification:unifier(R7, R8) =:= {relation,add,[{number,2},{number,2},{number,2}]}),

    %(unify '(?x + 1) '(2 + ?y))=> ((?Y . 1) (?X . 2))
    {[{fact, R9}], []} = prolog_parser:scan_and_parse("add(X, 1)."),
    {[{fact, R10}], []} = prolog_parser:scan_and_parse("add(2, Y)."),
    ?assert(unification:unifier(R9, R10) =:= {relation,add,[{number,2},{number,1}]}),    
    
    %(unify '?x '?y)=> ((?X . ?Y))
    %same as the above f(X) f(Y)
    %(unify '(?x ?x ?x) '(?y ?y ?y))
    {[{fact, R11}], []} = prolog_parser:scan_and_parse("f(X, X, X)."),
    {[{fact, R12}], []} = prolog_parser:scan_and_parse("f(Y, Y, Y)."),
    ?assert(unification:unifier(R11, R12) =:= {relation,f,[{var,'X'},{var,'X'},{var,'X'}]}),
    
    %(unify '(?x ?y a) '(?y ?x ?x))
    {[{fact, R13}], []} = prolog_parser:scan_and_parse("f(X, Y, a)."),
    {[{fact, R14}], []} = prolog_parser:scan_and_parse("f(Y, X, X)."),
    ?assert(unification:unifier(R13,R14) =:= {relation,f,[{atom,a},{atom,a},{atom,a}]}).

-endif.
