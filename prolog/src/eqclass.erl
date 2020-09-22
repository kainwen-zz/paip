-module(eqclass).

-include("types.hrl").

-export([build_ec/2, merge_ecs/1, empty_ecs/0]).

-export_type([ecs/0]).

-type const_val() :: not_set
		   | {number, number()}
		   | {atom, atom()}.
-type member() :: sets:set(atom()).
-type ec() :: {ConstVal::const_val(),
	       Member::member()}.
-type ecs() :: [ec()].

%% APIs

%% build_ec:
%%   create a eqclass via two args.
-type ec_result() :: fail | ec().
-spec build_ec(arg(), arg()) -> ec_result().
build_ec(A1, A2) ->
    C1 = is_const(A1),
    C2 = is_const(A2),
    case {C1, C2} of
	{false, false} ->
	    {not_set,
	     sets:from_list([element(2, A1),
			     element(2, A2)])};
	{true, false} ->
	    {A1,
	     sets:from_list([element(2, A2)])};
	{false, true} ->
	    {A2,
	     sets:from_list([element(2, A1)])};
	{true, true} ->
	    case A1 =:= A2 of
		true ->
		    {A1, sets:new()};
		false ->
		    fail
	    end
    end.

-spec empty_ecs() -> ecs().
empty_ecs() -> [].

-type merge_result() :: ecs() | fail.
-spec merge_ecs(ecs()) -> merge_result().
merge_ecs(ECs) ->
    Len = length(ECs),
    Pairs = [{I, J}
	     || I <- lists:seq(1, Len),
		J <- lists:seq(1, Len),
		I < J],
    R = search_mergable(ECs, Pairs),
    case R of
	fail ->
	    ECs;
	{II, JJ} ->
	    EI = lists:nth(II, ECs),
	    EJ = lists:nth(JJ, ECs),
	    MEc = merge_ec(EI, EJ),
	    case MEc of
		fail ->
		    fail;
		_ ->
		    {Ii, Jj} = R,
		    Ecs1 = del_list(ECs, [Ii, Jj]),
		    merge_ecs([MEc|Ecs1])
	    end
    end.

%% Internal helpers
-spec is_const(arg()) -> boolean().
is_const({Tag, _}) when Tag =:= number;
			Tag =:= atom ->
    true;
is_const({Tag, _}) when Tag =:= var -> false.

del_list(L, Index) ->
    del_list(L, Index, 1, []).

del_list(L, _Index, I, R) when I > length(L) -> R;
del_list(L, Index, I, R) ->
    case lists:member(I, Index) of
	true ->
	    del_list(L, Index, I+1, R);
	false ->
	    del_list(L, Index, I+1,
		     R ++ [lists:nth(I, L)])
    end.    

search_mergable(_ECs, [])  -> fail;
search_mergable(ECs, [{I, J}|Pairs]) -> 
    {C1, Mi} = lists:nth(I, ECs),
    {C2, Mj} = lists:nth(J, ECs),
    case sets:size(sets:intersection(Mi, Mj)) >= 1 of
	true ->
	    {I, J};
	false ->
	    case (C1 =:= C2 andalso 
		  element(1, C1) /= not_set) of
		true ->
		    {I, J};
		false ->
		    search_mergable(ECs, Pairs)
	    end
    end.

-type merge_ec_result() :: ec() | fail.
-spec merge_ec(ec(), ec()) -> merge_ec_result().
merge_ec({C1, M1}, {C1, M2}) ->
    {C1, sets:union(M1, M2)};
merge_ec({not_set, M1}, {C2, M2}) ->
    {C2, sets:union(M1, M2)};
merge_ec({C1, M1}, {not_set, M2}) ->
    {C1, sets:union(M1, M2)};
merge_ec(_, _) -> fail.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

build_ec_test() ->
    ?assert(eqclass:build_ec({atom, a}, {atom, b}) =:= fail),
    {CV, M} = eqclass:build_ec({atom, a}, {atom, a}),
    ?assert(CV =:= {atom, a}),
    ?assert(sets:size(M) =:= 0),
    
    {CV1, M1} = eqclass:build_ec({var, 'X'}, {var, 'Y'}),
    ?assert(CV1 =:= not_set),
    ?assert(sets:size(M1) =:= 2 andalso
	    sets:is_element('X', M1) andalso
	    sets:is_element('Y', M1)).


merge_ecs_test() ->
    Ec1 = {not_set, sets:from_list(['X', 'Y', 'Z'])},
    Ec2 = {not_set, sets:from_list(['Z'])},
    Ec3 = {{number, 1}, sets:new()},
    Ec4 = {{number, 1}, sets:from_list(['Z'])},
    [{{number,1}, M}] = merge_ecs([Ec1, Ec2, Ec3, Ec4]),
    S = sets:from_list(['X', 'Y', 'Z']),
    ?assert(sets:is_subset(S, M) andalso
	    sets:is_subset(M, S) ).    
-endif.
