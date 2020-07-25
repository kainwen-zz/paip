-module(pm).

-export([pattern_match/2, parse/1, sublis/2]).

-include("pm_type.hrl").

-spec pattern_match(pattern(), [atom()]) -> {boolean(), bindings()}.
pattern_match(Pattern, Input) ->
    B = bindings:start(),
    Result = case pattern_match(Pattern, Input, B) of
		 true ->
		     {true, bindings:dump(B)};
		 false ->
		     {false, []}
	     end,
    bindings:stop(B),
    Result.    

-spec pattern_match(pattern(), [atom()], binding_server()) -> boolean().
pattern_match([], [], _B) -> true;
pattern_match([], _, _B) -> false;
pattern_match([Var={var, _}], Input, B) ->
    pattern_match_var(Var, Input, B);
pattern_match(_, [], _B) -> false;
pattern_match([P|Ps], [I|Is], B) ->
    case pattern_match_ground(P, I, B) of
	true ->
	    pattern_match(Ps, Is, B);
	false ->
	    false
    end.

-spec pattern_match_ground(pattern_ground(), [atom()], binding_server()) -> boolean().
pattern_match_ground(Var={var, _}, Input, B) ->
    pattern_match_var(Var, [Input], B);
pattern_match_ground(P, I, _B) -> P =:= I.

-spec pattern_match_var(var(), [atom()], binding_server()) -> boolean().
pattern_match_var(Var, Input, B) ->
    case bindings:lookup(B, Var) of
	not_found ->
	    ok = bindings:extend(B, Var, Input),
	    true;
	{ok, Input} ->
	    true;
	_ ->
	    false
    end.

-spec sublis(pattern(), bindings()) -> [atom()].
sublis([], _) -> [];
sublis([Var={var, _}|Ps], B) ->
    case lists:keysearch(Var, 1, B) of
	false ->
	    erlang:error({"cannot find variable", Var});
	{value, {Var, Val}} ->
	    Val ++ sublis(Ps, B)
    end;
sublis([P|Ps], B) -> [P|sublis(Ps, B)].

-spec parse(string()) -> pattern().
parse(S) ->
    Ss = string:split(S, " ", all),
    [case string:prefix(Str, "?") of
	 nomatch ->
	     list_to_atom(Str);
	 X ->
	     {var, list_to_atom(X)}
     end
     || Str <- Ss].
