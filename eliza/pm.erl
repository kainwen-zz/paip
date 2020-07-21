-module(pm).

-export([pattern_match/2, sublis/2, parse/1]).

-type varname() :: atom().

-type pattern_ground() :: atom()
			| {variable, varname()}.
-type pattern() :: [pattern_ground()].

-spec pattern_match(pattern(), [atom()]) -> boolean().


pattern_match([], []) -> true;
pattern_match([], _) -> false;
pattern_match(_, []) -> false;
pattern_match([P|Ps], [I|Is]) ->
    pattern_match_ground(P, I) andalso
	pattern_match(Ps, Is).

-spec pattern_match_ground(pattern_ground(), atom()) -> boolean().
pattern_match_ground({variable, _V}, _A) -> true;
pattern_match_ground(P, A) when is_atom(P); is_atom(A) -> P =:= A.
    
-spec sublis({Old::{variable, varname()},
	      New::atom()},
	     pattern()) -> pattern().
sublis(_, []) -> [];
sublis(S={{variable, Var}, New}, [P|Ps]) ->
    case P of
	{variable, Var} ->
	    [New|sublis(S, Ps)];
	_ ->
	    [P|sublis(S, Ps)]
    end.

-spec parse(string()) -> pattern().
parse(S) ->
    Ss = string:split(S, " ", all),
    [case string:prefix(Str, "?") of
	 nomatch ->
	     list_to_atom(Str);
	 X ->
	     {variable, list_to_atom(X)}
     end
     || Str <- Ss].
