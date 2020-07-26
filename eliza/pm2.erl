-module(pm2).

-export([parse/1, pattern_match/2, sublis/2]).

-include("pm2_type.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec pattern_match(pattern(), [atom()]) -> {boolean(), bindings()}.
pattern_match(Pattern, Input) ->
    pattern_match(Pattern, Input, bindings:empty()).

-spec pattern_match(pattern(), [atom()], bindings()) -> {boolean(), bindings()}.
pattern_match([], [], B) -> {true, B};
pattern_match([], _, _B) -> {false, []};
pattern_match([Var={var, _}|Pt], Input, B) ->
    handle_simple_var(Var, Pt, Input, B);
pattern_match([Segvar={segvar, _}|Pt], Input, B) ->
    handle_segment_var(Segvar, Pt, Input, B);
pattern_match(_, [], _B) -> {false, []};
pattern_match([P|Ps], [I|Is], B) when is_atom(P), is_atom(I) ->
    case P =:= I of
	true ->
	    pattern_match(Ps, Is, B);
	false ->
	    {false, []}
    end.

-spec handle_simple_var(simple_var(), pattern(), [atom()], bindings()) -> {boolean(), bindings()}.
handle_simple_var(_Var, _Pt, [], _B) -> {false, []}; %% Var at least represent one atom
handle_simple_var(Var, Pt, [I|Is], B) ->
    case bindings:lookup(B, var_name(Var)) of
	not_found ->
	    NB = bindings:extend(B, var_name(Var), [I]),
	    pattern_match(Pt, Is, NB);
	{ok, I} ->
    	    pattern_match(Pt, Is, B);
	_ ->
	    {false, []}
    end.

-spec handle_segment_var(segment_var(), pattern(), [atom()], bindings()) -> {boolean(), bindings()}.
handle_segment_var(Segvar, Pt, Input, B) ->
    case bindings:lookup(B, var_name(Segvar)) of
	not_found ->
	    case Pt of
		[] ->
		    {true, bindings:extend(B, var_name(Segvar), Input)};
		[P|Ps] when is_atom(P) ->
		    handle_segment_var(Segvar, P, Ps, Input, B, 0);
		_ ->
		    {false, []}
	    end;
	{ok, Is} ->
	    case lists:prefix(Is, Input) of
		true ->
		    RemInput = lists:nthtail(length(Is), Input),
		    pattern_match(Pt, RemInput, B);
		false ->
		    {false, []}
	    end
    end.

-spec handle_segment_var(segment_var(), atom(), pattern(), [atom()], bindings(), integer()) -> {boolean(), bindings()}.
handle_segment_var(_Segvar, _P, _Pt, Input, _B, StartPos) when StartPos >= length(Input) ->
    {false, []};
handle_segment_var(Segvar, P, Pt, Input, B, StartPos) ->
    case split_list(Input, P, StartPos) of
	not_found ->
	    {false, []};	     
	{Pos, {H, T}} ->
	    case pattern_match(Pt, T, B) of
		{false, []} ->
		    handle_segment_var(Segvar, P, Pt, Input, B, Pos);
		{true, NB} ->
		    case bindings:lookup(NB, var_name(Segvar)) of
			not_found ->
			    NNB = bindings:extend(NB, var_name(Segvar), H),
			    {true, NNB};
			{ok, H} ->
			    {true, NB};
			_ ->
			    handle_segment_var(Segvar, P, Pt, Input, B, Pos)
		    end
	    end
    end.

%% List index is 1-based.
%% split_list find the A in As with index > Pos
-type split_result() :: not_found
		      | {integer(), {[atom()], [atom()]}}.
-spec split_list([atom()], atom(), integer()) -> split_result().
split_list(As, _A, Pos) when Pos >= length(As) -> not_found;
split_list(As, A, Pos) ->
    {_H, T} = lists:split(Pos, As),
    R = lists:dropwhile(fun (E) -> E =/= A end, T),
    case R of
	[] ->
	    not_found;
	[A|R1] ->
	    HeadLen = length(As) - 1 - length(R1),
	    {HeadLen+1, {lists:sublist(As, HeadLen), R1}}
    end.

-spec parse(string()) -> pattern().
parse(S) ->
    Ss = string:split(S, " ", all),
    [parse_ground_pattern(Str)
     || Str <- Ss].

-spec parse_ground_pattern(string()) -> ground_pattern().
parse_ground_pattern(S) ->
    case string:prefix(S, "?*") of
	nomatch ->
	    case string:prefix(S, "?") of
		nomatch ->
		    list_to_atom(S);
		X ->
		    {var, list_to_atom(X)}
	    end;
	Y ->
	    {segvar, list_to_atom(Y)}
    end.

-spec sublis(pattern(), bindings()) -> [atom()].
sublis(Pt, B) ->
    lists:flatmap(fun (P) ->
			  case is_atom(P) of
			      true ->
				  [P];
			      false ->
				  case bindings:lookup(B, var_name(P)) of
				      not_found ->
					  erlang:error({variable_not_found, P});
				      {ok, Val} ->
					  Val
				  end
			  end
		  end,
		  Pt).

-spec var_name(var()) -> varname().
var_name(Var) ->
    element(2, Var).

%% UnitTest
pm2_test() ->
    %% > (pat-match '((?* ?x) a b (?* ?x)) '(1 2 a b a b 1 2 a b))
    %% ((?X 1 2 A B))
    Pt1 = parse("?*x a b ?*x"),
    In1 = parse("1 2 a b a b 1 2 a b"),
    {true,[{x,['1','2',a,b]}]} = pattern_match(Pt1, In1),
    %% > (pat-match '((?* ?p) need (?* ?x))
    %%    '(Mr Hulot and I need a vacation))
    %% ((?P MR HULOT AND I) (?X A VACATION))
    %% > (pat-match '((?* ?x) is a (?* ?y)) '(what he is is a fool))
    %% ((?X WHAT HE IS) (?Y FOOL))
    Pt2 = parse("?*p need ?*x"),
    In2 = parse("Mr Hulot and I need a vacation"),
    {true,[{p, ['Mr','Hulot','and','I']},
	   {x, [a,vacation]}]} = pattern_match(Pt2, In2),
    Pt3 = parse("?*x is a ?*y"),
    In3 = parse("what he is is a fool"),
    {true,[{x, [what,he,is]},{y, [fool]}]} = pattern_match(Pt3,
									   In3),
    %% ?*x should match empty.
    Pt4 = parse("?*x I want to ?*y"),
    In4 = parse("I want to have a vocation"),
    {true,[{x,[]},{y,[have,a,vocation]}]} = pattern_match(Pt4, In4),
    {true, B} = pattern_match(Pt4, In4),
    ['I',want,to,have,a,vocation] = sublis(Pt4, B).
