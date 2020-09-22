-module(prolog_parser).

-include("types.hrl").

-export([scan_and_parse_file/1, scan_and_parse/1]).

-spec scan_and_parse(string()) -> {[fact()], [rule()]}.
scan_and_parse(Code) ->
    {ok, Toks, _} = prolog_tok:string(Code),
    parse(Toks, [], []).

-spec scan_and_parse_file(string()) -> {[fact()], [rule()]}.
scan_and_parse_file(Fn) ->
    {ok, Data} = file:read_file(Fn),
    Code = binary_to_list(Data),
    scan_and_parse(Code).

-spec parse([token()], [fact()], [rule()]) -> {[fact()], [rule()]}.
parse([], Facts, Rules) -> {Facts, Rules};
parse(Toks, Facts, Rules) ->
    {Exp, R} = parse_exp(Toks),
    case R of
	['<='|R1] ->
	    {Exps, ['.'|R2]} = parse_multi(fun parse_exp/1, R1, ','),
	    NewRule = {'<=', Exp, Exps},
	    parse(R2, Facts, [NewRule|Rules]);
	['.'|R3] ->
	    NewFact = {fact, Exp}, 
	    parse(R3, [NewFact|Facts], Rules)
    end.

parse_exp([{atom, Relation}, '.'|R]) ->
    Exp = {relation, Relation, []},
    {Exp, R};
parse_exp([{atom, Relation}, '('|R]) ->
    {Args, [')'|R1]} = parse_multi(fun parse_id/1, R, ','),
    {{relation, Relation, Args}, R1}.

parse_id([A={atom, _}|R]) ->
    {A, R};
parse_id([V={var, _}|R]) ->
    {V, R};
parse_id([N={number, _}|R]) ->
    {N, R}.

parse_multi(Fun, Toks, Delim) ->
    parse_multiple_with_delim_helper(Fun, Toks, Delim, []).

parse_multiple_with_delim_helper(Fun, [Delim|Toks], Delim, Acc_list) ->
    parse_multiple_with_delim_helper(Fun, Toks, Delim, Acc_list);
parse_multiple_with_delim_helper(Fun, Toks, Delim, Acc_list) ->
    try Fun(Toks) of
        {Term, R} -> parse_multiple_with_delim_helper(Fun, R, Delim, [Term|Acc_list])
    catch
        _:_ -> {lists:reverse(Acc_list), Toks}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_test() ->
    Code = "
    like(a, b).
    like(a, d).
    like(X, a) :- like(X, cat).
    ",
    ?assert(scan_and_parse(Code) =:= {[{fact,{relation,like,[{atom,a},{atom,d}]}},
				       {fact,{relation,like,[{atom,a},{atom,b}]}}],
				      [{'<=',{relation,like,[{var,'X'},{atom,a}]},
					[{relation,like,[{var,'X'},{atom,cat}]}]}]}).
    
-endif.
