-module(sym_math_simplify).

-include("types.hrl").

-export([simplify/1, simplify/2]).

simplify(Code) ->
    Exp = sym_math_parse:scan_and_parse(Code),
    Rules = sym_math_rules:load_rule_from_file(
	      filename:join(filename:dirname(?FILE), "rules")),
    simplify(Exp, Rules).
    
%% simplify(Exp={int_exp, _, _}, Rules) ->
%%     handle_int_exp(Exp, Rules);
simplify(Exp={diff_exp, _, _}, Rules) ->
    handle_diff_exp(Exp, Rules);
simplify(N={number, _}, _Rules) -> N;
simplify(C={const, _}, _Rules) -> C;
simplify(S={symbol, _}, _Rules) -> S;
simplify({binop_exp, Op, E1, E2}, Rules) ->
    SE1 = simplify(E1, Rules),
    SE2 = simplify(E2, Rules),
    SE = {binop_exp, Op, SE1, SE2},
    simp_helper(SE, Rules);
simplify({Tag, E}, Rules) when Tag =:= negative_exp;
			       Tag =:= log_exp;
			       Tag =:= exp_exp;
			       Tag =:= sin_exp;
			       Tag =:= cos_exp ->
    SE = {Tag, simplify(E, Rules)},
    simp_helper(SE, Rules).
    
simp_helper(SE, Rules) ->
    case sym_math_rules:pick_one_rule_and_apply(SE, Rules) of
    	fail ->
    	    case sym_math_util:is_just_simple_arith(SE) of
    		true ->
    		    sym_math_util:eval(SE);
    		false ->
    		    SE
    	    end;
    	{ok, E} ->
	    simplify(E, Rules)
    end.

handle_diff_exp({diff_exp, E, S}, Rules) ->
    SE = {diff_exp, simplify(E, Rules), simplify(S, Rules)},
    simp_helper(SE, Rules).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

simplify_test() ->
    ExpAns = [
	      {"2 + 2", {number,4}},
	      {"5 * 20 + 30 + 7", {number,137}},
	      {"5 * x - (4 + 1) * x", {number,0}},
	      {"y / z * (5 * x - (4 + 1) * x)", {number,0}},
	      {"(4-3) * x + (y / y - 1) * z", {symbol,x}},
	      {"3 * 2 * X", {binop_exp,'*',{number,6},{symbol,'X'}}},
	      {"2 * x * x * 3", {binop_exp,'*',
				 {number,6},
				 {binop_exp,'^',{symbol,x},{number,2}}}},
	      {"2 * x * 3 * y * 4 * z * 5 * 6",
	       {binop_exp,'*',
		{number,720},
		{binop_exp,'*',
		 {binop_exp,'*',{symbol,x},{symbol,y}},
		 {symbol,z}}}},
	      {"2 * x * 3 * x * 4 * (1 / x) * 5 * 6",
	       {binop_exp,'*',{number,720},{symbol,x}}},

	      {"x ^ cos {pi}", {binop_exp,'/',{number,1},{symbol,x}}},
	      {"log{x + x} - log{x}", {number,0.6931471805599453}},
	      
	      {"diff{x+x, x}", {number,2}},
	      {"diff{a*x^2+b*x+c, x}", {binop_exp,'+',
					{binop_exp,'*',
					 {number,2},
					 {binop_exp,'*',{symbol,a},{symbol,x}}},
					{symbol,b}}},
	      
	      {"diff{(a*x^2+b*x+c)/x, x}",
	       {binop_exp,'/',
		{binop_exp,'-',
		 {binop_exp,'*',
		  {binop_exp,'+',
		   {binop_exp,'*',
		    {number,2},
		    {binop_exp,'*',{symbol,a},{symbol,x}}},
		   {symbol,b}},
		  {symbol,x}},
		 {binop_exp,'+',
		  {binop_exp,'+',
		   {binop_exp,'*',
		    {symbol,a},
		    {binop_exp,'^',{symbol,x},{number,2}}},
		   {binop_exp,'*',{symbol,b},{symbol,x}}},
		  {symbol,c}}},
		{binop_exp,'^',{symbol,x},{number,2}}}},

	      {"log{diff{x+x, x}/2}", {number, 0}},

	      {"diff{3*x+cos{x}/x, x}",
	       {binop_exp,'+',
		{binop_exp,'/',
		 {binop_exp,'-',
		  {binop_exp,'*',
		   {negative_exp,{sin_exp,{symbol,x}}},
		   {symbol,x}},
		  {cos_exp,{symbol,x}}},
		 {binop_exp,'^',{symbol,x},{number,2}}},
		{number,3}}},
	      
	      {"diff{3*x^2+2*x+1, x}", {binop_exp,'+',
					{binop_exp,'*',{number,6},{symbol,x}},
					{number,2}}},

	      {"sin{x+x}^2+cos{diff{x^2,x}}^2", {number, 1}},
	      {"sin{x+x} * sin{diff{x^2, x}} + cos{x*2} * cos{x*diff{2*y, y}}",
	       {number, 1}},
	      {"3 + x + x + 4", {binop_exp,'+',
				 {binop_exp,'*',{number,2},{symbol,x}},
				 {number,7}}},
	      
	      {"diff{x*x, x^2}", {number, 1}},
	      {"diff{x^2, a}", {number, 0}}
	     ],
    lists:foreach(fun ({E, A}) ->
			  ?assert(simplify(E) =:= A)
		  end,
		  ExpAns).
-endif.
