-module(sym_math_parse).

-include("types.hrl").

-export([scan_and_parse/1]).

-spec scan_and_parse(string()) -> exp().
scan_and_parse(Code) ->
    {ok, Toks, _} = sym_math_tok:string(Code),
    {ok, SyntaxTree} = sym_math_grammar:parse(Toks ++ [{'$end', 1}]),
    SyntaxTree.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
parse_test() ->
    ?assert(sym_math_parse:scan_and_parse("3*2+2*3") =:=
		{binop_exp,'+',
		 {binop_exp,'*',{number,3},{number,2}},
		 {binop_exp,'*',{number,2},{number,3}}}),
    
    ?assert(sym_math_parse:scan_and_parse("3*(2+2)*3") =:=
		{binop_exp,'*',
		 {binop_exp,'*',
		  {number,3},
		  {binop_exp,'+',{number,2},{number,2}}},
		 {number,3}}),

    ?assert(sym_math_parse:scan_and_parse("~3*x") =:=
		{negative_exp,{binop_exp,'*',{number,3},{symbol,x}}}),
    
    ?assert(sym_math_parse:scan_and_parse("3+5^~2") =:= 
		{binop_exp,'+',
		 {number,3},
		 {binop_exp,'^',{number,5},{negative_exp,{number,2}}}}),
    
    ?assert(sym_math_parse:scan_and_parse("2 + 2") =:=
		{binop_exp,'+',{number,2},{number,2}}),
    
    ?assert(sym_math_parse:scan_and_parse("5 * 20 + 30 + 7") =:=
		{binop_exp,'+',
		 {binop_exp,'+',
		  {binop_exp,'*',{number,5},{number,20}},
		  {number,30}},
		 {number,7}}),
    
    ?assert(sym_math_parse:scan_and_parse("5 * x - (4 + 1) * x") =:=
		{binop_exp,'-',
		 {binop_exp,'*',{number,5},{symbol,x}},
		 {binop_exp,'*',
		  {binop_exp,'+',{number,4},{number,1}},
		  {symbol,x}}}),
    
    ?assert(sym_math_parse:scan_and_parse("y / z * (5 * x - (4 + 1) * x)") =:=
		{binop_exp,'*',
		 {binop_exp,'/',{symbol,y},{symbol,z}},
		 {binop_exp,'-',
		  {binop_exp,'*',{number,5},{symbol,x}},
		  {binop_exp,'*',
		   {binop_exp,'+',{number,4},{number,1}},
		   {symbol,x}}}}),
    
    ?assert(sym_math_parse:scan_and_parse("(4-3) * x + (y / y - 1) * z") =:=
		{binop_exp,'+',
		 {binop_exp,'*',
		  {binop_exp,'-',{number,4},{number,3}},
		  {symbol,x}},
		 {binop_exp,'*',
		  {binop_exp,'-',
		   {binop_exp,'/',{symbol,y},{symbol,y}},
		   {number,1}},
		  {symbol,z}}}),
    
    ?assert(sym_math_parse:scan_and_parse("1 * f{x} + 0") =:=
		{binop_exp,'+',
		 {binop_exp,'*',
		  {number,1},
		  {funcall_exp,{symbol,f},{symbol,x}}},
		 {number,0}}),
    
    ?assert(sym_math_parse:scan_and_parse("3 * 2 * X") =:=
		{binop_exp,'*',
		 {binop_exp,'*',{number,3},{number,2}},
		 {symbol,'X'}}),
    
    ?assert(sym_math_parse:scan_and_parse("2 * x * 3 * x * 4 * (l / x) * 5 * 6") =:=
		{binop_exp,'*',
		 {binop_exp,'*',
		  {binop_exp,'*',
		   {binop_exp,'*',
		    {binop_exp,'*',
		     {binop_exp,'*',
		      {binop_exp,'*',{number,2},{symbol,x}},
		      {number,3}},
		     {symbol,x}},
		    {number,4}},
		   {binop_exp,'/',{symbol,l},{symbol,x}}},
		  {number,5}},
		 {number,6}}),
    
    ?assert(sym_math_parse:scan_and_parse("diff{x+x, x}") =:=
		{diff_exp,{binop_exp,'+',{symbol,x},{symbol,x}},{symbol,x}}),
    
    ?assert(sym_math_parse:scan_and_parse("diff{a*x^2+b*x+c, x}") =:=
		{diff_exp,{binop_exp,'+',
			   {binop_exp,'+',
			    {binop_exp,'*',
			     {symbol,a},
			     {binop_exp,'^',{symbol,x},{number,2}}},
			    {binop_exp,'*',{symbol,b},{symbol,x}}},
			   {symbol,c}},
		 {symbol,x}}),
    
    ?assert(sym_math_parse:scan_and_parse("diff{(a*x^2+b*x+c)/x, x}") =:=
		{diff_exp,{binop_exp,'/',
			   {binop_exp,'+',
			    {binop_exp,'+',
			     {binop_exp,'*',
			      {symbol,a},
			      {binop_exp,'^',{symbol,x},{number,2}}},
			     {binop_exp,'*',{symbol,b},{symbol,x}}},
			    {symbol,c}},
			   {symbol,x}},
		 {symbol,x}}),
    
    ?assert(sym_math_parse:scan_and_parse("log{diff{x+x, x} / 2}") =:=
		{log_exp,{binop_exp,'/',
			  {diff_exp,{binop_exp,'+',{symbol,x},{symbol,x}},{symbol,x}},
			  {number,2}}}),
    
    ?assert(sym_math_parse:scan_and_parse("log{x + x} - log{x}") =:=
		{binop_exp,'-',
		 {log_exp,{binop_exp,'+',{symbol,x},{symbol,x}}},
		 {log_exp,{symbol,x}}}),
    
    ?assert(sym_math_parse:scan_and_parse("x ^ cos {pi}") =:=
		{binop_exp,'^',{symbol,x},{cos_exp,{symbol,pi}}}),
    
    ?assert(sym_math_parse:scan_and_parse("diff{3*x+cos{x}/x, x}") =:=
		{diff_exp,{binop_exp,'+',
			   {binop_exp,'*',{number,3},{symbol,x}},
			   {binop_exp,'/',{cos_exp,{symbol,x}},{symbol,x}}},
		 {symbol,x}}),
    
    ?assert(sym_math_parse:scan_and_parse("diff{cos{x}/x,x}") =:=
		{diff_exp,{binop_exp,'/',{cos_exp,{symbol,x}},{symbol,x}},
		 {symbol,x}}),
    
    ?assert(sym_math_parse:scan_and_parse("diff{3*x^2+2*x+1, x}") =:=
		{diff_exp,{binop_exp,'+',
			   {binop_exp,'+',
			    {binop_exp,'*',
			     {number,3},
			     {binop_exp,'^',{symbol,x},{number,2}}},
			    {binop_exp,'*',{number,2},{symbol,x}}},
			   {number,1}},
		 {symbol,x}}),
    
    ?assert(sym_math_parse:scan_and_parse("sin{x + x}^2 + cos{diff{x^2, x}}^2") =:=
		{binop_exp,'+',
		 {binop_exp,'^',
		  {sin_exp,{binop_exp,'+',{symbol,x},{symbol,x}}},
		  {number,2}},
		 {binop_exp,'^',
		  {cos_exp,{diff_exp,{binop_exp,'^',{symbol,x},{number,2}},
			    {symbol,x}}},
		  {number,2}}}),
    
    ?assert(sym_math_parse:scan_and_parse("sin{x + x} * sin{diff{x^2,x}} + cos{2 * x} * cos{x * diff{2*y, y}}") =:=
		{binop_exp,'+',
		 {binop_exp,'*',
		  {sin_exp,{binop_exp,'+',{symbol,x},{symbol,x}}},
		  {sin_exp,{diff_exp,{binop_exp,'^',{symbol,x},{number,2}},
			    {symbol,x}}}},
		 {binop_exp,'*',
		  {cos_exp,{binop_exp,'*',{number,2},{symbol,x}}},
		  {cos_exp,{binop_exp,'*',
			    {symbol,x},
			    {diff_exp,{binop_exp,'*',{number,2},{symbol,y}},
			     {symbol,y}}}}}}),
    
    ?assert(sym_math_parse:scan_and_parse("int{x*sin{x^2}, x}") =:=
		{int_exp,{binop_exp,'*',
			  {symbol,x},
			  {sin_exp,{binop_exp,'^',{symbol,x},{number,2}}}},
		 {symbol,x}}),
    
    ?assert(sym_math_parse:scan_and_parse("int{3*x^3-1/(3*x^3), x}") =:=
		{int_exp,{binop_exp,'-',
			  {binop_exp,'*',
			   {number,3},
			   {binop_exp,'^',{symbol,x},{number,3}}},
			  {binop_exp,'/',
			   {number,1},
			   {binop_exp,'*',
			    {number,3},
			    {binop_exp,'^',{symbol,x},{number,3}}}}},
		 {symbol,x}}),
    
    ?assert(sym_math_parse:scan_and_parse("int{(3*x+2)^(~2/3), x}") =:=
		{int_exp,{binop_exp,'^',
			  {binop_exp,'+',
			   {binop_exp,'*',{number,3},{symbol,x}},
			   {number,2}},
			  {negative_exp,{binop_exp,'/',{number,2},{number,3}}}},
		 {symbol,x}}),
    
    ?assert(sym_math_parse:scan_and_parse("int{sin{x}/(1+cos{x}), x}") =:=
		{int_exp,{binop_exp,'/',
			  {sin_exp,{symbol,x}},
			  {binop_exp,'+',{number,1},{cos_exp,{symbol,x}}}},
		 {symbol,x}}),
    
    ?assert(sym_math_parse:scan_and_parse("int{(2*x+1)/(x^2+x-1), x}") =:=
		{int_exp,{binop_exp,'/',
			  {binop_exp,'+',
			   {binop_exp,'*',{number,2},{symbol,x}},
			   {number,1}},
			  {binop_exp,'-',
			   {binop_exp,'+',
			    {binop_exp,'^',{symbol,x},{number,2}},
			    {symbol,x}},
			   {number,1}}},
		 {symbol,x}}),
    
    ?assert(sym_math_parse:scan_and_parse("int{8*x^2/(x^3+2)^3, x}") =:=
		{int_exp,{binop_exp,'/',
			  {binop_exp,'*',
			   {number,8},
			   {binop_exp,'^',{symbol,x},{number,2}}},
			  {binop_exp,'^',
			   {binop_exp,'+',
			    {binop_exp,'^',{symbol,x},{number,3}},
			    {number,2}},
			   {number,3}}},
		 {symbol,x}}),
    
    ?assert(sym_math_parse:scan_and_parse("log{2.718}") =:= {log_exp,{number,2.718}}).
-endif.
