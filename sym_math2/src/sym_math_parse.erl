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
		 {binop_exp,'*',{integer,3},{integer,2}},
		 {binop_exp,'*',{integer,2},{integer,3}}}),
    
    ?assert(sym_math_parse:scan_and_parse("3*(2+2)*3") =:=
		{binop_exp,'*',
		 {binop_exp,'*',
		  {integer,3},
		  {binop_exp,'+',{integer,2},{integer,2}}},
		 {integer,3}}),

    ?assert(sym_math_parse:scan_and_parse("~3*x") =:=
		{negative_exp,{binop_exp,'*',{integer,3},{symbol,x}}}),
    
    ?assert(sym_math_parse:scan_and_parse("3+5^~2") =:= 
		{binop_exp,'+',
		 {integer,3},
		 {binop_exp,'^',{integer,5},{negative_exp,{integer,2}}}}),
    
    ?assert(sym_math_parse:scan_and_parse("2 + 2") =:=
		{binop_exp,'+',{integer,2},{integer,2}}),
    
    ?assert(sym_math_parse:scan_and_parse("5 * 20 + 30 + 7") =:=
		{binop_exp,'+',
		 {binop_exp,'+',
		  {binop_exp,'*',{integer,5},{integer,20}},
		  {integer,30}},
		 {integer,7}}),
    
    ?assert(sym_math_parse:scan_and_parse("5 * x - (4 + 1) * x") =:=
		{binop_exp,'-',
		 {binop_exp,'*',{integer,5},{symbol,x}},
		 {binop_exp,'*',
		  {binop_exp,'+',{integer,4},{integer,1}},
		  {symbol,x}}}),
    
    ?assert(sym_math_parse:scan_and_parse("y / z * (5 * x - (4 + 1) * x)") =:=
		{binop_exp,'*',
		 {binop_exp,'/',{symbol,y},{symbol,z}},
		 {binop_exp,'-',
		  {binop_exp,'*',{integer,5},{symbol,x}},
		  {binop_exp,'*',
		   {binop_exp,'+',{integer,4},{integer,1}},
		   {symbol,x}}}}),
    
    ?assert(sym_math_parse:scan_and_parse("(4-3) * x + (y / y - 1) * z") =:=
		{binop_exp,'+',
		 {binop_exp,'*',
		  {binop_exp,'-',{integer,4},{integer,3}},
		  {symbol,x}},
		 {binop_exp,'*',
		  {binop_exp,'-',
		   {binop_exp,'/',{symbol,y},{symbol,y}},
		   {integer,1}},
		  {symbol,z}}}),
    
    ?assert(sym_math_parse:scan_and_parse("1 * f{x} + 0") =:=
		{binop_exp,'+',
		 {binop_exp,'*',
		  {integer,1},
		  {funcall_exp,{symbol,f},{symbol,x}}},
		 {integer,0}}),
    
    ?assert(sym_math_parse:scan_and_parse("3 * 2 * X") =:=
		{binop_exp,'*',
		 {binop_exp,'*',{integer,3},{integer,2}},
		 {symbol,'X'}}).
-endif.
