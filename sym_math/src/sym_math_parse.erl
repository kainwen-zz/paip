-module(sym_math_parse).

-export([scan_and_parse/1, scan_and_parse_file/1]).

scan_and_parse_file(Fn) ->
    {ok, Data} = file:read_file(Fn),
    Code = binary_to_list(Data),
    scan_and_parse(Code).

scan_and_parse(Code) ->
    {ok, Toks, _} = sym_math_tok:string(Code),
    {ok, SyntaxTree} = sym_math_grammar:parse(Toks ++ [{'$end', 1}]),
    SyntaxTree.
