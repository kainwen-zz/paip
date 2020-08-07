Nonterminals exp term negation_exp.

Terminals '+' '-' '*' '/' '(' ')' '^' integer symbol. 

Rootsymbol exp.

Left 300 '+'.
Left 300 '-'.
Left 400 '*'.
Left 400 '/'.
Right 500 '^'.
Unary 500 negation_exp.

exp ->
    term :
    '$1'.

exp ->
    exp '+' term :
    {binop_exp, '+', '$1', '$3'}.

exp ->
    exp '-' term :
    {binop_exp, '-', '$1', '$3'}.

exp ->
    negation_exp :
    '$1'.

negation_exp ->
    '-' exp :
    {negation_exp, '$2'}.	     

term ->
    integer :
    {integer, _, Int} = '$1',
    {integer, Int}.

term ->
    symbol :
    {symbol, _, S} = '$1',
    {symbol, S}.

term ->
    term '^' term :
    {binop_exp, '^', '$1', '$3'}. 

term ->
    term '*' term :
    {binop_exp, '*', '$1', '$3'}.

term ->
    term '/' term :
    {binop_exp, '/', '$1', '$3'}.

term ->
    '(' exp ')' :
    {exp_term, '$2'}.
