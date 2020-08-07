Nonterminals exp term.

Terminals '+' '-' '*' '/' '(' ')' integer symbol. 

Rootsymbol exp.

exp ->
    term :
    '$1'.

exp ->
    exp '+' term :
    {binop_exp, '+', '$1', '$3'}.

exp ->
    exp '-' term :
    {binop_exp, '-', '$1', '$3'}.

term ->
    integer :
    '$1'.

term ->
    symbol :
    '$1'.

term ->
    term '*' term :
    {binop_exp, '*', '$1', '$3'}.

term ->
    term '/' term :
    {binop_exp, '/', '$1', '$3'}.

term ->
    '(' exp ')' :
    {exp_term, '$2'}.
