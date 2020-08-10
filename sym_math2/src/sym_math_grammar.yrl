Nonterminals expr binop term.

Terminals '+' '-' '*' '/' '~' '(' ')' '{' '}' '^' ',' number symbol diff int log sin cos exp.

Rootsymbol expr.

Left 300 '+'.
Left 300 '-'.
Left 400 '*'.
Left 400 '/'.
Right 500 '^'.

expr ->
    term :
    '$1'.

expr ->
    expr '+' term :
    {binop_exp, '+', '$1', '$3'}.

expr ->
    expr '-' term :
    {binop_exp, '-', '$1', '$3'}.

term ->
    diff '{' expr ',' symbol '}' :
    {symbol, _, S} = '$5',
    {diff_exp, '$3', {symbol, S}}.

term ->
    int '{' expr ',' symbol '}' :
    {symbol, _, S} = '$5',
    {int_exp, '$3', {symbol, S}}.

term ->
    log '{' expr '}' :
    {log_exp, '$3'}.

term ->
    exp '{' expr '}' :
    {exp_exp, '$3'}.

term ->
    sin '{' expr '}' :
    {sin_exp, '$3'}.

term ->
    cos '{' expr '}' :
    {cos_exp, '$3'}.

term ->
    number :
    {number, _, N} = '$1',
    {number, N}.

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
    '~' term :
    {negative_exp, '$2'}.

term ->
    '(' expr ')' :
     '$2'.

term ->
    symbol '{' expr '}' :
    {symbol, _, S} = '$1',
    {funcall_exp, {symbol, S}, '$3'}.
