Nonterminals expr binop term.

Terminals '+' '-' '*' '/' '~' '(' ')' '{' '}' '^' ',' integer symbol diff int log sin cos exp.

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

expr ->
    diff '{' expr ',' symbol '}' :
    {symbol, _, S} = '$5',
    {diff_exp, '$3', {symbol, S}}.

expr ->
    int '{' expr ',' symbol '}' :
    {symbol, _, S} = '$5',
    {int_exp, '$3', {symbol, S}}.

expr ->
    log '{' expr '}' :
    {log_exp, '$3'}.

expr ->
    exp '{' expr '}' :
    {exp_exp, '$3'}.

expr ->
    sin '{' expr '}' :
    {sin_exp, '$3'}.

expr ->
    cos '{' expr '}' :
    {cos_exp, '$3'}.

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
    '~' term :
    {negative_exp, '$2'}.

term ->
    '(' expr ')' :
     '$2'.

term ->
    symbol '{' expr '}' :
    {symbol, _, S} = '$1',
    {funcall_exp, {symbol, S}, '$3'}.
