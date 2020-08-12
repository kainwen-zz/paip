Definitions.

L = [a-zA-Z]
D = [0-9]

Rules.

[+\-*/]                  : {token, {list_to_atom(TokenChars), TokenLine}}.
\^                       : {token, {list_to_atom(TokenChars), TokenLine}}.
[(,){}.]                 : {token, {list_to_atom(TokenChars), TokenLine}}.
~{D}                     : {token, {number, TokenLine, parse_neg(TokenChars)}}.
~[1-9]{D}*               : {token, {number, TokenLine, parse_neg(TokenChars)}}.
~{D}+[.]{D}+             : {token, {number, TokenLine, parse_neg(TokenChars)}}.
pi                       : {token, {const, TokenLine, pi}}.

~                        : {token, {list_to_atom(TokenChars), TokenLine}}.

diff                     : {token, {list_to_atom(TokenChars), TokenLine}}.
int                      : {token, {list_to_atom(TokenChars), TokenLine}}.

log                      : {token, {list_to_atom(TokenChars), TokenLine}}.
sin                      : {token, {list_to_atom(TokenChars), TokenLine}}.
cos                      : {token, {list_to_atom(TokenChars), TokenLine}}.
exp                      : {token, {list_to_atom(TokenChars), TokenLine}}.

{L}({L}|{D}|_)*          : {token, {symbol, TokenLine, list_to_atom(TokenChars)}}.
{D}                      : {token, {number, TokenLine, list_to_integer(TokenChars)}}.
[1-9]{D}*                : {token, {number, TokenLine, list_to_integer(TokenChars)}}.
{D}+[.]{D}+              : {token, {number, TokenLine, list_to_float(TokenChars)}}.

\t                       : skip_token.
\n                       : skip_token.
\s                       : skip_token.

Erlang code.

parse_neg([$~|TokenChars]) ->
    -list_to_integer(TokenChars).
