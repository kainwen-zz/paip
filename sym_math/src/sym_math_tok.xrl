Definitions.

L = [a-zA-Z]
D = [0-9]

Rules.

[+\-*/]                  : {token, {list_to_atom(TokenChars), TokenLine}}.
\^                       : {token, {list_to_atom(TokenChars), TokenLine}}.
[()]                     : {token, {list_to_atom(TokenChars), TokenLine}}.


{L}({L}|{D}|_)*          : {token, {symbol, TokenLine, list_to_atom(TokenChars)}}.
{D}                      : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
[1-9]{D}*                : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.

\t                       : skip_token.
\n                       : skip_token.
\s                       : skip_token.

Erlang code.
