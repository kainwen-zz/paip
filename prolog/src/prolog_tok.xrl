Definitions.

L = [a-zA-Z]
D = [0-9]

Rules.

:-                       : {token, '<='}.

{L}({L}|{D}|_)*          : {token, parse_identifier(TokenChars)}.
[(),.]                   : {token, list_to_atom(TokenChars)}.
{D}                      : {token, {number, list_to_integer(TokenChars)}}.
[1-9]{D}*                : {token, {number, list_to_integer(TokenChars)}}.
{D}+[.]{D}+              : {token, {number, list_to_float(TokenChars)}}.

\t                       : skip_token.
\n                       : skip_token.
\s                       : skip_token.

Erlang code.

parse_identifier(Tchars) ->
    case Tchars of
	[Hd|_] when Hd >= $A, Hd =< $Z -> 
	    {var, list_to_atom(Tchars)};
	[Hd|_] when Hd >= $a, Hd =< $z -> 
	    {atom, list_to_atom(Tchars)}
    end.
