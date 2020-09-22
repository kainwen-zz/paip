-type arg() :: {number, number()}
	     | {atom, atom()}
	     | {var, atom()}.

-type clause() :: {relation, atom(), [arg()]}.
-type head() :: clause().
-type fact() :: {fact, head()}.
-type rule() :: {'<=', head(), [clause()]}.

-type token() :: {number, number()}
	       | '<='
	       | '('
	       | ')'
	       | ','
	       | '.'.
