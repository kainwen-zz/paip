-type varname() :: atom().
-type simple_var() :: {var, varname()}.
-type segment_var() :: {segvar, varname()}.
-type var() :: simple_var()
	     | segment_var().
-type ground_pattern() :: atom()
			| simple_var() 
			| segment_var().
-type pattern() :: [ground_pattern()].

-type key() :: var().
-type val() :: [atom()].
-type bindings() :: [{key(), val()}].
