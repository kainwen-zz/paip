-type exp() :: {const, pi} 
	     | {number, number()}
	     | {symbol, atom()}
	     | {binop_exp,
		OP::atom(),
		Arg1::exp(),
		Arg2::exp()}
	     | {negative_exp,
		exp()}
	     | {diff_exp, exp(), exp()}
	     | {int_exp, exp(), exp()}
	     | {log_exp, exp()}
	     | {exp_exp, exp()}
	     | {sin_exp, exp()}
	     | {cos_exp, exp()}.

-type rule() :: {Pattern::exp(),
		 Response::exp(),
		 Guard::string()}.

-type key() :: atom().
-type value() :: exp().
-type bindings() :: [{key(), value()}].
