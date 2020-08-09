-type exp() :: {integer, integer()}
	     | {symbol, atom()}
	     | {binop_exp,
		OP::atom(),
		Arg1::exp(),
		Arg2::exp()}
	     | {negative_exp,
		exp()}
	     | {diff_exp, exp(), {symbol, atom()}}
	     | {int_exp, exp(), {symbol, atom()}}
	     | {log_exp, exp()}
	     | {exp_exp, exp()}
	     | {sin_exp, exp()}
	     | {cos_exp, exp()}.
