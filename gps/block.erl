-module(block).

-export([create_block_ops/1, test/0]).

create_block_ops(Blocks) ->
    Ops1 = [gen_move_from_to_op(A, table, B)
	    || A <- Blocks, B <- Blocks, A /= B
	   ],
    Ops2 = [gen_move_from_to_op(A, B, table)
	    || A <- Blocks, B <- Blocks, A /= B
	   ],
    Ops3 = [gen_move_from_to_op(A, B, C)
	    || A <- Blocks, B <- Blocks, C <- Blocks, A /= B, B /=C, A /= C],
    Ops1 ++ Ops2 ++ Ops3.

gen_move_from_to_op(A, B, C) ->
    Sa = atom_to_list(A),
    Sb = atom_to_list(B),
    Sc = atom_to_list(C),
    ActionName = list_to_atom(string:join(["move", Sa, "from", Sb, "to", Sc], "_")),
    PreConds = [list_to_atom(string:join(["space", "on", X], "_"))
		|| X <- [Sa, Sc]] ++ [list_to_atom(string:join([Sa, "on", Sb], "_"))],
    Adds = case B of
	       table ->
		   [list_to_atom(string:join([Sa, "on", Sc], "_"))];
	       _ ->
		   [list_to_atom(string:join([Sa, "on", Sc], "_")),
		    list_to_atom(string:join(["space", "on", Sb], "_"))]
	   end,
    Dels = case B of
	       table ->
		   [list_to_atom(string:join([Sa, "on", Sb], "_"))];
	       _ ->
		   [list_to_atom(string:join([Sa, "on", Sb], "_")),
		    list_to_atom(string:join(["space", "on", Sc], "_"))]
	   end,
    gps:create_op(ActionName, PreConds, Adds, Dels).

test() ->
    gps:gps([a_on_b, b_on_table, space_on_a, space_on_table],
	    [b_on_a],
	    block:create_block_ops([a, b])).
