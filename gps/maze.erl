-module(maze).

-export([create_maze_ops/0]).

create_maze_ops() ->
    Config = [{1,2},{2,3},{3,4},{4,9},{9,14},{9,8},{8,7},{7,12},{12,13},
	      {12,11},{11,6},{11,16},{16,17},{17,22},{21,22},{22,23},
	      {23,18},{23,24},{24,19},{19,20},{20,15},{15,10},{10,5},{20,25}],
    [create_maze_op(A, B)
     || {A, B} <- Config] ++
    [create_maze_op(B, A)
     || {A, B} <- Config].

create_maze_op(A, B) ->
    SA = integer_to_list(A),
    SB = integer_to_list(B),
    ActionName = gen_action_name(SA, SB),
    PreCondName = gen_at_name(SA),
    AddName = gen_at_name(SB),
    DelName = PreCondName,
    gps:create_op(ActionName,
		  [PreCondName],
		  [AddName],
		  [DelName]).

gen_action_name(A, B) ->
    list_to_atom(
      string:join(["move", "from", A, "to", B], "_")).

gen_at_name(A) ->
    list_to_atom(
      string:join(["at", A], "_")).

%% gps:gps(['at_1'], ['at_25'], maze:create_maze_ops()).
%% {true,[at_25],
%%       [{operator,move_from_20_to_25,[at_20],[at_25],[at_20]},
%%        {operator,move_from_19_to_20,[at_19],[at_20],[at_19]},
%%        {operator,move_from_24_to_19,[at_24],[at_19],[at_24]},
%%        {operator,move_from_23_to_24,[at_23],[at_24],[at_23]},
%%        {operator,move_from_22_to_23,[at_22],[at_23],[at_22]},
%%        {operator,move_from_17_to_22,[at_17],[at_22],[at_17]},
%%        {operator,move_from_16_to_17,[at_16],[at_17],[at_16]},
%%        {operator,move_from_11_to_16,[at_11],[at_16],[at_11]},
%%        {operator,move_from_12_to_11,[at_12],[at_11],[at_12]},
%%        {operator,move_from_7_to_12,[at_7],[at_12],[at_7]},
%%        {operator,move_from_8_to_7,[at_8],[at_7],[at_8]},
%%        {operator,move_from_9_to_8,[at_9],[at_8],[at_9]},
%%        {operator,move_from_4_to_9,[at_4],[at_9],[at_4]},
%%        {operator,move_from_3_to_4,[at_3],[at_4],[at_3]},
%%        {operator,move_from_2_to_3,[at_2],[at_3],[at_2]},
%%        {operator,move_from_1_to_2,[at_1],[at_2],[at_1]}]}
