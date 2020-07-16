-module(gps).

-export([gps/3,
	 create_op/4,
	 create_school_ops/0, create_banana_ops/0]).

%% Goal is just an atom in Erlang

%% A state is a set of goals, we use
%% list to implement state.

%% Action name is also an atom.
%%
%% Operator is a record. The effect
%% of it is modeled as three lists:
%% pre_conds, adds and dels.
-type goal() :: atom().
-type state() :: [goal()].
-type action() :: atom().
-record(operator, {action::action(),
		   pre_conds::[goal()],
		   adds::[goal()],
		   dels::[goal()]}).
-type op() :: #operator{}.

%% Runtime context of subgoals is a stack
%% of goals, we implement stack via list
%% here.
-type stack() :: [goal()].
-type context () :: stack().

-spec create_op(Action::action(),
		PreConds::[goal()],
		Adds::[goal()],
		Dels::[goal()]) -> op().
create_op(Action, PreConds, Adds, Dels) ->
    #operator{action=Action, pre_conds=PreConds,
	      adds=Adds, dels=Dels}.
%%
-spec gps(InitState::state(),
	  Goals    ::[goal()],
	  OpsRepo  ::[op()]) -> {Result::boolean(),
				 State::state(),
				 ActionList::[op()]}.
gps(InitState, Goals, OpsRepo) ->
    Context = empty_context(), %% Context is to solve recurrent subgoal issue
    Actions = [], %% Record each operations is to solve THE LEAPING BEFORE YOU LOOK PROBLEM
    achieve_all(InitState, Goals, OpsRepo, Actions, Context).

-spec achieve_all(State::state(),
		  Goals::[goal()],
		  OpsRepo::[op()],
		  Actions::[op()],
		  Context::context()) -> {Result::boolean(),
					  NewState::state(),
					  NewActions::[op()]}.
achieve_all(State, Goals, OpsRepo, Actions, Context) ->
    case achieve_all_internal(State, Goals, OpsRepo, Actions, Context) of
	{true, NewState, NewActions} ->
	    %% We have to check when finish the last subgoal
	    %% This is to solve the THE CLOBBERED SIBLING GOAL PROBLEM
	    %% And this is why we wrap it arond xxx_internal
	    case is_subset(Goals, NewState) of
		true ->
		    {true, NewState, NewActions};
		false ->
		    {false, State, Actions}
	    end;
	{false, _, _} ->
	    {false, State, Actions}
    end.

-spec achieve_all_internal(State::state(),
			   Goals::[goal()],
			   OpsRepo::[op()],
			   Actions::[op()],
			   Context::context()) -> {Result::boolean(),
						   NewState::state(),
						   NewActions::[op()]}.
achieve_all_internal(State, [], _OpsRepo, Actions, _Context) -> {true, State, Actions};
achieve_all_internal(State, [Goal|Goals], OpsRepo, Actions, Context) ->
    case achieve_single_goal(State, Goal, OpsRepo, Actions, Context) of
	{true, NewState, NewActions} ->
	    achieve_all_internal(NewState, Goals, OpsRepo, NewActions, Context);
	{false, _, _} ->
	    {false, State, Actions}
    end.

-spec achieve_single_goal(State::state(),
			  Goal::goal(),
			  OpsRepo::[op()],
			  Actions::[op()],
			  Context::context()) -> {Result::boolean(),
						  NewState::state(),
						  NewActions::[op()]}.
achieve_single_goal(State, Goal, OpsRepo, Actions, Context) ->
    case lists:member(Goal, State) of
	true ->
	    {true, State, Actions};
	false ->
	    case context_contain(Goal, Context) of
		true ->
		    %% recurrent subgoal!
		    {false, State, Actions};
		false ->
		    NewContext = push_context(Goal, Context),
		    PotentialOps = [Op ||
				       Op <- OpsRepo,
				       lists:member(Goal,
						    Op#operator.adds)],
		    tryOps(PotentialOps, State, Goal, OpsRepo, Actions, NewContext)
	    end
    end.

-spec tryOps(POps::[op()],
	     State::state(),
	     Goal::goal(),
	     OpsRepo::[op()],
	     Actions::[op()],
	     Context::context()) -> {Result::boolean(),
				     NewState::state(),
				     NewActions::[op()]}.
tryOps([], State, _Goal, _OpsRepo, Actions, _Context) -> {false, State, Actions};
tryOps([Op|POps], State, Goal, OpsRepo, Actions, Context) ->
    Pre_conds = Op#operator.pre_conds,
    case achieve_all(State, Pre_conds, OpsRepo, Actions, Context) of
	{false, _, _} ->
	    tryOps(POps, State, Goal, OpsRepo, Actions, Context);
	{true, NewState, NewActions} ->
	    {true, update_state(NewState, Op), [Op|NewActions]}
    end.

%% Internal helper functions
-spec update_state(state(), op()) -> state().
update_state(State, Op) ->
    S1 = add_with_unique(State, Op#operator.adds),
    del_with_unique(S1, Op#operator.dels).

-spec add_with_unique(state(), [goal()]) -> state().
add_with_unique(State, []) -> State;
add_with_unique(State, [Add|Adds]) ->
    case lists:member(Add, State) of
	true ->
	    add_with_unique(State, Adds);
	false ->
	    add_with_unique([Add|State], Adds)
    end.

-spec del_with_unique(state(), [goal()]) -> state().
del_with_unique(State, []) -> State;
del_with_unique(State, [Del|Dels]) ->
    del_with_unique(lists:delete(Del, State), Dels).    

-spec is_subset([goal()], state()) -> boolean().
is_subset(Goals, State) -> lists:all(fun (Goal) ->
					     lists:member(Goal, State)
				     end,
				     Goals).

%% Internal Functions for Context
-spec empty_context() -> context().
empty_context() -> [].

-spec push_context(goal(), context()) -> context().
push_context(Goal, Context) -> [Goal|Context].

-spec context_contain(goal(), context()) -> boolean().
context_contain(Goal, Context) ->
    lists:member(Goal, Context).

%% (defparameter *school_ops*
%%   (list
%%     (make_op :action 'drive_son_to_school
%%       :preconds '(son_at_home car_works)
%%       :add_list '(son_at_school)
%%       :del_list '(son_at_home))
%%     (make_op :action 'shop_installs_battery
%%       :preconds '(car_needs_battery shop_knows_problem shop_has_money)
%%       :add_list '(car_works))
%%     (make_op :action 'tell_shop_problem
%%       :preconds '(in_communication_with_shop)
%%       :add_list '(shop_knows_problem))
%%     (make_op :action 'telephone_shop
%%       :preconds '(know_phone_number)
%%       :add_list '(in_communication_with_shop))
%%     (make_op :action 'look_up_number
%%       :preconds '(have_phone_book)
%%       :add_list '(know_phone_number))
%%     (make_op :action 'give_shop_money
%%       :preconds '(have_money)
%%       :add_list '(shop_has_money)
%%       :del_list '(have_money))))

create_school_ops() ->
    [
     create_op(drive_son_to_school,
	       [son_at_home, car_works],
	       [son_at_school],
	       [son_at_home]),
     create_op(shop_installs_battery,
	       [car_needs_battery, shop_knows_problem, shop_has_money],
	       [car_works],
	       []),
     create_op(tell_shop_problem,
	       [in_communication_with_shop],
	       [shop_knows_problem],
	       []),
     create_op(telephone_shop,
	       [know_phone_number],
	       [in_communication_with_shop],
	       []),
     create_op(look_up_number,
	       [have_phone_book],
	       [know_phone_number],
	       []),
     create_op(give_shop_money,
	       [have_money],
	       [shop_has_money],
	       [have_money])
    ].

%% (defparameter *banana_ops*
%%   (list
%%     (op
%%       'climb_on_chair
%%       :preconds '(chair_at_middle_room at_middle_room on_floor)
%%       :add_list '(at_bananas on_chair)
%%       :del_list '(at_middle_room on_floor))
%%     (op 
%%       'push_chair_from_door_to_middle_room
%%       :preconds '(chair_at_door at_door)
%%       :add_list '(chair_at_middle_room at_middle_room)
%%       :del_list '(chair_at_door at_door))
%%     (op 
%%       'walk_from_door_to_middle_room
%%       :preconds '(at_door on_floor)
%%       :add_list '(at_middle_room)
%%       :del_list '(at_door))
%%     (op 
%%       'grasp_bananas
%%       :preconds '(at_bananas empty_handed)
%%       :add_list '(has_bananas)
%%       :del_list '(empty_handed))
%%     (op 
%%       'drop_ball
%%       :preconds '(has_ball)
%%       :add_list '(empty_handed)
%%       :del_list '(has_ball))
%%     (op 
%%       'eat_bananas
%%       :preconds '(has_bananas)
%%       :add_list '(empty_handed not_hungry)
%%       :del_list '(has_bananas hungry))))
create_banana_ops() ->
    [
     create_op(climb_on_chair,
	       [chair_at_middle_room, at_middle_room, on_floor],
	       [at_bananas, on_chair],
	       [at_middle_room, on_floor]),
     create_op(push_chair_from_door_to_middle_room,
	       [chair_at_door, at_door],
	       [chair_at_middle_room, at_middle_room],
	       [chair_at_door, at_door]),	 
     create_op(walk_from_door_to_middle_room,
	       [at_door, on_floor],
	       [at_middle_room],
	       [at_door]),
     create_op(grasp_bananas,
	       [at_bananas, empty_handed],
	       [has_bananas],
	       [empty_handed]),
     create_op(drop_ball,
	       [has_ball],
	       [empty_handed],
	       [has_ball]),
     create_op(eat_bananas,
	       [has_bananas],
	       [empty_handed, not_hungry],
	       [has_bananas, hungry])
    ].
