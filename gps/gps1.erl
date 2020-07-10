-module(gps1).

-export([gps1/3, gps1_test/0]).

-include("gps1.hrl").

gps1(Goals, State, Ops) ->
    case gps1(Goals, State, Ops, []) of
	{true, FinalState, Actions} ->
	    case sets:is_subset(sets:from_list(Goals), FinalState) of
		true ->
		    {true, FinalState, Actions};
		false ->
		    {false, State, []}
	    end;
	{false, _, _} ->
	    {false, State, []}
    end.

-spec gps1([goal()], state(), [op()], [action()]) ->
	  {boolean(), state(), [action()]}.
						      
gps1([], State, _Ops, Actions) -> {true, State, Actions};
gps1([Goal|Goals], State, Ops, Actions) ->
    case achieve(Goal, State, Ops, Actions) of
	{true, NewState, NewActions} ->
	    gps1(Goals, NewState, Ops, NewActions);
	{false, _, _} ->
	    {false, State, Actions}
    end.

-spec achieve(goal(), state(), [op()], [action()]) ->
	  {boolean(), state(), [action()]}.
achieve(Goal, State, Ops, Actions) ->
    case sets:is_element(Goal, State) of
	true ->
	    {true, State, Actions};
	false ->
	    PotentialOps = [Op ||
			       Op <- Ops, sets:is_element(Goal, Op#operator.adds)],
	    tryOps(PotentialOps, State, Ops, Actions)
    end.

-spec tryOps([op()], state(), [op()], [action()]) ->
	  {boolean(), state(), [action()]}.
tryOps([], State, _Ops, Actions) -> {false, State, Actions};
tryOps([Op|Pop], State, Ops, Actions) ->
    Pre_conds = sets:to_list(Op#operator.pre_conds),
    case achieve_all(Pre_conds, Op, State, Ops, Actions) of
	{true, NewState, NewActions} ->
	    {true, NewState, NewActions};
	{false, _, _} ->
	    tryOps(Pop, State, Ops, Actions)
    end.

-spec achieve_all([goal()], op(), state(), [op()], [action()]) -> 
	  {boolean(), state(), [action()]}.
achieve_all([], Op, State, _Ops, Actions) ->
    NewState = update_state(State, Op),
    NewActions = [Op#operator.action|Actions],
    {true, NewState, NewActions};
achieve_all([Goal|Goals], Op, State, Ops, Actions) ->
    case achieve(Goal, State, Ops, Actions) of
	{true, NewState, NewActions} ->
	    achieve_all(Goals, Op, NewState, Ops, NewActions);
	{false, _, _} ->
	    {false, State, Actions}
    end.

-spec update_state(state(), op()) -> state().
update_state(State, Op) ->
    sets:union(Op#operator.adds,
	       sets:subtract(State,
			     Op#operator.dels)).


%% UnitTest
%% (defparameter *school-ops*
%%   (list
%%     (make-op :action 'drive-son-to-school
%%       :preconds '(son-at-home car-works)
%%       :add-list '(son-at-school)
%%       :del-list '(son-at-home))
%%     (make-op :action 'shop-installs-battery
%%       :preconds '(car-needs-battery shop-knows-problem shop-has-money)
%%       :add-list '(car-works))
%%     (make-op :action 'tell-shop-problem
%%       :preconds '(in-communication-with-shop)
%%       :add-list '(shop-knows-problem))
%%     (make-op :action 'telephone-shop
%%       :preconds '(know-phone-number)
%%       :add-list '(in-communication-with-shop))
%%     (make-op :action 'look-up-number
%%       :preconds '(have-phone-book)
%%       :add-list '(know-phone-number))
%%     (make-op :action 'give-shop-money
%%       :preconds '(have-money)
%%       :add-list '(shop-has-money)
%%       :del-list '(have-money))))
gps1_test() ->
    Ops = [
	   #operator{action=drive_son_to_school,
		     pre_conds=sets:from_list([son_at_home, car_works]),
		     adds=sets:from_list([son_at_school]),
		     dels=sets:from_list([son_at_home])},
	   #operator{action=shop_installs_battery,
		     pre_conds=sets:from_list([car_needs_battery, shop_knows_problem, shop_has_money]),
		     adds=sets:from_list([car_works]),
		     dels=sets:from_list([])},
	   #operator{action=tell_shop_problem,
		     pre_conds=sets:from_list([in_communication_with_shop]),
		     adds=sets:from_list([shop_knows_problem]),
		     dels=sets:from_list([])},
	   #operator{action=telephone_shop,
		     pre_conds=sets:from_list([know_phone_number]),
		     adds=sets:from_list([in_communication_with_shop]),
		     dels=sets:from_list([])},
	   #operator{action=look_up_number,
		     pre_conds=sets:from_list([have_phone_book]),
		     adds=sets:from_list([know_phone_number]),
		     dels=sets:from_list([])},
	   #operator{action=give_shop_money,
		     pre_conds=sets:from_list([have_money]),
		     adds=sets:from_list([shop_has_money]),
		     dels=sets:from_list([have_money])},
	   %% 	   (push (make-op :action 'ask-phone-number
	   %%   :preconds '(in-communication-with-shop)
	   %%   :add-list '(know-phone-number))
	   %% *school-ops*)
	   #operator{action=ask_phone_number,
		     pre_conds=sets:from_list([in_communication_with_shop]),
		     adds=sets:from_list([know_phone_number])}
	  ],
    State1 = sets:from_list([son_at_home, car_needs_battery, have_money]),
    Goal1 = [son_at_school],
    gps1(Goal1, State1, Ops).
