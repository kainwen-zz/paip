%% A goal is represented as atom in Erlang
-type goal() :: atom().

%% Operator is modeled as a record in Erlang with the 4 fields:
%%   1. action: it is an atom that represent the name
%%   2. pre_conds: all the pre conditions before we can take the operation,
%%                 we use erlang sets here
%%   3. adds: after taking the operations, what state will be achieved (add into
%%            state)
%%   4. dels: after taking the operations, what state will be removed
-type set(T) :: sets:set(T).
-type action() :: atom().
-record(operator, {action::action(),
		   pre_conds::set(goal()),
		   adds::set(goal()),
		   dels::set(goal())}).
-type op() :: #operator{}.

%% State is a set of goal that means we currently
%% have already achieved them
-type state() :: set(goal()).

%% The GPS main routine is to solve the problem given the
%% current state and all operators and the goals. It returns
%% the a tuple, the first element is a bool to indicate is
%% the problem is solved, the second is the actions to achieve
%% the goals if only when it can be solved.
-spec gps1([goal()], state(), [op()]) -> {boolean(), state(), [action()]}.
