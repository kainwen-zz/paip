-module(eliza).

-export([eliza/0, eliza/1]).

-include("pm2_type.hrl").

-record(rule, {pattern::pattern(),
	       response::[pattern()]}).
-type rule() :: #rule{}.

eliza() ->
    Rules = eliza_rule(),
    eliza(Rules).

eliza(Rules) ->
    Input = pm2:parse(string:trim(io:get_line('>: '), both)),
    Output = random_pick_response(Rules, Input),
    io:format("~s~n", [(string:join([atom_to_list(T) || T <- Output], " "))]),
    eliza(Rules).

-spec eliza_rule() -> [rule()].
eliza_rule() ->
    [
     make_rule(pm2:parse("?*x hello ?*y"),
	       [pm2:parse("How do you do. Please state your problem.")]),
     make_rule(pm2:parse("?*x I want ?*y"),
	       [pm2:parse("What would it mean if you got ?y"),
		pm2:parse("Why do you want ?y"),
		pm2:parse("Suppose you got ?y soon")]),
     make_rule(pm2:parse("?*x if ?*y"),
	       [pm2:parse("Do you really think its likely that ?y"),
		pm2:parse("Do you wish that ?y"),
		pm2:parse("What do you think about ?y"),
		pm2:parse("Really-- if ?y")]),
     make_rule(pm2:parse("?*x no ?*y"),
	       [pm2:parse("Why not"),
		pm2:parse("You are being a bit negative"),
		pm2:parse("Are you saying \"NO\" just to be negative")]),
     make_rule(pm2:parse("?*x I was ?*y"),
	       [pm2:parse("Were you really"),
		pm2:parse("Perhaps I already knew you were ?y"),
		pm2:parse("Why do you tell me you were ?y now")]),
     make_rule(pm2:parse("?*x I feel ?*y"),
	       [pm2:parse("Do you often feel ?y")]),
     make_rule(pm2:parse("?*x I felt ?*y"),
	       [pm2:parse("What other feelings do you have?")])
    ].

-spec make_rule(pattern(), [pattern()]) -> rule().
make_rule(Pt, Res) ->
    #rule{pattern=Pt, response=Res}.

-spec switch_viewpoint([atom()]) -> [atom()].
switch_viewpoint(Words) ->
    [replace_word(Word)
     || Word <- Words
    ].

-spec replace_word(atom()) -> atom().
replace_word(Word) ->
    % ((I . you) (you . I) (me . you) (am . are))
    L = [
	 {'I', 'you'},
	 {'you', 'I'},
	 {'me', 'you'},
	 {'am', 'are'}
	],
    case lists:keysearch(Word, 1, L) of
	false ->
	    Word;
	{value, {Word, Rep}} ->
	    Rep
    end.

-spec random_ele([A]) -> A.
random_ele(Res) when length(Res) > 0 ->
    lists:nth(rand:uniform(length(Res)), Res).

-spec random_pick_response([rule()], [atom()]) -> [atom()].
random_pick_response(Rule, Input) ->
    Rs = get_all_response(Rule, Input),
    {Res, B} = random_ele(Rs),
    R = random_ele(Res),
    pm2:sublis(switch_viewpoint(R), B).

-spec get_all_response([rule()], [atom()]) -> [{[pattern()], bindings()}].
get_all_response([], _Input) -> [];
get_all_response([#rule{pattern=Pt, response=Res}|Rules], Input) -> 
    case pm2:pattern_match(Pt, Input) of
	{true, B} ->
	    [{Res, B}|get_all_response(Rules, Input)];
	{false, []} ->
	    get_all_response(Rules, Input)
    end.

%% Demo run
%% >: hello there
%% hello there
%% How do I do. Please state your problem.
%% >: I want to test this program
%% I want to test this program
%% Suppose I got to test this program soon
%% >: I could see if it works
%% I could see if it works
%% Really-- if it works
%% >: no not really
%% no not really
%% Why not
%% >: forget it-- I was wondering how general the program is
%% forget it-- I was wondering how general the program is
%% Were I really
%% >: I felt like it
%% I felt like it
%% What other feelings do I have?
%% >: I feel this is enough
%% I feel this is enough
%% Do I often feel this is enough
%% >: 
