-module(bindings).

-behavior(gen_server).

-include("pm_type.hrl").

-export([start/0, stop/1, lookup/2, extend/3, dump/1]).
-export([init/1, handle_call/3, handle_cast/2]).


%% APIs
-spec start() -> binding_server().
start() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    Pid.

-spec stop(binding_server()) -> ok.
stop(B) ->
    gen_server:stop(B).

-spec dump(binding_server()) -> bindings().
dump(B) ->
    gen_server:call(B, {dump}).

-type lookup_result() :: not_found
		       | {ok, [atom()]}.
-spec lookup(binding_server(), var()) -> lookup_result().
lookup(B, Var) ->
    gen_server:call(B, {lookup, Var}).

-spec extend(binding_server(), var(), [atom()]) -> ok.
extend(B, Var, Val) ->
    gen_server:call(B, {extend, Var, Val}).

%% Callbacks
-spec init(term()) -> {ok, bindings()}.
init([]) ->
    {ok, []}.

handle_call({lookup, Var}, _From, Bindings) ->
    case lists:keysearch(Var, 1, Bindings) of
	false ->
	    {reply, not_found, Bindings};
	{value, {Var, Val}} ->
	    {reply, {ok, Val}, Bindings}
    end;
handle_call({extend, Var, Val}, _From, Bindings) ->
    NewBindings = [{Var, Val} | Bindings],
    {reply, ok, NewBindings};
handle_call({dump}, _From, Bindings) ->
    {reply, Bindings, Bindings}.

handle_cast(_, _) -> {noreply, ok}.
