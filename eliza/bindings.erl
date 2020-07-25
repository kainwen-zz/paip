-module(bindings).

-export([empty/0, lookup/2, extend/3]).

-include("pm2_type.hrl").

-spec empty() -> bindings().
empty() -> [].

-type lookup_result() :: not_found
		       | {ok, val()}.
-spec lookup(bindings(), key()) -> lookup_result().
lookup(B, Key) ->    
    case lists:keysearch(Key, 1, B) of
	false ->
	    not_found;
	{value, {Key, Val}} ->
	    {ok, Val}
    end.

-spec extend(bindings(), key(), val()) -> bindings().
extend(B, K, V) -> [{K, V}|B].
