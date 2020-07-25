-type varname() :: atom().
-type var() :: {var, varname()}.

-type pattern_ground() :: atom()
			| var().
-type pattern() :: [pattern_ground()].

%% Erlang's cons only produce list, [a|XXX] -> list()
%% Lisp's cons may produce pair: (cons a b) -> (a . b)
%% So in Lisp, you can use pattern (a . ?X) to match (a b c d)
%% and you will get ?X <-> (b, c ,d)
%%
%% In order to make erlang implementation to support this
%% feature, a pattern variable can match a list of ground elements.
%% So in the output bindings, the key is the varible, the value
%% should be a list of atoms. Even for ?X match a single atom a,
%% we have ?X <-> [a].
%% Also, you should only expect this behavior for the variable
%% in the last position of the pattern. 

-type key() :: var().
-type val() :: [atom()].
-type bindings() :: [{var(), val()}].
-type binding_server() :: pid().
