Chapter1&2
==========

Lisp's syntax is quite simple and neat to encode syntax.

Compare the same syntax using erlang and lisp:

```lisp
;; lisp
(defparameter *bigger-grammar*
	'((sentence -> (noun-phrase verb-phrase))
	  (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
	  (verb-phrase -> (Verb noun-phrase PP*))
      (PP* -> () (PP PP*))
      (Adj* -> () (Adj Adj*))
      (PP -> (Prep noun-phrase))
      (Prep -> to in by with on)
      (Adj -> big little blue green adiabatic)
	  (Article -> the a)
      (Name -> Pat Kim Lee Terry Robin)
      (Noun -> man ball woman table)
      (Verb -> hit took saw liked)
      (Pronoun -> he she it these those that)))
```

```erlang
% erlang
Rules = [
	 {sentence, [[noun_phrase, verb_phrase]]},
	 {noun_phrase, [[article, adj_star, noun, pp_star], [name], [pronoun]]},
	 {verb_phrase, [[verb, noun_phrase, pp_star]]},
	 {pp_star, [[], [pp, pp_star]]},
	 {adj_star, [[], [adj, adj_star]]},
	 {pp, [[prep, noun_phrase]]},
	 {prep, [to, in, by, with, on]},
	 {adj, [big, little, blue, green, adiabatic]},
	 {name, [pat, kim, lee, terry, robin]},
	 {article, [the, a]},
	 {noun, [man, ball, woman, table]},
	 {verb, [hit, took, saw, liked]},
	 {pronoun, [he, she, it, these, those, that]}
	],
```

The reason I think is that lisp's syntax does not need `,`. And just for this simple
case, we are generating a word stream, and thus list is the good choice.
