:- module(descends_from, [descends_from/2]).

:- use_module(app/relations/child_of/rules, [child_of/2]).

descends_from(Person1, Person2) :-
  fact_descends_from(Person1, Person2).

descends_from(Person1, Person2) :-
  child_of(Person1, Person2).

descends_from(Person1, Person2) :-
  fact_descends_from(Person1, Person1sAncestor),
  descends_from(Person1sAncestor, Person2).
