:- module(child_of_negated, [child_of_negated/2]).

child_of_negated(Person1, Person2) :-
  fact_child_of_negated(Person1, Person2).
