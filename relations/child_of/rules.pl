:- module(child_of, [child_of/2]).

child_of(Person1, Person2) :-
  fact_child_of(Person1, Person2).
