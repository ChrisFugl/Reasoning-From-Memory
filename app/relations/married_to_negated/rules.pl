:- module(married_to_negated, [married_to_negated/2]).

married_to_negated(Person1, Person2) :-
  fact_married_to_negated(Person1, Person2).

married_to_negated(Person1, Person2) :-
  fact_married_to_negated(Person2, Person1).
