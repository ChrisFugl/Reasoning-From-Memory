:- module(married_to, [married_to/2]).

:- dynamic fact_married_to/2.

married_to(Person1, Person2) :-
  fact_married_to(Person1, Person2).

married_to(Person1, Person2) :-
  fact_married_to(Person2, Person1).
