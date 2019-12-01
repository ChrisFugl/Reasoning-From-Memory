:- module(has_object, [has_object/2]).

:- dynamic fact_has_object/2.

has_object(Person, Object) :-
  fact_has_object(Person, Object).
