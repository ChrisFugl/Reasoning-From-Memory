:- module(has_object_negated, [has_object_negated/2]).

:- dynamic fact_has_object_negated/2.

has_object_negated(Person, Object) :-
  fact_has_object_negated(Person, Object).
