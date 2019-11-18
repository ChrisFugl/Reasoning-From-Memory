:- module(related_to_negated, [related_to_negated/2]).

:- use_module(relations/child_of/rules, [child_of/2]).
:- use_module(relations/child_of_negated/rules, [child_of_negated/2]).
:- use_module(relations/descends_from_negated/rules, [descends_from_negated/2]).
:- use_module(relations/married_to_negated/rules, [married_to_negated/2]).

related_to_negated(Person1, Person2) :-
  fact_related_to_negated(Person1, Person2).

related_to_negated(Person1, Person2) :-
  fact_related_to_negated(Person2, Person1).

related_to_negated(Person1, Person2) :-
  % not child/parent
  child_of_negated(Person1, Person2),
  child_of_negated(Person2, Person1),
  % not married
  married_to_negated(Person1, Person2),
  % not descendant/ancestor
  descends_from_negated(Person1, Person2),
  descends_from_negated(Person2, Person1),
  % not siblings
  siblings_negated(Person1, Person2).

siblings_negated(Person1, Person2) :-
  child_of_negated(Person1, Parent),
  child_of(Person2, Parent).

siblings_negated(Person1, Person2) :-
  child_of(Person1, Parent),
  child_of_negated(Person2, Parent).
