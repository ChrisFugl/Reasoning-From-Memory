:- module(related_to, [related_to/2]).

:- use_module(relations/child_of/rules, [child_of/2]).
:- use_module(relations/married_to/rules, [married_to/2]).
:- use_module(relations/descends_from/rules, [descends_from/2]).

related_to(Person1, Person2) :-
  fact_related_to(Person1, Person2).

related_to(Person1, Person2) :-
  fact_related_to(Person2, Person1).

% child/parent
related_to(Person1, Person2) :-
  child_of(Person1, Person2).

related_to(Person1, Person2) :-
  child_of(Person2, Person1).

% married
related_to(Person1, Person2) :-
  married_to(Person1, Person2).

% descendant/ancestor
related_to(Person1, Person2) :-
  descends_from(Person1, Person2).

related_to(Person1, Person2) :-
  descends_from(Person2, Person1).

% siblings
related_to(Person1, Person2) :-
  child_of(Person1, Parent),
  child_of(Person2, Parent).
