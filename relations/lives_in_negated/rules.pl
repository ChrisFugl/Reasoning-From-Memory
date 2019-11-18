:- module(lives_in_negated, [lives_in_negated/2]).

:- use_module(relations/contained_in_location/rules, [contained_in_location/2]).

lives_in_negated(Person, Location) :-
  fact_lives_in_negated(Person, Location).

lives_in_negated(Person, Location) :-
  fact_lives_in_negated(Person, LocationContainer),
  contained_in_location(Location, LocationContainer).
