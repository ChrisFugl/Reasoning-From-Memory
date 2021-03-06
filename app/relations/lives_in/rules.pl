:- module(lives_in, [lives_in/2]).

:- use_module(app/relations/contained_in_location/rules, [contained_in_location/2]).

:- dynamic fact_lives_in/2.

lives_in(Person, Location) :-
  fact_lives_in(Person, Location).

lives_in(Person, LocationContainer) :-
  fact_lives_in(Person, Location),
  contained_in_location(Location, LocationContainer).
