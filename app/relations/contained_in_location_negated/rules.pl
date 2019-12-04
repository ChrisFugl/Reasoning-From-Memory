:- module(contained_in_location_negated, [contained_in_location_negated/2]).

:- use_module(app/relations/contained_in_location/rules, [contained_in_location/2]).

:- dynamic fact_contained_in_location_negated/2.

contained_in_location_negated(Location1, Location2) :-
  fact_contained_in_location_negated(Location1, Location2).

% a location (1) is not contained in another location (2) when (2) is contained in (1)
contained_in_location_negated(Location1, Location2) :-
  contained_in_location(Location2, Location1).

contained_in_location_negated(Location1, Location2) :-
  contained_in_location:fact_contained_in_location(ContainedLocation1, Location1),
  contained_in_location_negated(ContainedLocation1, Location2),
  Location1 \= Location2.
