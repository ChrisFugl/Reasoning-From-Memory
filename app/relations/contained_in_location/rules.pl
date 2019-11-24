:- module(contained_in_location, [contained_in_location/2]).

contained_in_location(Location1, Location2) :-
  fact_contained_in_location(Location1, Location2).

contained_in_location(Location1, Location2) :-
  fact_contained_in_location(Location1, Location1Container),
  contained_in_location(Location1Container, Location2).
