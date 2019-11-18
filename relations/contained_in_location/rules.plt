:- begin_tests(contained_in_location).

:- use_module(relations/contained_in_location/rules, [contained_in_location/2]).

assert_fact(Location1, Location2) :-
  assertz(contained_in_location:fact_contained_in_location(Location1, Location2)).

remove_facts :-
  retractall(contained_in_location:fact_contained_in_location(_, _)).

test('basic', [
  nondet,
  setup(assert_fact(seoul, korea)),
  cleanup(remove_facts)
]) :-
  contained_in_location(seoul, korea).

test('transitive', [
  nondet,
  setup((
    assert_fact(seoul, korea),
    assert_fact(korea, asia),
    assert_fact(asia, earth)
  )),
  cleanup(remove_facts)
]) :-
  contained_in_location(seoul, asia),
  contained_in_location(seoul, earth),
  contained_in_location(korea, earth).

test('finds all containers', [
  nondet,
  all(Containers == ['korea', 'asia', 'earth']),
  setup((
    assert_fact(seoul, korea),
    assert_fact(korea, asia),
    assert_fact(asia, earth)
  )),
  cleanup(remove_facts)
]) :-
  contained_in_location(seoul, Containers).

test('finds all contained locations', [
  nondet,
  all(Contained == ['asia', 'seoul', 'korea']),
  setup((
    assert_fact(seoul, korea),
    assert_fact(korea, asia),
    assert_fact(asia, earth)
  )),
  cleanup(remove_facts)
]) :-
  contained_in_location(Contained, earth).

:- end_tests(contained_in_location).
