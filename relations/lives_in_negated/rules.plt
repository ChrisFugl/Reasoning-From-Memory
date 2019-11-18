:- begin_tests(lives_in_negated).

:- use_module(relations/lives_in_negated/rules, [lives_in_negated/2]).

assert_contained_in(Location1, Location2) :-
  assertz(contained_in_location:fact_contained_in_location(Location1, Location2)).

assert_lives_in(Person, Location) :-
  assertz(lives_in:fact_lives_in(Person, Location)).

assert_does_not_lives_in(Person, Location) :-
  assertz(lives_in_negated:fact_lives_in_negated(Person, Location)).

remove_facts :-
  retractall(contained:fact_contained_in_location(_, _)),
  retractall(lives_in_negated:fact_lives_in(_, _)).

test('john does not live seoul', [
  nondet,
  setup(assert_does_not_lives_in(john, seoul)),
  cleanup(remove_facts)
]) :-
  lives_in_negated(john, seoul).

test('john does not live in seoul since seoul is in korea', [
  nondet,
  setup((
    assert_does_not_lives_in(john, korea),
    assert_contained_in(seoul, korea)
  )),
  cleanup(remove_facts)
]) :-
  lives_in_negated(john, seoul).

test('finds all places that john does not live', [
  nondet,
  set(Locations == [korea, seoul]),
  setup((
    assert_lives_in(john, copenhagen),
    assert_contained_in(copenhagen, denmark),
    assert_does_not_lives_in(john, korea),
    assert_contained_in(seoul, korea)
  )),
  cleanup(remove_facts)
]) :-
  lives_in_negated(john, Locations).

:- end_tests(lives_in_negated).
