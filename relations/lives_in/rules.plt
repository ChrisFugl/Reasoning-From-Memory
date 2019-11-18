:- begin_tests(lives_in).

:- use_module(relations/lives_in/rules, [lives_in/2]).

assert_contained_in(Location1, Location2) :-
  assertz(contained_in_location:fact_contained_in_location(Location1, Location2)).

assert_lives_in(Person, Location) :-
  assertz(lives_in:fact_lives_in(Person, Location)).

remove_facts :-
  retractall(contained:fact_contained_in_location(_, _)),
  retractall(lives_in:fact_lives_in(_, _)).

test('john lives in copenhagen', [
  nondet,
  setup(assert_lives_in(john, copenhagen)),
  cleanup(remove_facts)
]) :-
  lives_in(john, copenhagen).

test('john lives in denmark since copenhagen is contained by denmark', [
  nondet,
  setup((
    assert_lives_in(john, copenhagen),
    assert_contained_in(copenhagen, denmark)
  )),
  cleanup(remove_facts)
]) :-
  lives_in(john, denmark).

test('finds all places that john lives', [
  nondet,
  set(Locations == [copenhagen, denmark, narnia, the_wardrobe, the_world]),
  setup((
    assert_lives_in(john, copenhagen),
    assert_contained_in(copenhagen, denmark),
    assert_contained_in(denmark, the_world),
    assert_lives_in(john, the_wardrobe),
    assert_contained_in(the_wardrobe, narnia)
  )),
  cleanup(remove_facts)
]) :-
  lives_in(john, Locations).

:- end_tests(lives_in).
