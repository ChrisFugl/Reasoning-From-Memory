:- begin_tests(contained_in_location_negated).

:- use_module(app/relations/contained_in_location_negated/rules, [contained_in_location_negated/2]).

assert_fact(Location1, Location2) :-
  assertz(contained_in_location:fact_contained_in_location(Location1, Location2)).

assert_fact_negated(Location1, Location2) :-
  assertz(contained_in_location_negated:fact_contained_in_location_negated(Location1, Location2)).

remove_facts :-
  retractall(contained_in_location:fact_contained_in_location(_, _)),
  retractall(contained_in_location_negated:fact_contained_in_location_negated(_, _)).

test('basic', [
  nondet,
  setup(assert_fact_negated(copenhagen, korea)),
  cleanup(remove_facts)
]) :-
  contained_in_location_negated(copenhagen, korea).

test('does not contain a container', [
  nondet,
  setup(assert_fact(copenhagen, denmark)),
  cleanup(remove_facts)
]) :-
  contained_in_location_negated(denmark, copenhagen).

test('transitive through contained location', [
  nondet,
  setup((
    assert_fact(copenhagen, denmark),
    assert_fact_negated(copenhagen, narnia)
  )),
  cleanup(remove_facts)
]) :-
  contained_in_location_negated(denmark, narnia).

test('fails transitivity through negations', [
  fail,
  setup((
    assert_fact_negated(copenhagen, korea),
    assert_fact_negated(korea, narnia)
  )),
  cleanup(remove_facts)
]) :-
  contained_in_location_negated(copenhagen, narnia).

test('finds all negated containments', [
  set(NegatedContainments == [copenhagen, korea, narnia]),
  setup((
    assert_fact(copenhagen, denmark),
    assert_fact_negated(copenhagen, narnia),
    assert_fact_negated(denmark, korea)
  )),
  cleanup(remove_facts)
]) :-
  contained_in_location_negated(denmark, NegatedContainments).

:- end_tests(contained_in_location_negated).
