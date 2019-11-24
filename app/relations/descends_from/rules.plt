:- begin_tests(descends_from).

:- use_module(app/relations/child_of/rules, [child_of/2]).
:- use_module(app/relations/descends_from/rules, [descends_from/2]).

assert_child_of(Person1, Person2) :-
  assertz(child_of:fact_child_of(Person1, Person2)).

assert_descend(Person1, Person2) :-
  assertz(descends_from:fact_descends_from(Person1, Person2)).

remove_facts :-
  retractall(child_of:fact_child_of(_, _)),
  retractall(descends_from:fact_descends_from(_, _)).

test('basic', [
  nondet,
  setup(assert_descend(john, adam)),
  cleanup(remove_facts)
]) :-
  descends_from(john, adam).

test('child_of', [
  nondet,
  setup(assert_child_of(john, mia)),
  cleanup(remove_facts)
]) :-
  descends_from(john, mia).

test('transitive through child_of', [
  nondet,
  setup((
    assert_child_of(john, mia),
    assert_child_of(mia, carl)
  )),
  cleanup(remove_facts)
]) :-
  descends_from(john, mia).

test('transitive through descends relation', [
  nondet,
  setup((
    assert_descend(john, carl),
    assert_descend(carl, lois)
  )),
  cleanup(remove_facts)
]) :-
  descends_from(john, lois).

:- end_tests(descends_from).
