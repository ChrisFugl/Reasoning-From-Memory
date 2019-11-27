:- begin_tests(descends_from_negated).

:- use_module(app/relations/descends_from_negated/rules, [descends_from_negated/2]).

assert_child_of(Person1, Person2) :-
  assertz(child_of:fact_child_of(Person1, Person2)).

assert_not_child_of(Person1, Person2) :-
  assertz(child_of_negated:fact_child_of_negated(Person1, Person2)).

assert_descend(Person1, Person2) :-
  assertz(descends_from:fact_descends_from(Person1, Person2)).

assert_not_descend(Person1, Person2) :-
  assertz(descends_from_negated:fact_descends_from_negated(Person1, Person2)).

remove_facts :-
  retractall(child_of:fact_child_of(_, _)),
  retractall(child_of_negated:fact_child_of_negated(_, _)),
  retractall(descends_from:fact_descends_from(_, _)),
  retractall(descends_from_negated:fact_descends_from_negated(_, _)).

test('basic', [
  nondet,
  setup(assert_not_descend(john, carl)),
  cleanup(remove_facts)
]) :-
  descends_from_negated(john, carl).

test('does not descend from ancestor', [
  nondet,
  setup(assert_descend(john, mia)),
  cleanup(remove_facts)
]) :-
  descends_from_negated(mia, john).

test('does not share a common ancestor', [
  nondet,
  setup((
    assert_not_descend(john, steve),
    assert_descend(carl, steve)
  )),
  cleanup(remove_facts)
]) :-
  descends_from_negated(john, carl).

test('finds all that are not ancestors', [
  nondet,
  set(NotAncestors == [carl, john]),
  setup((
    assert_not_descend(mia, carl),
    assert_child_of(john, mia),
    assert_child_of(mia, frantz)
  )),
  cleanup(remove_facts)
]) :-
  descends_from_negated(mia, NotAncestors).

:- end_tests(descends_from_negated).
