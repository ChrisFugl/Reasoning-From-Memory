:- begin_tests(child_of_negated).

:- use_module(app/relations/child_of_negated/rules, [child_of_negated/2]).

assert_not_child_of(Person1, Person2) :-
  assertz(child_of_negated:fact_child_of_negated(Person1, Person2)).

remove_facts :-
  retractall(child_of_negated:fact_child_of_negated(_, _)).

test('john is not a child of mia', [
  setup(assert_not_child_of(john, mia)),
  cleanup(remove_facts)
]) :-
  child_of_negated(john, mia).

test('cannot prove that carl is not a child of mia', [fail]) :-
  child_of_negated(carl, mia).

test('finds all people that are known to not be a child of mia', [
  all(People == [john, eve]),
  setup((
    assert_not_child_of(john, mia),
    assert_not_child_of(eve, mia)
  )),
  cleanup(remove_facts)
]) :-
  child_of_negated(People, mia).

:- end_tests(child_of_negated).
