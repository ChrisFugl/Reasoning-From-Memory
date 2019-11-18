:- begin_tests(child_of).

:- use_module(relations/child_of/rules, [child_of/2]).

assert_child_of_mia(X) :-
  assertz(child_of:fact_child_of(X, mia)).

remove_facts :-
  retractall(child_of:fact_child_of(_, _)).

test('john is a child of mia', [
  setup(assert_child_of_mia(john)),
  cleanup(remove_facts)
]) :-
  child_of(john, mia).

test('johnny is not a child of mia', [fail]) :-
  child_of(johnny, mia).

test('all children of mia are found', [
  all(Children == [eve, john]),
  setup((
    assert_child_of_mia(eve),
    assert_child_of_mia(john)
  )),
  cleanup(remove_facts)
]) :-
  child_of(Children, mia).

:- end_tests(child_of).
