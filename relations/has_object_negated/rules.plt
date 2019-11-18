:- begin_tests(has_object_negated).

:- use_module(relations/has_object_negated/rules, [has_object_negated/2]).

assert_do_not_have_object(Person, Object) :-
  assertz(has_object_negated:fact_has_object_negated(Person, Object)).

remove_facts :-
  retractall(has_object_negated:fact_has_object_negated(_, _)).

test('john does not have a dog', [
  setup(assert_do_not_have_object(john, dog)),
  cleanup(remove_facts)
]) :-
  has_object_negated(john, dog).

test('cannot prove that john does not have cat', [fail]) :-
  has_object_negated(john, cat).

test('all john do not have are found', [
  all(Objects == [dog, ferrari]),
  setup((
    assert_do_not_have_object(john, dog),
    assert_do_not_have_object(john, ferrari)
  )),
  cleanup(remove_facts)
]) :-
  has_object_negated(john, Objects).

:- end_tests(has_object_negated).
