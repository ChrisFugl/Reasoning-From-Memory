:- begin_tests(has_object).

:- use_module(app/relations/has_object/rules, [has_object/2]).

assert_has_object(Person, Object) :-
  assertz(has_object:fact_has_object(Person, Object)).

remove_facts :-
  retractall(has_object:fact_has_object(_, _)).

test('john has a cat', [
  setup(assert_has_object(john, cat)),
  cleanup(remove_facts)
]) :-
  has_object(john, cat).

test('john does not have a dog', [fail]) :-
  has_object(john, dog).

test('all objects john has are found', [
  all(Objects == [cat, computer]),
  setup((
    assert_has_object(john, cat),
    assert_has_object(john, computer)
  )),
  cleanup(remove_facts)
]) :-
  has_object(john, Objects).

:- end_tests(has_object).
