:- use_module(relations/married_to_negated/rules, [married_to_negated/2]).

:- begin_tests(married_to_negated).

assert_not_married_to(Person1, Person2) :-
  assertz(married_to_negated:fact_married_to_negated(Person1, Person2)).

remove_facts :-
  retractall(married_to_negated:fact_married_to_negated(_, _)).

test('mia is not married to vincent', [
  nondet,
  setup(assert_not_married_to(mia, vincent)),
  cleanup(remove_facts)
]) :-
  married_to_negated(mia, vincent).

test('negated married to relation is commutative', [
  nondet,
  setup(assert_not_married_to(mia, vincent)),
  cleanup(remove_facts)
]) :-
  married_to_negated(vincent, mia).

:- end_tests(married_to_negated).
