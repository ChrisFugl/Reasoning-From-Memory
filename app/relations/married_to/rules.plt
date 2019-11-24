:- use_module(app/relations/married_to/rules, [married_to/2]).

:- begin_tests(married_to).

assert_married_to(Person1, Person2) :-
  assertz(married_to:fact_married_to(Person1, Person2)).

remove_facts :-
  retractall(married_to:fact_married_to(_, _)).

test('mia is married to vincent', [
  nondet,
  setup(assert_married_to(mia, vincent)),
  cleanup(remove_facts)
]) :-
  married_to(mia, vincent).

test('married_to relation is commutative', [
  nondet,
  setup(assert_married_to(mia, vincent)),
  cleanup(remove_facts)
]) :-
  married_to(vincent, mia).

:- end_tests(married_to).
