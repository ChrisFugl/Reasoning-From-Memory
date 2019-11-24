:- use_module(app/relations/related_to/rules, [related_to/2]).

:- begin_tests(related_to).

assert_child_of(Person1, Person2) :-
  assertz(child_of:fact_child_of(Person1, Person2)).

assert_descends_from(Person1, Person2) :-
  assertz(descends_from:fact_descends_from(Person1, Person2)).

assert_married_to(Person1, Person2) :-
  assertz(married_to:fact_married_to(Person1, Person2)).

assert_related_to(Person1, Person2) :-
  assertz(related_to:fact_related_to(Person1, Person2)).

remove_facts :-
  retractall(child_of:fact_child_of(_, _)),
  retractall(descends_from:fact_descends_from(_, _)),
  retractall(married_to:fact_married_to(_, _)),
  retractall(related_to:fact_related_to(_, _)).

test('john is related to alexander', [
  nondet,
  setup(assert_related_to(john, alexander)),
  cleanup(remove_facts)
]) :-
  related_to(john, alexander).

test('alexander is related to john through commutative relation', [
  nondet,
  setup(assert_related_to(john, alexander)),
  cleanup(remove_facts)
]) :-
  related_to(alexander, john).

test('the mother of john is related to him', [
  nondet,
  setup(assert_child_of(john, mia)),
  cleanup(remove_facts)
]) :-
  related_to(mia, john).

test('john is related to his mother', [
  nondet,
  setup(assert_child_of(john, mia)),
  cleanup(remove_facts)
]) :-
  related_to(john, mia).

test('descendants are related', [
  nondet,
  setup(assert_descends_from(john, adam)),
  cleanup(remove_facts)
]) :-
  related_to(john, adam).

test('ancestors are related', [
  nondet,
  setup(assert_descends_from(john, adam)),
  cleanup(remove_facts)
]) :-
  related_to(adam, john).

test('married people are related', [
  nondet,
  setup(assert_married_to(mia, vincent)),
  cleanup(remove_facts)
]) :-
  related_to(mia, vincent),
  related_to(vincent, mia).

test('siblings are related', [
  nondet,
  setup((
    assert_child_of(john, mia),
    assert_child_of(bart, mia)
  )),
  cleanup(remove_facts)
]) :-
  related_to(john, bart),
  related_to(bart, john).

:- end_tests(related_to).
