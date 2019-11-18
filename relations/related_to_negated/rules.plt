:- use_module(relations/related_to_negated/rules, [related_to_negated/2]).

:- begin_tests(related_to_negated).

assert_child_of(Person1, Person2) :-
  assertz(child_of:fact_child_of(Person1, Person2)).

assert_not_child_of(Person1, Person2) :-
  assertz(child_of_negated:fact_child_of_negated(Person1, Person2)).

assert_descends_from(Person1, Person2) :-
  assertz(descends_from:fact_descends_from(Person1, Person2)).

assert_not_descends_from(Person1, Person2) :-
  assertz(descends_from_negated:fact_descends_from_negated(Person1, Person2)).

assert_married_to(Person1, Person2) :-
  assertz(married_to:fact_married_to(Person1, Person2)).

assert_not_married_to(Person1, Person2) :-
  assertz(married_to_negated:fact_married_to_negated(Person1, Person2)).

assert_not_related_to(Person1, Person2) :-
  assertz(related_to_negated:fact_related_to_negated(Person1, Person2)).

remove_facts :-
  retractall(child_of:fact_child_of(_, _)),
  retractall(child_of_negated:fact_child_of_negated(_, _)),
  retractall(descends_from:fact_descends_from(_, _)),
  retractall(descends_from_negated:fact_descends_from_negated(_, _)),
  retractall(married_to:fact_married_to(_, _)),
  retractall(married_to_negated:fact_married_to_negated(_, _)),
  retractall(related_to:fact_related_to(_, _)),
  retractall(related_to_negated:fact_related_to_negated(_, _)).

test('john is not related to alexander', [
  nondet,
  setup(assert_not_related_to(john, alexander)),
  cleanup(remove_facts)
]) :-
  related_to_negated(john, alexander),
  related_to_negated(alexander, john).

test('the mother of john is related to him', [
  fail,
  setup(assert_child_of(john, mia)),
  cleanup(remove_facts)
]) :-
  related_to_negated(mia, john) ;
  related_to_negated(john, mia).

test('descendants and ancestors are related', [
  fail,
  setup(assert_descends_from(john, adam)),
  cleanup(remove_facts)
]) :-
  related_to_negated(john, adam) ;
  related_to_negated(adam, john).

test('married people are related', [
  fail,
  setup(assert_married_to(mia, vincent)),
  cleanup(remove_facts)
]) :-
  related_to_negated(mia, vincent) ;
  related_to_negated(vincent, mia).

test('siblings are related', [
  fail,
  setup((
    assert_child_of(john, mia),
    assert_child_of(bart, mia)
  )),
  cleanup(remove_facts)
]) :-
  related_to_negated(john, bart) ;
  related_to_negated(bart, john).

test('not related outide of family and descendants/ancestors', [
  nondet,
  setup((
    assert_not_child_of(john, alexander),
    assert_not_child_of(alexander, john),
    assert_not_married_to(john, alexander),
    assert_not_descends_from(john, alexander),
    assert_not_descends_from(alexander, john),
    assert_child_of(john, mia),
    assert_not_child_of(alexander, mia)
  )),
  cleanup(remove_facts)
]) :-
  related_to_negated(john, alexander),
  related_to_negated(alexander, john).

:- end_tests(related_to_negated).
