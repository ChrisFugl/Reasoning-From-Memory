:- module(relations, [
  child_of/2,
  child_of_negated/2,
  contained_in_location/2,
  contained_in_location_negated/2,
  descends_from/2,
  descends_from_negated/2,
  has_object/2,
  has_object_negated/2,
  lives_in/2,
  lives_in_negated/2,
  married_to/2,
  married_to_negated/2,
  related_to/2,
  related_to_negated/2
]).

:- use_module(child_of/rules, [child_of/2]).
:- use_module(child_of_negated/rules, [child_of_negated/2]).
:- use_module(contained_in_location/rules, [contained_in_location/2]).
:- use_module(contained_in_location_negated/rules, [contained_in_location_negated/2]).
:- use_module(descends_from/rules, [descends_from/2]).
:- use_module(descends_from_negated/rules, [descends_from_negated/2]).
:- use_module(has_object/rules, [has_object/2]).
:- use_module(has_object_negated/rules, [has_object_negated/2]).
:- use_module(lives_in/rules, [lives_in/2]).
:- use_module(lives_in_negated/rules, [lives_in_negated/2]).
:- use_module(married_to/rules, [married_to/2]).
:- use_module(married_to_negated/rules, [married_to_negated/2]).
:- use_module(related_to/rules, [related_to/2]).
:- use_module(related_to_negated/rules, [related_to_negated/2]).
