%lexicon
%nouns
noun(grass).
noun(cow).
noun(girl).
noun(boy).
noun(apple).
noun(song).
%articles
article(the).
article(a).
article(an).
%verbs
verb(eats).
verb(sings).
verb(chews).
verb(kicks).
%adverbs
adverb(slowly).
adverb(deliberately).
adverb(merrily).
adverb(sweetly).

%Q1
sentence([A, N|V]) :-
	noun_phrase([A, N]),
	verb_phrase(V).

noun_phrase([A, N]) :-
	article(A),
	noun(N).

verb_phrase([V]) :-
	verb(V).
verb_phrase([V|N]) :-
	verb(V),
	noun_phrase(N).

%Q2
is_vowel('a').
is_vowel('e').
is_vowel('i').
is_vowel('o').
is_vowel('u').

noun_phrase_better(['the', N]) :-
	noun(N).
noun_phrase_better(['an', N]) :-
	noun(N),
	atom_chars(N, [H|_]),
	is_vowel(H).
noun_phrase_better(['a', N]) :-
	noun(N),
	atom_chars(N, [H|_]),
	\+is_vowel(H).

sentence_better([A, N|V]) :-
	noun_phrase_better([A, N]),
	verb_phrase_better(V).

verb_phrase_better([V]) :-
	verb(V).
verb_phrase_better([V|N]) :-
	verb(V),
	noun_phrase_better(N).

%Q3
cadvs([A]) :-
	adverb(A).
cadvs(P) :-
	cadvs_check(P, []).

cadvs_check([A1, 'and', A2], H) :-
	adverb(A1),
	adverb(A2),
	A1 \== A2,
	\+member(A1, H),
	\+member(A1, H).
cadvs_check([A1, ','|P], H) :-
	adverb(A1),
	\+member(A1, H),
	cadvs_check(P, [A1|H]).

adverb_phrase(P) :-
	cadvs(A),
	verb_phrase_better(V),
	append(A, V, P).

%Q4
actions(A, P, []) :-
        \+member(A, P).
actions(A, P, []) :-
	member(A, P),
	append(_, [A, V|_], P),
	\+verb(V).
actions(A, P, As) :-
        findall(V, (append(_, [A, V|_], P), verb(V)), As).

%Q5
/*
actions_and_adverbs(A, P, As) :-
	setof((V, Adv), (append(_, [A|R], P), adverb_phrase(R), verb(V), member(V, R), append(_, [V|_], P), findall(Advs, (adverb(Advs), member(Advs, R)), Adv)), As).
*/
conjunction('and').
conjunction(',').

find_adverbs([], []).
find_adverbs([ToCheck|Rest], [ToCheck|Adverbs]) :-
	adverb(ToCheck),
	find_adverbs(Rest, Adverbs).
find_adverbs([ToCheck|Rest], Adverbs) :-
	conjunction(ToCheck),
	find_adverbs(Rest, Adverbs).

actions_and_adverbs(_, [], []).
actions_and_adverbs(Actor, [Actor|Rest], [(X, Adverbs),(As)]) :-
	member(X, Rest),
	verb(X),
	append(Sentence, [X|_], Rest),
	find_adverbs(Sentence, Adverbs),
	actions_and_adverbs(Actor, Rest, As).
actions_and_adverbs(Actor, [ToCheck|Text], As) :-
	(Actor \== ToCheck),
	actions_and_adverbs(Actor, Text, As).