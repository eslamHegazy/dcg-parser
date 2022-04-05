%before arrow for parse tree
%after arrow for actual sentence
ss(s(NP, VP)) --> np(NP), bvp(VP).
ss(s(NP, VP)) --> anp(NP), bvp(VP).
%s(s(NP, VP, RC)) --> np(NP), bvp(VP), rc(RC).
%s(s(NP, VP, RC)) --> anp(NP), bvp(VP), rc(RC).
s(S) --> ss(S).
s(s(S1, prop(and), S2)) --> ss(S1), [and], ss(S2).


anp(anded_noun_phrase(NP1, prop(and), NP2)) --> np(NP1), [and], np(NP2).


np(noun_phrase(P)) --> sp(P).
np(noun_phrase(D, N)) --> det(D), n(_, N).
np(noun_phrase(D, A, N)) --> det(D), adj(A), n(_, N).
np(noun_phrase(D, A1, A2, N)) --> det(D), adj(A1), adj(A2), n(_, N), {A1\==A2}.
np(noun_phrase(N)) --> n(plural, N).
np(noun_phrase(N)) --> n(x, N).
np(noun_phrase(A, N)) --> adj(A), n(plural, N).
np(noun_phrase(A1, A2, N)) --> adj(A1), adj(A2), n(plural, N), {A1\==A2}.

oph(object_phrase(NP, RC)) --> np(NP), rc(RC).
oph(object_phrase(NP)) --> np(NP).
oph(object_phrase(NP, RC)) --> anp(NP), rc(RC).
oph(object_phrase(NP)) --> anp(NP).

rc(rel_clause(W, NP, VP)) --> obp(W), np(NP), vp(VP).

bvp(VP) --> avp(VP).
bvp(VP) --> vp(VP).

avp(anded_verb_phrase(VP1, prop(and), VP2)) --> vp(VP1), [and], vp(VP2).

vp(verb_phrase(V)) --> v(V).
vp(verb_phrase(V, NP)) --> v(V), oph(NP).
vp(verb_phrase(V, NP1, NP2)) --> v(V), oph(NP1), oph(NP2).
vp(verb_phrase(V, P)) --> v(V), pp(P).
vp(verb_phrase(V, NP, P)) --> v(V), oph(NP), pp(P).
vp(verb_phrase(V, NP1, NP2, P)) --> v(V), oph(NP1), oph(NP2), pp(P).
vp(verb_phrase(A, V)) --> adv(A), v(V).
vp(verb_phrase(A, V, NP)) --> adv(A), v(V), oph(NP).
vp(verb_phrase(A, V, NP1, NP2)) --> adv(A), v(V), oph(NP1), oph(NP2).
vp(verb_phrase(A, V, P)) --> adv(A), v(V), pp(P).
vp(verb_phrase(A, V, NP, P)) --> adv(A), v(V), oph(NP), pp(P).
vp(verb_phrase(A, V, NP1, NP2, P)) --> adv(A), v(V), oph(NP1), oph(NP2), pp(P).


vp(verb_phrase(V1, prop(and), V2)) --> v(V1), [and], v(V2), {V1\==V2}.
vp(verb_phrase(V1, prop(and), V2, NP)) --> v(V1), [and], v(V2), {V1\==V2}, oph(NP).
vp(verb_phrase(V1, prop(and), V2, NP1, NP2)) --> v(V1), [and], v(V2), {V1\==V2}, oph(NP1), oph(NP2).
vp(verb_phrase(V1, prop(and), V2, P)) --> v(V1), [and], v(V2), {V1\==V2}, pp(P).
vp(verb_phrase(V1, prop(and), V2, NP, P)) --> v(V1), [and], v(V2), {V1\==V2}, oph(NP), pp(P).
vp(verb_phrase(V1, prop(and), V2, NP1, NP2, P)) --> v(V1), [and], v(V2), {V1\==V2}, oph(NP1), oph(NP2), pp(P).

vp(verb_phrase(A, V1, prop(and), V2)) --> adv(A), v(V1), [and], v(V2), {V1\==V2}.
vp(verb_phrase(A, V1, prop(and), V2, NP)) --> adv(A), v(V1), [and], v(V2), {V1\==V2}, oph(NP).
vp(verb_phrase(A, V1, prop(and), V2, NP1, NP2)) --> adv(A), v(V1), [and], v(V2), {V1\==V2}, oph(NP1), oph(NP2).
vp(verb_phrase(A, V1, prop(and), V2, P)) --> adv(A), v(V1), [and], v(V2), {V1\==V2}, pp(P).
vp(verb_phrase(A, V1, prop(and), V2, NP, P)) --> adv(A), v(V1), [and], v(V2), {V1\==V2}, oph(NP), pp(P).
vp(verb_phrase(A, V1, prop(and), V2, NP1, NP2, P)) --> adv(A), v(V1), [and], v(V2), {V1\==V2}, oph(NP1), oph(NP2), pp(P).


%pp(prop_phrase(P, NP)) --> prop(P), np(NP). %TODO: anp?
%pp(prop_phrase(P, NP)) --> prop(P), n(_, NP).
%spp(P, NP) --> prop(P), np(NP).
%spp(P, NP) --> prop(P), n(_, NP).

pp(prop_phrase(P, NP)) --> prop(P), np(NP).
pp(c_prop_phrase(P, NP)) --> prop(P), np(NP).
pp(compound_prop_phrase(P1, NP1, P2, NP2)) --> 
	prop(P1), np(NP1), prop(P2), np(NP2). %TODO: anp?



s(s(WHO, VP)) --> whoq(WHO), vp(VP).
whoq(interrogative_pronoun(who)) --> [who].

s(s(WHAT, MV, NP, IV)) --> whq(WHAT), mv(MV), np(NP), iv(IV).
whq(interrogative_pronoun(what)) --> [what].

%at least five determiners
det(det(the)) --> [the].
det(det(a)) --> [a].
det(det(some)) --> [some].
det(det(every)) --> [every].
det(det(many)) --> [many].



%at least twenty verbs with past tense or infinitve inflection
mv(modal_verb(did)) --> [did].

iv(infinitve_verb(do)) --> [do].
v(verb(climbed)) --> [climbed].
v(verb(pushed)) --> [pushed].
v(verb(liked)) --> [liked].
v(verb(stored)) --> [stored].
v(verb(gave)) --> [gave].
v(verb(watched)) --> [watched].
v(verb(admired)) --> [admired].
v(verb(appreciated)) --> [appreciated].

%at least twenty adjectives
adj(adjective(young)) --> [young].
adj(adjective(big)) --> [big].
adj(adjective(large)) --> [large].
adj(adjective(empty)) --> [empty].
adj(adjective(old)) --> [old].
adj(adjective(poor)) --> [poor].
adj(adjective(white)) --> [white].
adj(adjective(brilliant)) --> [brilliant].
adj(adjective(talented)) --> [talented].
adj(adjective(bright)) --> [bright].
adj(adjective(black)) --> [black].
adj(adjective(green)) --> [green].
adj(adjective(yellow)) --> [yellow].
adj(adjective(small)) --> [small].
adj(adjective(tiny)) --> [tiny].
adj(adjective(ambitious)) --> [ambitious].
adj(adjective(interested)) --> [interested].
adj(adjective(exciting)) --> [exciting].
adj(adjective(good)) --> [good].

%at least ten adverbs
adv_ph(adv_phrase(A)) --> adv(A).
adv_ph(A1, prop(and), A2) --> adv(A1), [and], adv(A2), {A1\==A2}.
adv(adverb(quickly)) --> [quickly].

%at least ten propositions
prop(proposition(after)) --> [after].
prop(proposition(in)) --> [in].
prop(proposition(behind)) --> [behind].


obp(object_pronoun(whom)) --> [whom].
sp(subject_pronoun(she)) --> [she].



%at least tweny nouns
n(singular, noun(boy)) --> [boy].
n(singular, noun(box)) --> [box].
n(singular, noun(room)) --> [room].
n(singular, noun(school)) --> [school].
n(singular, noun(woman)) --> [woman].

n(singular, noun(man)) --> [man].
n(singular, noun(envelope)) --> [envelope].
n(singular, noun(shed)) --> [shed].
n(singular, noun(building)) --> [building].
n(singular, noun(tree)) --> [tree].
n(singular, noun(girl)) --> [girl].

n(plural, noun(students)) --> [students].
n(plural, noun(professors)) --> [professors].
n(plural, noun(lecturers)) --> [lecturers].
n(plural, noun(scientists)) --> [scientists].
n(plural, noun(researchers)) --> [researchers].

n(singular, noun(student)) --> [student].
n(singular, noun(professor)) --> [professor].
n(singular, noun(lecturer)) --> [lecturer].
n(singular, noun(scientist)) --> [scientist].
n(singular, noun(researcher)) --> [researcher].

n(x, noun(school)) --> [school].