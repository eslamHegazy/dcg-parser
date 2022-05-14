s(SQ) --> sq(SQ).
s(OQ) --> oq(OQ).
s(S) --> ss(S).
s(anded_sentence(S1, conjunction(and), S2)) --> ss(S1), [and], ss(S2).
ss(sentence(NP, VP)) --> np(NP), bvp(VP).
ss(sentence(NP, VP)) --> anp(NP), bvp(VP).

sq(subject_question(IP, BVP)) --> ip(IP), bvp(BVP).
oq(object_question(IP, FVP)) --> ip(IP), qp(FVP).

qp(question_phrase(AV, NP, IVP)) --> 
	av(AV), np(NP), sh_ivp(IVP) ; 
	av(AV), anp(NP), sh_ivp(IVP).

anp(anded_noun_phrase(NP1, conjunction(and), NP2)) --> np(NP1), [and], np(NP2).

np(noun_phrase(P)) --> sp(P).

np(noun_phrase(D, N)) --> det(X, D), n(X, N).
np(noun_phrase(D, A, N)) --> det(X, D), adj(A), n(X, N).
np(noun_phrase(D, A1, A2, N)) --> det(X, D), adj(A1), adj(A2), n(X, N), {A1\==A2}.
np(noun_phrase(N)) --> n(plural, N).
np(noun_phrase(N)) --> n(uncountable, N).
np(noun_phrase(A, N)) --> adj(A), n(plural, N).
np(noun_phrase(A1, A2, N)) --> adj(A1), adj(A2), n(plural, N), {A1\==A2}.

oph(object_phrase(ONP)) --> onp(ONP).
oph(object_phrase(ONP, RC)) --> onp(ONP), rc(RC).
oph(object_phrase(AONP)) --> aonp(AONP).
oph(object_phrase(AONP, RC)) --> aonp(AONP), rc(RC).

aonp(anded_noun_phrase(ONP1, conjunction(and), ONP2)) --> onp(ONP1), [and], onp(ONP2).

onp(noun_phrase(D, N)) --> det(X, D), n(X, N).
onp(noun_phrase(D, A, N)) --> det(X, D), adj(A), n(X, N).
onp(noun_phrase(D, A1, A2, N)) --> det(X, D), adj(A1), adj(A2), n(X, N), {A1\==A2}.
onp(noun_phrase(N)) --> n(plural, N).
onp(noun_phrase(N)) --> n(uncountable, N).
onp(noun_phrase(A, N)) --> adj(A), n(plural, N).
onp(noun_phrase(A1, A2, N)) --> adj(A1), adj(A2), n(plural, N), {A1\==A2}.

rc(rel_clause(W, NP, VP)) --> obp(W), np(NP), sh_vp(VP).

bvp(VP) --> avp(VP); vp(VP).

avp(anded_verb_phrase(VP1, conjunction(and), VP2)) --> vp(VP1), [and], vp(VP2).

vp(verb_phrase(V)) --> v(_, V).
vp(verb_phrase(V, OPH)) --> v(_, V), oph(OPH).
vp(verb_phrase(V, OPH1, OPH2)) --> v(double_object, V), oph(OPH1), oph(OPH2).
vp(verb_phrase(V, P)) --> v(_, V), pp(P).
vp(verb_phrase(V, OPH, P)) --> v(_, V), oph(OPH), pp(P).
vp(verb_phrase(V, OPH1, OPH2, P)) --> v(double_object, V), oph(OPH1), oph(OPH2), pp(P).
vp(verb_phrase(A, V)) --> adv_ph(A), v(_, V).
vp(verb_phrase(A, V, OPH)) --> adv_ph(A), v(_, V), oph(OPH).
vp(verb_phrase(A, V, OPH1, OPH2)) --> adv_ph(A), v(double_object, V), oph(OPH1), oph(OPH2).
vp(verb_phrase(A, V, P)) --> adv_ph(A), v(_, V), pp(P).
vp(verb_phrase(A, V, OPH, P)) --> adv_ph(A), v(_, V), oph(OPH), pp(P).
vp(verb_phrase(A, V, OPH1, OPH2, P)) --> adv_ph(A), v(double_object, V), oph(OPH1), oph(OPH2), pp(P).

vp(verb_phrase(V1, conjunction(and), V2)) --> v(_, V1), [and], v(_, V2), {V1\==V2}.
vp(verb_phrase(V1, conjunction(and), V2, OPH)) --> v(_, V1), [and], v(_, V2), {V1\==V2}, oph(OPH).
vp(verb_phrase(V1, conjunction(and), V2, OPH1, OPH2)) --> v(_, V1), [and], v(double_object, V2), {V1\==V2}, oph(OPH1), oph(OPH2).
vp(verb_phrase(V1, conjunction(and), V2, P)) --> v(_, V1), [and], v(_, V2), {V1\==V2}, pp(P).
vp(verb_phrase(V1, conjunction(and), V2, OPH, P)) --> v(_, V1), [and], v(_, V2), {V1\==V2}, oph(OPH), pp(P).
vp(verb_phrase(V1, conjunction(and), V2, OPH1, OPH2, P)) --> v(_, V1), [and], v(double_object, V2), {V1\==V2}, oph(OPH1), oph(OPH2), pp(P).
vp(verb_phrase(A, V1, conjunction(and), V2)) --> adv_ph(A), v(_, V1), [and], v(_, V2), {V1\==V2}.
vp(verb_phrase(A, V1, conjunction(and), V2, OPH)) --> adv_ph(A), v(_, V1), [and], v(_, V2), {V1\==V2}, oph(OPH).
vp(verb_phrase(A, V1, conjunction(and), V2, OPH1, OPH2)) --> adv_ph(A), v(_, V1), [and], v(double_object, V2), {V1\==V2}, oph(OPH1), oph(OPH2).
vp(verb_phrase(A, V1, conjunction(and), V2, P)) --> adv_ph(A), v(_, V1), [and], v(_, V2), {V1\==V2}, pp(P).
vp(verb_phrase(A, V1, conjunction(and), V2, OPH, P)) --> adv_ph(A), v(_, V1), [and], v(_, V2), {V1\==V2}, oph(OPH), pp(P).
vp(verb_phrase(A, V1, conjunction(and), V2, OPH1, OPH2, P)) --> adv_ph(A), v(_, V1), [and], v(double_object, V2), {V1\==V2}, oph(OPH1), oph(OPH2), pp(P).

%sh_vp or short_verb_phrase: a verb phrase that comes after "whom" which has one object phrase ommitted when compared to the normal vp or verb_phrase
% so for a verb that can have two direct objects, the short_verb_phrase can contain up to only one object,
% while for a verb that can have only up to one direct objects, the short_verb_phrase can not have objects
sh_vp(short_verb_phrase(V)) --> v(_, V).
sh_vp(short_verb_phrase(V, P)) --> v(_, V), pp(P).
sh_vp(short_verb_phrase(V, OPH, P)) --> v(double_object, V), oph(OPH), pp(P).
sh_vp(short_verb_phrase(A, V)) --> adv_ph(A), v(_, V).
sh_vp(short_verb_phrase(A, V, OPH)) --> adv_ph(A), v(double_object, V), oph(OPH).
sh_vp(short_verb_phrase(A, V, P)) --> adv_ph(A), v(_, V), pp(P).
sh_vp(short_verb_phrase(A, V, OPH, P)) --> adv_ph(A), v(double_object, V), oph(OPH), pp(P).

sh_vp(short_verb_phrase(V1, conjunction(and), V2)) --> v(_, V1), [and], v(_, V2), {V1\==V2}.
sh_vp(short_verb_phrase(V1, conjunction(and), V2, OPH)) --> v(_, V1), [and], v(double_object, V2), {V1\==V2}, oph(OPH).
sh_vp(short_verb_phrase(V1, conjunction(and), V2, P)) --> v(_, V1), [and], v(_, V2), {V1\==V2}, pp(P).
sh_vp(short_verb_phrase(V1, conjunction(and), V2, OPH, P)) --> v(_, V1), [and], v(double_object, V2), {V1\==V2}, oph(OPH), pp(P).
sh_vp(short_verb_phrase(A, V1, conjunction(and), V2)) --> adv_ph(A), v(_, V1), [and], v(_, V2), {V1\==V2}.
sh_vp(short_verb_phrase(A, V1, conjunction(and), V2, OPH)) --> adv_ph(A), v(_, V1), [and], v(double_object, V2), {V1\==V2}, oph(OPH).
sh_vp(short_verb_phrase(A, V1, conjunction(and), V2, P)) --> adv_ph(A), v(_, V1), [and], v(_, V2), {V1\==V2}, pp(P).
sh_vp(short_verb_phrase(A, V1, conjunction(and), V2, OPH, P)) --> adv_ph(A), v(_, V1), [and], v(double_object, V2), {V1\==V2}, oph(OPH), pp(P).

sh_ivp(short_verb_phrase(IV)) --> iv(_, IV).
sh_ivp(short_verb_phrase(IV, P)) --> iv(_, IV), pp(P).
sh_ivp(short_verb_phrase(IV, OPH, P)) --> iv(double_object, IV), oph(OPH), pp(P).
sh_ivp(short_verb_phrase(A, IV)) --> adv_ph(A), iv(_, IV).
sh_ivp(short_verb_phrase(A, IV, OPH)) --> adv_ph(A), iv(double_object, IV), oph(OPH).
sh_ivp(short_verb_phrase(A, IV, P)) --> adv_ph(A), iv(_, IV), pp(P).
sh_ivp(short_verb_phrase(A, IV, OPH, P)) --> adv_ph(A), iv(double_object, IV), oph(OPH), pp(P).

sh_ivp(short_verb_phrase(IV1, conjunction(and), IV2)) --> iv(_, IV1), [and], iv(_, IV2), {IV1\==IV2}.
sh_ivp(short_verb_phrase(IV1, conjunction(and), IV2, OPH)) --> iv(_, IV1), [and], iv(double_object, IV2), {IV1\==IV2}, oph(OPH).
sh_ivp(short_verb_phrase(IV1, conjunction(and), IV2, P)) --> iv(_, IV1), [and], iv(_, IV2), {IV1\==IV2}, pp(P).
sh_ivp(short_verb_phrase(IV1, conjunction(and), IV2, OPH, P)) --> iv(_, IV1), [and], iv(double_object, IV2), {IV1\==IV2}, oph(OPH), pp(P).
sh_ivp(short_verb_phrase(A, IV1, conjunction(and), IV2)) --> adv_ph(A), iv(_, IV1), [and], iv(_, IV2), {IV1\==IV2}.
sh_ivp(short_verb_phrase(A, IV1, conjunction(and), IV2, OPH)) --> adv_ph(A), iv(_, IV1), [and], iv(double_object, IV2), {IV1\==IV2}, oph(OPH).
sh_ivp(short_verb_phrase(A, IV1, conjunction(and), IV2, P)) --> adv_ph(A), iv(_, IV1), [and], iv(_, IV2), {IV1\==IV2}, pp(P).
sh_ivp(short_verb_phrase(A, IV1, conjunction(and), IV2, OPH, P)) --> adv_ph(A), iv(_, IV1), [and], iv(double_object, IV2), {IV1\==IV2}, oph(OPH), pp(P).


pp(prop_phrase(P, ONP)) --> prop(P), onp(ONP).
pp(prop_phrase(P, AONP)) --> prop(P), aonp(AONP).
pp(compound_prop_phrase(P1, ONP1, P2, ONP2)) --> prop(P1), onp(ONP1), prop(P2), onp(ONP2).
pp(compound_prop_phrase(P1, AONP1, P2, ONP2)) --> prop(P1), aonp(AONP1), prop(P2), onp(ONP2).
pp(compound_prop_phrase(P1, ONP1, P2, AONP2)) --> prop(P1), onp(ONP1), prop(P2), aonp(AONP2).
pp(compound_prop_phrase(P1, AONP1, P2, AONP2)) --> prop(P1), aonp(AONP1), prop(P2), aonp(AONP2).


adv_ph(adverb_phrase(A)) --> adv(A).
adv_ph(adverb_phrase(A1, conjunction(and), A2)) --> adv(A1), [and], adv(A2), {A1\==A2}.


%at least tweny nouns
n(singular, noun(boy)) --> [boy].
n(singular, noun(box)) --> [box].
n(singular, noun(room)) --> [room].
n(singular, noun(school)) --> [school].
n(uncountable, noun(school)) --> [school].
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

n(singular, noun(thing)) --> [thing].
n(singular, noun(government)) --> [government].
n(singular, noun(company)) --> [company].
n(singular, noun(group)) --> [group].
n(singular, noun(team)) --> [team].




%at least twenty verbs with past tense or infinitve inflection
% Here:  13 past verbs + 9 infinitve_verbs + 3 auxiliary verbs = 25 verbs > 20
v(single_object, verb(climbed)) --> [climbed].
v(single_object, verb(pushed)) --> [pushed].
v(single_object, verb(liked)) --> [liked].
v(single_object, verb(stored)) --> [stored].
v(single_object, verb(watched)) --> [watched].
v(single_object, verb(admired)) --> [admired].
v(single_object, verb(appreciated)) --> [appreciated].

v(single_object, verb(fought)) --> [fought].
v(single_object, verb(loved)) --> [loved].
v(single_object, verb(saw)) --> [saw].
v(single_object, verb(heard)) --> [heard].
v(single_object, verb(noticed)) --> [noticed].

v(double_object, verb(gave)) --> [gave].

iv(single_object, infinitve_verb(do)) --> [do].
iv(single_object, infinitve_verb(climb)) --> [climb].
iv(single_object, infinitve_verb(push)) --> [push].
iv(single_object, infinitve_verb(like)) --> [like].
iv(single_object, infinitve_verb(store)) --> [store].
iv(single_object, infinitve_verb(watch)) --> [watch].
iv(single_object, infinitve_verb(admire)) --> [admire].
iv(single_object, infinitve_verb(appreciate)) --> [appreciate].
iv(double_object, infinitve_verb(give)) --> [give].

av(auxiliary_verb(did)) --> [did].
av(auxiliary_verb(can)) --> [can].
av(auxiliary_verb(should)) --> [should].


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
adj(adjective(fantastic)) --> [fantastic].


%at least ten adverbs
adv(adverb(quickly)) --> [quickly].
adv(adverb(easily)) --> [easily].
adv(adverb(early)) --> [early].
adv(adverb(simply)) --> [simply].
adv(adverb(normally)) --> [normally].
adv(adverb(extremely)) --> [extremely].
adv(adverb(slowly)) --> [slowly].
adv(adverb(recently)) --> [recently].
adv(adverb(carefully)) --> [carefully].
adv(adverb(usually)) --> [usually].
adv(adverb(really)) --> [really].
adv(adverb(actually)) --> [actually].
adv(adverb(probably)) --> [probably].
adv(adverb(directly)) --> [directly].


%at least ten propositions
prop(proposition(after)) --> [after].
prop(proposition(in)) --> [in].
prop(proposition(behind)) --> [behind].

prop(proposition(before)) --> [before].
prop(proposition(during)) --> [during].

prop(proposition(at)) --> [at].
prop(proposition(on)) --> [on].
prop(proposition(to)) --> [to].

prop(proposition(with)) --> [with].
prop(proposition(without)) --> [without].


%at least five determiners
% [the, a, some, every, many]
det(singular, determiner(the)) --> [the].
det(plural, determiner(the)) --> [the].
det(singular, determiner(a)) --> [a].
det(singular, determiner(some)) --> [some].
det(plural, determiner(some)) --> [some].
det(singular, determiner(every)) --> [every].
det(plural, determiner(many)) --> [many].


sp(subject_pronoun(she)) --> [she].

%the object pronoun “whom”, and the interrogative pronouns “who” and “what”.
obp(object_pronoun(whom)) --> [whom].

ip(interrogative_pronoun(who)) --> [who].
ip(interrogative_pronoun(what)) --> [what].