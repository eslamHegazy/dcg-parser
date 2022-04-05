s(s(NP,VP)) --> np(NP),vp(VP).

s(s(WHO, VP)) --> whoq(WHO), vp(VP).
whoq(whoq(who)) --> [who].

s(s(WHAT, MV, NP, IV)) --> whq(WHAT), mv(MV), np(NP), iv(IV).
whq(whq(what)) --> [what].



np(np(DET, N)) --> det(DET), n(N).
np(np(DET, ADJ, N)) --> det(DET), adj_ph(ADJ), n(N).
np(np(DET, N, P)) --> det(DET), n(N), prop_ph(P).
np(np(DET, ADJ, N, P)) --> det(DET), adj_ph(ADJ), n(N), prop_ph(P).

adj_ph(adj_ph(A)) --> adj(A).
%adj_ph(adj_ph(A, B)) --> adj(A), adj_ph(B).
%adj_ph(adj_ph(A, B)) --> adj(A), adj(B).

%vp(vp(V1, [and], V2)) --> vp(V), vp(V2).
vp(vp(V)) --> v(V).
vp(vp(V1, [and], V2)) --> v(V1), v(V2).%,{V1=\=V2}.
vp(vp(V,NP)) --> v(V), np(NP).
vp(vp(V, PP)) --> v(V), prop_ph(PP).
vp(vp(V,NP, PP)) --> v(V), np(NP), prop_ph(PP).

prop_ph(prop_ph(P, NP)) --> prop(P), np(NP).

%at least five determiners
det(det(the)) --> [the].
%det(det(a)) --> [a].
%det(det(every)) --> [every].
%det(det(many)) --> [many].
%det(det(some)) --> [some].

%at least tweny nouns
n(n(woman)) --> [woman].
%n(n(man)) --> [man].
%n(n(boy)) --> [boy].
%n(n(girl)) --> [girl].
%n(n(school)) --> [school].
%n(n(box)) --> [box].

%at least twenty verbs with past tense or infinitve inflection
mv(mv(did)) --> [did].
%mv(mv(can)) --> [can].
%mv(mv(could)) --> [could].

iv(iv(do)) --> [do].
%iv(iv(climb)) --> [climb].
%iv(iv(shoot)) --> [shoot].
%iv(iv(hear)) --> [hear].
v(v(climbed)) --> [climbed].
v(v(pushed)) --> [pushed].
%v(v(stored)) --> [stored].
%v(v(gave)) --> [gave].
%v(v(watched)) --> [watched].
%v(v(admired)) --> [admired].
%v(v(appreciated)) --> [appreciated].
%v(v(liked)) --> [liked].

%at least twenty adjectives
adj(adj(young)) --> [young].
%adj(adj(big)) --> [big].
%adj(adj(large)) --> [large].
%adj(adj(empty)) --> [empty].

%at least ten adverbs
adv(adv(quickly)) --> [quickly].
%adv(adv(slowly)) --> [slowly].
%adv(adv(correctly)) --> [correctly].
%at least ten propositions
prop(prop(after)) --> [after].
%prop(prop(behind)) --> [behind].
%prop(prop(in)) --> [in].

%object pronoun "whom"
%interrogative pronouns "who" and "what"