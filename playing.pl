s(s(NP,VP)) --> noun_phrase(NP),vp(VP).

s(s(WHO, VP)) --> whoq(WHO), vp(VP).
s(s(WH, MV, NP, V)) --> whq(WH), modal_verb(MV), np(NP), verb(V).
whoq(whoq(who)) --> [who].

s(s(QW, MV, NP, IV)) --> whq(QW), modal_verb(MV), noun_phrase(NP), infinitive_verb(IV).

whq(whq(what)) --> [what].
%whq(whq(who)) --> [who].


verb(verb(climbed)) --> [climbed].
verb(verb(pushed)) --> [pushed].
verb(verb(stored)) --> [stored].
verb(verb(gave)) --> [gave].
verb(verb(watched)) --> [watched].
verb(verb(admired)) --> [admired].
verb(verb(appreciated)) --> [appreciated].
verb(verb(liked)) --> [liked].


modal_verb(modal_verb(did)) --> [did].
modal_verb(modal_verb(can)) --> [can].
modal_verb(modal_verb(could)) --> [could].
infinitive_verb(infinitive_verb(climb)) --> [climb].
infinitive_verb(infinitive_verb(shoot)) --> [shoot].
infinitive_verb(infinitive_verb(hear)) --> [hear].

noun_phrase(noun_phrase(DET, N)) --> det(DET), n(N).

vp(vp(V,NP)) --> v(V), noun_phrase(NP).
vp(vp(V)) --> v(V).

det(det(the)) --> [the].
det(det(a)) --> [a].

n(n(woman)) --> [woman].
n(n(man)) --> [man].
n(n(boy)) --> [boy].
n(n(girl)) --> [girl].
n(n(tree)) --> [tree].
n(n(envelope)) --> [envelope].
n(n(building)) --> [building].
% n(n(boy)) --> [boy].
% n(n(boy)) --> [boy].
% n(n(boy)) --> [boy].
% n(n(boy)) --> [boy].
% n(n(boy)) --> [boy].