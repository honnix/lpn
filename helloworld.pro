/* -*- Mode:Prolog; coding:iso-8859-1; -*- */

%% loves(vincent, mia).
%% loves(marsellus, mia).
%% loves(pumpkin, honey_bunny).
%% loves(honey_bunny, pumpkin).

%% woman(mia).
%% man(vincent).

%% person(X):- man(X); woman(X).

%% jealous(X, Y):- loves(X, Z), loves(Y, Z).


%% killer(butch).
%% married(mia, marsellus).
%% dead(zed).
%% footmassage(vincent, mia).
%% footmassage(vincent, marsellus).
%% footmassage(butch, mia).
%% good_dancer(vincent).
%% good_dancer(butch).
%% nutritious(apple).
%% tasty(pizza).

%% kills(marsellus, X):- footmassage(X, mia).
%% loves(mia, X):- good_dancer(X).
%% eats(jules, X):- nutritious(X); tasty(X).

%% wizard(ron).
%% hasWand(harry).
%% quidditchPlayer(harry).
%% wizard(X):- hasBroom(X), hasWand(X).
%% hasBroom(X):- quidditchPlayer(X).

%% horizontal(line(point(X,Y),point(Z,Y))).

house_elf(dobby).
witch(hermione).
witch('McGonagall').
witch(rita_skeeter).

magic(X):- house_elf(X).
magic(X):- wizard(X).
magic(X):- witch(X).

word(determiner,a). 
word(determiner,every). 
word(noun,criminal). 
word(noun,'big  kahuna  burger'). 
word(verb,eats). 
word(verb,likes). 

sentence(Word1,Word2,Word3,Word4,Word5):- 
    word(determiner,Word1), 
    word(noun,Word2), 
    word(verb,Word3), 
    word(determiner,Word4), 
    word(noun,Word5).

word(astante,  a,s,t,a,n,t,e). 
word(astoria,  a,s,t,o,r,i,a). 
word(baratto,  b,a,r,a,t,t,o). 
word(cobalto,  c,o,b,a,l,t,o). 
word(pistola,  p,i,s,t,o,l,a). 
word(statale,  s,t,a,t,a,l,e).

crossword(A,B,C,D,E,F):-
    word(A, _, A2, _, A4, _, A6, _),
    word(B, _, B2, _, B4, _, B6, _),
    word(C, _, C2, _, C4, _, C6, _),
    word(D, _, A2, _, B2, _, C2, _),
    word(E, _, A4, _, B4, _, C4, _),
    word(F, _, A6, _, B6, _, C6, _),
    A \= D,
    B \= E,
    C \= F.

just_ate(mosquito, blood(john)).
just_ate(frog, mosquito).
just_ate(stork, frog).

is_digesting(X, Y):- just_ate(X, Y).
is_digesting(X, Y):-
    just_ate(X, Z),
    is_digesting(Z, Y).

child(bridget, caroline).
child(caroline, donna).

descend(X, Y):- child(X, Y).
descend(X, Y):-
    child(X, Z),
    child(Z, Y).

numeral(0).
numeral(succ(X)):- numeral(X).

add(0, Y, Y).
add(succ(X), Y, succ(Z)):-
    add(X, Y, Z).

directlyIn(natasha, irina).
directlyIn(olga, natasha).
directlyIn(katarina, olga).

in(X, Y):- directlyIn(X, Y).
in(X, Y):-
    directlyIn(Z, Y),
    in(X, Z).

greater_than(succ(0), 0).
greater_than(X, Y):- X = succ(Y).
greater_than(X, Y):-
    greater_than(X, succ(Y)).


swap(A, B):- A = leaf(X), B = leaf(X).
swap(A, B):-
    A = tree(La, Ra), B = tree(Lb, Rb),
    swap(La, Rb), swap(Ra, Lb).

connected(1, 2).
connected(3, 4).
connected(5, 6).
connected(7, 8).
connected(9, 10).
connected(12, 13).
connected(13, 14).
connected(15, 16).
connected(17, 18).
connected(19, 20).
connected(4, 1).
connected(6, 3).
connected(4, 7).
connected(6, 11).
connected(14, 9).
connected(11, 15).
connected(16, 12).
connected(14, 17).
connected(16, 19).

path(From, To):- connected(From, To).
path(From, To):-
    connected(From, Via),
    path(Via, To).

byCar(auckland, hamilton).
byCar(hamilton, raglan).
byCar(valmont, saarbruecken).
byCar(valmont, metz).

byTrain(metz, frankfurt).
byTrain(saarbruecken, frankfurt).
byTrain(metz, paris).
byTrain(saarbruecken, paris).

byPlane(frankfurt, bangkok).
byPlane(frankfurt, singapore).
byPlane(paris, losAngeles).
byPlane(bangkok, auckland).
byPlane(singapore, auckland).
byPlane(losAngeles, auckland).

travel(From, To):-
    byCar(From, To);
    byTrain(From, To);
    byPlane(From, To).
travel(From, To):-
    (byCar(From, Via);
    byTrain(From, Via);
    byPlane(From, Via)),
    travel(Via, To).

%% travel(From, To, Route):-
%%     (byCar(From, To);
%%     byTrain(From, To);
%%     byPlane(From, To)),
%%     Route = go(From, To).
%% travel(From, To, Route):-
%%     (byCar(From, Via);
%%     byTrain(From, Via);
%%     byPlane(From, Via)),
%%     travel(Via, To, NewRoute),
%%     Route = go(From, Via, NewRoute).

travel(From, To, Route):-
    (byCar(From, To), Route = goByCar(From, To));
    (byTrain(From, To), Route = goByTrainn(From, To));
    (byPlane(From, To), Route = goByPlane(From, To)).
travel(From, To, Route):-
    (byCar(From, Via), travel(Via, To, NewRoute), Route = goByCar(From, Via, NewRoute));
    (byTrain(From, Via), travel(Via, To, NewRoute), Route = goByTrain(From, Via, NewRoute));
    (byPlane(From, Via), travel(Via, To, NewRoute), Route = goByPlane(From, Via, NewRoute)).

member1(X, [X|_]).
member1(X, [_|T]):- member1(X, T).

a2b([], []).
a2b([a | Ta], [b | Tb]):- a2b(Ta, Tb).

second(X, [_, X | _]).

swap12([], []).
swap12([X | T1], [X | T2]):- swap12(T1, T2).

tran(eins, one).
tran(zwei, two).
tran(drei, three).
tran(vier, four).
tran(fuenf, five).
tran(sechs, six).
tran(sieben, seven).
tran(acht, eight).
tran(neun, nine).

listtran([], []).
listtran([H1 | T1], [H2 | T2]):- tran(H1, H2), listtran(T1, T2).

twice([], []).
twice([H | T1], [H, H | T2]):- twice(T1, T2).

combine([], [], []).
combine([H1 | T1], [H2 | T2], [H1, H2 | T3]):- combine(T1, T2, T3).

combine2([], [], []).
combine2([H1 | T1], [H2 | T2], [[H1, H2] | T3]):- combine2(T1, T2, T3).

combine3([], [], []).
combine3([H1 | T1], [H2 | T2], [j(H1, H2) | T3]):- combine3(T1, T2, T3).

len([], 0).
len([_ | T], N):- len(T, X), N is X + 1.

accLen([_ | T], A, N):- Anew is A + 1, accLen(T, Anew, N).
accLen([], A, A).

accMax([H | T], A, Max):-
    H > A,
    accMax(T, H, Max).
accMax([H | T], A, Max):-
    H =< A,
    accMax(T, A, Max).
accMax([], A, A).

max([H | T], Max):-
    accMax(T, H, Max).

increment(X, Y):- Y > X + 1.

sum(X, Y, S):- S =:= X + Y.

addone([], []).
addone([H1 | T1], [H2 | T2]):- H2 is H1 + 1, addone(T1, T2).

scalarMult(_, [], []).
scalarMult(N, [H1 | T1], [H2 | T2]):- H2 is H1 * N, scalarMult(N, T1, T2).

dot([], [], 0).
dot([H1 | T1], [H2 | T2], Result):- dot(T1, T2, NewResult), Result is NewResult + H1 * H2.

prefix(P, L):- append(P, _, L).
suffix(S, L):- append(_, S, L).
sublist(SubL, L):- suffix(S, L), prefix(SubL, S).

naiverev([], []).
naiverev([H | T], R):- naiverev(T, RevT), append(RevT, [H], R).

accRev([H | T], A, R):- accRev(T, [H | A], R).
accRev([], A, A).
rev(L, R):- accRev(L, [], R).

doubled(L):- append(X, X, L).

palindrome(L):- rev(L, L).

toptail([_ | T], X):- rev(T, [_ | R]), rev(R, X).

last(L, X):- rev(L, [X | _]).
last1([X | []], X).
last1([_ | T], X):- last1(T, X).

member2(X, L):- append(_, [X | _], L).

set([], []).
set([H | T], X):- set(T, Y), member2(H, Y), X = Y.
set([H | T], X):- set(T, Y), X = [H | Y].

%% flatten([], F).
%% flatten([[H | X] | T], F):- flatten([H | T], F1), flatten(X, F2), append(F1, F2, F).
%% flatten([H | T], F):- flatten(T, [H | F]).
