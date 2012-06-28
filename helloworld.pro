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
    word(A, A1, A2, A3, A4, A5, A6, A7),
    word(B, B1, B2, B3, B4, B5, B6, B7),
    word(C, C1, C2, C3, C4, C5, C6, C7),
    word(D, D1, A2, D2, B2, D3, C2, D4),
    word(E, E1, A4, E2, B4, E3, C4, E4),
    word(F, F1, A6, F2, B6, F3, C6, F4),
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
