% Fichier MONEY.P
%
% Résoud le problème de cryptarithmétique SEND + MORE = MONEY.
% Example extrait de "L'ANATOMIE DE PROLOG" par Michel Van Caneghem.

solution([S,E,N,D,M,O,R,Y]) :-
    M \= 0,
    S \= 0,
    dif([S,E,N,D,M,O,R,Y]),		% Contraintes d'inégalité
    somme(0,  D, E, Y, R2),		%  0 + D + E --> Y avec retenue R2
    somme(R2, N, R, E, R3),		% R2 + N + R --> E avec retenue R3
    somme(R3, E, O, N, R4),		% R3 + E + O --> N avec retenue R4
    somme(R4, S, M, O, R5),		% R4 + S + M --> O avec retenue R5
    somme(R5, 0, 0, M, 0).		% R5 + 0 + 0 --> M avec retenue 0


somme(Retenue, C1, C2, Resultat, NouvelleRetenue) :-
    retenue(Retenue),			% Essaye toutes les retenues possibles
    chiffre(C1),			% avec tous les chiffres possibles en
    chiffre(C2),			% 1er et 2ème argument
    Somme is Retenue + C1 + C2,
    Resultat is Somme mod 10,
    NouvelleRetenue is Somme / 10.


chiffre(0).
chiffre(1).
chiffre(2).
chiffre(3).
chiffre(4).
chiffre(5).
chiffre(6).
chiffre(7).
chiffre(8).
chiffre(9).


retenue(0).
retenue(1).


solution :- solution(X), jolieSortie(X).


jolieSortie([S,E,N,D,M,O,R,Y]) :-
   write(' '), write(S), write(E), write(N), write(D), nl,
   write(' '), write(M), write(O), write(R), write(E), nl,
   write('-----'), nl,
   write(M),   write(O), write(N), write(E), write(Y), nl, nl.


:- write('Problème de cryptarithmétique :'), nl.
:- write('Résoud le problème  SEND'), nl.
:- write('                   +MORE'), nl.
:- write('                   -----'), nl.
:- write('                   MONEY'), nl.
:- write('Tapez ''solution.'' pour résoudre ce problème'), nl.
