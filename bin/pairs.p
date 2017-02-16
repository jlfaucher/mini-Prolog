% Fichier PAIRS.P
%
% Résolution du problème des paires :
% Comment placer 14 pions sur un échiquier 5x5 de telle façon
% qu'il y ait deux pions par ligne, deux pions par colonnes
% et deux pions sur les deux diagonales principales...
% Example extrait de "L'ANATOMIE DE PROLOG" par Michel Van Caneghem.

premier(2).
premier(3).
premier(5).
premier(7).
premier(11).

paire(X) :-
    premier(P1),
    premier(P2),
    X is P1 * P2.

contrainteAB(A, B) :-
    B > A,
    LigneA is A / 8,
    LigneB is B / 8,
    LigneA \= LigneB,
    ColonneA is A mod 8,
    ColonneB is B mod 8,
    ColonneA \= ColonneB,
    DeltaLigne is abs(LigneA - LigneB),
    DeltaColonne is abs(ColonneA - ColonneB),
    DeltaLigne \= DeltaColonne.

contrainteB(A, B) :-
    freeze(B, contrainteAB(A, B)).

contrainte(A,B) :-
    freeze(A, contrainteB(A, B)).

contraintes(X, []).
contraintes(X, [T|Q]) :-
    contrainte(X,T),
    contraintes(X, Q).

contraintes([]).
contraintes([T|Q]) :-
    contraintes(T, Q),
    contraintes(Q).

solution([A,B,C,D,E]) :-
    contraintes([A,B,C,D,E]),
    paire(A),
    paire(B),
    paire(C),
    paire(D),
    paire(E).

