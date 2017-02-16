% Fichier QUEENS.P
%
% Résolution du problème des 8 reines :
% Comment placer 8 reines sur un échiquier de telle façon
% qu'aucune reine ne soit menacée par d'autres reines...
% Example extrait de "L'ANATOMIE DE PROLOG" par Michel Van Caneghem.

valeur(0).
valeur(1).
valeur(2).
valeur(3).
valeur(4).
valeur(5).
valeur(6).
valeur(7).

pos(X) :-
    valeur(Ligne),
    valeur(Colonne),
    X is (8* Ligne) + Colonne.

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

solution([A,B,C,D,E,F,G,H]) :-
    contraintes([A,B,C,D,E,F,G,H]),
    pos(A),
    pos(B),
    pos(C),
    pos(D),
    pos(E),
    pos(F),
    pos(G),
    pos(H).

