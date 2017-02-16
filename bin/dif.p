% Fichier DIF.P
%
% Permutations d'une suite de quatre éléments.
% Example extrait de "L'ANATOMIE DE PROLOG" par Michel Van Caneghem.

permutation(A,B,C,D) :-
    dif([A,B,C,D]),
    digit(A),
    digit(B),
    digit(C),
    digit(D).

digit(1).
digit(2).
digit(3).
digit(4).
