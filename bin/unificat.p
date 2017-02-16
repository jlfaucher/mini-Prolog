% Fichier UNIFICAT.P
% Example extrait de "L'ANATOMIE DE PROLOG" par Michel Van Caneghem.

:- echo(on).

% Ce programme ne comprend qu'une seule unification : U = V.
% Alors que dans PROLOG II le temps est raisonnable, pour beaucoup de PROLOG
% il faut des dizaines de minutes pour faire cette unification avec
% test([1,2,3,4,5,6,7,8,9,10]).

% Si test(L) avec L liste de n éléments alors :
%   U va être un arbre de 8^n feuilles, idem pour V.
%   Donc : n=0  ==>                      1 unification.
%          n=1  ==> 8^1   =              8 unifications.
%          n=2  ==> 8^2   =             64 unifications.
%          n=3  ==> 8^3   =            512 unifications.
%          n=4  ==> 8^4   =          4 096 unifications.
%          n=5  ==> 8^5   =         32 768 unifications.
%          n=6  ==> 8^6   =        262 144 unifications.
%          n=7  ==> 8^7   =      2 097 152 unifications.
%          n=8  ==> 8^8   =     16 777 216 unifications.
%          n=9  ==> 8^9   =    134 217 728 unifications.
%          n=10 ==> 8^10  =  1 073 741 824 unifications.
%          etc...

:- echo(off).

test(L) :- boucle(U, V, L),
           write('Je commence l''unification...'), nl,
           U = V,
           write('J''ai fini !'), nl.

boucle(0, 0, []).
boucle([X,X,X,X,X,X,X,X], [Y,Y,Y,Y,Y,Y,Y,Y], [E|L]) :- boucle(X, Y, L).
