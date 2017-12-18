% Fichier SOLVER.P
%
% The equation X+Y=Z is represented by plus(X,Y,Z).

%-----------------------------%
% Zero free variable: checker %
%-----------------------------%

plus(I1, I2, I3) :- integer(I1), integer(I2), integer(I3), !, I3 is I1+I2.


%---------------------------------%
% One free variable: one solution %
%---------------------------------%

plus(V1, I2, I3) :- var(V1),     integer(I2), integer(I3), !, V1 is I3-I2.
plus(I1, V2, I3) :- integer(I1), var(V2),     integer(I3), !, V2 is I3-I1.
plus(I1, I2, V3) :- integer(I1), integer(I2), var(V3),     !, V3 is I1+I2.


%-------------------------------------------------------------------------------------%
% Two free variables: generate all the integer solutions of the equation V1 + V2 = V3 %
%-------------------------------------------------------------------------------------%

% By default, the domain of a free variable is 0..infinity.
plus(V1, V2, I3) :- var(V1),     var(V2),     integer(I3), !, integer_sequence(0, I3,       1, V1), V2 is I3-V1.
plus(V1, I2, V3) :- var(V1),     integer(I2), var(V3),     !, integer_sequence(0, infinity, 1, V1), V3 is V1+I2.
plus(I1, V2, V3) :- integer(I1), var(V2),     var(V3),     !, integer_sequence(0, infinity, 1, V2), V3 is I1+V2.

% Constraints on sequence of 1st variable: From
plus([V1, From], V2,         I3) :- var(V1),     var(V2),     integer(I3), !, integer_sequence(From, I3,       1, V1), V2 is I3-V1.
plus([V1, From], I2,         V3) :- var(V1),     integer(I2), var(V3),     !, integer_sequence(From, infinity, 1, V1), V3 is V1+I2.
plus(I1,         [V2, From], V3) :- integer(I1), var(V2),     var(V3),     !, integer_sequence(From, infinity, 1, V2), V3 is I1+V2.

% Constraints on sequence of 1st variable: From, To
plus([V1, From, To], V2,             I3) :- var(V1),     var(V2),     integer(I3), !, min(To, I3, ToMin), integer_sequence(From, ToMin, 1, V1), V2 is I3-V1.
plus([V1, From, To], I2,             V3) :- var(V1),     integer(I2), var(V3),     !,                     integer_sequence(From, To,    1, V1), V3 is V1+I2.
plus(I1,             [V2, From, To], V3) :- integer(I1), var(V2),     var(V3),     !,                     integer_sequence(From, To,    1, V2), V3 is I1+V2.

% Constraints on sequence of 2nd variable: From
plus(V1, [V2, From], I3)         :- var(V1),     var(V2),     integer(I3), !, integer_sequence(From, I3,       1, V2), V1 is I3-V2.
plus(V1, I2,         [V3, From]) :- var(V1),     integer(I2), var(V3),     !, integer_sequence(From, infinity, 1, V3), V1 is V3-I2.
plus(I1, V2,         [V3, From]) :- integer(I1), var(V2),     var(V3),     !, integer_sequence(From, infinity, 1, V3), V2 is V3-I1.

% Constraints on sequence of 2nd variable: From, To
plus(V1, [V2, From, To], I3)             :- var(V1),     var(V2),     integer(I3), !, min(To, I3, ToMin), integer_sequence(From, ToMin, 1, V2), V1 is I3-V2.
plus(V1, I2,             [V3, From, To]) :- var(V1),     integer(I2), var(V3),     !,                     integer_sequence(From, To,    1, V3), V1 is V3-I2.
plus(I1, V2,             [V3, From, To]) :- integer(I1), var(V2),     var(V3),     !,                     integer_sequence(From, To,    1, V3), V2 is V3-I1.


%--------------------------------------------------------%
% Three free variables: not supported, too many unknowns %
%--------------------------------------------------------%

plus(V1, V2, V3) :- var(V1), var(V2), var(V3), !, fail.
