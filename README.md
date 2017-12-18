Port of mini prolog II to Ada-83
================================

Software from the 80s (educational).


Description
-----------

This is a port to Ada-83, with adaptations, of the mini Prolog II described in the book "L'anatomie de Prolog". This book was published by Michel Van Caneghem in 1986.

I made this port around 1988 or 1989. The underlying data model and the parser of this Ada port are derived from [muLISP/muSIMP][mulisp_musimp], that I was studying during the same period.

The garbage collector is not implemented. When the memory is full, _c'est fini_.

The syntax is Edinburgh, not Marseille.


History
-------

Alain Colmerauer [papers][alain_colmerauer_papers], [site][alain_colmerauer_site].

M. Van Caneghem. L’Anatomie de Prolog II, thèse d’Etat, October 1984. Supervised by Alain Colmerauer.

The site [Prolog Héritage][prolog_heritage] gives access to the documentation and Windows executables of PrologI, PrologII, PrologIII and PrologIV.


Source files
------------

Encoded UTF-8.

In french.

[HTML](https://jlfaucher.github.io/prolog2/html/index.htm) with cross-references.

Build
-----

With [GNAT Ada][gnat_ada]:

    gprclean -r -P/local/prolog/prolog2/prolog2.gpr
    gprbuild -d -P/local/prolog/prolog2/prolog2.gpr

Build output:

    gcc -c -gnatQ -O3 prolog.adb
    gcc -c -gnatQ -O3 interpreteur_prolog.adb
    interpreteur_prolog.adb:1327:48: warning: "Env_Regle" may be referenced before it has a value
    gcc -c -gnatQ -O3 es_prolog.adb
    gcc -c -gnatQ -O3 infos.adb
    gcc -c -gnatQ -O3 int32_io.adb
    gcc -c -gnatQ -O3 objets_prolog.adb
    gprbind prolog.bexch
    gnatbind prolog.ali
    gcc -c b__prolog.adb
    gcc prolog.o -o prolog


Primitives
----------

#### answer(yes_no | first | all | X)

    answer(yes_no).     % from now, display yes or no
    answer(0).          % same as answer(yes_no)
    answer(first).      % from now, display the first solution only
    answer(1).          % same as answer(first)
    answer(2).          % from now, display at most 2 solutions
    answer(all).        % from now, display all the solutions
    answer(X).          % current setting

#### asserta(Fact), assert(Fact), assertz(Fact)

asserta(Fact) : insert fact at the begining of the predicate.

assert(Fact), assertz(Fact) : insert a fact at the end of the predicate.

    ?- assertz(city(lyon)).
    yes
    ?- assertz(city(marseille)).
    yes
    ?- asserta(city(angers)).
    yes
    ?- asserta(city(amiens)).
    yes
    ?- listing.
    city(amiens).
    city(angers).
    city(lyon).
    city(marseille).

    yes
    ?- city(X).
    [1] X = amiens
    [2] X = angers
    [3] X = lyon
    [4] X = marseille
    yes

#### display(X)

Display X, with the free variables denoted by numbers, the strings possibly surrounded by quotes.

    ?- X=Y, Y=f(X), Z=['text', 'more text', symbol, 100, (vector), Variable], display(X), nl, display(Y), nl, display(Z), nl.
    f(_1)
    f(_1)
    [text, 'more text', symbol, 100, (vector), _4]
    [1] X = f(*1), Y = f(*1), Z = [text, more text, symbol, 100, (vector), Variable], Variable = Variable
    yes

#### dlength(X, Length, Depth)

Deep length: count the number of items at all levels, calculate the max depth.

The depth lets make a distinction between lists and non-lists.

- The depth of a non-list is zero.
- The depth of a list is the greatest number of successive [ needed to display the list.

dlength and length are equivalent when the list contains no sublist, or sublists with 0 or 1 item:

    ?- X=[], length(X, LL), dlength(X, DL, DD).
    [1] X = [], LL = 0, DL = 0, DD = 1
    yes
    ?- X=[a, b], length(X, LL), dlength(X, DL, DD).
    [1] X = [a, b], LL = 2, DL = 2, DD = 1
    yes
    ?- X=[a, b, [], []], length(X, LL), dlength(X, DL, DD).
    [1] X = [a, b, [], []], LL = 4, DL = 4, DD = 2
    yes
    ?- X=[a, [b], c], length(X, LL), dlength(X, DL, DD).
    [1] X = [a, [b], c], LL = 3, DL = 3, DD = 2
    yes
    ?- X=[a, [[b]], c], length(X, LL), dlength(X, DL, DD).
    [1] X = [a, [[b]], c], LL = 3, DL = 3, DD = 3
    yes

The length & depth of a non-list is 1 & 0.

    ?- dlength(x, L, D).
    [1] L = 1, D = 0
    yes
    ?- dlength(f(x), L, D).
    [1] L = 1, D = 0
    yes
    ?- dlength((a,b,c), L, D).
    [1] L = 1, D = 0
    yes

dlength and length are different when the list contains sublists with more than 1 item:

    ?- X=[a, [b1, b2], c], length(X, LL), dlength(X, DL, DD).
    [1] X = [a, [b1, b2], c], LL = 3, DL = 4, DD = 2
    yes

#### echo(off | on | X)

    echo(off).      % from now, don't display the lines read from input
    echo(on).       % from now, display the lines read from input
    echo(X).        % current setting

#### freezeA(Var, Goal)

Used by the primitive freeze :

- If Var is bound to a value then Goal is executed.
- Otherwise the execution of Goal is delayed until Var changes of state.

#### halt

Quit the interpreter.

#### integer_sequence(From, To, Step, N)

Generate a sequence of integers.

Ascending:

    ?- integer_sequence(0, 10, 2, N).
    [1] N = 0
    [2] N = 2
    [3] N = 4
    [4] N = 6
    [5] N = 8
    [6] N = 10
    yes

    ?- integer_sequence(0, infinity, 1000, N).
    [1] N = 0
    [2] N = 1000
    [3] N = 2000
    ...
    [503] N = 502000
    [504] N = 503000
    [505] N = 504000
    Not enough memory for substitutions
    Aborting current evaluation...

Descending:

    ?- integer_sequence(0, -10, -3, N).
    [1] N = 0
    [2] N = -3
    [3] N = -6
    [4] N = -9
    yes

    ?- integer_sequence(0, -infinity, -1000, N).
    [1] N = 0
    [2] N = -1000
    [3] N = -2000
    ...
    [503] N = -502000
    [504] N = -503000
    [505] N = -504000
    Not enough memory for substitutions
    Aborting current evaluation...

#### is(Var, Expression)

Evaluate the Expression and binds Var to the result.

#### length(X, Length)

Can be used to check/get the length of a list

    ?- length([a,b,c], 3).
    yes
    ?- length([a,b,c], L).
    [1] L = 3
    yes

or generate a list of a given length

    ?- length(L, 3).
    [1] L = [_9, _14, _19]
    yes

or generate all the lists from 0 to infinity length.

    ?- answer(10).
    yes
    ?- length(List, Length).
    [1] List = [], Length = 0
    [2] List = [_8], Length = 1
    [3] List = [_8, _13], Length = 2
    [4] List = [_8, _13, _18], Length = 3
    [5] List = [_8, _13, _18, _23], Length = 4
    [6] List = [_8, _13, _18, _23, _28], Length = 5
    [7] List = [_8, _13, _18, _23, _28, _33], Length = 6
    [8] List = [_8, _13, _18, _23, _28, _33, _38], Length = 7
    [9] List = [_8, _13, _18, _23, _28, _33, _38, _43], Length = 8
    [10] List = [_8, _13, _18, _23, _28, _33, _38, _43, _48], Length = 9
    ...
    yes

The length of a non-list is 1.

    length(x, L).
    [1] L = 1
    yes
    length(f(x), L).
    [1] L = 1
    yes
    ?- length((a,b,c), L).
    [1] L = 1
    yes

#### listing, listing(X)

listing: display the rules of all the user-defined predicates.

listing(X): display the rules of the predicate X.

#### nl

Display a newline.

#### reduce(X, Y, V)

Used by the primitive dif :

*   If X and Y are different then fails.
*   If X and Y are equal without having to make hypotheses on the values of the variables then V <-- 1
    otherwise V represents the first variable on which an hypothesis must be done.

#### statistics

Display statistics.

    Mot_Valeur'Size =  4
    Mot'Size =         8

    ------------------------------------------------------------------------------
    |                                    |TAILLE |POSITION|COURANT| MAXI |MEMOIRE|
    |                                    |ELEMENT|COURANTE|  MAXI |ALLOUE|ALLOUEE|
    ------------------------------------------------------------------------------
    | Zone des symboles                  |     24|      92|.......|  2000|  48000|
    | Pnames associes aux symboles       |      1|     360|.......| 32767|  32767|
    | Table des hash-codes pour symboles |      1|........|.......|   256|    256|
    | Table association variable-symbole |      1|........|.......|   100|    100|
    | Zone des doublets, FIRST           |      8|     644|.......| 16300| 130400|
    | Zone des doublets, REST            |      8|     644|.......| 16300| 130400|
    | Table des operateurs               |     21|........|.......|   100|   2100|
    | Table des tokens                   |      3|........|.......|    17|     51|
    | Pile des substitutions             |     16|       3|     65|  8100| 129600|
    | Pile de sauvegarde                 |     20|       1|      3|  5000| 100000|
    | Pile des noms de variables         |      8|       1|      5|   200|   1600|
    | Pile des equations                 |     24|       1|     84|  5000| 120000|
    | Pile des etapes                    |     16|       1|      6|  8000| 128000|
    | Pile des choix                     |     36|       1|      2|  5000| 180000|
    | Pile de renommage des variables    |      3|       0|      1|   100|    300|
    ------------------------------------------------------------------------------

#### system

Display the rules of the system predicates defined in prolog.sys.

#### true

Always succeeds.

#### write(X)

Display X, with the free variables denoted by their name, the strings never surrounded by quotes.

    ?- X=Y, Y=f(X), Z=['text', 'more text', symbol, 100, (vector), Variable], write(X), nl, write(Y), nl, write(Z), nl.
    f(f(*2))
    f(f(*2))
    [text, more text, symbol, 100, (vector), Variable]
    [1] X = f(*1), Y = f(*1), Z = [text, more text, symbol, 100, (vector), Variable], Variable = Variable
    yes


Predicates
----------

#### atom(X)

Succeeds if X is an identifier.

#### integer(X)

Succeeds if X is an integer.

#### atomic(X)

Succeeds if X is a constant.

#### var(X)

Succeeds if X is a variable.

#### list(X)

Succeeds if X is a list [...].

#### vector(X)

Succeeds if X is a vector (...).

#### function(X)

Succeeds if X is a function f(...)

#### <(X, Y)

Succeeds if X lesser than Y.

#### =<(X, Y)

Succeeds if X lesser than or equal Y.

#### >(X, Y)

Succeeds if X greater than Y.

#### >=(X, Y)

Succeeds if X greater than or equal Y.

Demo
----

    rlwrap ./prolog
    Interpreteur PROLOG avec syntaxe 'Edimbourg'.
    Algorithmes du PROLOG II de Marseille.

To load a file: ['file.p'].

### Expressions

    ?- % The expression is evaluated
    X is (14+2)^3 / (-3^4 + 2*4).
    [1] X = -56
    yes

    ?- % No evaluation when using = (unification)
    Expression = solve(A*X^2 + B*X + C == 0, X).
    [1] Expression = solve(==(+(*(A, ^(X, 2)), *(B, X), C), 0), X), A = A, X = X, B = B, C = C
    yes

    ?- % no reals, just integers.
    X is 3/4, Y is 4/3.
    [1] X = 0, Y = 1
    yes

    ?- halt.

### [Solver of the equation X+Y=Z](bin/solver.p)

    ?- ['solver.p'].

The equation X+Y=Z is represented by plus(X,Y,Z).

By default, the domain of a free variable is 0..infinity.

When no variable is free, the equation is checked.

    ?- plus(0,10,10).
    yes
    ?- plus(0,10,11).
    no

When 1 variable is free, there is only 1 solution.

    ?- plus(0, 10, Z).
    [1] Z = 10
    yes
    ?- plus(0, Y, 10).
    [1] Y = 10
    yes
    ?- plus(X, 0, 10).
    [1] X = 10
    yes

When 2 variables are free, and Z is known, the number of solutions is finite.

    ?- plus(X,Y,10).
    [1] X = 0, Y = 10
    [2] X = 1, Y = 9
    [3] X = 2, Y = 8
    [4] X = 3, Y = 7
    [5] X = 4, Y = 6
    [6] X = 5, Y = 5
    [7] X = 6, Y = 4
    [8] X = 7, Y = 3
    [9] X = 8, Y = 2
    [10] X = 9, Y = 1
    [11] X = 10, Y = 0
    yes

When 2 variables are free, including Z, the number of solutions is infinite.

    ?- plus(X,10,Z).
    [1] X = 0, Z = 10
    [2] X = 1, Z = 11
    [3] X = 2, Z = 12
    ...
    [502] X = 501, Z = 511
    [503] X = 502, Z = 512
    [504] X = 503, Z = 513
    Not enough memory for substitutions
    Aborting current evaluation...

It's possible to change the domain of a free argument.

    ?- plus([X,-100], Y, -90).
    [1] X = -100, Y = 10
    [2] X = -99, Y = 9
    [3] X = -98, Y = 8
    [4] X = -97, Y = 7
    [5] X = -96, Y = 6
    [6] X = -95, Y = 5
    [7] X = -94, Y = 4
    [8] X = -93, Y = 3
    [9] X = -92, Y = 2
    [10] X = -91, Y = 1
    [11] X = -90, Y = 0
    yes

    ?- plus([X,-100], -90, Z).
    [1] X = -100, Z = -190
    [2] X = -99, Z = -189
    [3] X = -98, Z = -188
    [4] X = -97, Z = -187
    ...
    [501] X = 400, Z = 310
    [502] X = 401, Z = 311
    [503] X = 402, Z = 312
    [504] X = 403, Z = 313
    Not enough memory for substitutions
    Aborting current evaluation...

    ?- plus([X,-100,-91], -90, Z).
    [1] X = -100, Z = -190
    [2] X = -99, Z = -189
    [3] X = -98, Z = -188
    [4] X = -97, Z = -187
    [5] X = -96, Z = -186
    [6] X = -95, Z = -185
    [7] X = -94, Z = -184
    [8] X = -93, Z = -183
    [9] X = -92, Z = -182
    [10] X = -91, Z = -181
    yes

### [Permutations of a sequence of four elements](bin/dif.p)

    ?- ['dif.p'].
    yes
    ?- permutation(A,B,C,D).
    [1] A = 1, B = 2, C = 3, D = 4
    [2] A = 1, B = 2, C = 4, D = 3
    [3] A = 1, B = 3, C = 2, D = 4
    [4] A = 1, B = 3, C = 4, D = 2
    [5] A = 1, B = 4, C = 2, D = 3
    [6] A = 1, B = 4, C = 3, D = 2
    [7] A = 2, B = 1, C = 3, D = 4
    [8] A = 2, B = 1, C = 4, D = 3
    [9] A = 2, B = 3, C = 1, D = 4
    [10] A = 2, B = 3, C = 4, D = 1
    [11] A = 2, B = 4, C = 1, D = 3
    [12] A = 2, B = 4, C = 3, D = 1
    [13] A = 3, B = 1, C = 2, D = 4
    [14] A = 3, B = 1, C = 4, D = 2
    [15] A = 3, B = 2, C = 1, D = 4
    [16] A = 3, B = 2, C = 4, D = 1
    [17] A = 3, B = 4, C = 1, D = 2
    [18] A = 3, B = 4, C = 2, D = 1
    [19] A = 4, B = 1, C = 2, D = 3
    [20] A = 4, B = 1, C = 3, D = 2
    [21] A = 4, B = 2, C = 1, D = 3
    [22] A = 4, B = 2, C = 3, D = 1
    [23] A = 4, B = 3, C = 1, D = 2
    [24] A = 4, B = 3, C = 2, D = 1
    yes

    ?- halt.

### [SEND + MORE = MONEY](bin/money.p)

    ?- ['money.p'].
    Problème de cryptarithmétique :
    Résoud le problème  SEND
                       +MORE
                       -----
                       MONEY
    Tapez 'solution.' pour résoudre ce problème
    yes
    ?- solution.
     9567
     1085
    -----
    10652

    yes

    ?- halt.

### [The 8 queens problem](bin/queens.p)

    ?- ['queens.p'].
    yes
    ?- solution(X).
    [1] X = [0, 12, 23, 29, 34, 46, 49, 59]
    [2] X = [0, 13, 23, 26, 38, 43, 49, 60]
    [3] X = [0, 14, 19, 29, 39, 41, 52, 58]
    [4] X = [0, 14, 20, 31, 33, 43, 53, 58]
    [5] X = [1, 11, 21, 31, 34, 40, 54, 60]
    [6] X = [1, 12, 22, 24, 34, 47, 53, 59]
    [7] X = [1, 12, 22, 27, 32, 47, 53, 58]
    [8] X = [1, 13, 16, 30, 35, 47, 50, 60]
    [9] X = [1, 13, 23, 26, 32, 43, 54, 60]
    [10] X = [1, 14, 18, 29, 39, 44, 48, 59]
    [11] X = [1, 14, 20, 31, 32, 43, 53, 58]
    [12] X = [1, 15, 21, 24, 34, 44, 54, 59]
    [13] X = [2, 8, 22, 28, 39, 41, 51, 61]
    [14] X = [2, 12, 17, 31, 32, 46, 51, 61]
    [15] X = [2, 12, 17, 31, 37, 43, 54, 56]
    [16] X = [2, 12, 22, 24, 35, 41, 55, 61]
    [17] X = [2, 12, 23, 27, 32, 46, 49, 61]
    [18] X = [2, 13, 17, 28, 39, 40, 54, 59]
    [19] X = [2, 13, 17, 30, 32, 43, 55, 60]
    [20] X = [2, 13, 17, 30, 36, 40, 55, 59]
    [21] X = [2, 13, 19, 24, 39, 44, 54, 57]
    [22] X = [2, 13, 19, 25, 39, 44, 54, 56]
    [23] X = [2, 13, 23, 24, 35, 46, 52, 57]
    [24] X = [2, 13, 23, 24, 36, 46, 49, 59]
    [25] X = [2, 13, 23, 25, 35, 40, 54, 60]
    [26] X = [2, 14, 17, 31, 36, 40, 51, 61]
    [27] X = [2, 14, 17, 31, 37, 43, 48, 60]
    [28] X = [2, 15, 19, 30, 32, 45, 49, 60]
    [29] X = [3, 8, 20, 31, 33, 46, 50, 61]
    [30] X = [3, 8, 20, 31, 37, 42, 54, 57]
    [31] X = [3, 9, 20, 31, 37, 40, 50, 62]
    [32] X = [3, 9, 22, 26, 37, 47, 48, 60]
    [33] X = [3, 9, 22, 26, 37, 47, 52, 56]
    [34] X = [3, 9, 22, 28, 32, 47, 53, 58]
    [35] X = [3, 9, 23, 28, 38, 40, 50, 61]
    [36] X = [3, 9, 23, 29, 32, 42, 52, 62]
    [37] X = [3, 13, 16, 28, 33, 47, 50, 62]
    [38] X = [3, 13, 23, 25, 38, 40, 50, 60]
    [39] X = [3, 13, 23, 26, 32, 46, 52, 57]
    [40] X = [3, 14, 16, 31, 36, 41, 53, 58]
    [41] X = [3, 14, 18, 31, 33, 44, 48, 61]
    [42] X = [3, 14, 20, 25, 37, 40, 50, 63]
    [43] X = [3, 14, 20, 26, 32, 45, 55, 57]
    [44] X = [3, 15, 16, 26, 37, 41, 54, 60]
    [45] X = [3, 15, 16, 28, 38, 41, 53, 58]
    [46] X = [3, 15, 20, 26, 32, 46, 49, 61]
    [47] X = [4, 8, 19, 29, 39, 41, 54, 58]
    [48] X = [4, 8, 23, 27, 33, 46, 50, 61]
    [49] X = [4, 8, 23, 29, 34, 46, 49, 59]
    [50] X = [4, 9, 19, 29, 39, 42, 48, 62]
    [51] X = [4, 9, 19, 30, 34, 47, 53, 56]
    [52] X = [4, 9, 21, 24, 38, 43, 55, 58]
    [53] X = [4, 9, 23, 24, 35, 46, 50, 61]
    [54] X = [4, 10, 16, 29, 39, 41, 51, 62]
    [55] X = [4, 10, 16, 30, 33, 47, 53, 59]
    [56] X = [4, 10, 23, 27, 38, 40, 53, 57]
    [57] X = [4, 14, 16, 26, 39, 45, 51, 57]
    [58] X = [4, 14, 16, 27, 33, 47, 53, 58]
    [59] X = [4, 14, 17, 27, 39, 40, 50, 61]
    [60] X = [4, 14, 17, 29, 34, 40, 51, 63]
    [61] X = [4, 14, 17, 29, 34, 40, 55, 59]
    [62] X = [4, 14, 19, 24, 34, 47, 53, 57]
    [63] X = [4, 15, 19, 24, 34, 45, 49, 62]
    [64] X = [4, 15, 19, 24, 38, 41, 53, 58]
    [65] X = [5, 8, 20, 25, 39, 42, 54, 59]
    [66] X = [5, 9, 22, 24, 34, 44, 55, 59]
    [67] X = [5, 9, 22, 24, 35, 47, 52, 58]
    [68] X = [5, 10, 16, 30, 36, 47, 49, 59]
    [69] X = [5, 10, 16, 31, 35, 41, 54, 60]
    [70] X = [5, 10, 16, 31, 36, 41, 51, 62]
    [71] X = [5, 10, 20, 30, 32, 43, 49, 63]
    [72] X = [5, 10, 20, 31, 32, 43, 49, 62]
    [73] X = [5, 10, 22, 25, 35, 47, 48, 60]
    [74] X = [5, 10, 22, 25, 39, 44, 48, 59]
    [75] X = [5, 10, 22, 27, 32, 47, 49, 60]
    [76] X = [5, 11, 16, 28, 39, 41, 54, 58]
    [77] X = [5, 11, 17, 31, 36, 46, 48, 58]
    [78] X = [5, 11, 22, 24, 34, 44, 49, 63]
    [79] X = [5, 11, 22, 24, 39, 41, 52, 58]
    [80] X = [5, 15, 17, 27, 32, 46, 52, 58]
    [81] X = [6, 8, 18, 31, 37, 43, 49, 60]
    [82] X = [6, 9, 19, 24, 39, 44, 50, 61]
    [83] X = [6, 9, 21, 26, 32, 43, 55, 60]
    [84] X = [6, 10, 16, 29, 39, 44, 49, 59]
    [85] X = [6, 10, 23, 25, 36, 40, 53, 59]
    [86] X = [6, 11, 17, 28, 39, 40, 50, 61]
    [87] X = [6, 11, 17, 31, 37, 40, 50, 60]
    [88] X = [6, 12, 18, 24, 37, 47, 49, 59]
    [89] X = [7, 9, 19, 24, 38, 44, 50, 61]
    [90] X = [7, 9, 20, 26, 32, 46, 51, 61]
    [91] X = [7, 10, 16, 29, 33, 44, 54, 59]
    [92] X = [7, 11, 16, 26, 37, 41, 54, 60]
    yes

    ?- halt.

### [Unification](bin/unificat.p)

    ?- ['unificat.p'].
    yes

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
    yes
    ?- test([1,2,3,4,5,6,7,8,9,10]).
    Je commence l'unification...
    J'ai fini !
    yes

    ?- halt.


[alain_colmerauer_site]: http://alain.colmerauer.free.fr "Alain Colmerauer"
[alain_colmerauer_papers]: http://pageperso.lif.univ-mrs.fr/~alain.colmerauer/tablematiere.html "Alain Colmerauer papers"
[gnat_ada]: http://libre.adacore.com "GNAT Ada"
[mulisp_musimp]: https://github.com/jlfaucher/FormMath/blob/master/SRC/MUSIMP.LSP "muLISP/muSIMP"
[prolog_heritage]: http://www.prolog-heritage.org "Prolog Héritage"
