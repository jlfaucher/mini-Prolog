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

[HTML](html/index.htm) with cross-references.

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
    X = amiens
    X = angers
    X = lyon
    X = marseille
    yes

#### display(X)

Display X, with the free variables denoted by numbers, the strings possibly surrounded by quotes.

    ?- X=Y, Y=f(X), Z=['text', 'more text', symbol, 100, (vector), Variable], display(X), nl, display(Y), nl, display(Z), nl.
    f(_1)
    f(_1)
    [text, 'more text', symbol, 100, (vector), _4]
    X = f(*1), Y = f(*1), Z = [text, more text, symbol, 100, (vector), Variable], Variable = Variable
    yes

#### echo(off | on | X)

    echo(off).      % from now, don't display the lines read from input
    echo(on).       % from now, display the lines read from input
    echo(X).        % current setting

#### freezeA(Var, Goal)

Used by the primitive freeze :

*   If Var is bound to a value then Goal is executed.

*   Otherwise the execution of Goal is delayed until Var changes of state.

#### halt

Quit the interpreter.

#### is(Var, Expression)

Evaluate the Expression and binds Var to the result.

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
    X = f(*1), Y = f(*1), Z = [text, more text, symbol, 100, (vector), Variable], Variable = Variable
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

To load a file: ['file.p'].

### Expressions

    rlwrap ./prolog
    Interpreteur PROLOG avec syntaxe 'Edimbourg'.
    Algorithmes du PROLOG II de Marseille.

    ?- % The expression is evaluated
    X is (14+2)^3 / (-3^4 + 2*4)
    X = -56
    yes

    ?- % No evaluation when using = (unification)
    Expression = solve(A*X^2 + B*X + C == 0, X).
    Expression = solve(==(+(*(A, ^(X, 2)), *(B, X), C), 0), X), A = A, X = X, B = B, C = C
    yes

    ?- % no reals, just integers.
    X is 3/4, Y is 4/3.
    X = 0, Y = 1
    yes

    ?- halt.

### [Permutations of a sequence of four elements](bin/dif.p)

    rlwrap ./prolog
    Interpreteur PROLOG avec syntaxe 'Edimbourg'.
    Algorithmes du PROLOG II de Marseille.

    ?- ['dif.p'].
    yes
    ?- permutation(A,B,C,D).
    A = 1, B = 2, C = 3, D = 4
    A = 1, B = 2, C = 4, D = 3
    A = 1, B = 3, C = 2, D = 4
    A = 1, B = 3, C = 4, D = 2
    A = 1, B = 4, C = 2, D = 3
    A = 1, B = 4, C = 3, D = 2
    A = 2, B = 1, C = 3, D = 4
    A = 2, B = 1, C = 4, D = 3
    A = 2, B = 3, C = 1, D = 4
    A = 2, B = 3, C = 4, D = 1
    A = 2, B = 4, C = 1, D = 3
    A = 2, B = 4, C = 3, D = 1
    A = 3, B = 1, C = 2, D = 4
    A = 3, B = 1, C = 4, D = 2
    A = 3, B = 2, C = 1, D = 4
    A = 3, B = 2, C = 4, D = 1
    A = 3, B = 4, C = 1, D = 2
    A = 3, B = 4, C = 2, D = 1
    A = 4, B = 1, C = 2, D = 3
    A = 4, B = 1, C = 3, D = 2
    A = 4, B = 2, C = 1, D = 3
    A = 4, B = 2, C = 3, D = 1
    A = 4, B = 3, C = 1, D = 2
    A = 4, B = 3, C = 2, D = 1
    yes

    ?- halt.

### [SEND + MORE = MONEY](bin/money.p)

    rlwrap ./prolog
    Interpreteur PROLOG avec syntaxe 'Edimbourg'.
    Algorithmes du PROLOG II de Marseille.

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

    rlwrap ./prolog
    Interpreteur PROLOG avec syntaxe 'Edimbourg'.
    Algorithmes du PROLOG II de Marseille.

    ?- ['queens.p'].
    yes
    ?- solution(X).
    X = [0, 12, 23, 29, 34, 46, 49, 59]
    X = [0, 13, 23, 26, 38, 43, 49, 60]
    X = [0, 14, 19, 29, 39, 41, 52, 58]
    X = [0, 14, 20, 31, 33, 43, 53, 58]
    X = [1, 11, 21, 31, 34, 40, 54, 60]
    X = [1, 12, 22, 24, 34, 47, 53, 59]
    X = [1, 12, 22, 27, 32, 47, 53, 58]
    X = [1, 13, 16, 30, 35, 47, 50, 60]
    X = [1, 13, 23, 26, 32, 43, 54, 60]
    X = [1, 14, 18, 29, 39, 44, 48, 59]
    X = [1, 14, 20, 31, 32, 43, 53, 58]
    X = [1, 15, 21, 24, 34, 44, 54, 59]
    X = [2, 8, 22, 28, 39, 41, 51, 61]
    X = [2, 12, 17, 31, 32, 46, 51, 61]
    X = [2, 12, 17, 31, 37, 43, 54, 56]
    X = [2, 12, 22, 24, 35, 41, 55, 61]
    X = [2, 12, 23, 27, 32, 46, 49, 61]
    X = [2, 13, 17, 28, 39, 40, 54, 59]
    X = [2, 13, 17, 30, 32, 43, 55, 60]
    X = [2, 13, 17, 30, 36, 40, 55, 59]
    X = [2, 13, 19, 24, 39, 44, 54, 57]
    X = [2, 13, 19, 25, 39, 44, 54, 56]
    X = [2, 13, 23, 24, 35, 46, 52, 57]
    X = [2, 13, 23, 24, 36, 46, 49, 59]
    X = [2, 13, 23, 25, 35, 40, 54, 60]
    X = [2, 14, 17, 31, 36, 40, 51, 61]
    X = [2, 14, 17, 31, 37, 43, 48, 60]
    X = [2, 15, 19, 30, 32, 45, 49, 60]
    X = [3, 8, 20, 31, 33, 46, 50, 61]
    X = [3, 8, 20, 31, 37, 42, 54, 57]
    X = [3, 9, 20, 31, 37, 40, 50, 62]
    X = [3, 9, 22, 26, 37, 47, 48, 60]
    X = [3, 9, 22, 26, 37, 47, 52, 56]
    X = [3, 9, 22, 28, 32, 47, 53, 58]
    X = [3, 9, 23, 28, 38, 40, 50, 61]
    X = [3, 9, 23, 29, 32, 42, 52, 62]
    X = [3, 13, 16, 28, 33, 47, 50, 62]
    X = [3, 13, 23, 25, 38, 40, 50, 60]
    X = [3, 13, 23, 26, 32, 46, 52, 57]
    X = [3, 14, 16, 31, 36, 41, 53, 58]
    X = [3, 14, 18, 31, 33, 44, 48, 61]
    X = [3, 14, 20, 25, 37, 40, 50, 63]
    X = [3, 14, 20, 26, 32, 45, 55, 57]
    X = [3, 15, 16, 26, 37, 41, 54, 60]
    X = [3, 15, 16, 28, 38, 41, 53, 58]
    X = [3, 15, 20, 26, 32, 46, 49, 61]
    X = [4, 8, 19, 29, 39, 41, 54, 58]
    X = [4, 8, 23, 27, 33, 46, 50, 61]
    X = [4, 8, 23, 29, 34, 46, 49, 59]
    X = [4, 9, 19, 29, 39, 42, 48, 62]
    X = [4, 9, 19, 30, 34, 47, 53, 56]
    X = [4, 9, 21, 24, 38, 43, 55, 58]
    X = [4, 9, 23, 24, 35, 46, 50, 61]
    X = [4, 10, 16, 29, 39, 41, 51, 62]
    X = [4, 10, 16, 30, 33, 47, 53, 59]
    X = [4, 10, 23, 27, 38, 40, 53, 57]
    X = [4, 14, 16, 26, 39, 45, 51, 57]
    X = [4, 14, 16, 27, 33, 47, 53, 58]
    X = [4, 14, 17, 27, 39, 40, 50, 61]
    X = [4, 14, 17, 29, 34, 40, 51, 63]
    X = [4, 14, 17, 29, 34, 40, 55, 59]
    X = [4, 14, 19, 24, 34, 47, 53, 57]
    X = [4, 15, 19, 24, 34, 45, 49, 62]
    X = [4, 15, 19, 24, 38, 41, 53, 58]
    X = [5, 8, 20, 25, 39, 42, 54, 59]
    X = [5, 9, 22, 24, 34, 44, 55, 59]
    X = [5, 9, 22, 24, 35, 47, 52, 58]
    X = [5, 10, 16, 30, 36, 47, 49, 59]
    X = [5, 10, 16, 31, 35, 41, 54, 60]
    X = [5, 10, 16, 31, 36, 41, 51, 62]
    X = [5, 10, 20, 30, 32, 43, 49, 63]
    X = [5, 10, 20, 31, 32, 43, 49, 62]
    X = [5, 10, 22, 25, 35, 47, 48, 60]
    X = [5, 10, 22, 25, 39, 44, 48, 59]
    X = [5, 10, 22, 27, 32, 47, 49, 60]
    X = [5, 11, 16, 28, 39, 41, 54, 58]
    X = [5, 11, 17, 31, 36, 46, 48, 58]
    X = [5, 11, 22, 24, 34, 44, 49, 63]
    X = [5, 11, 22, 24, 39, 41, 52, 58]
    X = [5, 15, 17, 27, 32, 46, 52, 58]
    X = [6, 8, 18, 31, 37, 43, 49, 60]
    X = [6, 9, 19, 24, 39, 44, 50, 61]
    X = [6, 9, 21, 26, 32, 43, 55, 60]
    X = [6, 10, 16, 29, 39, 44, 49, 59]
    X = [6, 10, 23, 25, 36, 40, 53, 59]
    X = [6, 11, 17, 28, 39, 40, 50, 61]
    X = [6, 11, 17, 31, 37, 40, 50, 60]
    X = [6, 12, 18, 24, 37, 47, 49, 59]
    X = [7, 9, 19, 24, 38, 44, 50, 61]
    X = [7, 9, 20, 26, 32, 46, 51, 61]
    X = [7, 10, 16, 29, 33, 44, 54, 59]
    X = [7, 11, 16, 26, 37, 41, 54, 60]
    yes

    ?- halt.

### [Unification](bin/unificat.p)

    rlwrap ./prolog
    Interpreteur PROLOG avec syntaxe 'Edimbourg'.
    Algorithmes du PROLOG II de Marseille.

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
