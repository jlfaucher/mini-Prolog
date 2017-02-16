-- Fichier OBJETS.ADS
-- Spécifications du package de manipulation des objets de base PROLOG.


----------------------------------------------------------------------------------------------------------------------------------


package Objets_Prolog is


   ---------------------------------------------
   -- Les exceptions susceptibles d'etre levées.
   ---------------------------------------------
   Table_Doublets_Pleine : exception;                       -- par CREE_DOUBLET.
   Table_Carac_Pleine   : exception;                        -- par CREE_SYMBOLE.
   Table_Symb_Pleine    : exception;                        -- par CREE_SYMBOLE.
   Table_Var_Pleine     : exception;                        -- par CREE_VARIABLE.


   --------------------------------
   -- Description d'un mot memoire.
   --------------------------------
   type Mot is private;
   subtype Mot_Valeur is Integer;


   ----------------------------------------------------------------------
   -- Caractéristique des valeurs internes des atomes (entier et chaine).
   ----------------------------------------------------------------------
   subtype Type_Nombre is Mot_Valeur;                       -- La taille des entiers dépend de la taille d'un mot de la mémoire.
   Longmax_Pname : constant Positive := 255;                -- Longueur maximale d'une chaine de p-name.
   Pname_Buffer : String(1..Longmax_Pname);                 -- Servira pour les entrées-sorties et pour toute opération sur p-names
   Pname_Long : Natural;                                    -- Longueur du p-name courant.
   Pname_Print_Quote : Boolean;                             -- Flag indiquant s'il faut entourer le p-name par des quotes.


   ----------------------------------------------
   -- Nombre maximum de variables dans une regle.
   ----------------------------------------------
   Nbre_Max_Var_Par_Regle : constant := 100;


   -------------------------------
   -- Procedures de mise au point.
   -------------------------------
   procedure Put_Mot(Arg : Mot);
   procedure Debug;


   --------------------------------------------------------------------------------------------
   -- Procédure utilitaire pour appeler une fonction renvoyant un MOT sans récuperer sa valeur.
   --------------------------------------------------------------------------------------------
   procedure Call(V : Mot);


   ------------------------------------
   -- Comparaison de deux mots mémoire.
   ------------------------------------
   function Egalite_Mot(Obj1, Obj2 : Mot) return Boolean;


   ----------------
   -- Informations.
   ----------------

   procedure Types_Informations;
   procedure Informations;


   ---------------------------------------------------------
   -- Fonctions de reconnaissance du type d'un objet PROLOG.
   ---------------------------------------------------------
   function Entier(Obj : Mot) return Boolean;
   function Symbole(Obj : Mot) return Boolean;
   function Atome(Obj : Mot) return Boolean;
   function Doublet_V(Obj : Mot) return Boolean;
   function Doublet_F(Obj : Mot) return Boolean;
   function Doublet_L(Obj : Mot) return Boolean;
   function Doublet(Obj : Mot) return Boolean;
   function Variable(Obj : Mot) return Boolean;
   function Vect1(Obj : Mot) return Boolean;
   function Vect2(Obj : Mot) return Boolean;
   function Func1(Obj : Mot) return Boolean;
   function Func2(Obj : Mot) return Boolean;


   ---------------------------------------
   -- Fonctions d'acces aux objets PROLOG.
   ---------------------------------------
   procedure Symbole_Chaine(Obj : in Mot;
                            Chaine : out String; Long : out Natural;
                            Print_Quote: out Boolean);    -- Représentation externe d'un symbole.
   function Entier_Val(Obj : Mot) return Type_Nombre;     -- Renvoie la valeur numérique d'un entier.
   function Variable_Rang(Obj : Mot) return Type_Nombre;  -- Renvoie le rang d'une variable.
   function Variable_Nom(Ind : Type_Nombre) return Mot;   -- Renvoie le symbole correspondant au nom de la variable.
   function Premier(Obj : Mot) return Mot;                -- Correspond au CAR de LISP.
   function Reste(Obj : Mot) return Mot;                  -- Correspond au CDR de LISP.
   function Id_Liste_Regles(Symb : Mot) return Mot;       -- Renvoie la liste des regles rattachées au symbole SYMB.
   function Nbre_De_Variables return Natural;             -- Renvoie le nbre de variables crées depuis le dernier RAZ_VARIABLES.


   ----------------------------------------------
   -- Fonctions de fabrication des objets PROLOG.
   ----------------------------------------------
   function Cree_Symbole(Str : String; Print_Quote : Boolean := False) return Mot;
   function Cree_Variable(Str : String) return Mot;
   function Cree_Variable(Num : Natural) return Mot;
   procedure Raz_Variables;
   function Cree_Entier(Nombre : Type_Nombre) return Mot;
   function Cree_Doublet_V(Car, Cdr : Mot) return Mot;
   function Cree_Doublet_F(Car, Cdr : Mot) return Mot;
   function Cree_Doublet_L(Car, Cdr : Mot) return Mot;
   function Concatene(Obj1, Obj2 : Mot) return Mot;
   function Cree_Liste(Obj1 : Mot) return Mot;               -- Liste d'un objet
   function Cree_Liste(Obj1, Obj2 : Mot) return Mot;         -- Liste de deux objets
   function Cree_Liste(Obj1, Obj2, Obj3 : Mot) return Mot;   -- Liste de trois objets
   function Cree_Vecteur(Obj1 : Mot) return Mot;             -- Vecteur d'un objet
   function Cree_Vecteur(Obj1, Obj2 : Mot) return Mot;       -- Vecteur de deux objets
   function Cree_Vecteur(Obj1, Obj2, Obj3 : Mot) return Mot; -- Vecteur de trois objets
   function Vecteur_Liste(Obj : Mot) return Mot;             -- Transformation physique d'un vecteur en liste
   procedure Id_Liste_Regles(Symb, Liste : Mot);             -- Rattache la liste de regles LISTE au symbole SYMB


   ---------------------------------------
   -- Déclaration des expansions en ligne.
   ---------------------------------------
   -- pragma INLINE(ENTIER, SYMBOLE, ATOME, DOUBLET_V, DOUBLET_F, DOUBLET_L, DOUBLET, VARIABLE,
   --              ENTIER_VAL, VARIABLE_RANG,
   --              PREMIER, RESTE,
   --              EGALITE_MOT,
   --              CREE_ENTIER, CREE_DOUBLET_V, CREE_DOUBLET_F, CREE_DOUBLET_L);


private


   ---------------------------------------------------------------
   -- Déclaration des types possibles du contenu d'un mot mémoire.
   -- ATTENTION !!! Ne pas changer l'ordre des types.
   ---------------------------------------------------------------
   type Contenu_Memoire is (Libre,                  -- Le mot est libre
                            Variable,               -- Le mot contient un numéro de variable
                            Symbole,                -- Le mot contient un indice faisant référence à la table des symboles
                            Entier,                 -- Le mot contient un entier
                            Doublet_V,              -- Le mot contient une adresse pointant sur un doublet-vecteur
                            Doublet_F,              -- Le mot contient une adresse pointant sur un doublet-fonction
                            Doublet_L);             -- Le mot contient une adresse pointant sur un doublet-liste


   --------------------------------
   -- Description d'un mot mémoire.
   --------------------------------
   type Mot is record
      T  : Contenu_Memoire;                         -- Le type du mot mémoire
      Val    : Mot_Valeur;                          -- Le contenu du mot mémoire
   end record;


end Objets_Prolog;