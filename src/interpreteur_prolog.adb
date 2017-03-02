-- Fichier INTERPRE.ADB
-- Corps du package de l'interpréteur PROLOG.
-- Algorithmes extraits de "L'ANATOMIE DE PROLOG" par Michel Van Caneghem.


with Text_Io; use Text_Io;
with Int32_Io; use Int32_Io;
with Objets_Prolog; use Objets_Prolog;
with Es_Prolog; use Es_Prolog;
with Infos;


----------------------------------------------------------------------------------------------------------------------------------


package body Interpreteur_Prolog is


   ----------------------------------------
   -- Les chaines de caracteres constantes.
   ----------------------------------------
   Point                    : constant String := ".";
   Virgule                  : constant String := ",";
   Parenthese_Gauche_Espace : constant String := "( ";
   Point_Virgule_Espace     : constant String := "; ";
   Parenthese_Droite        : constant String := ")";
   Espace_2points_Moins     : constant String := " :-";
   Virgule_Espace           : constant String := ", ";
   Espace_Egal_Espace       : constant String := " = ";
   Indicateur_Gele          : constant String := "???";
   Ansi_Cursor_Up           : constant String := Ascii.Esc & "[1A";
   Traits_H                 : constant String := "------------------------------------------------------------------------------";


   --------------------------
   -- Les messages constants.
   --------------------------
   Yes                    : constant String := "yes";
   No                     : constant String := "no";
   Recorded               : constant String := "Recorded";
   Illegal_Rule           : constant String := "Illegal rule";
   Titre1                 : constant String := "Interpreteur PROLOG avec syntaxe 'Edimbourg'.";
   Titre2                 : constant String := "Algorithmes du PROLOG II de Marseille.";
   Messagefin             : constant String := "halt.......";
   Chargefichier          : constant String := "Loading file PROLOG.SYS...";
   Effacechargefichier    : constant String := "                          ";
   Cannot_Load_Prolog_Sys : constant String := "Can't load PROLOG.SYS - Aborting...";
   Xstorage_Error         : constant String := "Not enough memory in CPU stack";
   Xtable_Doublets_Pleine : constant String := "Not enough memory in nodes space";
   Xtable_Symb_Pleine     : constant String := "Not enough memory in names space";
   Xtable_Var_Pleine      : constant String := "Not enough memory in variables table";
   Xpile_Subst_Pleine     : constant String := "Not enough memory for substitutions";
   Xpile_Sauve_Pleine     : constant String := "Not enough memory for saving substitutions";
   Xpile_Nomvar_Pleine    : constant String := "Not enough memory for saving variables names";
   Xpile_Eq_Pleine        : constant String := "Not enough memory in equations space";
   Xpile_Etape_Pleine     : constant String := "Not enough memory in goals stack";
   Xpile_Choix_Pleine     : constant String := "Not enough memory in choices stack";
   Xpile_Ren_Pleine       : constant String := "Not enough memory for renaming variables";
   Aborting               : constant String := "Aborting current evaluation...";


   --------------------------------------------
   -- Les symboles utilisés par l'interpréteur.
   --------------------------------------------
   Liste_Vide         : constant Mot := Cree_Symbole("[]");
   Vecteur_Vide       : constant Mot := Cree_Symbole("()");
   S_Point_Virgule    : constant Mot := Cree_Symbole(";");
   S_2points_Moins    : constant Mot := Cree_Symbole(":-");
   S_Moins            : constant Mot := Cree_Symbole("-");
   S_Plus             : constant Mot := Cree_Symbole("+");
   S_Etoile           : constant Mot := Cree_Symbole("*");
   S_Slash            : constant Mot := Cree_Symbole("/");
   S_Mod              : constant Mot := Cree_Symbole("mod");
   S_Puissance        : constant Mot := Cree_Symbole("^");
   S_Regle_Predefinie : constant Mot := Cree_Symbole("internal_call");
   S_Err              : constant Mot := Cree_Symbole("err");
   S_Fail             : constant Mot := Cree_Symbole("fail");
   S_Cut              : constant Mot := Cree_Symbole("!");
   S_Prolog_Sys       : constant Mot := Cree_Symbole("PROLOG.SYS");
   S_Abs              : constant Mot := Cree_Symbole("abs");
   S_Yes_No           : constant Mot := Cree_Symbole("yes_no");
   S_First            : constant Mot := Cree_Symbole("first");
   S_All              : constant Mot := Cree_Symbole("all");
   S_On               : constant Mot := Cree_Symbole("on");
   S_Off              : constant Mot := Cree_Symbole("off");


   ------------------------------------
   -- Pile des substitutions (valeurs).
   ------------------------------------
   -- La pile des substitutions contient les valeurs des variables.
   -- Une variable étant codée par un numéro relatif, il faut fournir la base de l'environnement courant dans PILE_SUBST pour
   -- pouvoir accéder à la valeur correspondante : Adresse = Environnement + numéro relatif.

   Taille_Pile_Subst : constant := 8100;
   subtype Indice_Pile_Subst is Positive range 1..Taille_Pile_Subst;
   subtype Indice_Pile_Subst_Etendu is Natural range 0..Taille_Pile_Subst;

   Sp_Subst : Indice_Pile_Subst;                             -- Pointe sur le 1er emplacement libre.
   Sp_Subst_Max : Indice_Pile_Subst := Indice_Pile_Subst'First;

   type Type_Equation is (Libre,                             -- La variable est libre.
      Gele,                              -- La variable est rattachée à un processus gelé.
      Lie,                               -- La variable est liée à un objet PROLOG (elle a une valeur).
      Ref);                              -- La variable pointe sur une autre variable plus ancienne.

   type P_Subst is record
      Stype : Type_Equation;
      Sub   : Indice_Pile_Subst;
      Ter   : Mot;
   end record;

   -- SUB : Si STYPE = LIE ou GELE alors SUB = base de l'environnement des variables du terme.
   --       Si STYPE = REF alors SUB = position absolue dans PILE_SUBST de la variable pointée.
   -- TER : Si STYPE = LIE alors TER = l'objet PROLOG correspondant à la valeur de la variable.
   --       Si STYPE = GELE alors TER = le processus gelé (qui est un objet PROLOG).

   Sub_Vide : constant Indice_Pile_Subst := Indice_Pile_Subst'First; -- Valeur de remplissage.
   Ter_Vide : constant Mot               := Liste_Vide;              -- idem.

   type T_Pile_Subst is array(Indice_Pile_Subst) of P_Subst;
   type A_Pile_Subst is access T_Pile_Subst;
   Pile_Subst : constant A_Pile_Subst := new T_Pile_Subst;

   Indice_Variable_Muette : constant Indice_Pile_Subst_Etendu := Indice_Pile_Subst_Etendu'First; -- Adresse absolue de variable '_'


   ---------------------------------------------------
   -- Pile de sauvegarde (restauration des variables).
   ---------------------------------------------------
   -- Pile associée à PILE_SUBST pour pouvoir restaurer les valeurs des variables lors d'un retour en arriere.

   Taille_Pile_Sauve : constant := 5000;
   subtype Indice_Pile_Sauve is Positive range 1..Taille_Pile_Sauve;
   Sp_Sauve : Indice_Pile_Sauve;                             -- Pointe sur le 1er emplacement libre.
   Sp_Sauve_Max : Indice_Pile_Sauve := Indice_Pile_Sauve'First;

   type P_Sauve is record
      Ptr_Pile_Subst : Indice_Pile_Subst;                     -- Position absolue dans PILE_SUBST pour restaurer TYPE, SUB et TER.
      Stype          : Type_Equation;
      Sub            : Indice_Pile_Subst;
      Ter            : Mot;
   end record;

   type T_Pile_Sauve is array(Indice_Pile_Sauve) of P_Sauve;
   type A_Pile_Sauve is access T_Pile_Sauve;
   Pile_Sauve : constant A_Pile_Sauve := new T_Pile_Sauve;


   -------------------------------
   -- Pile des noms des variables.
   -------------------------------
   -- Pile contenant les noms des variables de l'environnement initial, pour chaque niveau de driver.

   Taille_Pile_Nomvar : constant := 2 * Nbre_Max_Var_Par_Regle;
   subtype Indice_Pile_Nomvar is Positive range 1..Taille_Pile_Nomvar;
   Sp_Nomvar : Indice_Pile_Nomvar;
   Sp_Nomvar_Max : Indice_Pile_Nomvar := Indice_Pile_Nomvar'First;

   type T_Pile_Nomvar is array(Indice_Pile_Nomvar) of Mot;
   type A_Pile_Nomvar is access T_Pile_Nomvar;
   Pile_Nomvar : constant A_Pile_Nomvar := new T_Pile_Nomvar;


   --------------------------------------------------------
   -- Pile des équations (utilisée lors de l'unification ).
   --------------------------------------------------------
   -- La pile de substitution permet de représenter des équations de la forme "variable = terme".
   -- La pile des équations permet de représenter des équations de la forme "terme = terme".

   Taille_Pile_Eq : constant := 5000;
   subtype Indice_Pile_Eq is Positive range 1..Taille_Pile_Eq;
   Sp_Eq : Indice_Pile_Eq;                                   -- Pointe sur le 1er emplacement libre.
   Sp_Eq_Max : Indice_Pile_Eq := Indice_Pile_Eq'First;

   type P_Eq is record
      Terme1 : Mot;                                           -- L'objet PROLOG représentant le 1er terme
      Env1 : Indice_Pile_Subst;                               -- et son environnement.
      Terme2 : Mot;                                           -- L'objet PROLOG représentant le 2nd terme
      Env2 : Indice_Pile_Subst;                               -- et son environnement.
   end record;

   type T_Pile_Eq is array(Indice_Pile_Eq) of P_Eq;
   type A_Pile_Eq is access T_Pile_Eq;
   Pile_Eq : constant A_Pile_Eq := new T_Pile_Eq;


   ------------------------------------------------------------------
   -- Pile des étapes (contient la liste des buts restant à effacer).
   ------------------------------------------------------------------

   Taille_Pile_Etape : constant := 8000;
   subtype Indice_Pile_Etape is Positive range 1..Taille_Pile_Etape;
   Sp_Etape : Indice_Pile_Etape;                             -- Pointe juste avant le 1er emplacement libre.
   Sp_Etape_Max : Indice_Pile_Etape := Indice_Pile_Etape'First;

   type P_Etape is record
      Reste_Sous_Buts : Mot;                                  -- Reste des sous-buts du but courant à effacer.
      Reste_Buts      : Indice_Pile_Etape;                    -- Reste des buts à effacer.
      Environnement   : Indice_Pile_Subst;                    -- Environnement associé.
   end record;

   type T_Pile_Etape is array(Indice_Pile_Etape) of P_Etape;
   type A_Pile_Etape is access T_Pile_Etape;
   Pile_Etape : constant A_Pile_Etape := new T_Pile_Etape;


   ----------------------------------------------
   -- Pile des choix (pour le retour en arriere).
   ----------------------------------------------

   Taille_Pile_Choix : constant := 5000;
   subtype Indice_Pile_Choix is Positive range 1..Taille_Pile_Choix;
   Sp_Choix : Indice_Pile_Choix;                             -- Pointe juste avant le 1er emplacement libre.
   Sp_Choix_Max : Indice_Pile_Choix := Indice_Pile_Choix'First;

   type P_Choix is record
      Regles_Restantes : Mot;
      Etape_Retour     : Indice_Pile_Etape;
      Env_Retour       : Indice_Pile_Subst;
      Sauve_Retour     : Indice_Pile_Sauve;
      But_Retour       : Mot;
      Env_But_Retour   : Indice_Pile_Subst;
      Reste_Retour     : Indice_Pile_Etape;
   end record;

   type T_Pile_Choix is array(Indice_Pile_Choix) of P_Choix;
   type A_Pile_Choix is access T_Pile_Choix;
   Pile_Choix : constant A_Pile_Choix := new T_Pile_Choix;


   -----------------------------------
   -- Pile de renommage des variables.
   -----------------------------------

   subtype Indice_Pile_Ren is Positive range 1..Nbre_Max_Var_Par_Regle;
   Sp_Ren : Indice_Pile_Ren;
   Sp_Ren_Max : Indice_Pile_Ren := Indice_Pile_Ren'First;

   type T_Pile_Ren is array(Indice_Pile_Ren) of Natural;
   type A_Pile_Ren is access T_Pile_Ren;
   Pile_Ren : constant A_Pile_Ren := new T_Pile_Ren;


   -----------------------------------------------
   -- Variables globales de l'interpréteur PROLOG.
   -----------------------------------------------

   But_Courant : Mot;
   Env_Courant : Indice_Pile_Subst;
   Nb_Var_But  : Natural;

   Reste_De_Buts : Indice_Pile_Etape;

   Buts_Geles : Mot;                                         -- Vecteur des processus gelés.
   Env_Geles : Indice_Pile_Subst;                            -- Environnement des processus gelés.

   Liste_Regles : Mot;                                       -- Pour PROLOG : Liste des regles rattachées à identif courant.

   Env_Echec : Indice_Pile_Subst;

   X, X1, X2 : Indice_Pile_Subst_Etendu;                     -- Utilisé par unification. Déclaré global pour économiser pile CPU.

   X_Point_Y : constant Mot := Cree_Doublet_V(Cree_Variable(1),
      Cree_Variable(2)); -- Utilisé pour relancer des processus gelés (réunion de vecteurs)

   Compteur_Reponses : Natural;
   Reponse_Affirmative : Boolean;                            -- C'est la réponse à la question posée.
   Fini : Boolean;

   Sp_Subst_Initial  : Indice_Pile_Subst;                    -- Pour les appels récursifs de l'interpréteur.
   Sp_Sauve_Initial  : Indice_Pile_Sauve;
   Sp_Nomvar_Initial : Indice_Pile_Nomvar;
   Sp_Etape_Initial  : Indice_Pile_Etape;
   Sp_Choix_Initial  : Indice_Pile_Choix;

   type Fonctionnement is (Interrogation, Consultation, Reconsultation); -- Le mode de fonctionnement de l'interpréteur.
   Mode_F : Fonctionnement;

   subtype Reponse is Natural; -- Le mode de réponse de l'interpréteur.
   Oui_Non : constant := 0;
   Premiere_Reponse : constant := 1;
   Toutes_Reponses : constant := Natural'Last;
   Mode_R : Reponse;

   type Mode_Ajout is (Debut, Fin);                          -- Pour les primitives assert, asserta et assertz.

   Liste_Def    : Mot := Liste_Vide;                         -- Liste des identifs avec une regle rattachée, dans l'ordre de saisie
   Liste_System : Mot := Liste_Vide;                         -- Liste des identifs avec une regle prédéfinie rattachée (PROLOG.SYS)


   ----------------
   -- Informations.
   ----------------


   procedure Informations is
      Taille : Integer;
   begin
      Taille := P_Subst'Size / 8;
      Infos("Pile des substitutions",
         Int32(Taille), Int32(Sp_Subst), Int32(Sp_Subst_Max),
         Int32(Taille_Pile_Subst), Int32(Taille_Pile_Subst) * Int32(Taille));
      Taille := P_Sauve'Size / 8;
      Infos("Pile de sauvegarde",
         Int32(Taille), Int32(Sp_Sauve), Int32(Sp_Sauve_Max),
         Int32(Taille_Pile_Sauve), Int32(Taille_Pile_Sauve) * Int32(Taille));
      Taille := Mot'Size / 8;
      Infos("Pile des noms de variables",
         Int32(Taille), Int32(Sp_Nomvar), Int32(Sp_Nomvar_Max),
         Int32(Taille_Pile_Nomvar), Int32(Taille_Pile_Nomvar) * Int32(Taille));
      Taille := P_Eq'Size / 8;
      Infos("Pile des equations",
         Int32(Taille), Int32(Sp_Eq), Int32(Sp_Eq_Max),
         Int32(Taille_Pile_Eq), Int32(Taille_Pile_Eq) * Int32(Taille));
      Taille := P_Etape'Size / 8;
      Infos("Pile des etapes",
         Int32(Taille), Int32(Sp_Etape), Int32(Sp_Etape_Max),
         Int32(Taille_Pile_Etape), Int32(Taille_Pile_Etape) * Int32(Taille));
      Taille := P_Choix'Size / 8;
      Infos("Pile des choix",
         Int32(Taille), Int32(Sp_Choix), Int32(Sp_Choix_Max),
         Int32(Taille_Pile_Choix), Int32(Taille_Pile_Choix) * Int32(Taille));
      Taille := Natural'Size / 8;
      Infos("Pile de renommage des variables",
         Int32(Taille), Int32(Sp_Ren), Int32(Sp_Ren_Max),
         Int32(Nbre_Max_Var_Par_Regle), Int32(Nbre_Max_Var_Par_Regle) * Int32(Taille));
   end Informations;


   ---------------------------------------------
   -- Manipulation de la pile des substitutions.
   ---------------------------------------------


   function Libre(Ptr : Indice_Pile_Subst) return Boolean is -- Vrai si substitution de type LIBRE.
   begin
      return Pile_Subst(Ptr).Stype = Libre;
   end Libre;


   function Gele(Ptr : Indice_Pile_Subst) return Boolean is  -- Vrai si substitution de type GELE.
   begin
      return Pile_Subst(Ptr).Stype = Gele;
   end Gele;


   function Lie(Ptr : Indice_Pile_Subst) return Boolean is   -- Vrai si substitution de type LIE.
   begin
      return Pile_Subst(Ptr).Stype = Lie;
   end Lie;


   function Ref(Ptr : Indice_Pile_Subst) return Boolean is   -- Vrai si substitution de type REF.
   begin
      return Pile_Subst(Ptr).Stype = Ref;
   end Ref;

   function Stype(Ptr : Indice_Pile_Subst) return Type_Equation is   -- Renvoie le champ TYPE.
   begin
      return Pile_Subst(Ptr).Stype;
   end Stype;


   function Sub(Ptr : Indice_Pile_Subst) return Indice_Pile_Subst is -- Renvoie le champ SUB.
   begin
      return Pile_Subst(Ptr).Sub;
   end Sub;


   function Ter(Ptr : Indice_Pile_Subst) return Mot is       -- Renvoie le champ TER.
   begin
      return Pile_Subst(Ptr).Ter;
   end Ter;


   -----------------------------------------
   -- Manipulation de la pile de sauvegarde.
   -----------------------------------------


   procedure Sauve_Subst(Ptr : Indice_Pile_Subst) is         -- Sauvegarde la substitution indiquée.
   begin
      if Ptr < Env_Echec then
         Pile_Sauve(Sp_Sauve) := (Ptr, Stype(Ptr), Sub(Ptr), Ter(Ptr));
         if Sp_Sauve /= Pile_Sauve'Last then
            Sp_Sauve := Sp_Sauve + 1;
            if Sp_Sauve > Sp_Sauve_Max then Sp_Sauve_Max := Sp_Sauve; end if;
         else
            raise Pile_Sauve_Pleine;
         end if;
      end if;
   end Sauve_Subst;


   procedure Restaure_Subst is                               -- Restauration de la derniere substitution empilée.
   begin
      Sp_Sauve := Sp_Sauve - 1;
      Pile_Subst(Pile_Sauve(Sp_Sauve).Ptr_Pile_Subst) :=
         (Pile_Sauve(Sp_Sauve).Stype,
         Pile_Sauve(Sp_Sauve).Sub,
         Pile_Sauve(Sp_Sauve).Ter);
   end Restaure_Subst;


   -------------------------------------------------
   -- Manipulation de la pile des noms de variables.
   -------------------------------------------------

   procedure Empile_Nomvar(Objet : Mot) is
   begin
      Pile_Nomvar(Sp_Nomvar) := Objet;
      if Sp_Nomvar /= Pile_Nomvar'Last then
         Sp_Nomvar := Sp_Nomvar + 1;
         if Sp_Nomvar > Sp_Nomvar_Max then Sp_Nomvar_Max := Sp_Nomvar; end if;
      else
         raise Pile_Nomvar_Pleine;
      end if;
   end Empile_Nomvar;


   function Nom_Global(Numvar : Positive) return Mot is
   begin
      return Pile_Nomvar(Sp_Nomvar_Initial + Numvar - 1);
   end Nom_Global;


   --------------------------------------
   -- Manipulation de la pile des étapes.
   --------------------------------------
   -- Pas de dépilage car un retour en arriere peut faire sauter de nombreuses étapes.


   procedure Empile_Etape(Terme : Mot; Env : Indice_Pile_Subst) is   -- Empile la liste des buts restants à effacer.
   begin
      if Sp_Etape /= Pile_Etape'Last then
         Sp_Etape := Sp_Etape + 1;
         if Sp_Etape > Sp_Etape_Max then Sp_Etape_Max := Sp_Etape; end if;
      else
         raise Pile_Etape_Pleine;
      end if;
      Pile_Etape(Sp_Etape) := (Terme, Reste_De_Buts, Env);
      Reste_De_Buts := Sp_Etape;
   end Empile_Etape;


   -----------------------------------------
   -- Manipulation de la pile des équations.
   -----------------------------------------

   procedure Nouvelle_Equation(Terme1 : Mot; Env1 : Indice_Pile_Subst;
         Terme2 : Mot; Env2 : Indice_Pile_Subst) is    -- Ajoute une equation <TERME1, ENV1> = <TERME2, ENV2>
   begin
      Pile_Eq(Sp_Eq) := (Terme1, Env1, Terme2, Env2);
      if Sp_Eq /= Pile_Eq'Last then
         Sp_Eq := Sp_Eq + 1;
         if Sp_Eq > Sp_Eq_Max then Sp_Eq_Max := Sp_Eq; end if;
      else
         raise Pile_Eq_Pleine;
      end if;
   end Nouvelle_Equation;


   procedure Nouvelle_Liste(Objet : Mot; Env_Objet : Indice_Pile_Subst) is   -- Pour la sortie des arbres infinis.
   begin
      Pile_Eq(Sp_Eq).Terme1 := Objet;
      Pile_Eq(Sp_Eq).Env1   := Env_Objet;
      if Sp_Eq /= Pile_Eq'Last then
         Sp_Eq := Sp_Eq + 1;
         if Sp_Eq > Sp_Eq_Max then Sp_Eq_Max := Sp_Eq; end if;
      else
         raise Pile_Eq_Pleine;
      end if;
   end Nouvelle_Liste;


   procedure Position_Liste(Objet : in Mot; Env_Objet : in Indice_Pile_Subst;
         Position : out Indice_Pile_Eq; Trouve : out Boolean) is
   begin
      Trouve := False;
      for I in Pile_Eq'First..Sp_Eq-1 loop
         if Egalite_Mot(Pile_Eq(I).Terme1, Objet) and then Pile_Eq(I).Env1 = Env_Objet then
            Position := I;
            Trouve := True;
            return;
         end if;
      end loop;
   end Position_Liste;


   -------------------------------------
   -- Manipulation de la pile des choix.
   -------------------------------------

   procedure Empile_Choix(Regles_Restantes : Mot;
         Etape_Retour     : Indice_Pile_Etape;
         Env_Retour       : Indice_Pile_Subst;
         Sauve_Retour     : Indice_Pile_Sauve;
         But_Retour       : Mot;
         Env_But_Retour   : Indice_Pile_Subst;
         Reste_Retour     : Indice_Pile_Etape) is
   begin
      if Sp_Choix /= Pile_Choix'Last then
         Sp_Choix := Sp_Choix + 1;
         if Sp_Choix > Sp_Choix_Max then Sp_Choix_Max := Sp_Choix; end if;
      else
         raise Pile_Choix_Pleine;
      end if;
      Pile_Choix(Sp_Choix) := (Regles_Restantes,
         Etape_Retour,
         Env_Retour,
         Sauve_Retour,
         But_Retour,
         Env_But_Retour,
         Reste_Retour);
   end Empile_Choix;


   procedure Depile_Choix(Regles_Restantes : out Mot;
         Etape_Retour     : out Indice_Pile_Etape;
         Env_Retour       : out Indice_Pile_Subst;
         Sauve_Retour     : out Indice_Pile_Sauve;
         But_Retour       : out Mot;
         Env_But_Retour   : out Indice_Pile_Subst;
         Reste_Retour     : out Indice_Pile_Etape) is
   begin
      Regles_Restantes := Pile_Choix(Sp_Choix).Regles_Restantes;
      Etape_Retour     := Pile_Choix(Sp_Choix).Etape_Retour;
      Env_Retour       := Pile_Choix(Sp_Choix).Env_Retour;
      Sauve_Retour     := Pile_Choix(Sp_Choix).Sauve_Retour;
      But_Retour       := Pile_Choix(Sp_Choix).But_Retour;
      Env_But_Retour   := Pile_Choix(Sp_Choix).Env_But_Retour;
      Reste_Retour     := Pile_Choix(Sp_Choix).Reste_Retour;
      Sp_Choix := Sp_Choix - 1;
   end Depile_Choix;


   ----------------------------------------
   -- Manipulation de la pile de renommage.
   ----------------------------------------

   procedure Empile_Ren(Numvar : Natural) is
   begin
      Pile_Ren(Sp_Ren) := Numvar;
      if Sp_Ren /= Pile_Ren'Last then
         Sp_Ren := Sp_Ren + 1;
         if Sp_Ren > Sp_Ren_Max then Sp_Ren_Max := Sp_Ren; end if;
      else
         raise Pile_Ren_Pleine;
      end if;
   end Empile_Ren;


   ----------------------------------------------------------------------------
   -- Les routines d'écriture d'un objet PROLOG avec les valeurs des variables.
   ----------------------------------------------------------------------------


   procedure Rep(Terme : in out Mot;                         -- L'objet PROLOG.
      Env : in out Indice_Pile_Subst;             -- L'environnement associé au terme.
      Indice : in out Indice_Pile_Subst_Etendu;   -- Adresse absolue dans PILE_SUBST ou dans PILE_EQ. (ne sert que si
      Recherche_Eq : in Boolean := True);         -- TERME en sortie est une variable).


   procedure Write(Objet : Mot; Env_Objet : Indice_Pile_Subst) is

      Position : Positive;                                    -- Pour les arbres infinis.
      Niveau   : Positive := 1;                               -- Idem.
      Trouve, Flagvar : Boolean;                              -- Idem.


      -- Inclus dans WRITE.
      procedure Write_Obj(Objet : Mot; Env_Objet : Indice_Pile_Subst);


      -- Inclus dans WRITE.
      procedure Write_Liste(Objet : Mot; Env_Objet : Indice_Pile_Subst) is
         Obj : Mot := Objet;
         Env : Indice_Pile_Subst := Env_Objet;
      begin
         Niveau := Niveau + 1;
         Put('[');
         loop
            Flagvar := False;                                   -- On le met à faux, mais PREMIER(OBJ) peut etre une variable.
            Write_Obj(Premier(Obj), Env);
            Obj := Reste(Obj);
            Flagvar := Variable(Obj);                           -- Va servir pour le prochain WRITE_OBJ.
            Rep(Obj, Env, X, False);
            if not Doublet_L(Obj) then
               if not Egalite_Mot(Obj, Liste_Vide) then Put('|'); Write_Obj(Obj, Env); end if;
               exit;
            else
               Put(Virgule_Espace);
            end if;
         end loop;
         Put(']');
         Niveau := Niveau - 1;
      end Write_Liste;


      -- Inclus dans WRITE.
      procedure Write_Vecteur(Objet : Mot; Env_Objet : Indice_Pile_Subst) is
         Obj : Mot := Objet;
         Env : Indice_Pile_Subst := Env_Objet;
      begin
         Niveau := Niveau + 1;
         Put('(');
         loop
            Flagvar := False;                                   -- On le met à faux, mais PREMIER(OBJ) peut etre une variable.
            Write_Obj(Premier(Obj), Env);
            Obj := Reste(Obj);
            Rep(Obj, Env, X, False);
            exit when not Doublet_V(Obj);
            Put(Virgule_Espace);
         end loop;
         Put(')');
         Niveau := Niveau - 1;
      end Write_Vecteur;


      -- Inclus dans WRITE.
      procedure Write_Obj(Objet : Mot; Env_Objet : Indice_Pile_Subst) is
         Obj : Mot := Objet;
         Env : Indice_Pile_Subst := Env_Objet;
         Sauve_Sp_Eq : Indice_Pile_Eq := Sp_Eq;
      begin
         Flagvar := Variable(Obj) or Flagvar;
         Rep(Obj, Env, X, False);                              -- FALSE pour ne pas parcourir la pile des équations.
         if Atome(Obj) then
            Ecrit(Obj, False);                                  -- FALSE pour indiquer de ne pas entourer avec des quotes.
         elsif Variable(Obj) then
            if Gele(X) then Put(Indicateur_Gele);
            elsif X - Sp_Subst_Initial <= Nb_Var_But then Ecrit(Nom_Global(X - Sp_Subst_Initial));
            else Ecrit(Cree_Variable(X));
            end if;
         else
            if Flagvar then
               -- Ici OBJ est un doublet correspondant à la valeur d'une variable. Attention aux arbres infinis.
               Position_Liste(Obj, Env, Position, Trouve);
               if Trouve then
                  Put('*'); Ecrit(Cree_Entier(Niveau - Position));
                  goto Restaure_Sp_Eq;
               else
                  Nouvelle_Liste(Obj, Env);
               end if;
            end if;
            if Doublet_L(Obj) then Write_Liste(Obj, Env);
            elsif Doublet_V(Obj) then Write_Vecteur(Obj, Env);
            elsif Doublet_F(Obj) then
               Flagvar := False;                                   -- On le met à faux, mais PREMIER(OBJ) peut etre une variable.
               Write_Obj(Premier(Obj), Env);
               Obj := Reste(Obj);
               Rep(Obj, Env, X, False);
               if Doublet(Obj) then
                  Flagvar := False;
                  Write_Obj(Obj, Env);
               else
                  Ecrit(Vecteur_Vide);
               end if;
            end if;
         end if;
         <<Restaure_Sp_Eq>>
            Sp_Eq := Sauve_Sp_Eq;
      end Write_Obj;


   begin -- WRITE
      Sp_Eq := Pile_Eq'First;                                 -- Pour les arbres infinis.
      Flagvar := False;                                       -- On le met à faux, mais OBJET peut etre une variable.
      Write_Obj(Objet, Env_Objet);
   end Write;


   function Echo_Actif return Boolean is
   begin
      return Echo or else Entree_Depuis_Standard;
   end Echo_Actif;


   procedure Imprimer_Reponse is
   begin
      Reponse_Affirmative := True;
      if Mode_R /= Oui_Non and Nb_Var_But /= 0 and Echo_Actif then
         Put("[");
         Put(Compteur_Reponses);
         Put("] ");
         for I in 1..Nb_Var_But loop
            Ecrit(Nom_Global(I));
            Put(Espace_Egal_Espace);
            Write(Cree_Variable(I), Sp_Subst_Initial);
            if I /= Nb_Var_But then Put(Virgule_Espace); end if;
         end loop;
         New_Line;
      end if;
      if Compteur_Reponses >= Mode_R then
         -- We have reached the maximum number of solutions to display.
         -- Maybe more solutions exist, display "..." to indicate that.
         Put("...");
         New_Line;
      end if;
   end Imprimer_Reponse;


   ---------------------------------------------------------------------
   -- Acces à la tete, à la queue et au nombre de variables d'une regle.
   ---------------------------------------------------------------------


   function Tete(Regle : Mot) return Mot is
   begin
      return Premier(Reste(Regle));
   end Tete;


   function Queue(Regle : Mot) return Mot is
   begin
      return Reste(Reste(Regle));
   end Queue;


   function Nb_Var(Regle : Mot) return Natural is
   begin
      return Entier_Val(Premier(Regle));
   end Nb_Var;


   ----------------------
   -- Listage des regles.
   ----------------------

   procedure Listage_Queue(Queue : Mot; Indent : Positive_Count) is
      Q : Mot := Queue;
   begin
      if Doublet_V(Q) then
         loop
            Listage_Queue(Premier(Q), Indent);
            Q := Reste(Q);
            exit when not Doublet_V(Q);
            Put_Line(Virgule);
         end loop;
      elsif Doublet_F(Q) and then Egalite_Mot(Premier(Q), S_Point_Virgule) and then Doublet_V(Reste(Q)) then
         Set_Col(Indent);
         Put(Parenthese_Gauche_Espace);
         Q := Reste(Q);                                        -- Le vecteur des arguments de ';'
         loop
            Listage_Queue(Premier(Q), Indent+2);
            New_Line;
            Set_Col(Indent);
            Q := Reste(Q);
            if Doublet_V(Q) then
               Put(Point_Virgule_Espace);
            else
               Put(Parenthese_Droite);
               exit;
            end if;
         end loop;
      else
         Set_Col(Indent);
         Ecrit(Q);
      end if;
   end Listage_Queue;


   procedure Listage(Symb : Mot) is
      Liste_Regles, Regle : Mot;
   begin
      if Symbole(Symb) then
         Liste_Regles := Id_Liste_Regles(Symb);
         while Doublet_L(Liste_Regles) loop
            Regle := Premier(Liste_Regles);
            Ecrit(Tete(Regle));
            if not Egalite_Mot(Queue(Regle), Vecteur_Vide) then
               Put_Line(Espace_2points_Moins);
               Listage_Queue(Queue(Regle), 4);
            end if;
            Put_Line(Point);
            Liste_Regles := Reste(Liste_Regles);
            if not Doublet_L(Liste_Regles) then New_Line; end if;
         end loop;
      end if;
   end Listage;


   procedure Listing(Objet : Mot) is
      Obj : Mot := Objet;
   begin
      if Symbole(Obj) then
         Listage(Obj);
      else
         while Doublet_L(Obj) loop
            Listage(Premier(Obj));
            Obj := Reste(Obj);
         end loop;
      end if;
   end Listing;


   -------------------------------------------------
   -- Mise à jour des regles associées à un symbole.
   -------------------------------------------------


   procedure Maj_Liste_Def(Identif : Mot) is
      -- Recherche si l'identif est dans la liste des définitions.
      Liste : Mot := Liste_Def;
      Doublet_Precedent : Mot := Liste_Vide;
   begin
      while Doublet_L(Liste) loop
         if Egalite_Mot(Premier(Liste), Identif) then return; end if;
         Doublet_Precedent := Liste;
         Liste := Reste(Liste);
      end loop;
      -- Ici DOUBLET_PRECEDENT est le dernier doublet de la liste des symboles.
      if Egalite_Mot(Doublet_Precedent, Liste_Vide) then      -- Si la liste des symboles est vide
         Liste_Def := Cree_Liste(Identif);
      else
         Call(Concatene(Doublet_Precedent, Cree_Liste(Identif)));
      end if;
   end Maj_Liste_Def;


   procedure Ajout_Regle(Identif : Mot; Mode_A : Mode_Ajout; Regle : Mot) is -- Rajoute la regle à la liste des regles de IDENTIF.
   begin
      if Doublet_L(Id_Liste_Regles(Identif)) then
         case Mode_A is
            when Debut =>
               Id_Liste_Regles(Identif,
                  Cree_Doublet_L(Regle,
                     Id_Liste_Regles(Identif)));
            when Fin =>
               Id_Liste_Regles(Identif,
                  Concatene(Id_Liste_Regles(Identif),
                     Cree_Liste(Regle)));
         end case;
      else
         Id_Liste_Regles(Identif, Cree_Liste(Regle));
      end if;
      Maj_Liste_Def(Identif);
   end Ajout_Regle;


   ---------------------------------------------
   -- Assertion statique (en mode consultation).
   ---------------------------------------------
   -- Ici les variables n'ont pas de valeur associée.


   procedure Assert_Statique(Objet : Mot; Mode_A : Mode_Ajout) is

      -- Inclus dans ASSERT_STATIQUE.
      procedure Ajout_Regle_Statique(Identif, Tete, Queue : Mot) is
      begin
         Ajout_Regle(Identif, Mode_A,
            Cree_Doublet_L(Cree_Entier(Nb_Var_But),
               Cree_Doublet_L(Tete, Queue)));
      end Ajout_Regle_Statique;


      -- Inclus dans ASSERT_STATIQUE.
      function Assertion_Statique(Tete, Queue : Mot) return Boolean is
         Obj : Mot := Tete;
      begin
         if Vect1(Obj) then Obj := Premier(Obj); end if;                       -- (expr) équivalent à expr.
         if Symbole(Obj) then
            Ajout_Regle_Statique(Obj, Obj, Queue);
            return True;
         elsif Doublet_F(Obj) then
            declare
               Functor : Mot := Premier(Obj);
            begin
               if Symbole(Functor) then
                  Ajout_Regle_Statique(Functor, Obj, Queue);
                  return True;
               end if;
            end;
         end if;
         return False;
      end Assertion_Statique;


      -- Inclus dans ASSERT_STATIQUE.
      function Assertion_Statique(Objet : Mot) return Boolean is
         Obj : Mot := Objet;
      begin
         if Vect1(Obj) then Obj := Premier(Obj); end if;                       -- (expr) équivalent à expr.
         if Doublet_F(Obj) then
            declare
               Functor : Mot := Premier(Obj);
               Arg : Mot := Reste(Obj);
            begin
               if Egalite_Mot(Functor, S_2points_Moins) and then Vect2(Arg) then -- Si forme :-(tete, queue).
                  return Assertion_Statique(Premier(Arg),
                     Premier(Reste(Arg)));
               end if;
            end;
         end if;
         return Assertion_Statique(Obj, Vecteur_Vide);
      end Assertion_Statique;

   begin -- ASSERT_STATIQUE
      if Assertion_Statique(Objet) then
         if Echo_Actif then Put_Line(Recorded); end if;
      else
         Put_Line(Illegal_Rule);
      end if;
   end Assert_Statique;


   ---------------------------------------------------
   -- Assertion dynamique (en cours d'interprétation).
   ---------------------------------------------------
   -- Ici les variables peuvent avoir des valeurs rattachées. Il faut tenir compte de la possibilité d'arbres infinis.


   procedure Assert_Dynamique(Objet : Mot; Env_Objet : Indice_Pile_Subst; Mode_A : Mode_Ajout) is


      -- Inclus dans ASSERT_DYNAMIQUE.
      function Renomme(Numvar : Natural) return Natural is
      begin
         if Numvar = 0 then return 0; end if;                  -- La variable muette n'est pas renommée.
         for I in Pile_Ren'First..Sp_Ren - 1 loop
            if Pile_Ren(I) = Numvar then return I; end if;
         end loop;
         Empile_Ren(Numvar);
         return Sp_Ren - 1;
      end Renomme;


      -- Inclus dans ASSERT_DYNAMIQUE.
      function Expansion(Objet : Mot; Env_Objet : Indice_Pile_Subst) return Mot is
         -- Renvoie l'expression OBJ apres avoir remplacé les variables par leurs valeurs.
      begin
         if Variable(Objet) then
            declare
               Obj : Mot := Objet;
               Env : Indice_Pile_Subst := Env_Objet;
            begin
               Rep(Obj, Env, X);
               if Variable(Obj) then                             -- Si la variable est libre.
                  return Cree_Variable(Renomme(X));               -- alors on renomme la variable.
               else
                  return Expansion(Obj, Env);
               end if;
            end;
         elsif Doublet_L(Objet) then
            return Cree_Doublet_L(Expansion(Premier(Objet), Env_Objet),
               Expansion(Reste(Objet), Env_Objet));
         elsif Doublet_V(Objet) then
            return Cree_Doublet_V(Expansion(Premier(Objet), Env_Objet),
               Expansion(Reste(Objet), Env_Objet));
         elsif Doublet_F(Objet) then
            return Cree_Doublet_F(Expansion(Premier(Objet), Env_Objet),
               Expansion(Reste(Objet), Env_Objet));
         else
            return Objet;                                       -- C'est un atome.
         end if;
      end Expansion;


      -- Inclus dans ASSERT_DYNAMIQUE.
      procedure Ajout_Regle_Dynamique(Identif, Tete  : Mot; Env_Tete  : Indice_Pile_Subst;
            Queue : Mot; Env_Queue : Indice_Pile_Subst) is
         Tete_Queue : Mot;
      begin
         Sp_Ren := Pile_Ren'First;
         Tete_Queue := Cree_Doublet_L(Expansion(Tete, Env_Tete),
            Expansion(Queue, Env_Queue));
         Ajout_Regle(Identif, Mode_A,
            Cree_Doublet_L(Cree_Entier(Sp_Ren - 1), Tete_Queue));
      end Ajout_Regle_Dynamique;


      -- Inclus dans ASSERT_DYNAMIQUE.
      function Assertion_Dynamique(Tete  : Mot; Env_Tete  : Indice_Pile_Subst;
            Queue : Mot; Env_Queue : Indice_Pile_Subst) return Boolean is
         Obj : Mot := Tete;
      begin
         if Vect1(Obj) then Obj := Premier(Obj); end if;                       -- (expr) équivalent à expr.
         if Symbole(Obj) then
            Ajout_Regle_Dynamique(Obj, Obj, Env_Tete, Queue, Env_Queue);
            return True;
         elsif Doublet_F(Obj) then
            declare
               Functor : Mot := Premier(Obj);
               Env_Functor : Indice_Pile_Subst := Env_Tete;
            begin
               Rep(Functor, Env_Functor, X);
               if Symbole(Functor) then
                  Ajout_Regle_Dynamique(Functor, Obj, Env_Tete, Queue, Env_Queue);
                  return True;
               end if;
            end;
         end if;
         return False;
      end Assertion_Dynamique;


      -- Inclus dans ASSERT_DYNAMIQUE.
      function Assertion_Dynamique(Objet : Mot; Env_Objet : Indice_Pile_Subst) return Boolean is
         Obj : Mot := Objet;
      begin
         if Vect1(Obj) then Obj := Premier(Obj); end if;                       -- (expr) équivalent à expr.
         if Doublet_F(Obj) then
            declare
               Functor : Mot := Premier(Obj);
               Arg : Mot := Reste(Obj);
               Env_Functor, Env_Arg : Indice_Pile_Subst := Env_Objet;
            begin
               Rep(Functor, Env_Functor, X);
               Rep(Arg, Env_Arg, X);
               if Egalite_Mot(Functor, S_2points_Moins) and then Vect2(Arg) then -- Si forme :-(tete, queue).
                  declare
                     Tete : Mot := Premier(Arg);
                     Queue : Mot := Premier(Reste(Arg));
                     Env_Tete, Env_Queue : Indice_Pile_Subst := Env_Arg;
                  begin
                     Rep(Tete, Env_Tete, X);
                     Rep(Queue, Env_Queue, X);
                     return Assertion_Dynamique(Tete, Env_Tete, Queue, Env_Queue);
                  end;
               end if;
            end;
         end if;
         return Assertion_Dynamique(Obj, Env_Objet, Vecteur_Vide, Sub_Vide);
      end Assertion_Dynamique;

   begin -- ASSERT_DYNAMIQUE
      if Assertion_Dynamique(Objet, Env_Objet) then
         But_Courant := Vecteur_Vide;                          -- L'assertion s'est bien passée, donc le but s'efface.
      else
         But_Courant := S_Fail;                                -- L'assertion ne peut pas se faire, donc échec.
      end if;
   end Assert_Dynamique;


   ---------------------------
   -- Chargement d'un fichier.
   ---------------------------

   procedure Driver(Mode_Fonctionnement : Fonctionnement; Mode_Reponse : Reponse);


   procedure Charge_Fichier(Objet : Mot; Env_Objet : Indice_Pile_Subst) is
      -- En entrée OBJET est une liste.
      Liste : Mot := Objet;
      Env_Liste : Indice_Pile_Subst := Env_Objet;
      Fichier_Courant : Mot;
      Env_Fichier_Courant : Indice_Pile_Subst;
      Mode_F : Fonctionnement;
   begin
      But_Courant := Vecteur_Vide;                            -- A priori on considere que ce but va s'effacer.
      while Doublet_L(Liste) and not Egalite_Mot(But_Courant, S_Fail) loop
         Fichier_Courant := Premier(Liste);
         Env_Fichier_Courant := Env_Liste;
         Rep(Fichier_Courant, Env_Fichier_Courant, X);
         Mode_F := Consultation;                               -- Mode consultation si [fichier...].
         if Func1(Fichier_Courant) and then Egalite_Mot(Premier(Fichier_Courant), S_Moins) then
            Fichier_Courant := Premier(Reste(Fichier_Courant)); -- L'argument du moins unaire.
            Rep(Fichier_Courant, Env_Fichier_Courant, X);
            Mode_F := Reconsultation;                           -- Mode reconsultation si [-fichier...].
         end if;
         if Symbole(Fichier_Courant) then
            if Entree_Fichier(Fichier_Courant) then
               Driver(Mode_F, Oui_Non);
               Entree_Standard;
            else
               But_Courant := S_Fail;
            end if;
         else
            But_Courant := S_Fail;
         end if;
         Liste := Reste(Liste);                                -- Passe au fichier suivant.
      end loop;
   end Charge_Fichier;


   -----------------
   -- L'unification.
   -----------------


   procedure Rep(Terme : in out Mot;                 -- L'objet PROLOG.
         Env : in out Indice_Pile_Subst;             -- L'environnement associé au terme.
         Indice : in out Indice_Pile_Subst_Etendu;   -- Adresse absolue dans PILE_SUBST ou dans PILE_EQ. (ne sert que si
         Recherche_Eq : in Boolean := True) is       -- TERME en sortie est une variable).
   begin
      if Variable(Terme) then
         if Variable_Rang(Terme) = 0 then            -- Si variable muette '_'
            Indice := Indice_Variable_Muette;        -- Cet indice se situe en dehors de l'étendue de PILE_SUBST.
            return;
         end if;
         Indice := Env + Variable_Rang(Terme);
         while Ref(Indice) loop Indice := Sub(Indice); end loop;
         if Lie(Indice) then                         -- Si INDICE pointe sur une liaison à un objet PROLOG alors
            Env := Sub(Indice);                      -- fait pointer ENV et TERME sur l'objet lié.
            Terme := Ter(Indice);
         end if;
      end if;
      if not Recherche_Eq then return; end if;
      if Doublet(Terme) then
         for I in Pile_Eq'First..Sp_Eq-1 loop
            if Egalite_Mot(Terme, Pile_Eq(I).Terme1) and Env = Pile_Eq(I).Env1 then
               Terme := Pile_Eq(I).Terme2;
               Env   := Pile_Eq(I).Env2;
            end if;
         end loop;
      end if;
   end Rep;


   procedure Relancer(Buts : in out Mot;
         Env_Buts : in out Indice_Pile_Subst;
         Subst : in Indice_Pile_Subst) is              -- Construit la liste des processus à relancer.
   begin
      if not Egalite_Mot(Buts, Vecteur_Vide) then
         if Pile_Subst'Last - Sp_Subst < 2 then
            raise Pile_Subst_Pleine;
         else
            Pile_Subst(Sp_Subst) := (Lie, Env_Buts, Buts);             -- Variable 1 de l'environnement.
            Pile_Subst(Sp_Subst + 1) := (Lie, Sub(Subst), Ter(Subst)); -- Variable 2 de l'environnement.
            Buts := X_Point_Y;
            Env_Buts := Sp_Subst - 1;                                  -- Pour respecter convention '/'
            Sp_Subst := Sp_Subst + 2;
            if Sp_Subst > Sp_Subst_Max then Sp_Subst_Max := Sp_Subst; end if;
         end if;
      else
         Buts := Ter(Subst);
         Env_Buts := Sub(Subst);
      end if;
   end Relancer;


   function Unifiable(Terme1 : in Mot; Env1 : in Indice_Pile_Subst;
         Terme2 : in Mot; Env2 : in Indice_Pile_Subst) return Boolean is
      Pas_Unifiable : exception;                              -- Retour brutal des qu'on sait qu'on ne peut pas unifier.

      procedure Unifier(Terme1 : in Mot; Env1 : in Indice_Pile_Subst;
            Terme2 : in Mot; Env2 : in Indice_Pile_Subst) is
         T1 : Mot := Terme1;
         E1 : Indice_Pile_Subst := Env1;
         T2 : Mot := Terme2;
         E2 : Indice_Pile_Subst := Env2;
      begin
         loop
            Rep(T1, E1, X1);
            Rep(T2, E2, X2);
            if Variable(T1) then
               if Variable(T2) then
                  -- Ici X1 et X2 sont des pointeurs dans PILE_SUBST.
                  if X1 = Indice_Variable_Muette
                        or else X2 = Indice_Variable_Muette then
                     return;
                  end if;
                  if X1 = X2 then return; end if;                 -- X = X toujours effacé.
                  if X2 < X1 then
                     X := X1; X1 := X2; X2 := X;                  -- Ordre chronologique (important pour éviter les bouclages).
                  end if;
                  Sauve_Subst(X2);
                  if Gele(X2) then Relancer(Buts_Geles, Env_Geles, X2); end if;
                  Pile_Subst(X2) := (Ref, X1, Ter_Vide);          -- Fait pointer la variable la plus récente vers la plus ancienne.
                  return;
               else
                  -- Ici T1 est une variable mais pas T2. X1 est un pointeur dans PILE_SUBST.
                  if X1 /= Indice_Variable_Muette then
                     Sauve_Subst(X1);
                     if Gele(X1) then Relancer(Buts_Geles, Env_Geles, X1); end if;
                     Pile_Subst(X1) := (Lie, E2, T2);              -- Rattache la valeur à la variable.
                  end if;
                  return;
               end if;
            elsif Variable(T2) then
               -- Ici T2 est une variable, mais pas T1. X2 est un pointeur dans PILE_SUBST.
               if X2 /= Indice_Variable_Muette then
                  Sauve_Subst(X2);
                  if Gele(X2) then Relancer(Buts_Geles, Env_Geles, X2); end if;
                  Pile_Subst(X2) := (Lie, E1, T1);                -- Rattache la valeur à la variable.
               end if;
               return;
               -- A partir d'ici aucun des termes n'est une variable.
            elsif E1 = E2 and then Egalite_Mot(T1, T2) then
               return;                                           -- Egalité stricte.
            elsif Atome(T1) and then Egalite_Mot(T1, T2) then
               return;                                           -- Si T1 et T2 atomes, l'environnement n'a pas d'importance.
            else
               if (Doublet_L(T1) and Doublet_L(T2)) or else
                     (Doublet_V(T1) and Doublet_V(T2)) or else
                     (Doublet_F(T1) and Doublet_F(T2)) then
                  Nouvelle_Equation(T1, E1, T2, E2);
                  Unifier(Premier(T1), E1, Premier(T2), E2);
                  T1 := Reste(T1);
                  T2 := Reste(T2);
               else
                  raise Pas_Unifiable;                            -- Echec, retour brutal.
               end if;
            end if;
         end loop;
      end Unifier;

   begin -- UNIFIABLE
      Sp_Eq := Pile_Eq'First;                                 -- Vide la pile des equations.
      Unifier(Terme1, Env1, Terme2, Env2);
      Sp_Eq := Pile_Eq'First;                                 -- La pile des équations ne sert plus, une fois l'unification réalisée
      return True;
   exception
      when Pas_Unifiable => Sp_Eq := Pile_Eq'First;           -- Meme remarque.
         return False;
   end Unifiable;


   --------------------
   -- Le moteur PROLOG.
   --------------------


   procedure Regles_Accessibles(Objet : Mot; Env_Objet : Indice_Pile_Subst) is
      -- En entrée OBJET est soit un atome, soit une fonction.
      Obj : Mot := Objet;
      Env : Indice_Pile_Subst := Env_Objet;
   begin
      if Symbole(Obj) then
         Liste_Regles := Id_Liste_Regles(Obj);
      elsif Doublet_F(Obj) then
         Obj := Premier(Obj);                                  -- Le functor
         if Variable(Obj) then Rep(Obj, Env, X); end if;       -- Si forme X(...)
         if Symbole(Obj) then
            Liste_Regles := Id_Liste_Regles(Obj);
         else
            Liste_Regles := Liste_Vide;
         end if;
      else                                                    -- Si OBJ est un entier
         Liste_Regles := Liste_Vide;
      end if;
   end Regles_Accessibles;


   procedure Regle_Predefinie(Num_Regle : Mot; Env_Regle : Indice_Pile_Subst);


   function Avancer return Boolean is
   begin
      loop
         if Egalite_Mot(But_Courant, Vecteur_Vide) then
            if Reste_De_Buts = Sp_Etape_Initial then
               return False;
            else
               But_Courant   := Pile_Etape(Reste_De_Buts).Reste_Sous_Buts;
               Env_Courant   := Pile_Etape(Reste_De_Buts).Environnement;
               Reste_De_Buts := Pile_Etape(Reste_De_Buts).Reste_Buts;
            end if;
         end if;
         if Variable(But_Courant) then
            Rep(But_Courant, Env_Courant, X);
         end if;
         while Doublet_V(But_Courant) loop
            if not Egalite_Mot(Reste(But_Courant), Vecteur_Vide) then
               Empile_Etape(Reste(But_Courant), Env_Courant);
            end if;
            But_Courant := Premier(But_Courant);
            if Variable(But_Courant) then
               Rep(But_Courant, Env_Courant, X);
            end if;
         end loop;
         if Doublet_L(But_Courant) then
            Charge_Fichier(But_Courant, Env_Courant);
         elsif Egalite_Mot(But_Courant, S_Cut) then                        -- Le cut
            But_Courant := Vecteur_Vide;
            Sp_Choix := Sub(Env_Courant);
            if Sp_Choix /= Sp_Choix_Initial and then
                  Pile_Choix(Sp_Choix).Env_Retour = Env_Courant then       -- Tient compte du fait que le choix à supprimer peut etre
               Sp_Choix := Sp_Choix -1;                                    -- au niveau de la regle contenant le cut.
            end if;
         elsif Doublet_F(But_Courant) and then
               Egalite_Mot(Premier(But_Courant), S_Regle_Predefinie) and then
               Vect1(Reste(But_Courant)) then                              -- Si forme internal_call(arg).
            Regle_Predefinie(Premier(Reste(But_Courant)), Env_Courant);
         else                                                              -- Ici BUT_COURANT est soit un atome, soit une fonction.
            Regles_Accessibles(But_Courant, Env_Courant);
            return True;
         end if;
      end loop;
   end Avancer;


   procedure Preparer_Unification(Liste_Regles : in Mot;
         Nouvel_Env : in out Indice_Pile_Subst;
         Tete_Regle : out Mot;
         Base_Pile_Sauve : out Indice_Pile_Sauve) is
      Nbre_Var : Natural;
   begin
      Tete_Regle := Tete(Premier(Liste_Regles));
      Nbre_Var := Nb_Var(Premier(Liste_Regles));
      Nouvel_Env := Sp_Subst;
      if Pile_Subst'Last - Sp_Subst < 1 + Nbre_Var then
         raise Pile_Subst_Pleine;
      else
         Sp_Subst := Sp_Subst + 1 + Nbre_Var;
         if Sp_Subst > Sp_Subst_Max then Sp_Subst_Max := Sp_Subst; end if;
      end if;
      for I in 1..Nbre_Var loop
         Pile_Subst(Nouvel_Env + I) := (Libre, Sub_Vide, Ter_Vide);
      end loop;
      Base_Pile_Sauve := Sp_Sauve;
      if Egalite_Mot(Reste(Liste_Regles), Liste_Vide) then
         Env_Echec := Pile_Choix(Sp_Choix).Env_Retour;
      else
         Env_Echec := Nouvel_Env;
      end if;
      Buts_Geles := Vecteur_Vide;
   end Preparer_Unification;


   procedure Effacer_Suite_De_Buts is
      Base_Pile_Sauve : Indice_Pile_Sauve;
      Env_Regle : Indice_Pile_Subst;
      Tete_Regle : Mot;
   begin
      if not Avancer then
         -- Si la suite des buts initiaux ne comportait que des regles prédéfinies, alors elles ont été effacés directement depuis
         -- AVANCER. BUT_COURANT va nous indiquer si les buts ont pu etre effacés.
         Reponse_Affirmative := (But_Courant = Vecteur_Vide);
         return;
      end if;
      loop
         loop
            if Egalite_Mot(Liste_Regles, Liste_Vide) then goto Backtracking; end if;
            Preparer_Unification(Liste_Regles, Env_Regle, Tete_Regle, Base_Pile_Sauve);
            while not Unifiable(But_Courant, Env_Courant, Tete_Regle, Env_Regle) loop
               while Sp_Sauve /= Base_Pile_Sauve loop Restaure_Subst; end loop;
               Sp_Subst := Env_Regle;
               Liste_Regles := Reste(Liste_Regles);
               if Egalite_Mot(Liste_Regles, Liste_Vide) then goto Backtracking; end if;
               Preparer_Unification(Liste_Regles, Env_Regle, Tete_Regle, Base_Pile_Sauve);
            end loop;
            if not Egalite_Mot(Reste(Liste_Regles), Liste_Vide) then
               Empile_Choix(Reste(Liste_Regles),
                  Sp_Etape,
                  Env_Regle,
                  Base_Pile_Sauve,
                  But_Courant,
                  Env_Courant,
                  Reste_De_Buts);
            end if;
            Pile_Subst(Env_Regle) := (Libre, Sp_Choix, Liste_Regles);   -- Attention à SP_CHOIX
            if not Egalite_Mot(Buts_Geles, Vecteur_Vide) then
               But_Courant := Buts_Geles;
               Env_Courant := Env_Geles;
               if not Egalite_Mot(Queue(Premier(Liste_Regles)), Vecteur_Vide) then
                  Empile_Etape(Queue(Premier(Liste_Regles)), Env_Regle);
               end if;
            else
               But_Courant := Queue(Premier(Liste_Regles));
               Env_Courant := Env_Regle;
            end if;
            exit when not Avancer;
         end loop;
         ---------------------------
         -- Affichage de la réponse.
         ---------------------------
         Compteur_Reponses := Compteur_Reponses + 1;
         Imprimer_Reponse;
         if Compteur_Reponses >= Mode_R then return; end if;
         ---------------------
         -- Retour en arriere.
         ---------------------
         <<Backtracking>>
            if Sp_Choix = Sp_Choix_Initial then return;
         else
            Depile_Choix(Liste_Regles,
               Sp_Etape,
               Sp_Subst,
               Base_Pile_Sauve,
               But_Courant,
               Env_Courant,
               Reste_De_Buts);
            while Sp_Sauve /= Base_Pile_Sauve loop Restaure_Subst; end loop;
         end if;
      end loop;
   end Effacer_Suite_De_Buts;


   procedure Effacer(Expression : in Mot; Nb_Var : in Natural) is
      -- Sauve les variables globales de l'interpréteur.
      Sauve_But_Courant         : Mot               := But_Courant;
      Sauve_Env_Courant         : Indice_Pile_Subst := Env_Courant;
      Sauve_Nb_Var_But          : Natural           := Nb_Var_But;
      Sauve_Reste_De_Buts       : Indice_Pile_Etape := Reste_De_Buts;
      Sauve_Buts_Geles          : Mot               := Buts_Geles;
      Sauve_Env_Geles           : Indice_Pile_Subst := Env_Geles;
      Sauve_Liste_Regles        : Mot               := Liste_Regles;
      Sauve_Env_Echec           : Indice_Pile_Subst := Env_Echec;
      Sauve_Reponse_Affirmative : Boolean           := Reponse_Affirmative;
      -- Sauve les pointeurs des différentes piles (sauf PILE_EQ qui est une pile temporaire).
      Sauve_Sp_Subst            : Indice_Pile_Subst := Sp_Subst;
      Sauve_Sp_Sauve            : Indice_Pile_Sauve := Sp_Sauve;
      Sauve_Sp_Etape            : Indice_Pile_Etape := Sp_Etape;
      Sauve_Sp_Choix            : Indice_Pile_Choix := Sp_Choix;
      -- Sauve les pointeurs de base initiaux des différentes piles. (Sauf PILE_EQ qui est une pile temporaire).
      Sauve_Sp_Subst_Initial    : Indice_Pile_Subst := Sp_Subst_Initial;
      Sauve_Sp_Sauve_Initial    : Indice_Pile_Sauve := Sp_Sauve_Initial;
      Sauve_Sp_Etape_Initial    : Indice_Pile_Etape := Sp_Etape_Initial;
      Sauve_Sp_Choix_Initial    : Indice_Pile_Choix := Sp_Choix_Initial;
   begin
      -- Initialise les nouveaux pointeurs de base initiaux.
      Sp_Subst_Initial := Sp_Subst;
      Sp_Sauve_Initial := Sp_Sauve;
      Sp_Etape_Initial := Sp_Etape;
      Sp_Choix_Initial := Sp_Choix;
      -- Initialise certaines variables globales de l'interpréteur.
      But_Courant := Expression;                                          -- Le vecteur des buts à effacer.
      Nb_Var_But := Nb_Var;                                               -- Le nombre de variables dans cette suite de buts.
      Reste_De_Buts := Sp_Etape_Initial;
      Reponse_Affirmative := False;
      -- Préparation de l'environnement.
      Env_Courant := Sp_Subst;
      if Pile_Subst'Last - Sp_Subst < 1 + Nb_Var_But then
         raise Pile_Subst_Pleine;
      else
         Pile_Subst(Sp_Subst) := (Libre, Sp_Choix_Initial, Liste_Vide);    -- Attention à SP_CHOIX_INITIAL.
         for I in 1..Nb_Var_But loop
            Pile_Subst(Sp_Subst + I) := (Libre, Sub_Vide, Ter_Vide);
         end loop;
         Sp_Subst := Sp_Subst + 1 + Nb_Var_But;
         if Sp_Subst > Sp_Subst_Max then Sp_Subst_Max := Sp_Subst; end if;
      end if;
      -- Effacement des buts saisis.
      Effacer_Suite_De_Buts;
      if not Fini and Echo_Actif then
         if Reponse_Affirmative then Put_Line(Yes); else Put_Line(No); end if;
      end if;
      -- Restaure les variables globales de l'interpréteur.
      But_Courant         := Sauve_But_Courant;
      Env_Courant         := Sauve_Env_Courant;
      Nb_Var_But          := Sauve_Nb_Var_But;
      Reste_De_Buts       := Sauve_Reste_De_Buts;
      Buts_Geles          := Sauve_Buts_Geles;
      Env_Geles           := Sauve_Env_Geles;
      Liste_Regles        := Sauve_Liste_Regles;
      Env_Echec           := Sauve_Env_Echec;
      Reponse_Affirmative := Sauve_Reponse_Affirmative;
      -- Restaure les pointeurs des différentes piles.
      Sp_Subst            := Sauve_Sp_Subst;
      Sp_Sauve            := Sauve_Sp_Sauve;
      Sp_Etape            := Sauve_Sp_Etape;
      Sp_Choix            := Sauve_Sp_Choix;
      -- Restaure les pointeurs de base initiaux.
      Sp_Subst_Initial    := Sauve_Sp_Subst_Initial;
      Sp_Sauve_Initial    := Sauve_Sp_Sauve_Initial;
      Sp_Etape_Initial    := Sauve_Sp_Etape_Initial;
      Sp_Choix_Initial    := Sauve_Sp_Choix_Initial;
   end Effacer;


   --------------------------
   -- Les regles prédéfinies.
   --------------------------
   -- REMARQUE IMPORTANTE : Toutes les regles prédéfinies sont appelées par :
   --       regle(X1...Xn) :- internal_call(num).
   -- Donc les arguments sont dans PILE_SUBST lors de l'effacement de internal_call(num).


   function Eval(Expr : Mot; Env_Expr : Indice_Pile_Subst) return Mot is
      Erreur_Evaluation : exception;
      type Func1arg is (V_Abs, Moins_Unaire, Div_Unaire);
      type Func2arg is (Modulo, Puissance);
      type Funcnarg is (Plus, Moins, Mult, Div);

      function Evaluation(Expression : Mot; Env_Expression : Indice_Pile_Subst) return Mot;

      function Apply1(Func : Func1arg; Args : Mot; Env_Args : Indice_Pile_Subst) return Mot is
         Arg : Mot_Valeur;
      begin
         if not Vect1(Args) then raise Erreur_Evaluation; end if;
         Arg := Entier_Val(Evaluation(Premier(Args), Env_Args));
         case Func is
            when V_Abs        => Arg := abs Arg;
            when Moins_Unaire => Arg := -Arg;
            when Div_Unaire   => Arg := 1 / Arg;
         end case;
         return Cree_Entier(Arg);
      end Apply1;

      function Apply2(Func : Func2arg; Args : Mot; Env_Args : Indice_Pile_Subst) return Mot is
         Arg1, Arg2 : Mot_Valeur;
      begin
         if not Vect2(Args) then raise Erreur_Evaluation; end if;
         Arg1 := Entier_Val(Evaluation(Premier(Args), Env_Args));
         Arg2 := Entier_Val(Evaluation(Premier(Reste(Args)), Env_Args));
         case Func is
            when Modulo    => Arg1 := Arg1 mod Arg2;
            when Puissance => Arg1 := Arg1 ** Arg2;
         end case;
         return Cree_Entier(Arg1);
      end Apply2;

      function Apply(Func : Funcnarg; Args : Mot; Env_Args : Indice_Pile_Subst; Initial : Mot_Valeur) return Mot is
         Passe1  : Boolean := True;
         Result  : Mot_Valeur := Initial;
         Vectarg : Mot := Args;

         function Next return Mot_Valeur is
            V : Mot_Valeur := Entier_Val(Evaluation(Premier(Vectarg), Env_Args));
         begin
            Passe1 := False;
            Vectarg := Reste(Vectarg);
            return V;
         end Next;

      begin -- APPLY
         case Func is
            when Plus =>
               while Doublet_V(Vectarg) loop Result := Result + Next; end loop;
            when Moins =>
               while Doublet_V(Vectarg) loop
                  if Passe1 then Result := Next; else Result := Result - Next; end if;
               end loop;
            when Mult =>
               while Doublet_V(Vectarg) loop Result := Result * Next; end loop;
            when Div =>
               while Doublet_V(Vectarg) loop
                  if Passe1 then Result := Next; else Result := Result / Next; end if;
               end loop;
         end case;
         return Cree_Entier(Result);
      end Apply;

      function Evaluation(Expression : Mot; Env_Expression : Indice_Pile_Subst) return Mot is
         Func, Args : Mot;
         Expr : Mot := Expression;
         Env_Expr : Indice_Pile_Subst := Env_Expression;
      begin
         if Vect1(Expr) then Expr := Premier(Expr); end if;
         if Variable(Expr) then Rep(Expr, Env_Expr, X); end if;
         if Entier(Expr) then
            return Expr;
         elsif Doublet_F(Expr) then
            Func := Premier(Expr);
            Args := Reste(Expr);
            if Egalite_Mot(Func, S_Plus) then
               return Apply(Plus, Args, Env_Expr, 0);
            elsif Egalite_Mot(Func, S_Moins) then
               if Vect1(Args) then return Apply1(Moins_Unaire, Args, Env_Expr);
               else return Apply(Moins, Args, Env_Expr, 0);
               end if;
            elsif Egalite_Mot(Func, S_Etoile) then
               return Apply(Mult, Args, Env_Expr, 1);
            elsif Egalite_Mot(Func, S_Slash) then
               if Vect1(Args) then return Apply1(Div_Unaire, Args, Env_Expr);
               else return Apply(Div, Args, Env_Expr, 1);
               end if;
            elsif Egalite_Mot(Func, S_Mod) then
               return Apply2(Modulo, Args, Env_Expr);
            elsif Egalite_Mot(Func, S_Puissance) then
               return Apply2(Puissance, Args, Env_Expr);
            elsif Egalite_Mot(Func, S_Abs) then
               return Apply1(V_Abs, Args, Env_Expr);
            else
               raise Erreur_Evaluation;
            end if;
         else
            raise Erreur_Evaluation;
         end if;
      end Evaluation;

   begin -- EVAL
      return Evaluation(Expr, Env_Expr);
   exception
      when Erreur_Evaluation | Numeric_Error => return S_Err;
   end Eval;


   procedure Regle_Predefinie(Num_Regle : Mot; Env_Regle : Indice_Pile_Subst) is
      Subst: Indice_Pile_Subst;
      Arg1 : Mot := Cree_Variable(1);
      Arg2 : Mot := Cree_Variable(2);
      Env_Arg1, Env_Arg2 : Indice_Pile_Subst := Env_Regle;

      procedure Put_Arg(Num_Arg : Positive; Substitution : P_Subst) is
         Resultat : Mot := Cree_Variable(Num_Arg);
         Env_Resultat : Indice_Pile_Subst := Env_Regle;
      begin
         Rep(Resultat, Env_Resultat, X);
         if Variable(Resultat) then
            Sauve_Subst(X);
            if Gele(X) then Relancer(But_Courant, Env_Courant, X); end if;
            Pile_Subst(X) := Substitution;
         else                                                          -- Ici la variable est liée à une valeur.
            if Substitution.Stype = Ref or else
                  (Substitution.Stype = Lie and then not Egalite_Mot(Resultat, Substitution.Ter)) then
               But_Courant := S_Fail;
            end if;
         end if;
      end Put_Arg;

   begin
      But_Courant := Vecteur_Vide;                                                        -- A priori, on suppose que regle s'efface
      if not Entier(Num_Regle) then
         But_Courant := S_Fail;
         return;
      end if;
      case Entier_Val(Num_Regle) is
         -------------------- Controle de l'interpréteur -----------------------
         when 0  => -- answer(X)
            Rep(Arg1, Env_Arg1, X);
            if Variable(Arg1) then
               case Mode_R is
                  when Oui_Non          => Arg1 := S_Yes_No;
                  when Premiere_Reponse => Arg1 := S_First;
                  when Toutes_Reponses  => Arg1 := S_All;
                  when others           => Arg1 := Cree_Entier(Mode_R);
               end case;
               Put_Arg(1, (Lie, Sub_Vide, Arg1));                                   -- Pas besoin d'env. pour atome.
            elsif Egalite_Mot(Arg1, S_Yes_No) then
               Mode_R := Oui_Non;
            elsif Egalite_Mot(Arg1, S_First) then
               Mode_R := Premiere_Reponse;
            elsif Egalite_Mot(Arg1, S_All) then
               Mode_R := Toutes_Reponses;
            elsif Entier(Arg1) and Entier_Val(Arg1) >= 0 then
               Mode_R := Entier_Val(Arg1);
            else
               But_Courant := S_Fail;
            end if;
         when 1  => -- true
            null;                                                                  -- true s'efface toujours.
         when 2  => -- statistics
            New_Line;
            Objets_Prolog.Types_Informations;
            Put_Line(Traits_H);
            Put('|'); Set_Col(38); Put('|'); Put("TAILLE "); Put('|'); Put("POSITION"); Put('|'); Put("COURANT"); Put('|');
            Put(" MAXI "); Put('|'); Put("MEMOIRE"); Put('|'); New_Line;
            Put('|'); Set_Col(38); Put('|'); Put("ELEMENT"); Put('|'); Put("COURANTE"); Put('|'); Put("  MAXI "); Put('|');
            Put("ALLOUE"); Put('|'); Put("ALLOUEE"); Put('|'); New_Line;
            Put_Line(Traits_H);
            Objets_Prolog.Informations;
            Es_Prolog.Informations;
            Interpreteur_Prolog.Informations;
            Put_Line(Traits_H);
            New_Line;
         --------------------- Les prédicats d'écriture ------------------------
         when 10 => -- write(X)
            Rep(Arg1, Env_Arg1, X);
            Write(Arg1, Env_Arg1);
         when 11 => -- display(X)
            Rep(Arg1, Env_Arg1, X);
            Ecrit(Arg1);
         when 12 => -- nl
            New_Line;
         --------------- Les prédicats de reconnaissance de type ---------------
         when 20 => -- atom(X)
            Rep(Arg1, Env_Arg1, X);
            if not Symbole(Arg1) then
               But_Courant := S_Fail;
            end if;
         when 21 => -- integer(X)
            Rep(Arg1, Env_Arg1, X);
            if not Entier(Arg1) then
               But_Courant := S_Fail;
            end if;
         when 22 => -- atomic(X)
            Rep(Arg1, Env_Arg1, X);
            if not Atome(Arg1) then
               But_Courant := S_Fail;
            end if;
         when 23 => -- var(X)
            Rep(Arg1, Env_Arg1, X);
            if not Variable(Arg1) then
               But_Courant := S_Fail;
            end if;
         when 24 => --list(X)
            Rep(Arg1, Env_Arg1, X);
            if not (Doublet_L(Arg1) or else Egalite_Mot(Arg1, Liste_Vide)) then
               But_Courant := S_Fail;
            end if;
         when 25 => -- vector(X)
            Rep(Arg1, Env_Arg1, X);
            if not (Doublet_V(Arg1) or else Egalite_Mot(Arg1, Vecteur_Vide)) then
               But_Courant := S_Fail;
            end if;
         when 26 => -- function(X)
            Rep(Arg1, Env_Arg1, X);
            if not (Doublet_F(Arg1)) then
               But_Courant := S_Fail;
            end if;
         -------------- Effacement retardé et contrainte différée --------------
         when 30 => -- freezeA(Var, But)
            Subst := Env_Regle + 1;
            if Lie(Subst) then
               But_Courant := Ter(Env_Regle + 2);
               Env_Courant := Sub(Env_Regle + 2);
            else
               Subst := Sub(Subst);
               Sauve_Subst(Subst);
               if Gele(Subst) then
                  if Pile_Subst'Last - Sp_Subst < 2 then
                     raise Pile_Subst_Pleine;
                  else
                     Pile_Subst(Sp_Subst) := (Lie, Sub(Subst), Ter(Subst));
                     Pile_Subst(Sp_Subst + 1) := (Stype(Env_Regle + 2), Sub(Env_Regle + 2), Ter(Env_Regle + 2));
                     Pile_Subst(Subst) := (Gele, Sp_Subst - 1, X_Point_Y);
                     Sp_Subst := Sp_Subst + 2;
                     if Sp_Subst > Sp_Subst_Max then Sp_Subst_Max := Sp_Subst; end if;
                  end if;
               else
                  Pile_Subst(Subst) := (Gele, Sub(Env_Regle + 2), Ter(Env_Regle + 2));
               end if;
            end if;
         when 31 => -- reduce(Arg1, Arg2, Var)
            declare
               type Result_Reduce is (Differents, Identiques, Hypothese);
               Result : Result_Reduce;
               Base_Pile_Sauve : Indice_Pile_Sauve := Sp_Sauve;
            begin
               Buts_Geles := Vecteur_Vide;
               Env_Echec := Sp_Subst;
               if Unifiable(Cree_Variable(1), Env_Regle, Cree_Variable(2), Env_Regle) then
                  if Sp_Sauve /= Base_Pile_Sauve then
                     Subst := Pile_Sauve(Base_Pile_Sauve).Ptr_Pile_Subst;
                     Result := Hypothese;                                             -- Il faut faire une hypothese sur une var
                  else
                     Result := Identiques;                                            -- Les deux termes sont identiques.
                  end if;
               else
                  Result := Differents;
               end if;
               while Sp_Sauve /= Base_Pile_Sauve loop Restaure_Subst; end loop;
               case Result is
                  when Differents => But_Courant := S_Fail;
                  when Identiques => Put_Arg(3, (Lie, Sub_Vide, Cree_Entier(1)));     -- Pas besoin d'env. pour atome.
                  when Hypothese  => Put_Arg(3, (Ref, Subst, Ter_Vide));              -- Pas besoin de terme pour variable.
               end case;
            end;
         -------------------------- Listage des regles -------------------------
         when 40 => -- listing
            Listing(Liste_Def);
         when 41 => -- listing(Arg)
            Rep(Arg1, Env_Arg1, X);
            Listing(Arg1);
         when 42 => -- system
            Listing(Liste_System);
         ------------------------- Evaluation numérique ------------------------
         when 50 => -- is
            Rep(Arg2, Env_Arg2, X);
            Put_Arg(1, (Lie, Sub_Vide, Eval(Arg2, Env_Arg2)));                     -- Pas besoin d'env. pour atome.
         ------------------------ Comparaison numérique ------------------------
         when 60 => -- <
            Rep(Arg1, Env_Arg1, X);
            Rep(Arg2, Env_Arg2, X);
            if Entier(Arg1) and then Entier(Arg2) then
               if not (Entier_Val(Arg1) < Entier_Val(Arg2)) then But_Courant := S_Fail; end if;
            else
               But_Courant := S_Fail;
            end if;
         when 61 => -- =<
            Rep(Arg1, Env_Arg1, X);
            Rep(Arg2, Env_Arg2, X);
            if Entier(Arg1) and then Entier(Arg2) then
               if not (Entier_Val(Arg1) <= Entier_Val(Arg2)) then But_Courant := S_Fail; end if;
            else
               But_Courant := S_Fail;
            end if;
         when 62 => -- >
            Rep(Arg1, Env_Arg1, X);
            Rep(Arg2, Env_Arg2, X);
            if Entier(Arg1) and then Entier(Arg2) then
               if not (Entier_Val(Arg1) > Entier_Val(Arg2)) then But_Courant := S_Fail; end if;
            else
               But_Courant := S_Fail;
            end if;
         when 63 => -- >=
            Rep(Arg1, Env_Arg1, X);
            Rep(Arg2, Env_Arg2, X);
            if Entier(Arg1) and then Entier(Arg2) then
               if not (Entier_Val(Arg1) >= Entier_Val(Arg2)) then But_Courant := S_Fail; end if;
            else
               But_Courant := S_Fail;
            end if;
         -------------- Les prédicats de manipulation des regles ---------------
         when 70 => -- asserta
            Rep(Arg1, Env_Arg1, X);
            Assert_Dynamique(Arg1, Env_Arg1, Debut);
         when 71 => -- assert et assertz
            Rep(Arg1, Env_Arg1, X);
            Assert_Dynamique(Arg1, Env_Arg1, Fin);
         -------------------------- Controle de l'echo -------------------------
         when 80 => -- echo(X)
            Rep(Arg1, Env_Arg1, X);
            if Variable(Arg1) then
               if Echo then Arg1 := S_On; else Arg1 := S_Off; end if;
               Put_Arg(1, (Lie, Sub_Vide, Arg1));
            elsif Egalite_Mot(Arg1, S_On) then
               Echo := True;
            elsif Egalite_Mot(Arg1, S_Off) then
               Echo := False;
            else
               But_Courant := S_Fail;
            end if;
         ------------------------ Quitter l'interpréteur -----------------------
         when 1000 => -- halt
            Fini := True;
         --------------------------------- Autres ------------------------------
         when others =>
            But_Courant := S_Fail;
      end case;
   end Regle_Predefinie;


   --------------------
   -- Le driver PROLOG.
   --------------------

   procedure Prompt is
   begin
      if Echo_Actif and Entree_Depuis_Standard then
         if Mode_F = Interrogation then
            Put("?- ");
         else
            Put("assert- ");
         end if;
      end if;
   end Prompt;


   procedure Driver(Mode_Fonctionnement : Fonctionnement; Mode_Reponse : Reponse) is
      Expression              : Mot;
      Sauve_Mode_F            : Fonctionnement     := Mode_F;
      Sauve_Mode_R            : Reponse            := Mode_R;
      Sauve_Compteur_Reponses : Natural            := Compteur_Reponses;
      Sauve_Sp_Nomvar         : Indice_Pile_Nomvar := Sp_Nomvar;
      Sauve_Sp_Nomvar_Initial : Indice_Pile_Nomvar := Sp_Nomvar_Initial;
      Sauve_Nb_Var_But        : Natural            := Nb_Var_But;
   begin
      Mode_F := Mode_Fonctionnement;
      Mode_R := Mode_Reponse;
      Sp_Nomvar_Initial := Sp_Nomvar;
      while not Fini loop
         begin
            if not Entree_Depuis_Standard then
               Carac_Lu := Caractere_Significatif(Carac_Lu);
            end if;
            exit when Carac_Lu = Fin_De_Fichier;
            Raz_Variables;
            Compteur_Reponses := 0;
            Prompt;
            Expression := Analyse_Complete(Lit_Token);
            Nb_Var_But := Nbre_De_Variables;
            Sp_Nomvar := Sp_Nomvar_Initial;                     -- Vide la pile des noms de variables
            for I in 1..Nb_Var_But loop                         -- Sauve les noms des variables.
               Empile_Nomvar(Variable_Nom(I));
            end loop;
            if Vect1(Expression) then Expression := Premier(Expression); end if;                      -- (expr) équivalent à expr.
            if Mode_F = Interrogation then
               if Func1(Expression) and then Egalite_Mot(Premier(Expression), S_2points_Moins) then    -- Si :-(arg).
                  Mode_R := Oui_Non;
                  Effacer(Reste(Expression), Nb_Var_But);
                  Mode_R := Mode_Reponse;
               elsif Func2(Expression) and then Egalite_Mot(Premier(Expression), S_2points_Moins) then -- Si :-(arg1, arg2).
                  Assert_Statique(Expression, Fin);
               else
                  Effacer(Expression, Nb_Var_But);
               end if;
            else
               if Func1(Expression) and then Egalite_Mot(Premier(Expression), S_2points_Moins) then    -- Si :-(arg).
                  Mode_R := Oui_Non;
                  Effacer(Reste(Expression), Nb_Var_But);
                  Mode_R := Mode_Reponse;
               else
                  Assert_Statique(Expression, Fin);
               end if;
            end if;
         exception
            when Erreur_De_Syntaxe => null;
         end;
      end loop;
      Mode_F            := Sauve_Mode_F;
      Mode_R            := Sauve_Mode_R;
      Compteur_Reponses := Sauve_Compteur_Reponses;
      Sp_Nomvar         := Sauve_Sp_Nomvar;
      Sp_Nomvar_Initial := Sauve_Sp_Nomvar_Initial;
      Nb_Var_But        := Sauve_Nb_Var_But;
   end Driver;


   procedure Driver_Prolog is

      Premiere_Passe : Boolean := True;

      procedure Traite_Exception(Msg : in String) is
      begin
         New_Line;
         Put_Line(Msg);
         if Premiere_Passe then                                -- Si pas possible de charger le fichier PROLOG.SYS
            Put_Line(Cannot_Load_Prolog_Sys);
            Fini := True;                                       -- alors il faut tout arreter et revenir au systeme.
         else
            Put_Line(Aborting);
         end if;
      end Traite_Exception;

   begin
      Fini := False;
      while not Fini loop
         begin
            Sp_Etape  := Pile_Etape'First;
            Sp_Choix  := Pile_Choix'First;
            Sp_Subst  := Pile_Subst'First;
            Sp_Sauve  := Pile_Sauve'First;
            Sp_Nomvar := Pile_Nomvar'First;
            Echo := False;
            Carac_Lu := ' ';
            if Premiere_Passe then
               Put_Line(Titre1);
               Put_Line(Titre2);
               -- Lecture du fichier PROLOG.SYS.
               Put_Line(Chargefichier);
               if not Entree_Fichier(S_Prolog_Sys) then return; end if;
               Driver(Consultation, Oui_Non);
               --Put(Ansi_Cursor_Up);
               Put_Line(Effacechargefichier);
               Premiere_Passe := False;                          -- IMPORTANT ! On ne le met à faux qu'apres avoir chargé PROLOG.SYS
               Liste_System := Liste_Def;                        -- Pour pouvoir ne lister que les regles prédéfinies.
               Liste_Def := Liste_Vide;                          -- Pour ne pas lister les regles prédéfinies avec les regles normales.
            end if;
            -- Lecture depuis console.
            Entree_Standard;
            Driver(Interrogation, Toutes_Reponses);
            Put_Line(Messagefin);
         exception
            when Storage_Error         => Traite_Exception(Xstorage_Error);
            when Table_Doublets_Pleine => Traite_Exception(Xtable_Doublets_Pleine);
            when Table_Symb_Pleine     => Traite_Exception(Xtable_Symb_Pleine);
            when Table_Var_Pleine      => Traite_Exception(Xtable_Var_Pleine);
            when Pile_Subst_Pleine     => Traite_Exception(Xpile_Subst_Pleine);
            when Pile_Sauve_Pleine     => Traite_Exception(Xpile_Sauve_Pleine);
            when Pile_Nomvar_Pleine    => Traite_Exception(Xpile_Nomvar_Pleine);
            when Pile_Eq_Pleine        => Traite_Exception(Xpile_Eq_Pleine);
            when Pile_Etape_Pleine     => Traite_Exception(Xpile_Etape_Pleine);
            when Pile_Choix_Pleine     => Traite_Exception(Xpile_Choix_Pleine);
            when Pile_Ren_Pleine       => Traite_Exception(Xpile_Ren_Pleine);
         end;
      end loop;
   end Driver_Prolog;

begin

   --------------------------
   -- Pour la mise au point :
   --------------------------

   --PUT_LINE("Elaboration du package INTERPRETEUR_PROLOG------------------------");

   null;

end Interpreteur_Prolog;
