-- Fichier OBJETS.ADB
-- Package de manipulation des objets de base PROLOG.


--with TRACE;
with Text_Io; use Text_Io;
with Int32_Io; use Int32_Io;
with Infos;

----------------------------------------------------------------------------------------------------------------------------------


package body Objets_Prolog is


   --TRACE1 : INTEGER := TRACE("Declaration de la table des doublets");
   ------------------------------------------------
   -- Déclaration de la table des doublets (nodes).
   ------------------------------------------------
   Taille_Table_Doublets : constant := 16300;
   subtype Indice_Table_Doublets is Mot_Valeur range 0..Taille_Table_Doublets-1;

   type T_Table_Doublets is array(Indice_Table_Doublets) of Mot;
   type A_Table_Doublets is access T_Table_Doublets;
   First : constant A_Table_Doublets := new T_Table_Doublets; -- FIRST est un pointeur sur un tableau.
   Rest : constant A_Table_Doublets := new T_Table_Doublets; -- REST  est un pointeur sur un tableau.

   Topnod : Indice_Table_Doublets := T_Table_Doublets'First; -- Adresse du 1er doublet (node) libre.


   --TRACE2 : INTEGER := TRACE("Declaration de la table des caracteres");
   ------------------------------------------------------
   -- Déclaration de la table des caracteres des p-names.
   ------------------------------------------------------
   Taille_Table_Carac : constant := 32767;
   subtype Indice_Table_Carac is Positive range 1..Taille_Table_Carac;

   subtype T_Table_Carac is String(Indice_Table_Carac); -- Si déclaré array(INDICE_TABLE_CARAC) of CHARACTER, erreur compil.
   type A_Table_Carac is access T_Table_Carac;
   Table_Carac : constant A_Table_Carac := new T_Table_Carac; -- TABLE_CARAC est un pointeur sur un tableau de caracteres.

   Topcarac : Indice_Table_Carac := Table_Carac'First;


   --TRACE3 : INTEGER := TRACE("Description d'un p-name");
   --------------------------------------------------------
   -- Description de la représentation interne d'un p-name.
   --------------------------------------------------------
   type R_Pname is record
      Long      : Natural range 0..Longmax_Pname := 0;
      Ind_Carac : Indice_Table_Carac;
      Print_Quote : Boolean;
   end record;

   --pragma PACK(R_PNAME);                                    -- Le but étant de le faire loger sur 4 octets.


   --TRACE4 : INTEGER := TRACE("Declaration de la table des symboles");
   ----------------------------------------
   -- Déclaration de la table des symboles.
   ----------------------------------------
   Nbre_Max_Symboles : constant := 2000;
   subtype Indice_Table_Symb is Mot_Valeur range 0..Nbre_Max_Symboles-1;

   subtype Symbole_Suivant is Mot_Valeur range -1..Nbre_Max_Symboles-1; -- -1 indique fin de liste.

   type Struct_Symbole is record
      Liste_Regles : Mot;
      Pname      : R_Pname;
      Hash_Link  : Symbole_Suivant;
   end record;

   type T_Table_Symb is array(Indice_Table_Symb) of Struct_Symbole;
   type A_Table_Symb is access T_Table_Symb;
   Table_Symb : constant A_Table_Symb := new T_Table_Symb;   -- TABLE_SYMB est un pointeur sur un tableau.

   Topsymb : Indice_Table_Symb := Table_Symb'First;


   --TRACE5 : INTEGER := TRACE("Declaration de la table des hash-codes");
   ------------------------------------------
   -- Déclaration de la table des hash-codes.
   ------------------------------------------
   -- Table des entrées dans les listes chainées des symboles de meme hash-code.
   -- Elle sera initialisée lors de l'élaboration du package.

   subtype Hash_Code is Natural range 0..255;

   type T_Table_Hashcode is array(Hash_Code) of Symbole_Suivant;
   type A_Table_Hashcode is access T_Table_Hashcode;
   Table_Hashcode : constant A_Table_Hashcode := new T_Table_Hashcode;  -- TABLE_HASHCODE est un pointeur sur un tableau.


   --TRACE6 : INTEGER := TRACE("Declaration de la table des variables");
   -----------------------------------------
   -- Declaration de la table des variables.
   -----------------------------------------
   subtype Indice_Table_Var is Mot_Valeur range 1..Nbre_Max_Var_Par_Regle;   -- Les variables sont numérotées à partir de 1 (sauf _)

   type T_Table_Var is array(Indice_Table_Var) of Indice_Table_Symb;
   type A_Table_Var is access T_Table_Var;
   Table_Var : constant A_Table_Var := new T_Table_Var;     -- TABLE_VAR est un pointeur sur un tableau.

   Topvar : Indice_Table_Var := Table_Var'First;


   --TRACE7 : INTEGER := TRACE("Declaration [] et ()");
   ---------------------------------------------------
   -- Déclaration de la liste vide et du vecteur vide.
   ---------------------------------------------------
   Liste_Vide : constant Mot := (Symbole, Table_Symb'First); -- Le symbole '[]' sera le 1er symbole fabriqué (voir fin du package)
   Vecteur_Vide : constant Mot := (Symbole, Table_Symb'First + 1);   -- Le symbole '()' sera le 2nd symbole fabriqué.


   ------------------------------------
   -- Comparaison de deux mots mémoire.
   ------------------------------------

   function Egalite_Mot(Obj1, Obj2 : Mot) return Boolean is
   begin
      return Obj1.T = Obj2.T and then Obj1.Val = Obj2.Val;
   end Egalite_Mot;


   -------------------------------
   -- Procédures de mise au point.
   -------------------------------

   procedure Put_Mot(Arg : Mot) is
   begin
      Put('(');
      Put(Contenu_Memoire'Image(Arg.T));
      Put(", ");
      Put(Arg.Val, 4);
      Put(')');
   end;


   procedure Put_String(Arg : String) is
   begin
      Put('"');
      Put(Arg);
      Put('"');
   end;


   procedure Put_R_Pname(Arg : R_Pname) is
   begin
      Put('(');
      Put(Arg.Long, 2);
      Put(", ");
      Put_String(Table_Carac(Arg.Ind_Carac..
            Arg.Ind_Carac+Arg.Long-1));     -- Il ne faut pas que le 1er p-name soit "".
      Put(", ");
      Put(Boolean'Image(Arg.Print_Quote));
      Put(')');
   end;


   procedure Put_Struct_Symbole(Arg : Struct_Symbole) is
   begin
      Put('<');
      Put_Mot(Arg.Liste_Regles);
      Put(", ");
      Put_R_Pname(Arg.Pname);
      Put('>');
   end;


   procedure Dump_Table_Symb is
   begin
      New_Line;
      Put_Line("----- TABLE DES SYMBOLES -----");
      Put("TOPSYMB = "); Put(Topsymb); New_Line;
      for I in Table_Symb'First..Topsymb-1 loop
         Set_Col(5);
         Put(I, 3); Put(" - ");
         Put_Struct_Symbole(Table_Symb(I));
         New_Line;
      end loop;
   end;


   procedure Dump_Table_Doublets is
   begin
      New_Line;
      Put_Line("----- TABLE DES DOUBLETS -----");
      Put("TOPNOD = "); Put(Topnod); New_Line;
      for I in T_Table_Doublets'First..Topnod-1 loop
         Set_Col(5);
         Put(I, 4); Put(" - ");
         Put_Mot(First(I));
         Put(' ');
         Put_Mot(Rest(I));
         New_Line;
      end loop;
   end;


   procedure Dump_Table_Var is
   begin
      New_Line;
      Put_Line("----- TABLE DES VARIABLES -----");
      Put("TOPVAR = "); Put(Topvar); New_Line;
      for I in Table_Var'First..Topvar-1 loop
         Set_Col(5);
         Put(I, 3); Put(" - ");
         Put(Table_Var(I));
         New_Line;
      end loop;
   end;


   procedure Debug is
      C : Character;
   begin
      loop
         New_Line;
         Put_Line("Utilitaire de mise au point.");
         Put_Line("----------------------------");
         Put_Line("(D) --> Dump doublets    (S) --> Dump symboles    (V) --> Dump variables");
         Put_Line("(Q) --> Retour au programme");
         Put("Votre choix ? ");
         Get(C);
         case C is
            when 'D' | 'd' => Dump_Table_Doublets;
            when 'S' | 's' => Dump_Table_Symb;
            when 'V' | 'v' => Dump_Table_Var;
            when 'Q' | 'q' => New_Line; return;
            when others    => null;
         end case;
      end loop;
   end;


   --------------------------------------------------------------------------------------------
   -- Procédure utilitaire pour appeler une fonction renvoyant un MOT sans récupérer sa valeur.
   --------------------------------------------------------------------------------------------

   procedure Call(V : Mot) is
   begin
      null;
   end;


   -----------------------------------------
   -- Procédure d'information sur les types.
   -----------------------------------------

   procedure Types_Informations is
   begin
      Put("Mot_Valeur'Size = "); Set_Col(20); Put(Int32(Mot_Valeur'Size / 8)); New_Line;
      Put("Mot'Size = "); Set_Col(20); Put(Int32(Mot'Size / 8)); New_Line;
      New_Line;
   end;


   ------------------------------------------
   -- Procédure d'information sur les tables.
   ------------------------------------------

   procedure Informations is
      Taille : Integer;
   begin
      Taille := Struct_Symbole'Size / 8;
      Infos("Zone des symboles",
         Int32(Taille), Int32(Topsymb), -1, Int32(Nbre_Max_Symboles), Int32(Nbre_Max_Symboles) * Int32(Taille));
      Taille := Character'Size / 8;
      Infos("Pnames associes aux symboles",
         Int32(Taille), Int32(Topcarac), -1, Int32(Taille_Table_Carac), Int32(Taille_Table_Carac) * Int32(Taille));
      Taille := Symbole_Suivant'Size / 8;
      Infos("Table des hash-codes pour symboles",
         Int32(Taille), -1, -1, Int32(Table_Hashcode'Length), Int32(Table_Hashcode'Length) * Int32(Taille));
      Taille := Indice_Table_Symb'Size / 8;
      Infos("Table association variable-symbole",
         Int32(Taille), -1, -1, Int32(Nbre_Max_Var_Par_Regle), Int32(Nbre_Max_Var_Par_Regle) * Int32(Taille));
      Taille := Mot'Size / 8;
      Infos("Zone des doublets, FIRST",
         Int32(Taille), Int32(Topnod), -1, Int32(Taille_Table_Doublets), Int32(Taille_Table_Doublets) * Int32(Taille));
      Infos("Zone des doublets, REST",
         Int32(Taille), Int32(Topnod), -1, Int32(Taille_Table_Doublets), Int32(Taille_Table_Doublets) * Int32(Taille));
   end Informations;


   ---------------------------------------------------------
   -- Fonctions de reconnaissance du type d'un objet PROLOG.
   ----------------------------------------------------------

   function Entier(Obj : Mot) return Boolean is
   begin
      return Obj.T = Entier;
   end Entier;


   function Symbole(Obj : Mot) return Boolean is
   begin
      return Obj.T = Symbole;
   end Symbole;


   function Atome(Obj : Mot) return Boolean is
   begin
      return Obj.T in Symbole..Entier;
   end Atome;


   function Doublet(Obj : Mot) return Boolean is
   begin
      return Obj.T in Doublet_V..Doublet_L;
   end Doublet;


   function Doublet_V(Obj : Mot) return Boolean is
   begin
      return Obj.T = Doublet_V;
   end Doublet_V;


   function Doublet_F(Obj : Mot) return Boolean is
   begin
      return Obj.T = Doublet_F;
   end Doublet_F;


   function Doublet_L(Obj : Mot) return Boolean is
   begin
      return Obj.T = Doublet_L;
   end Doublet_L;


   function Variable(Obj : Mot) return Boolean is
   begin
      return Obj.T = Variable;
   end Variable;


   function Vect1(Obj : Mot) return Boolean is
   begin
      return Obj.T = Doublet_V and then Egalite_Mot(Reste(Obj), Vecteur_Vide);
   end Vect1;


   function Vect2(Obj : Mot) return Boolean is
   begin
      return Obj.T = Doublet_V and then Vect1(Reste(Obj));
   end Vect2;


   function Func1(Obj : Mot) return Boolean is
   begin
      return Doublet_F(Obj) and then Vect1(Reste(Obj));
   end Func1;


   function Func2(Obj : Mot) return Boolean is
   begin
      return Doublet_F(Obj) and then Vect2(Reste(Obj));
   end Func2;


   ------------------------------------------------
   -- Garbage collecting : récupération de mémoire.
   ------------------------------------------------
   -- Pas encore implémenté : Quand la mémoire est pleine, c'est fini.


   function Pname_Libre(Longueur : Natural) return Indice_Table_Carac is -- Renvoie adresse zone de LONGUEUR caracteres libres.
      Adresse : Indice_Table_Carac;
   begin
      if Table_Carac'Last - Topcarac < Longueur then raise Table_Carac_Pleine;
      else
         Adresse := Topcarac;
         Topcarac := Topcarac + Longueur;
         return Adresse;
      end if;
   end Pname_Libre;


   function Symbole_Libre return Indice_Table_Symb is       -- Renvoie l'adresse d'un symbole libre.
      Adresse : Indice_Table_Symb;
   begin
      if Topsymb = Table_Symb'Last then raise Table_Symb_Pleine;
      else
         Adresse := Topsymb;
         Topsymb := Topsymb + 1;
         return Adresse;
      end if;
   end Symbole_Libre;


   function Doublet_Libre return Indice_Table_Doublets is    -- Renvoie l'adresse d'un doublet libre.
      Adresse : Indice_Table_Doublets;
   begin
      if Topnod = T_Table_Doublets'Last then raise Table_Doublets_Pleine;
      else
         Adresse := Topnod;
         Topnod := Topnod + 1;
         return Adresse;
      end if;
   end Doublet_Libre;


   ---------------------------------------
   -- Fonctions d'acces aux objets PROLOG.
   ---------------------------------------

   procedure Symbole_Chaine(Obj : in Mot;
         Chaine : out String; Long : out Natural;
         Print_Quote : out Boolean) is            -- Représentation externe d'un symbole.
      Pname : R_Pname;
   begin
      if not Symbole(Obj) then Long := 0; Print_Quote := False; return;
      else
         Pname := Table_Symb(Obj.Val).Pname;
         Long := Pname.Long;
         Chaine(Chaine'First..Pname.Long) := Table_Carac(Pname.Ind_Carac..
            Pname.Ind_Carac+Pname.Long-1); -- 1er p-name ne doit pas etre ""
         Print_Quote := Pname.Print_Quote;
      end if;
   end Symbole_Chaine;


   function Entier_Val(Obj : Mot) return Type_Nombre is     -- Renvoie la valeur numérique d'un entier.
   begin
      return Obj.Val;
   end Entier_Val;


   function Variable_Rang(Obj : Mot) return Type_Nombre is   -- Renvoie le rang d'une variable.
   begin
      return Obj.Val;
   end Variable_Rang;


   function Variable_Nom(Ind : Type_Nombre) return Mot is    -- Renvoie le symbole correspondant au nom d'une variable.
   begin
      return (Symbole, Table_Var(Ind));
   end Variable_Nom;


   function Premier(Obj : Mot) return Mot is                -- Correspond au CAR de LISP.
   begin
      if Doublet(Obj) then return First(Obj.Val);
      else return Liste_Vide;
      end if;
   end Premier;


   function Reste(Obj : Mot) return Mot is                  -- Correspond au CDR de LISP. Le CDR d'un n-uplet est un n-uplet.
   begin
      if Doublet(Obj) then return Rest(Obj.Val);
      else return Liste_Vide;
      end if;
   end Reste;


   function Id_Liste_Regles(Symb : Mot) return Mot is       -- Renvoie la liste des regles rattachées au symbole SYMB.
   begin
      if Symbole(Symb) then
         return Table_Symb(Symb.Val).Liste_Regles;
      else
         return Liste_Vide;
      end if;
   end Id_Liste_Regles;


   function Nbre_De_Variables return Natural is             -- Renvoie le nbre de variables crées depuis le dernier RAZ_VARIABLES.
   begin
      return Topvar - 1;
   end Nbre_De_Variables;


   ----------------------------------------------
   -- Fonctions de fabrication des objets PROLOG.
   ----------------------------------------------

   function Egal_Pname_Chaine(Pname : R_Pname; Chaine : String) return Boolean is
   begin
      return Pname.Long = Chaine'Length and then
         Table_Carac(Pname.Ind_Carac..Pname.Ind_Carac + Pname.Long - 1) = Chaine(Chaine'range);
   end Egal_Pname_Chaine;


   function Cree_Pname(Str : String; Print_Quote : Boolean := False) return R_Pname is
      -- En entrée : Une chaine, en sortie un record (long, ind_carac, flag).
      Longueur : Natural := Str'Length;
      Pname : R_Pname;
      Adresse : Indice_Table_Carac;
   begin
      if Longueur > Longmax_Pname then
         Longueur := Longmax_Pname;                         -- Si chaine trop longue alors on tronque.
      end if;
      Pname.Long := Longueur;
      Pname.Ind_Carac := Topcarac;
      Pname.Print_Quote := Print_Quote;
      Adresse := Pname_Libre(Longueur);
      Table_Carac(Adresse..Adresse+Longueur-1) := Str(Str'First..Str'First+Longueur-1);
      return Pname;
   end Cree_Pname;


   function Cree_Symbole(Str : String; Print_Quote : Boolean := False) return Mot is
      -- En entree : La représentation externe du symbole, en sortie : l'objet symbole.
      Indice_Symb : Indice_Table_Symb;
      Symb_Suiv : Symbole_Suivant;
      H : Hash_Code;
   begin
      if Str'Length = 0 then
         H := 0;
      else
         H := Character'Pos(Str(Str'First));
      end if;
      Symb_Suiv := Table_Hashcode(H);
      while Symb_Suiv /= -1 loop
         if Egal_Pname_Chaine(Table_Symb(Symb_Suiv).Pname, Str) then
            return (Symbole, Symb_Suiv);
         end if;
         Symb_Suiv := Table_Symb(Symb_Suiv).Hash_Link;
      end loop;
      Indice_Symb := Symbole_Libre;
      Table_Symb(Indice_Symb) := (Liste_Vide, Cree_Pname(Str, Print_Quote), Table_Hashcode(H));
      Table_Hashcode(H) := Indice_Symb;
      return (Symbole, Indice_Symb);
   end Cree_Symbole;


   function Cree_Variable(Str : String) return Mot is       -- Renvoie l'objet 'variable' correspondant
      Indice_Symb : Indice_Table_Symb;
      Indice_Var        : Indice_Table_Var;
   begin
      if Str = "_" then return (Variable, 0); end if;       -- Variable muette pour optimiser les unifications.
      Indice_Symb := Cree_Symbole(Str).Val;
      if Topvar /= Table_Var'First then                       -- Si table non vide
         for I in Table_Var'First..Topvar-1 loop
            if Indice_Symb = Table_Var(I) then
               return (Variable, I);
            end if;
         end loop;
      end if;
      Table_Var(Topvar) := Indice_Symb;
      Indice_Var := Topvar;
      if Topvar = Table_Var'Last then raise Table_Var_Pleine; end if;
      Topvar := Topvar + 1;
      return (Variable, Indice_Var);
   end Cree_Variable;


   function Cree_Variable(Num : Natural) return Mot is      -- Création d'une variable à partir de son numéro.
   begin
      return (Variable, Num);
   end Cree_Variable;


   procedure Raz_Variables is                               -- Vide la table des variables (nécessaire lorsqu'on change de regle)
   begin
      Topvar := Table_Var'First;
   end Raz_Variables;


   function Cree_Entier(Nombre: Type_Nombre) return Mot is
   begin
      return (Entier, Nombre);
   end Cree_Entier;


   function Cree_Doublet_V(Car, Cdr : Mot) return Mot is     -- Renvoie le doublet [CAR | CDR] étiqueté 'VECTEUR'.
      Adresse : Indice_Table_Doublets := Doublet_Libre;
   begin
      First(Adresse) := Car;
      Rest(Adresse)  := Cdr;
      return (Doublet_V, Adresse);
   end Cree_Doublet_V;


   function Cree_Doublet_F(Car, Cdr : Mot) return Mot is     -- Renvoie le doublet [CAR | CDR] étiqueté 'FONCTION'.
      Adresse : Indice_Table_Doublets := Doublet_Libre;
   begin
      First(Adresse) := Car;
      Rest(Adresse)  := Cdr;
      return (Doublet_F, Adresse);
   end Cree_Doublet_F;


   function Cree_Doublet_L(Car, Cdr : Mot) return Mot is     -- Renvoie le doublet [CAR | CDR] étiqueté 'LISTE'.
      Adresse : Indice_Table_Doublets := Doublet_Libre;
   begin
      First(Adresse) := Car;
      Rest(Adresse)  := Cdr;
      return (Doublet_L, Adresse);
   end Cree_Doublet_L;


   function Concatene(Obj1, Obj2 : Mot) return Mot is       -- Concatene le vecteur, la fonction ou la liste OBJ1 avec OBJ2.
      Dernier_Doublet : Mot := Obj1;                        -- Le résultat sera du meme type que OBJ1 (vecteur, fonction ou liste)
   begin
      if not Doublet(Obj1) then return Obj1;
      else
         while Doublet(Reste(Dernier_Doublet)) loop
            Dernier_Doublet := Reste(Dernier_Doublet);
         end loop;
         Rest(Dernier_Doublet.Val) := Obj2;                 -- Fait pointer le dernier doublet sur OBJ2.
         return Obj1;
      end if;
   end Concatene;


   function Cree_Liste(Obj1 : Mot) return Mot is            -- Renvoie la liste [OBJ1]
   begin
      return Cree_Doublet_L(Obj1, Liste_Vide);
   end Cree_Liste;


   function Cree_Liste(Obj1, Obj2 : Mot) return Mot is      -- Renvoie la liste [OBJ1, OBJ2]
   begin
      return Cree_Doublet_L(Obj1, Cree_Doublet_L(Obj2, Liste_Vide));
   end Cree_Liste;


   function Cree_Liste(Obj1, Obj2, Obj3 : Mot) return Mot is -- Renvoie la liste [OBJ1, OBJ2, OBJ3]
   begin
      return Cree_Doublet_L(Obj1, Cree_Doublet_L(Obj2, Cree_Doublet_L(Obj3, Liste_Vide)));
   end Cree_Liste;


   function Cree_Vecteur(Obj1 : Mot) return Mot is          -- Renvoie le vecteur <OBJ1>
   begin
      return Cree_Doublet_V(Obj1, Vecteur_Vide);
   end Cree_Vecteur;


   function Cree_Vecteur(Obj1, Obj2 : Mot) return Mot is     -- Renvoie le vecteur <OBJ1, OBJ2>
   begin
      return Cree_Doublet_V(Obj1, Cree_Doublet_V(Obj2, Vecteur_Vide));
   end Cree_Vecteur;


   function Cree_Vecteur(Obj1, Obj2, Obj3 : Mot) return Mot is -- Renvoie le vecteur <OBJ1, OBJ2, OBJ3>
   begin
      return Cree_Doublet_V(Obj1, Cree_Doublet_V(Obj2, Cree_Doublet_V(Obj3, Vecteur_Vide)));
   end Cree_Vecteur;


   function Vecteur_Liste(Obj : Mot) return Mot is          -- Transforme physiquement un vecteur en liste.
      Objet : Mot := Obj;
   begin
      if not Doublet_V(Obj) then return Obj;
      else
         while Doublet_V(Reste(Objet)) loop
            Rest(Objet.Val).T := Doublet_L;                 -- Modifie physiquement le type du reste de OBJET.
            Objet := Reste(Objet);                                  -- Maintenant c'est un doublet liste.
         end loop;
         return (Doublet_L, Obj.Val);                       -- Ne pas oublier de modifier le type du pointeur initial.
      end if;
   end Vecteur_Liste;


   procedure Id_Liste_Regles(Symb, Liste : Mot) is          -- Rattache la liste de regles LISTE au symbole SYMB.
   begin
      if Symbole(Symb) then
         Table_Symb(Symb.Val).Liste_Regles := Liste;
      end if;
   end Id_Liste_Regles;


begin

   ---------------------------
   --  Pour la mise au point :
   ---------------------------

   --PUT_LINE("Elaboration du package OBJETS_PROLOG------------------------------");
   --PUT("MOT = "); PUT(MOT'SIZE); NEW_LINE;
   --PUT("R_PNAME = "); PUT(R_PNAME'SIZE); NEW_LINE;
   --PUT("STRUCT_SYMBOLE = "); PUT(STRUCT_SYMBOLE'SIZE); NEW_LINE;

   ---------------------------------------------
   -- Initialisation de la table des hash-codes.
   ---------------------------------------------

   for I in Table_Hashcode'range loop
      Table_Hashcode(I) := -1;                              -- La fin de la liste est indiquée par -1.
   end loop;

   -------------------------------------------------------------------------
   -- Fabrication des symboles représentant la liste vide et le vecteur vide.
   --------------------------------------------------------------------------

   Call(Cree_Symbole("[]"));                                -- Le symbole '[]' doit etre impérativement le 1er symbole fabriqué.
   Call(Cree_Symbole("()"));                                -- Le symbole '()' doit etre impérativement le 2nd symbole fabriqué.


end Objets_Prolog;
