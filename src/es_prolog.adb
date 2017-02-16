-- Fichier ENT_SOR.ADB
-- Package d'entrée/sortie de l'interpréteur PROLOG.


with Text_Io; use Text_Io;
with Int32_Io; use Int32_Io;
with Objets_Prolog; use Objets_Prolog;
with Infos;


----------------------------------------------------------------------------------------------------------------------------------


package body Es_Prolog is


   ----------------------------
   -- Les symboles pré-définis.
   ----------------------------
   Liste_Vide           : constant Mot := Cree_Symbole("[]");
   Vecteur_Vide         : constant Mot := Cree_Symbole("()");
   S_Puissance          : constant Mot := Cree_Symbole("^");
   S_Slash              : constant Mot := Cree_Symbole("/");
   S_Etoile             : constant Mot := Cree_Symbole("*");
   S_Moins              : constant Mot := Cree_Symbole("-");
   S_Plus               : constant Mot := Cree_Symbole("+");
   S_Sup                : constant Mot := Cree_Symbole(">");
   S_Sup_Egal           : constant Mot := Cree_Symbole(">=");
   S_Inf                : constant Mot := Cree_Symbole("<");
   S_Egal_Inf           : constant Mot := Cree_Symbole("=<");
   S_Egal               : constant Mot := Cree_Symbole("=");
   S_Par_Gauche         : constant Mot := Cree_Symbole("(");
   S_Par_Droite         : constant Mot := Cree_Symbole(")");
   S_Point              : constant Mot := Cree_Symbole(".");
   S_Point_Cr           : constant Mot := Cree_Symbole('.' & Ascii.Cr);
   S_Point_Virgule      : constant Mot := Cree_Symbole(";");
   S_2points_Moins      : constant Mot := Cree_Symbole(":-");
   S_Virgule            : constant Mot := Cree_Symbole(",");
   S_Is                 : constant Mot := Cree_Symbole("is");
   S_Egal_Egal          : constant Mot := Cree_Symbole("==");
   S_Egal_Point_Point   : constant Mot := Cree_Symbole("=..");
   S_Egal_2points_Egal  : constant Mot := Cree_Symbole("=:=");
   S_Egal_Antislash_Egal : constant Mot := Cree_Symbole("=\=");
   S_Antislash_Plus     : constant Mot := Cree_Symbole("\+");
   S_Antislash_Egal     : constant Mot := Cree_Symbole("\=");
   S_Antislash_Egal_Egal : constant Mot := Cree_Symbole("\==");
   S_Arobasque_Inf      : constant Mot := Cree_Symbole("@<");
   S_Arobasque_Sup      : constant Mot := Cree_Symbole("@>");
   S_Arobasque_Sup_Egal : constant Mot := Cree_Symbole("@>=");
   S_Arobasque_Egal_Inf : constant Mot := Cree_Symbole("@=<");
   S_Slash_Slash        : constant Mot := Cree_Symbole("//");
   S_Crochet_Gauche     : constant Mot := Cree_Symbole("[");
   S_Crochet_Droit      : constant Mot := Cree_Symbole("]");
   S_Barre_V            : constant Mot := Cree_Symbole("|");
   S_Mod                : constant Mot := Cree_Symbole("mod");
   S_Not                : constant Mot := Cree_Symbole("not");


   -----------------------------------------------------------------
   -- Les types et les variables nécessaires à l'analyse syntaxique.
   -----------------------------------------------------------------


   Sortie_Analyse_Passe1 : Boolean; -- Le seul moyen trouvé de distinguer a,(b,c) de a,b,c (à améliorer !)


   -- La parenthese droite, le ']' et le '|' sont des séparateurs.
   type A_Delimiteur is array(Positive range <>) of Mot;
   Delimit : constant A_Delimiteur := (S_Par_Droite, S_Crochet_Droit, S_Barre_V);


   -- Le point suivi d'un retour chariot indique que l'utilisateur a terminé sa saisie.
   type A_Terminateur is array(Positive range <>) of Mot;
   Terminat : constant A_Terminateur := (1 => S_Point_Cr);


   -- Les opérateurs sont caractérisés par une priorité gauche et une priorité droite.

   subtype Priorite is Natural;                             -- La priorité des opérateurs.
   Priorite_Prefixe_Unaire : constant Priorite := 800;      -- Servira pour le signe moins unaire. ex : -2.

   type Type_Operateur is (Libre,   -- Entrée non utilisée dans la table des opérateurs
      Prefixe,                      -- Forme op expr
      Infixe,                       -- Forme E1 op E2 op E3...
      Postfixe);                    -- Forme expr op
   -- Un opérateur peut etre à la fois préfixé, infixé et postfixé.
   -- C'est le contexte qui permettra de choisir le bon type d'opérateur.


   type Operateur is record
      Op             : Mot := Liste_Vide;
      Type_Op        : Type_Operateur := Libre;
      Prio_G, Prio_D : Priorite;
      Multiple       : Boolean; -- Ne concerne que les opérateurs infixes. (Codage dense).
   end record;

   --pragma PACK(OPERATEUR);  -- Pour loger sur 10 octets plutot que 12.


   Carac_Op : array(Prefixe..Postfixe) of Operateur; -- Servira lors de l'analyse syntaxique : voir la fonction ANALYSE.

   Nbre_Max_Operateurs : constant := 100;
   subtype Indice_Table_Op is Positive range 1..Nbre_Max_Operateurs;

   Topop : Indice_Table_Op; -- Sera initialisé au moment de l'élaboration du package.
   Table_Op : array(Indice_Table_Op) of Operateur :=
      -----------------------------------------------------
      ( (S_Not,               Prefixe,    0,  400, False),
      (S_2points_Moins,       Prefixe,    0,   10, False),
      -----------------------------------------------------
      (S_Par_Gauche,          Infixe,  2000,    0, False),
      (S_Puissance,           Infixe,   900,  899, False),
      (S_Mod,                 Infixe,   700,  700, False),
      (S_Slash,               Infixe,   700,  700, False),
      (S_Etoile,              Infixe,   700,  700, True),
      (S_Moins,               Infixe,   600,  600, True),
      (S_Plus,                Infixe,   600,  600, True),
      (S_Sup,                 Infixe,   500,  500, True),
      (S_Sup_Egal,            Infixe,   500,  500, True),
      (S_Inf,                 Infixe,   500,  500, True),
      (S_Egal_Inf,            Infixe,   500,  500, True),
      (S_Egal,                Infixe,   500,  500, True),
      (S_Egal_Egal,           Infixe,   500,  500, True),
      (S_Egal_Point_Point,    Infixe,   500,  500, True),
      (S_Egal_2points_Egal,   Infixe,   500,  500, True),
      (S_Egal_Antislash_Egal, Infixe,   500,  500, True),
      (S_Antislash_Plus,      Infixe,   500,  500, True),
      (S_Antislash_Egal,      Infixe,   500,  500, True),
      (S_Antislash_Egal_Egal, Infixe,   500,  500, True),
      (S_Arobasque_Inf,       Infixe,   500,  500, True),
      (S_Arobasque_Sup,       Infixe,   500,  500, True),
      (S_Arobasque_Sup_Egal,  Infixe,   500,  500, True),
      (S_Arobasque_Egal_Inf,  Infixe,   500,  500, True),
      (S_Egal_Egal,           Infixe,   500,  500, True),
      (S_Is,                  Infixe,   300,  300, True),
      (S_Barre_V,             Infixe,     0,   30, False),  -- Pour interdire expression du genre [a,b|c,d]
      (S_Virgule,             Infixe,    30,   29, True),
      (S_Point_Virgule,       Infixe,    20,   20, True),
      (S_2points_Moins,       Infixe,    10,   10, False),
      -----------------------------------------------------
      others => (Liste_Vide,  Libre,      0,    0,  False) );
   -----------------------------------------------------


   Taille_Token_Maxi : constant Positive := 3;
   type A_Token is array(Positive range <>) of String(1..Taille_Token_Maxi);

   -- ATTENTION ! Respecter l'ordre d'apparition :
   -- De la chaine la plus courte à la chaine la plus longue, lorsque les n 1ers caracteres sont égaux.
   -- Si le token a moins de TAILLE_TOKEN_MAXI caracteres alors on complete avec des espaces.
   Table_Token : constant A_Token :=
      ("== ", "=< ", "=..", "=:=", "=\=",
      ">= ",
      "\+ ", "\= ", "\==",
      "@< ", "@> ", "@>=", "@=<",
      "// ",
      ":- ",
      "/\ ",
      "\/ " );


   -- Les entrées se font caractere par caractere, ce qui permet de ne pas imposer de longueur maximale à une ligne.
   -- La lecture des tokens ci-dessus pose un probleme : Il faut anticiper la lecture des caracteres afin de savoir si ce qui suit
   -- est un token. Comme on ne peut pas anticiper la lecture, il faut lire les caracteres et les placer dans un buffer de
   -- relecture si ces caracteres ne correspondent à aucun des token ci-dessus.
   Buffer_Relecture : String(1..Taille_Token_Maxi);
   Tete_Buffer  : Positive := Buffer_Relecture'First;
   Queue_Buffer : Positive := Tete_Buffer;


   ------------------------------------
   -- Les constantes messages d'erreur.
   ------------------------------------
   Missing_2nd_Quote      : constant String := "missing 2nd quote";
   Used_As_Argument       : constant String := "used as argument";
   Used_As_Prefix_Operator: constant String := "used as prefix operator";
   Operator_Not_Found     : constant String := "operator not found";
   Or_Operator_Not_Found  : constant String := "or " & Operator_Not_Found;
   Syntax_Error           : constant String := "Syntax Error !  ";
   Not_Found              : constant String := "not found";
   File_Not_Found         : constant String := "File not found ";
   Overflow_Error         : constant String := "Overflow error !";


   --------------------------------------------------------
   -- Déclaration du fichier d'entrée et de sortie courant.
   --------------------------------------------------------
   Entree_Courante : File_Type;
   Sortie_Courante : File_Type;


   ----------------
   -- Informations.
   ----------------


   procedure Informations is
      Taille : Integer;
   begin
      Taille := Operateur'Size / 8;
      Infos("Table des operateurs",
         Int32(Taille), -1, -1, Int32(Nbre_Max_Operateurs), Int32(Nbre_Max_Operateurs) * Int32(Taille));
      Taille := Taille_Token_Maxi;
      Infos("Table des tokens",
         Int32(Taille), -1, -1, Int32(Table_Token'Length), Int32(Table_Token'Length) * Int32(Taille));
   end Informations;


   -----------------------------------------------
   -- Les primitives d'écriture d'un objet PROLOG.
   -----------------------------------------------


   procedure Ecrit(Objet : Mot; Avec_Quote : Boolean := True) is -- Ecrit l'objet PROLOG sur la sortie standard.


      procedure Ecrit_Obj(Objet : Mot); -- Référence en avant.


      -- Inclus dans ECRIT.
      procedure Ecrit_Liste(Objet : Mot) is
         -- En entrée : OBJET = la liste à afficher.
         -- Exemple de sortie : [E1, E2, ..., En]   ou   [E1, E2, ... En|Em]
         Obj : Mot := Objet;
      begin
         Put('[');
         Ecrit_Obj(Premier(Obj));
         Obj := Reste(Obj);
         loop
            if not Doublet_L(Obj) then
               if not Egalite_Mot(Obj, Liste_Vide) then Put('|'); Ecrit_Obj(Obj); end if;
               exit;
            else
               Put(", ");
               Ecrit_Obj(Premier(Obj));
               Obj := Reste(Obj);
            end if;
         end loop;
         Put(']');
      end Ecrit_Liste;


      -- Inclus dans ECRIT.
      procedure Ecrit_Vecteur(Objet : Mot) is
         -- En entrée : OBJET = le vecteur à afficher. Exemple de sortie : (E1, E2, ..., En).
         Obj : Mot := Objet;
      begin
         Put('(');
         loop
            Ecrit_Obj(Premier(Obj));
            Obj := Reste(Obj);
            exit when not Doublet_V(Obj);
            Put(", ");
         end loop;
         Put(')');
      end Ecrit_Vecteur;


      -- Inclus dans ECRIT.
      procedure Ecrit_Obj(Objet : Mot) is -- Ecrit l'objet PROLOG sur la sortie standard.
      begin
         if Symbole(Objet) then
            Symbole_Chaine(Objet, Pname_Buffer, Pname_Long, Pname_Print_Quote); -- Représentation externe brute du symbole
            if Pname_Print_Quote and Avec_Quote then Put('''); end if; -- Si nécessaire on entoure avec des quotes '...'
            for I in 1..Pname_Long loop
               if Pname_Buffer(I) = ''' and Avec_Quote then
                  Put("''"); -- Si une quote alors on double la quote (convention)
               else
                  Put(Pname_Buffer(I));
               end if;
            end loop;
            if Pname_Print_Quote and Avec_Quote then Put('''); end if;
         elsif Entier(Objet) then Put(Entier_Val(Objet), 1);
         elsif Variable(Objet) then
            Put('_');
            if Variable_Rang(Objet) /= 0 then Put(Variable_Rang(Objet), 1); end if;
         elsif Doublet_L(Objet) then -- Forme [E1, E2, ..., En|Em]
            Ecrit_Liste(Objet);
         elsif Doublet_V(Objet) then -- Forme (E1, E2, ..., En)
            Ecrit_Vecteur(Objet);
         elsif Doublet_F(Objet) then -- Forme f(...)
            Ecrit_Obj(Premier(Objet));
            if Doublet(Reste(Objet)) then
               Ecrit_Obj(Reste(Objet));
            else
               Ecrit_Obj(Vecteur_Vide);
            end if;
         end if;
      end Ecrit_Obj;


   begin -- ECRIT
      Ecrit_Obj(Objet);
   end Ecrit;


   ----------------------------------
   -- Gestion des erreurs de syntaxe.
   ----------------------------------


   procedure Vide_Entree is -- Vide le tampon d'entrée, suite à une erreur de syntaxe.
      Sauve_Echo : Boolean := Echo;
      Pas_Fichier : Boolean := Name(Standard_Input) = Name(Current_Input);
   begin
      Echo := True;
      loop
         if Carac_Lu = Fin_De_Fichier then exit;
         elsif Carac_Lu = '.' then
            if Lit_Carac <= ' ' or else Carac_Lu = '%' then exit; end if;
         elsif Pas_Fichier and then Carac_Lu = Ascii.Cr then exit; -- Si entrée depuis clavier alors on arrete si <CR>.
         else
            Carac_Lu := Lit_Carac;
         end if;
      end loop;
      Echo := Sauve_Echo;
   end Vide_Entree;


   procedure Syntaxe(Msg : String) is -- Juste un message à afficher.
   begin
      New_Line;
      Put(Syntax_Error);
      Put_Line(Msg);
      Vide_Entree;
      raise Erreur_De_Syntaxe;
   end Syntaxe;


   procedure Syntaxe(Objet : Mot; Msg : String) is -- Un objet PROLOG et un message à afficher.
      Obj : Mot := Objet;
   begin
      if Egalite_Mot(Obj, S_Point_Cr) then Obj := S_Point; end if; -- Remplacé car affichage provoque retour à la ligne.
      New_Line;
      Put(Syntax_Error);
      Put('''); Ecrit(Obj); Put(''');
      Put(' ');
      Put_Line(Msg);
      Vide_Entree;
      raise Erreur_De_Syntaxe;
   end Syntaxe;


   -------------------------------------------
   -- Entrée de bas niveau (niveau caractere).
   -------------------------------------------


   procedure Entree_Standard is -- Entrée depuis l'unité standard par défaut.
   begin
      if Is_Open(Entree_Courante) then Close(Entree_Courante); end if;
      Set_Input(Standard_Input);
      Entree_Depuis_Standard := True;
      Carac_Lu := ' ';
   end Entree_Standard;


   function Entree_Fichier(Symb : Mot) return Boolean is -- Entrée depuis le fichier indiqué par le symbole SYMB.
   begin
      Symbole_Chaine(Symb, Pname_Buffer, Pname_Long, Pname_Print_Quote);
      if Is_Open(Entree_Courante) then Close(Entree_Courante); end if;
      Open(Entree_Courante, In_File, Pname_Buffer(1..Pname_Long));
      Set_Input(Entree_Courante);
      Entree_Depuis_Standard := Name(Current_Input) = Name(Standard_Input);
      Carac_Lu := ' ';
      return True;
   exception
      when Name_Error =>
         Put(File_Not_Found);
         Put('"'); Put(Pname_Buffer(1..Pname_Long)); Put('"'); New_Line;
         Entree_Standard;
         return False;
   end Entree_Fichier;


   procedure Ecrit_Buffer_Relecture(C: Character) is -- Place le caractere dans le buffer de re-lecture.
   begin
      Buffer_Relecture(Tete_Buffer) := C;
      Tete_Buffer := Tete_Buffer + 1;
      if Tete_Buffer > Buffer_Relecture'Last then
         Tete_Buffer := Buffer_Relecture'First;
      end if;
      if Tete_Buffer = Queue_Buffer then
         raise Buffer_Relect_Plein;
      end if;
   end Ecrit_Buffer_Relecture;


   function Buffer_Relecture_Vide return Boolean is -- Renvoie vrai si le buffer de re-lecture est vide.
   begin
      return Tete_Buffer = Queue_Buffer;
   end Buffer_Relecture_Vide;


   function Lit_Buffer_Relecture return Character is -- Lecture depuis le buffer de re-lecture.
   begin
      if Buffer_Relecture_Vide then
         raise Buffer_Relect_Vide;
      end if;
      Carac_Lu := Buffer_Relecture(Queue_Buffer);
      Queue_Buffer := Queue_Buffer + 1;
      if Queue_Buffer > Buffer_Relecture'Last then
         Queue_Buffer := Buffer_Relecture'First;
      end if;
      return Carac_Lu;
   end Lit_Buffer_Relecture;


   function Lit_Carac return Character is -- Lecture d'un caractere. Ne reconnait pas les commentaires.
   begin
      if not Buffer_Relecture_Vide then
         return Lit_Buffer_Relecture;
      elsif End_Of_Line then
         Skip_Line;
         Carac_Lu := Ascii.Cr;
         if Echo then New_Line; end if;
         return Ascii.Cr;
      else
         Get(Carac_Lu);
         if Echo then Put(Carac_Lu); end if;
         if Carac_Lu = Ascii.Eot then Debug; end if;
         if Carac_Lu = Ascii.Enq then Echo := not Echo; end if;
         return Carac_Lu;
      end if;
   exception
      when End_Error => Carac_Lu := Fin_De_Fichier;
         return Fin_De_Fichier;
   end Lit_Carac;


   function Caractere_Significatif(C : Character) return Character is -- Renvoie le 1er caractere significatif à partir de C.
   begin
      Carac_Lu := C;
      loop
         if Carac_Lu = '%' then                             -- Début de commentaire
            while Lit_Carac /= Fin_De_Fichier and then      -- Recherche la fin de la ligne
                  Carac_Lu /= Ascii.Cr loop null; end loop;
         elsif Carac_Lu = Fin_De_Fichier or Carac_Lu > ' ' then return Carac_Lu;
         elsif Lit_Carac /= '%' and then
               (Carac_Lu = Fin_De_Fichier or Carac_Lu > ' ') then return Carac_Lu;
         end if;
      end loop;
   end Caractere_Significatif;


   ------------------------------------------------------------
   -- Entrée de niveau intermédiaire : reconnaissance de token.
   ------------------------------------------------------------


   function Val_Chiffre(C : Character) return Natural is -- Renvoie la valeur entre 0 et 9 du caractere C dans '0'..'9'.
   begin
      return Character'Pos(C) - Character'Pos('0');
   end Val_Chiffre;


   function Lit_Nombre return Mot is -- On part d'un caractere déjà lu.
      V : Type_Nombre := Val_Chiffre(Carac_Lu); -- TYPE_NOMBRE fourni par le package OBJETS_PROLOG.
   begin
      while Lit_Carac in '0'..'9' loop
         V := V * 10 + Val_Chiffre(Carac_Lu);
      end loop;
      return Cree_Entier(V);
   exception
      when Numeric_Error => Put_Line(Overflow_Error);
         Vide_Entree;
         raise Erreur_De_Syntaxe;
   end Lit_Nombre;


   function Caractere_De_Symbole(C : Character) return Boolean is -- Caracteres autorisés dans un symbole non quoté
   begin
      return C in 'A'..'Z' or else
         C in 'a'..'z' or else
         C in '0'..'9' or else
         C = '_'         or else
         Character'Pos(C) in 129..154; -- Tous les caracteres accentués
   end Caractere_De_Symbole;


   procedure Lit_Symbvar is -- On part d'un caractere déjà lu.
   begin
      Pname_Print_Quote := False;
      for I in Pname_Buffer'range loop
         Pname_Buffer(I) := Carac_Lu;
         if not Caractere_De_Symbole(Lit_Carac) then
            Pname_Long := I;
            return;
         end if;
      end loop;
      -- Ici le buffer est plein. On va rechercher la fin du symbole sans stocker les caracteres rencontrés.
      while Caractere_De_Symbole(Lit_Carac) loop
         null;
      end loop;
      Pname_Long := Pname_Buffer'Last;
      return;
   end Lit_Symbvar;


   function Lit_Symbole return Mot is -- Lecture d'un symbole. On a déjà lu le 1er caractere.
   begin
      Lit_Symbvar;
      return Cree_Symbole(Pname_Buffer(1..Pname_Long), Pname_Print_Quote);
   end Lit_Symbole;


   function Lit_Variable return Mot is -- Lecture d'une variable. On a déjà lu le 1er caractere (majuscule).
   begin
      Lit_Symbvar;
      return Cree_Variable(Pname_Buffer(1..Pname_Long));
   end Lit_Variable;


   function Lit_Symbchaine return Mot is -- On part d'un caractere déjà lu et qui est ' (quote).
   begin
      Pname_Print_Quote := False; -- Passera à vrai si la chaine contient des break-characters.
      Pname_Long := 0;
      loop
         if Lit_Carac = Fin_De_Fichier then
            Syntaxe(Missing_2nd_Quote);
         elsif Carac_Lu = Ascii.Cr then -- Une fin de ligne est codée par CR + LF.
            Pname_Long := Pname_Long + 1;
            exit when Pname_Long > Pname_Buffer'Length;
            Pname_Buffer(Pname_Long) := Ascii.Cr;
            Pname_Print_Quote := True; -- A partir d'ici on est sur que le CR a été stocké.
            Pname_Long := Pname_Long + 1;
            exit when Pname_Long > Pname_Buffer'Length;
            Pname_Buffer(Pname_Long) := Ascii.Lf;
         elsif Carac_Lu = ''' then
            if Lit_Carac = ''' then -- Une double quote signifie une quote dans la chaine.
               Pname_Long := Pname_Long + 1;
               exit when Pname_Long > Pname_Buffer'Length;
               Pname_Buffer(Pname_Long) := Carac_Lu;
               Pname_Print_Quote := True;
            else -- Une simple quote indique la fin de la chaine.
               Pname_Print_Quote := (Pname_Long=0) or Pname_Print_Quote; -- Si chaine vide alors il faut entourer par des quotes.
               return Cree_Symbole(Pname_Buffer(1..Pname_Long), Pname_Print_Quote); -- Renvoie symbole correspondant à chaine analysée.
            end if;
         else
            Pname_Long := Pname_Long + 1;
            exit when Pname_Long > Pname_Buffer'Length;
            Pname_Buffer(Pname_Long) := Carac_Lu;
            Pname_Print_Quote := not Caractere_De_Symbole(Carac_Lu) or Pname_Print_Quote;
         end if;
      end loop;
      -- Ici le buffer est plein. On va rechercher la fin de la chaine.
      loop
         if Lit_Carac = Fin_De_Fichier then
            Syntaxe(Missing_2nd_Quote);
         elsif Carac_Lu = ''' and then
               Lit_Carac /= ''' then
            return Cree_Symbole(Pname_Buffer(1..Pname_Long), Pname_Print_Quote); -- Renvoie symbole correspondant à chaine analysée
         end if;
      end loop;
   end Lit_Symbchaine;


   function Lit_Token_Aux return Mot is -- Reconnaissance des tokens spéciaux tels que  =:=  :-  =..  etc...
      type Result is (Non_Trouve, Possible, Trouve, Trouve_Long_Max);
      Indice_Debut : Integer := Table_Token'First;
      Indice_Fin   : Integer := Table_Token'Last;
      Pos_Carac    : Integer := 1;

      function Cherche_Token(Carac : Character) return Result is -- Recherche du token en cours dans la table des tokens.
      begin
         for I in Indice_Debut..Indice_Fin loop
            if Table_Token(I)(Pos_Carac) = ' ' then -- Un espace indique la fin du token.
               return Trouve;
            elsif Table_Token(I)(Pos_Carac) = Carac then
               if Pos_Carac = Taille_Token_Maxi then -- Si on en est au dernier caractere alors terminé.
                  return Trouve_Long_Max;
               else
                  Indice_Debut := I;
                  for J in Indice_Debut..Indice_Fin loop -- Recherche la position du dernier token ayant les memes caracteres
                     exit when Table_Token(J)(Pos_Carac) /= Carac;
                     Indice_Fin := J;
                  end loop;
                  return Possible;
               end if;
            end if;
         end loop;
         return Non_Trouve;
      end Cherche_Token;

   begin
      loop
         Pname_Buffer(Pos_Carac) := Carac_Lu;
         case Cherche_Token(Carac_Lu) is
            when Non_Trouve =>
               for I in 2..Pos_Carac loop -- Pour faire comme si on n'avait lu que le 1er caractere
                  Ecrit_Buffer_Relecture(Pname_Buffer(I));
               end loop;
               Carac_Lu := Lit_Carac; -- Pour la suite de l'analyse
               if Pname_Buffer(1) = '.' and then (Carac_Lu <= ' ' or else Carac_Lu ='%') then
                  return S_Point_Cr; -- Renvoie un symbole spécial indiquant la fin de la saisie
               else
                  return Cree_Symbole(Pname_Buffer(1..1)); -- Le token est un break character
               end if;
            when Trouve => -- Ici on a lu un caractere de trop, ce qui est parfait
               return Cree_Symbole(Pname_Buffer(1..Pos_Carac-1));
            when Trouve_Long_Max => -- Ici on n'a lu que les caracteres du token
               Carac_Lu := Lit_Carac;
               return Cree_Symbole(Pname_Buffer(1..Pos_Carac));
            when Possible => -- Jusque là, les caracteres lus concordent avec le début d'un token
               Pos_Carac := Pos_Carac + 1; -- Pour comparer le prochain caractere du token...
               Carac_Lu  := Lit_Carac;     -- ...avec le caractere lu suivant
         end case;
      end loop;
   end Lit_Token_Aux;


   function Lit_Token return Mot is -- Lecture d'un token. On part d'un caractere déjà lu.
   begin
      if Caractere_Significatif(Carac_Lu) = Fin_De_Fichier then raise Fin_Du_Fichier; end if;
      case Carac_Lu is
         when '0'..'9'       => Token := Lit_Nombre;
         when 'A'..'Z' | '_' => Token := Lit_Variable;
         when '''            => Token := Lit_Symbchaine;
         when others         => if Caractere_De_Symbole(Carac_Lu) then
               Token := Lit_Symbole;
            else
               Token := Lit_Token_Aux;
            end if;
      end case;
      return Token;
   end Lit_Token;


   -------------------------------------------------
   -- Entrée de haut niveau avec analyse syntaxique.
   -------------------------------------------------


   function Analyse(Objet : Mot; Coef_Droit : Priorite) return Mot; -- Pré-déclaration car fonction récursive indirecte.


   function Terminateur(Objet : Mot) return Boolean is -- Vrai si OBJET indique la fin de la saisie. Ex : '.' suivi d'un <CR>
   begin
      if Symbole(Objet) then
         for I in Terminat'range loop
            if Egalite_Mot(Objet, Terminat(I)) then return True; end if;
         end loop;
      end if;
      return False;
   end Terminateur;


   function Delimiteur(Objet : Mot) return Boolean is -- Vrai si OBJET est un délimiteur. Ex : ')'  ']'  '|'
   begin
      if Symbole(Objet) then
         for I in Delimit'range loop
            if Egalite_Mot(Objet, Delimit(I)) then return True; end if;
         end loop;
      end if;
      return Terminateur(Objet);
   end Delimiteur;


   procedure Recherche_Operateur(Objet : Mot) is -- Recherche les caractéristiques de l'opérateur OBJET.
   begin
      for I in Carac_Op'range loop      -- Pour chaque type PREFIXE, INFIXE et POSTFIXE
         Carac_Op(I).Op   := Objet;     -- L'opérateur étudié est OBJET
         Carac_Op(I).Type_Op := Libre;  -- Pour l'instant, il n'est d'aucun type connu.
      end loop;
      if Topop /= Table_Op'First then   -- Si table non vide
         for I in Table_Op'First..Topop-1 loop -- Parcourt la table pour voir si OBJET y existe
            if Table_Op(I).Type_Op /= Libre and then Egalite_Mot(Objet, Table_Op(I).Op) then
               Carac_Op(Table_Op(I).Type_Op) := Table_Op(I);
            end if;
         end loop;
      end if;
   end Recherche_Operateur;


   function Op_Prefixe(Objet : Mot) return Boolean is -- Vrai si OBJET est un opérateur préfixé.
   begin
      if not Symbole(Objet) then return False; end if;
      if not Egalite_Mot(Objet, Carac_Op(Prefixe).Op) then
         Recherche_Operateur(Objet);
      end if;
      return Carac_Op(Prefixe).Type_Op /= Libre;
   end Op_Prefixe;


   function Op_Infixe(Objet : Mot) return Boolean is -- Vrai si OBJET est un opérateur infixé.
   begin
      if not Symbole(Objet) then return False; end if;
      if not Egalite_Mot(Objet, Carac_Op(Infixe).Op) then
         Recherche_Operateur(Objet);
      end if;
      return Carac_Op(Infixe).Type_Op /= Libre;
   end Op_Infixe;


   function Op_Postfixe(Objet : Mot) return Boolean is -- Vrai si OBJET est un opérateur postfixé.
   begin
      if not Symbole(Objet) then return False; end if;
      if not Egalite_Mot(Objet, Carac_Op(Postfixe).Op) then
         Recherche_Operateur(Objet);
      end if;
      return Carac_Op(Postfixe).Type_Op /= Libre;
   end Op_Postfixe;


   function Renvoie_Coef_Gauche(Objet : Mot; Type_Op : Type_Operateur) return Priorite is -- Coef gauche de l'opérateur OBJET.
   begin
      if not Egalite_Mot(Objet, Carac_Op(Type_Op).Op) then
         Recherche_Operateur(Objet);
      end if;
      if Carac_Op(Type_Op).Type_Op /= Libre then
         return Carac_Op(Type_Op).Prio_G;
      else
         return 0;
      end if;
   end Renvoie_Coef_Gauche;


   function Renvoie_Coef_Droit(Objet : Mot; Type_Op : Type_Operateur) return Priorite is -- Coef droit de l'opérateur OBJET.
   begin
      if not Egalite_Mot(Objet, Carac_Op(Type_Op).Op) then
         Recherche_Operateur(Objet);
      end if;
      if Carac_Op(Type_Op).Type_Op /= Libre then
         return Carac_Op(Type_Op).Prio_D;
      else
         return 0;
      end if;
   end Renvoie_Coef_Droit;


   function Multarg(Objet : Mot) return Boolean is -- Renvoie vrai si l'opérateur accepte un nbre quelconque d'arguments.
   begin
      if not Symbole(Objet) then return False; end if;
      if not Egalite_Mot(Objet, Carac_Op(Infixe).Op) then -- Seuls les opérateurs infixés peuvent avoir cette propriété.
         Recherche_Operateur(Objet);
      end if;
      if Carac_Op(Infixe).Type_Op /= Libre then
         return Carac_Op(Infixe).Multiple;
      else
         return False;
      end if;
   end Multarg;


   function Associe(Objet, Delim : Mot) return Mot is -- Renvoie le vecteur des expressions rencontrées jusqu'à DELIM.
      Obj : Mot;
   begin
      Token := Objet;
      if Egalite_Mot(Token, Delim) then                     -- Si on a trouvé le délimiteur.
         Token := Lit_Token;                                -- Saute le délimiteur.
         return Vecteur_Vide;                               -- Pas d'expression rencontrée avant le délimiteur.
      elsif Delimiteur(Token) then                          -- On a trouvé un délimiteur, mais ce n'est pas le bon.
         Syntaxe(Delim, Not_Found);
         return Vecteur_Vide; -- Pour éviter warning
      else
         Obj := Analyse(Token, 0);
         if Egalite_Mot(Token, Delim) then                  -- Si on a trouvé le délimiteur.
            Token := Lit_Token;                             -- Saute le délimiteur.
            if Doublet_V(Obj) then
               return Obj;                                  -- Si vecteur alors renvoie tel quel.
            else
               return Cree_Vecteur(Obj);                    -- Sinon renvoie <expression>.
            end if;
         else
            Syntaxe(Delim, Or_Operator_Not_Found);          -- On n'a pas trouvé le délimiteur.
            return Vecteur_Vide; -- Pour éviter warning
         end if;
      end if;
   end Associe;


   function Analyse_Liste return Mot is                     -- On vient de lire le token '['.
      Objet1, Objet2 : Mot;
   begin
      if Egalite_Mot(Lit_Token, S_Crochet_Droit) then       -- Si déjà ']' alors...
         Token := Lit_Token;                                -- ...saute le ']' et...
         return Liste_Vide;                                 -- ...renvoie [].
      else
         Objet1 := Analyse(Token, 0);
         if Doublet_V(Objet1) then
            Objet1 := Vecteur_Liste(Objet1);                -- Convertit physiquement le vecteur en liste.
         else
            Objet1 := Cree_Liste(Objet1);
         end if;
         if Egalite_Mot(Token, S_Barre_V) then              -- Si '|' rencontré alors...
            Objet2 := Analyse(Lit_Token,                    -- ...lit l'expression suivante
               Renvoie_Coef_Droit(S_Barre_V, Infixe));
         else
            Objet2 := Liste_Vide;                           -- sinon assume la queue de la liste à [].
         end if;
         if Egalite_Mot(Token, S_Crochet_Droit) then        -- Si on a rencontré ']' alors c'est correct.
            Token := Lit_Token;                             -- Saute le ']'.
            return Concatene(Objet1, Objet2);               -- Rattache la queue de la liste.
         else
            Syntaxe(S_Crochet_Droit, Not_Found);
            return Liste_Vide; -- Pour éviter warning
         end if;
      end if;
   end Analyse_Liste;


   function Op_Prefixe_Special(Objet : Mot) return Boolean is-- Renvoie VRAI si OBJET est un opérateur préfixé spécial.
   begin
      return Egalite_Mot(Objet, S_Moins) or else             --  -expr
         Egalite_Mot(Objet, S_Plus)  or else                 --  +expr
         Egalite_Mot(Objet, S_Par_Gauche) or else            --  (expr
         Egalite_Mot(Objet, S_Crochet_Gauche);               --  [expr
   end Op_Prefixe_Special;


   function Analyse_Prefixe_Special(Op : Mot; Coef_Droit : Priorite) return Mot is   -- Renvoie résultat du traitement spécial.
      -- Précise les traitements particuliers lorsque les coefs de priorité ne suffisent pas pour coder l'expression.
      Objet : Mot;
   begin
      if Egalite_Mot(Op, S_Moins) then                      -- Le 'moins' unaire.
         Objet := Analyse(Lit_Token, Priorite_Prefixe_Unaire);
         if Entier(Objet) then
            return Cree_Entier(- Entier_Val(Objet));
         elsif Doublet_V(Objet) or else
               Egalite_Mot(Objet, Vecteur_Vide) then        -- Si '-' utilisé comme functor. Ex : -(a,b).
            return Cree_Doublet_F(S_Moins, Objet);
         else                                               -- C'est vraiment le '-' unaire.
            return Cree_Doublet_F(S_Moins, Cree_Vecteur(Objet));
         end if;
      elsif Egalite_Mot(Op, S_Plus) then                    -- Le 'plus' unaire.
         Objet := Analyse(Lit_Token, Priorite_Prefixe_Unaire);
         if Entier(Objet) then
            return Objet;
         elsif Doublet_V(Objet) or else
               Egalite_Mot(Objet, Vecteur_Vide) then        -- Si '+' utilisé comme functor. Ex : +(a,b).
            return Cree_Doublet_F(S_Plus, Objet);
         else                                               -- C'est vraiment le '+' unaire.
            return Cree_Doublet_F(S_Plus, Cree_Vecteur(Objet));
         end if;
      elsif Egalite_Mot(Op, S_Par_Gauche) then              -- La parenthese en utilisation préfixée.
         if Egalite_Mot(Lit_Token, S_Par_Droite) then
            Token := Lit_Token;                             -- Saute la ')'.
            return Vecteur_Vide;
         else
            Objet := Analyse(Token, 0);
            if Egalite_Mot(Token, S_Par_Droite) then
               Token := Lit_Token;                          -- Saute la ')'.
               if Doublet_V(Objet) then
                  return Objet;
               else
                  return Cree_Vecteur(Objet);
               end if;
            else
               Syntaxe(S_Par_Droite, Not_Found);
            end if;
         end if;
      elsif Egalite_Mot(Op, S_Crochet_Gauche) then          -- Le '[' de début de liste.
         return Analyse_Liste;
      end if;
      return Vecteur_Vide; -- To avoid warning
   end Analyse_Prefixe_Special;


   function Op_Infixe_Special(Objet : Mot) return Boolean is -- Renvoie vrai si OBJET est un opérateur infixé spécial.
   begin
      return Egalite_Mot(Objet, S_Par_Gauche) or else       -- E1 ( E2
         Egalite_Mot(Objet, S_Virgule);                     -- E1 , E2
   end Op_Infixe_Special;


   function Analyse_Infixe_Special(Expr_Gauche, Op : Mot) return Mot is --Renvoie expression résultant du traitement spécial.
      -- Précise les traitements particuliers lorsque les coefs de priorité ne suffisent pas pour coder l'expression.
      Objet : Mot;
   begin
      if Egalite_Mot(Op, S_Par_Gauche) then                             -- Transforme f(a,b...) en (f a b ...)
         if Symbole(Expr_Gauche) or else Variable(Expr_Gauche) then     -- Autorise f(...), 9(...) et X(...)
            return Cree_Doublet_F(Expr_Gauche, Associe(Lit_Token, S_Par_Droite));
         else
            Syntaxe(Operator_Not_Found);
            return Vecteur_Vide; -- Pour éviter warning
         end if;
      elsif Egalite_Mot(Op, S_Virgule) then                 -- La virgule est l'opérateur de création de vecteur.
         Objet := Analyse(Lit_Token, Renvoie_Coef_Droit(S_Virgule, Infixe));
         if Doublet_V(Objet) and not Sortie_Analyse_Passe1 then
            return Cree_Doublet_V(Expr_Gauche, Objet);
         else
            return Cree_Doublet_V(Expr_Gauche, Cree_Vecteur(Objet));
         end if;
      end if;
      return Vecteur_Vide; -- Pour éviter warning
   end Analyse_Infixe_Special;


   function Op_Postfixe_Special(Objet : Mot) return Boolean is  -- Renvoie vrai si OBJET est un opérateur postfixé spécial.
   begin
      return False;
   end Op_Postfixe_Special;


   function Analyse_Postfixe_Special(Expr_Gauche, Op : Mot) return Mot is    -- Renvoie expression résultant du traitement spécial.
      -- Précise les traitements particuliers lorsque les coefs de priorité ne suffisent pas pour coder l'expression.
   begin
      return Vecteur_Vide;
   end Analyse_Postfixe_Special;


   function Analyse(Objet : Mot; Coef_Droit : Priorite) return Mot is
      Objet1 : Mot := Objet;                                -- L'expression de gauche courante.
      Objet2 : Mot;                                         -- L'opérateur infixé ou préfixé courant.
   begin
      if Delimiteur(Objet1) then
         Syntaxe(Objet1, Used_As_Argument);
      elsif Op_Prefixe_Special(Objet1) then                 -- Par exemple : +expr..., -expr... , (expr..., [expr...
         Objet1 := Analyse_Prefixe_Special(Objet1, Coef_Droit);
      else
         Token := Lit_Token;
         if not Egalite_Mot(Token, S_Par_Gauche) then
            if Op_Prefixe(Objet1) then
               -- Ici OBJET1 est un opérateur préfixé.
               Objet1 := Cree_Doublet_F(Objet1,
                  Cree_Vecteur(Analyse(Token,
                        Renvoie_Coef_Droit(Objet1, Prefixe))));
            elsif Op_Infixe(Objet1) or else Op_Postfixe(Objet1) then
               -- Erreur car ici on ne peut avoir qu'un opérateur préfixé.
               Syntaxe(Objet1, Used_As_Prefix_Operator);
            end if;
         end if;
      end if;
      -- Ici on s'occupe des expressions de la forme ...op E1 opA E2 opB E3 opC...
      -- L'analyse a debuté à partir de E1 en connaissant le coef à droite de op.
      -- Dans la boucle : COEF_DROIT = coef à droite de op
      --                        OBJET1 = E1, puis E1 opA E2, puis (E1 opA E2) opB E3 etc...
      --                        OBJET2 = l'opérateur infixé ou postfixé opX courant
      -- Les opérateurs à arguments multiples sont codés sous la forme (op E1 E2...En) plutot que (op...(op (op E1 E2) E3)...En)
      Sortie_Analyse_Passe1 := True;                        -- Pour savoir si résultat sans opérateur infixe ou postfixe
      loop
         Objet2 := Token;
         if Delimiteur(Objet2) then return Objet1;
         elsif (Op_Infixe(Objet2)        and then Renvoie_Coef_Gauche(Objet2, Infixe)   <= Coef_Droit) or else
               (Op_Postfixe(Objet2) and then Renvoie_Coef_Gauche(Objet2, Postfixe) <= Coef_Droit) then return Objet1;
         elsif Op_Infixe_Special(Objet2) then                        -- Par exemple : E1 ( E2
            Objet1 := Analyse_Infixe_Special(Objet1, Objet2);
         elsif Op_Infixe(Objet2) then
            -- Ici OBJET2 est un opérateur infixé.
            if Doublet_F(Objet1)
                  and then Egalite_Mot(Objet2, Premier(Objet1))     -- Si associativité...
                  and then Multarg(Objet2) then                     -- ...et opérateur à arguments multiples...
               Call(Concatene(Objet1,                               -- alors regroupe les arguments
                     Cree_Vecteur(Analyse(Lit_Token,
                           Renvoie_Coef_Droit(Objet2, Infixe)))));
            else                                               -- Ici opérateur infixé à deux arguments : (op E1 E2)
               Objet1 := Cree_Doublet_F(Objet2,
                  Cree_Vecteur(Objet1,
                     Analyse(Lit_Token,
                        Renvoie_Coef_Droit(Objet2, Infixe))));
            end if;
         elsif Op_Postfixe_Special(Objet2) then
            Objet1 := Analyse_Postfixe_Special(Objet2, Objet1);
         elsif Op_Postfixe(Objet2) then
            -- Ici OBJET2 est un opérateur postfixé, donc il s'applique à l'expression de gauche OBJET1.
            Objet1 := Cree_Doublet_F(Objet2, Cree_Vecteur(Objet1));
            Call(Lit_Token);
         elsif Op_Prefixe(Objet2) then
            -- Ici OBJET2 est un opérateur préfixé, donc ne peut pas s'appliquer à l'expression de gauche.
            Syntaxe(Operator_Not_Found);
         else
            -- Ici OBJET2 n'est pas un opérateur, donc on a deux expressions sans opérateur entre elles.
            Syntaxe(Operator_Not_Found);
         end if;
         Sortie_Analyse_Passe1 := False;
      end loop;
   end Analyse;


   function Analyse_Complete(Objet : Mot) return Mot is -- La fonction d'analyse d'une expression PROLOG. On part d'un token lu
      Obj : Mot;
   begin
      Obj := Analyse(Objet, 0);
      if not Terminateur(Token) then Syntaxe(Operator_Not_Found); end if;
      return Obj;
   end Analyse_Complete;


begin

   --------------------------
   -- Pour la mise au point :
   --------------------------

   --PUT_LINE("Elaboration du package ES_PROLOG----------------------------------");

   ------------------------------------------------------------------
   -- Recherche la premiere ligne libre dans la table des opérateurs.
   ------------------------------------------------------------------

   if Table_Op(Table_Op'Last).Type_Op /= Libre then -- Teste le cas extreme ou la table serait déjà pleine.
      raise Table_Op_Pleine;
   end if;
   for I in Table_Op'range loop
      if Table_Op(I).Type_Op = Libre then
         Topop := I;
         exit;
      end if;
   end loop;


end Es_Prolog;