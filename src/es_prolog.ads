-- Fichier ENT_SOR.ADS
-- Spécifications du package d'entrée/sortie de l'interpréteur PROLOG.


with Objets_Prolog; use  Objets_Prolog;


----------------------------------------------------------------------------------------------------------------------------------


package Es_Prolog is


   ---------------------------------------------
   -- Les exceptions susceptibles d'etre levées.
   ---------------------------------------------
   Erreur_De_Syntaxe   : exception;                          -- Par LIT_TOKEN, ANALYSE_TOKEN.
   Fin_Du_Fichier      : exception;                          -- Par LIT_TOKEN, ANALYSE_TOKEN.
   Table_Op_Pleine     : exception;                          -- Par élaboration du package.
   Buffer_Relect_Plein : exception;                          -- Indique une erreur interne.
   Buffer_Relect_Vide  : exception;                          -- Idem.


   ----------------
   -- Informations.
   ----------------
   procedure Informations;


   -------------------------------------------
   -- Entrée de bas niveau (niveau caractere).
   -------------------------------------------
   Fin_De_Fichier : constant Character := Ascii.Sub;         -- Fin de fichier indiquée par Ctrl-Z (SUB).
   Carac_Lu : Character := ' ';                              -- Le dernier caractere lu.
   Echo : Boolean := False;                                  -- Pour controler l'affichage à l'écran lors d'une lecture de fichier

   Entree_Depuis_Standard : Boolean := True;
   Sortie_Vers_Standard   : Boolean := True;

   procedure Entree_Standard;                                -- L'entrée des caracteres se fera à partir du clavier.
   function Entree_Fichier(Symb : Mot) return Boolean;       -- L'entrée des caracteres se fera à partir du fichier indiqué.
   procedure Vide_Entree;
   function Lit_Carac return Character;                      -- Renvoie le caractere suivant.
   function Caractere_Significatif(C : Character) return Character;-- Renvoie le 1er caractere significatif à partir de C.


   ----------------------------------------------
   -- Entrée de haut niveau (analyse syntaxique).
   ----------------------------------------------
   Token : Mot;                                              -- Le dernier token lu.

   function Lit_Token return Mot;                            -- Renvoie le token suivant du fichier d'entrée.
   function Analyse_Complete(Objet : Mot) return Mot;        -- Renvoie expression suivante du fichier d'entrée.


   ------------------------------
   -- Ecriture d'un objet PROLOG.
   ------------------------------
   procedure Ecrit(Objet : Mot; Avec_Quote : Boolean := True);


end Es_Prolog;