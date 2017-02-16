-- Fichier PROLOG.ADA


--with TEXT_IO; use TEXT_IO;
--with INT32_IO; use INT32_IO;
--with OBJETS_PROLOG; use OBJETS_PROLOG;
--with ES_PROLOG; use ES_PROLOG;
with Interpreteur_Prolog; use Interpreteur_Prolog;


----------------------------------------------------------------------------------------------------------------------------------

procedure Prolog is
   --  C : CHARACTER;
   --  OBJ : MOT;
begin
   --  ECHO := TRUE;
   --  ENTREE_STANDARD;
   --  PUT_LINE("Test de la lecture bufferisee avec echo :");
   --  PUT_LINE("-----------------------------------------");
   --  loop
   --    C := CARACTERE_SIGNIFICATIF(LIT_CARAC);
   --    if C = ASCII.CR then PUT("[ CR ]"); NEW_LINE;
   --    else PUT('[' & C & "] ");
   --    end if;
   --    exit when C = 'q';
   --  end loop;
   --  NEW_LINE;
   --  PUT_LINE("Test des diverses fonctions de construction et d'affichage :");
   --  PUT_LINE("------------------------------------------------------------");
   --  PUT("LISTE_VIDE = "); ECRIT(LISTE_VIDE); NEW_LINE;
   --  if EGALITE_MOT(LISTE_VIDE, RESTE(CREE_LISTE(CREE_ENTIER(10)))) then
   --    PUT_LINE("Egalite");
   --  else
   --    PUT_LINE("Non egalite");
   --  end if;
   --  PUT("Creation d'un symbole = "); ECRIT(CREE_SYMBOLE("Jean Louis", TRUE)); NEW_LINE;
   --  PUT("Creation d'un entier = "); ECRIT(CREE_ENTIER(-15)); NEW_LINE;
   --  PUT("Creation d'une variable = "); ECRIT(CREE_VARIABLE("X")); NEW_LINE;
   --  PUT("Creation d'un doublet = "); ECRIT(CREE_DOUBLET_L(CREE_ENTIER(5), CREE_SYMBOLE("A"))); NEW_LINE;
   --  PUT("Creation d'une liste = "); ECRIT(CREE_LISTE(CREE_SYMBOLE("hello"), CREE_VARIABLE("Y"), CREE_ENTIER(12))); NEW_LINE;
   --  PUT("Creation d'une fonction = "); ECRIT(CREE_DOUBLET_F(CREE_SYMBOLE("f"), CREE_VECTEUR(CREE_ENTIER(12), CREE_VARIABLE("Y")))); NEW_LINE;
   --  PUT("Creation d'un vecteur = "); ECRIT(CREE_VECTEUR(CREE_SYMBOLE("f"), CREE_ENTIER(10), CREE_SYMBOLE("**|**", TRUE))); NEW_LINE;
   --  debug;
   --  PUT_LINE("Test de lecture de token :");
   --  PUT_LINE("--------------------------");
   --  C := CARACTERE_SIGNIFICATIF(LIT_CARAC);
   --  loop
   --    begin
   --    OBJ := LIT_TOKEN;
   --    PUT(" --> "); ECRIT(OBJ); NEW_LINE;
   --    exit when EGALITE_MOT(OBJ, S_POINT_CR);
   --  exception
   --    when ERREUR_DE_SYNTAXE => null;
   --  end;
   --  end loop;
   --  PUT_LINE("Test d'analyse syntaxique :");
   --  PUT_LINE("---------------------------");
   --  loop
   --    begin
   --      PUT("?- ");
   --      RAZ_VARIABLES;
   --      OBJ := ANALYSE_COMPLETE(LIT_TOKEN);
   --      PUT("--> "); ECRIT(OBJ); NEW_LINE;
   --      PUT("Nombre de variables = "); PUT(NBRE_DE_VARIABLES); NEW_LINE;
   --      exit when EGALITE_MOT(OBJ, LISTE_VIDE);
   --    exception
   --      when ERREUR_DE_SYNTAXE => null;
   --    end;
   --  end loop;
   --  PUT_LINE("Lancement de l'interpreteur PROLOG");
   --  PUT_LINE("----------------------------------");
   --  ECHO := FALSE;
   Driver_Prolog;
end;