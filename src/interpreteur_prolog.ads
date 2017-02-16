-- Fichier INTERPRE.ADS
-- Spécifications du package de l'interpréteur PROLOG.
-- Algorithmes extraits de "L'ANATOMIE DE PROLOG" par Michel Van Caneghem.


----------------------------------------------------------------------------------------------------------------------------------


package Interpreteur_Prolog is

   ---------------------------------------------
   -- Les exceptions susceptibles d'etre levées.
   ---------------------------------------------
   Pile_Subst_Pleine : exception;
   Pile_Sauve_Pleine : exception;
   Pile_Nomvar_Pleine: exception;
   Pile_Eq_Pleine    : exception;
   Pile_Etape_Pleine : exception;
   Pile_Choix_Pleine : exception;
   Pile_Ren_Pleine   : exception;

   procedure Driver_Prolog;


end Interpreteur_Prolog;