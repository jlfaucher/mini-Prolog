-- Fichier INT32_IO.ADA
-- Package d'entrées-sorties pour les entiers de 32 bits et de 16 bits.

package Int32_Io is
   type Int32 is range -2_147_483_648..2_147_483_647;
   procedure Put(Item : in Int32; Width : in Natural := 0);
   procedure Put(Item : in Integer; Width : in Natural := 0);
end Int32_Io;