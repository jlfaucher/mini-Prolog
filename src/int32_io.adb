-- Fichier INT32_IO.ADA
-- Package d'entrées-sorties pour les entiers de 32 bits et de 16 bits.

with Text_Io;

package body Int32_Io is
   package Int32io is new Text_Io.Integer_Io(Int32);

   procedure Put(Item : in Int32; Width : in Natural := 0) is
   begin
      Int32io.Put(Item, Width);
   end Put;

   procedure Put(Item : in Integer; Width : in Natural := 0) is
   begin
      Int32io.Put(Int32(Item), Width);
   end Put;

end Int32_Io;