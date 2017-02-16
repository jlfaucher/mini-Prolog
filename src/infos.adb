-- Fichier INFOS.ADA
-- Pour afficher des statistiques en cours de fonctionnement.

with Text_Io; use Text_Io;
with Int32_Io; use Int32_Io;

procedure Infos(Texte : String; Taille, Courant, Courant_Max, Max, Mem : Int32) is
begin
   Put("| "); Put(Texte); Set_Col(38); Put('|');
   Put(Taille, 7); Put('|');
   if Courant = -1 then Put("........"); else Put(Courant, 8); end if; Put('|');
   if Courant_Max = -1 then Put("......."); else Put(Courant_Max, 7); end if; Put('|');
   Put(Max, 6); Put('|');
   Put(Max * Taille, 7); Put('|'); New_Line;
end Infos;