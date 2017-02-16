-- Fichier TRACE.ADA

with Text_Io; use Text_Io;


function Trace(Msg : String) return Integer is
begin
   Put_Line(Msg);
   return 0;
end Trace;