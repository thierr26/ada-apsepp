-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Output;

package body Apsepp.Trace_Debugging_Class.Output is

   ----------------------------------------------------------------------------

   overriding
   procedure Trace (Obj : Trace_Debugging_Output; S : String) is

      pragma Unreferenced (Obj);

   begin

      Apsepp.Output.Output.Put_Line (S);

   end Trace;

   ----------------------------------------------------------------------------

end Apsepp.Trace_Debugging_Class.Output;
