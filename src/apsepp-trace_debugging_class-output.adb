-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Output;

package body Apsepp.Trace_Debugging_Class.Output is

   ----------------------------------------------------------------------------

   overriding
   procedure Trace (Obj         : Trace_Debugging_Output;
                    Message     : String;
                    Entity_Name : String                 := "") is

   begin

      Apsepp.Output.Output.Put_Line
        (if Entity_Name'Length /= 0 then
            Trace_Debugging_Output'Class (Obj).Message_W_Entity
              (Message, Entity_Name)
         else
            Message);

   end Trace;

   ----------------------------------------------------------------------------

end Apsepp.Trace_Debugging_Class.Output;
