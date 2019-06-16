-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Output;

package body Apsepp.Debug_Trace_Class.Output is

   ----------------------------------------------------------------------------

   overriding
   procedure Trace (Obj         : in out Debug_Trace_Output;
                    Message     :        String;
                    Entity_Name :        String             := "") is

   begin

      Apsepp.Output.Output.Put_Line
        (if Entity_Name'Length /= 0 then
            Debug_Trace_Output'Class (Obj).Message_W_Entity
              (Message, Entity_Name)
         else
            Message);

   end Trace;

   ----------------------------------------------------------------------------

   overriding
   procedure Trace_E (Obj         : in out Debug_Trace_Output;
                      E           :        Exception_Occurrence;
                      Entity_Name :        String               := "") is

   begin

      Debug_Trace_Output'Class (Obj).Trace
        (Debug_Trace_Output'Class (Obj).E_To_String (E), Entity_Name);

   end Trace_E;

   ----------------------------------------------------------------------------

end Apsepp.Debug_Trace_Class.Output;
