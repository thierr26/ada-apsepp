-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Text_IO;

package body Apsepp.Debug_Trace_Class.Standard is

   ----------------------------------------------------------------------------

   overriding
   procedure Trace (Obj         : Debug_Trace_Standard;
                    Message     : String;
                    Entity_Name : String             := "") is

   begin

      Ada.Text_IO.Put_Line
        (if Entity_Name'Length /= 0 then
            Debug_Trace_Standard'Class (Obj).Message_W_Entity
              (Message, Entity_Name)
         else
            Message);

   end Trace;

   ----------------------------------------------------------------------------

end Apsepp.Debug_Trace_Class.Standard;
