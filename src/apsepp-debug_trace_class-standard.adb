-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Text_IO;

package body Apsepp.Debug_Trace_Class.Standard is

   ----------------------------------------------------------------------------

   protected body Debug_Trace_Standard is

      -----------------------------------------------------

      procedure Trace (Message     : String;
                       Entity_Name : String := "") is

         function Message_W_Entity (Message     : String;
                                    Entity_Name : String) return String
           renames Debug_Trace_Stub_Instance.Message_W_Entity;

      begin

         Ada.Text_IO.Put_Line (if Entity_Name'Length /= 0 then
                                  Message_W_Entity (Message, Entity_Name)
                               else
                                  Message);

      end Trace;

      -----------------------------------------------------

   end Debug_Trace_Standard;

   ----------------------------------------------------------------------------

end Apsepp.Debug_Trace_Class.Standard;