-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Output;

package body Apsepp.Debug_Trace_Class.Output is

   ----------------------------------------------------------------------------

   overriding
   procedure Trace (Obj         : in out Debug_Trace_Output;
                    Item        :        String;
                    Entity_Name :        String             := "") is

   begin

      Apsepp.Output.Output.Put_Line
        (if Entity_Name'Length /= 0 then
            Debug_Trace_Output'Class (Obj).Item_W_Entity (Item,
                                                          Entity_Name)
         else
            Item);

   end Trace;

   ----------------------------------------------------------------------------

end Apsepp.Debug_Trace_Class.Output;
