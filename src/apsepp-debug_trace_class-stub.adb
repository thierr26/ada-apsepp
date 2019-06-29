-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

package body Apsepp.Debug_Trace_Class.Stub is

   ----------------------------------------------------------------------------

   overriding
   procedure Trace_E (Obj         : in out Debug_Trace_Stub;
                      E           :        Exception_Occurrence;
                      Entity_Name :        String               := "") is

   begin

      Debug_Trace_Stub'Class (Obj).Trace
        (Debug_Trace_Stub'Class (Obj).E_To_String (E), Entity_Name);

   end Trace_E;

   ----------------------------------------------------------------------------

end Apsepp.Debug_Trace_Class.Stub;
