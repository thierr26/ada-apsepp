-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

package Apsepp.Debug_Trace_Class.Stub is

   type Debug_Trace_Stub is limited new Debug_Trace_Interfa with private;

   overriding
   function Message_W_Entity (Obj         : Debug_Trace_Stub;
                              Message     : String;
                              Entity_Name : String) return String
     is (Entity_Name & ": " & Message);

private

   type Debug_Trace_Stub is limited new Debug_Trace_Interfa with null record;

end Apsepp.Debug_Trace_Class.Stub;
