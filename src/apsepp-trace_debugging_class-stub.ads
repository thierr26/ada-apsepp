-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Trace_Debugging_Class.Interfa;
  use Apsepp.Trace_Debugging_Class.Interfa;

package Apsepp.Trace_Debugging_Class.Stub is

   type Trace_Debugging_Stub
     is limited new Trace_Debugging_Interfa with private;

   overriding
   function Message_W_Entity (Obj         : Trace_Debugging_Stub;
                              Message     : String;
                              Entity_Name : String) return String
     is (Entity_Name & ": " & Message);

private

   type Trace_Debugging_Stub
     is limited new Trace_Debugging_Interfa with null record;

end Apsepp.Trace_Debugging_Class.Stub;
