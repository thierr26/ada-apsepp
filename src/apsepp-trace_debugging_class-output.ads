-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Trace_Debugging_Class.Stub; use Apsepp.Trace_Debugging_Class.Stub;

package Apsepp.Trace_Debugging_Class.Output is

   type Trace_Debugging_Output
     is limited new Trace_Debugging_Stub with private;

   overriding
   procedure Trace (Obj         : Trace_Debugging_Output;
                    Message     : String;
                    Entity_Name : String                 := "");

private

   type Trace_Debugging_Output
     is limited new Trace_Debugging_Stub with null record;

end Apsepp.Trace_Debugging_Class.Output;
