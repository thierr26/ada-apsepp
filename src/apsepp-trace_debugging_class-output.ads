-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Trace_Debugging_Class.Interfa;
  use Apsepp.Trace_Debugging_Class.Interfa;

package Apsepp.Trace_Debugging_Class.Output is

   type Trace_Debugging_Output
     is limited new Trace_Debugging_Interfa with private;

   overriding
   procedure Trace (Obj : Trace_Debugging_Output; S : String);

private

   type Trace_Debugging_Output
     is limited new Trace_Debugging_Interfa with null record;

end Apsepp.Trace_Debugging_Class.Output;
