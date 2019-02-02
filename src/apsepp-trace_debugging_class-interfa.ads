-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

package Apsepp.Trace_Debugging_Class.Interfa is

   type Trace_Debugging_Interfa is limited interface;

   not overriding
   procedure Trace (Obj : Trace_Debugging_Interfa; S : String) is abstract;

end Apsepp.Trace_Debugging_Class.Interfa;
