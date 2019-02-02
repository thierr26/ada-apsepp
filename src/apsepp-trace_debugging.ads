-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Generic_Shared_Instance;
with Apsepp.Trace_Debugging_Class.Interfa;
  use Apsepp.Trace_Debugging_Class.Interfa;

package Apsepp.Trace_Debugging is

   package Shared_Instance
     is new Apsepp.Generic_Shared_Instance (Trace_Debugging_Interfa);

   subtype Trace_Debugging_Access is Shared_Instance.Instance_Type_Access;

   function Trace_Debugging return Trace_Debugging_Access
     renames Shared_Instance.Instance;

end Apsepp.Trace_Debugging;
