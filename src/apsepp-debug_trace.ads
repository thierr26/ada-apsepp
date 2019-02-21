-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Generic_Shared_Instance;
with Apsepp.Debug_Trace_Class; use Apsepp.Debug_Trace_Class;

package Apsepp.Debug_Trace is

   package Shared_Instance
     is new Apsepp.Generic_Shared_Instance (Debug_Trace_Interfa);

   subtype Debug_Trace_Access is Shared_Instance.Instance_Type_Access;

   function Debug_Trace return Debug_Trace_Access
     renames Shared_Instance.Instance;

end Apsepp.Debug_Trace;
