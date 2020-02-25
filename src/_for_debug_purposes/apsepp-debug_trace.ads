-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Generic_Shared_Instance;

with Apsepp.Scope_Bound_Locking; use Apsepp.Scope_Bound_Locking;
with Apsepp.Debug_Trace_Class;   use Apsepp.Debug_Trace_Class;

package Apsepp.Debug_Trace is

   package Debug_Trace_Shared_Instance is new Generic_Shared_Instance
     (Instance_Ancestor_Type => Debug_Trace_Interfa,
      Lock_Type              => Lock);

   function Debug_Trace return access Debug_Trace_Interfa'Class
     renames Debug_Trace_Shared_Instance.Instance_Access;

end Apsepp.Debug_Trace;
