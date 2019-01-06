-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Scope_Bound_Locks; use Apsepp.Scope_Bound_Locks;

package Apsepp.Output.Config is

   Lock : aliased SB_Lock;

   procedure Setup (Locker : not null access SB_L_Locker'Class;
                    Inst   : Output_Access)

     with Post => Instance /= null;

end Apsepp.Output.Config;
