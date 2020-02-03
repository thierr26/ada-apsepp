-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Scope_Bound_Locks; use Apsepp.Scope_Bound_Locks;

generic
   type Instance_Ancestor_Type (<>) is abstract tagged limited private;
   Lock_CB, Unlock_CB : SB_Lock_CB := null;
package Apsepp.Generic_Shared_Instance is

   type Instance_Type_Access is access all Instance_Ancestor_Type'Class;

   function Instance return Instance_Type_Access;

   function Locked return Boolean;

   function Instantiated return Boolean

     with Post => (
                    (Instantiated'Result xor (Instance = null))
                      and then
                    (Locked or else not Instantiated'Result)
                  );

private

   procedure Unl_CB;

   Lock : aliased SB_Lock (Lock_CB, Unl_CB'Access);

   Instance_Access : Instance_Type_Access;

   Deallocation_Needed : Boolean := False;

end Apsepp.Generic_Shared_Instance;
