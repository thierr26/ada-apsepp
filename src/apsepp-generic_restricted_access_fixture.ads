-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Fixture_Class;       use Apsepp.Fixture_Class;
with Apsepp.Scope_Bound_Locking;

generic

   type Fixture_Type is limited new Fixture_Interfa with private;

   Deallocate_On_Unlock : Boolean := False;

package Apsepp.Generic_Restricted_Access_Fixture is

   -- Force pre-conditions evaluation in this package.
   pragma Assertion_Policy (Pre => Check);

   type Fixture_Lock
     is limited new Apsepp.Scope_Bound_Locking.Lock with null record;

   overriding
   procedure On_Lock (Obj : Fixture_Lock);

   overriding
   procedure On_Unlock (Obj : Fixture_Lock);

   Fixture_Instance_Lock : aliased Fixture_Lock;

   function Fixture_Instance_Access
     (Lock_Holder : Apsepp.Scope_Bound_Locking.Controlled_Lock_Holder'Class)
     return not null access Fixture_Type
     with Pre => Lock_Holder.L = Fixture_Instance_Lock'Access
                   and then
                 Lock_Holder.Holds;

private

   type Fixture_Access is access Fixture_Type
     with Storage_Size => Fixture_Type'Max_Size_In_Storage_Elements;

   I_A : Fixture_Access;

end Apsepp.Generic_Restricted_Access_Fixture;