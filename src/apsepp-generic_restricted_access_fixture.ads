-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Fixture_Class;       use Apsepp.Fixture_Class;
with Apsepp.Scope_Bound_Locking;

generic

   type Fixture_Type is limited new Fixture_Interfa with private;

   Deallocate_On_Unlock : Boolean := False;

package Apsepp.Generic_Restricted_Access_Fixture is

   -- Force run-time pre-condition check in this package.
   pragma Assertion_Policy (Pre => Check);

   package Fixture_Lock is

      type Lock
        is limited new Apsepp.Scope_Bound_Locking.Lock with private;

      overriding
      procedure On_Lock (Obj : Lock);

      overriding
      procedure On_Unlock (Obj : Lock);

   private

      type Lock
        is limited new Apsepp.Scope_Bound_Locking.Lock with null record;

   end Fixture_Lock;

   Fixture_Instance_Lock : aliased Fixture_Lock.Lock;

   function Fixture_Instance_Access
     (L_H : Apsepp.Scope_Bound_Locking.Lock_Holder'Class)
     return not null access Fixture_Type
     with Pre => L_H.L = Fixture_Instance_Lock'Access
                   and then
                 L_H.Holds;

private

   type Fixture_Access is access Fixture_Type
     with Storage_Size => Fixture_Type'Max_Size_In_Storage_Elements;

   I_A : Fixture_Access;

end Apsepp.Generic_Restricted_Access_Fixture;
