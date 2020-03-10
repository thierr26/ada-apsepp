-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Unchecked_Deallocation;

package body Apsepp.Generic_Restricted_Access_Fixture is

   ----------------------------------------------------------------------------

   package body Fixture_Lock is

      -----------------------------------------------------

      overriding
      procedure On_Lock (Obj : Lock) is

         pragma Unreferenced (Obj);

      begin

         if I_A = null then

            I_A := new Fixture_Type;

            -- The allocated storage is reclaimed by the 'On_Unlock' primitive
            -- if 'Deallocate_On_Unlock' is true. In the opposite case, it is
            -- reclaimed when the package instance goes out of scope (because
            -- the access type ('Fixture_Access') has the 'Storage_Size'
            -- aspect). REF: ARM13.11(18.4). <2020-02-17>

         end if;

         I_A.Setup;

      end On_Lock;

      -----------------------------------------------------

      overriding
      procedure On_Unlock (Obj : Lock) is

               pragma Unreferenced (Obj);

      begin

         I_A.Clean_Up;

         if Deallocate_On_Unlock then

            declare

               procedure Free is new Ada.Unchecked_Deallocation
                 (Object => Fixture_Type,
                  Name   => Fixture_Access);

            begin

               Free (I_A);

            end;

         end if;

      end On_Unlock;

      -----------------------------------------------------

   end Fixture_Lock;

   ----------------------------------------------------------------------------

   function Fixture_Instance_Access
     (L_H : Apsepp.Scope_Bound_Locking.Lock_Holder'Class)
     return not null access Fixture_Type
     is (I_A);

   ----------------------------------------------------------------------------

end Apsepp.Generic_Restricted_Access_Fixture;
