-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Fixture_Class;       use Apsepp.Fixture_Class;
with Apsepp.Scope_Bound_Locking; use Apsepp.Scope_Bound_Locking;

with Apsepp.Generic_Singleton;

package Apsepp_Scope_Bound_Locking_Test_Fixture is

   type Apsepp_Scope_Bound_Locking_T_F
     is limited new Fixture_Interfa with private;

   overriding
   procedure Set_Up (Obj : Apsepp_Scope_Bound_Locking_T_F);

   not overriding
   function Lock_Count (Obj : Apsepp_Scope_Bound_Locking_T_F) return Natural;

   type Test_Lock is limited new Lock with private;

   overriding
   procedure On_Lock (Obj : Test_Lock);

private

   package Apsepp_Scope_Bound_Locking_T_F_Singleton
     is new Apsepp.Generic_Singleton;

   type Apsepp_Scope_Bound_Locking_T_F
     is limited new Fixture_Interfa with record

      Singleton_Instance : Apsepp_Scope_Bound_Locking_T_F_Singleton.Singleton;

   end record;

   type Test_Lock is limited new Lock with null record;

end Apsepp_Scope_Bound_Locking_Test_Fixture;
