-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Generic_Fixture;
with Apsepp.Scope_Bound_Locks; use Apsepp.Scope_Bound_Locks;

package Apsepp_Scope_Bound_Locks_Test_Fixture is

   procedure Increment_N;

   procedure Decrement_N;

   type Scope_Bound_Locks_Test_Fixture is tagged limited record
      Lock      : aliased SB_Lock;
      Lock_W_CB : aliased SB_Lock (Lock_CB   => Increment_N'Access,
                                   Unlock_CB => Decrement_N'Access);
   end record;

   type Scope_Bound_Locks_Test_Fixture_Access
     is not null access all Scope_Bound_Locks_Test_Fixture;

   not overriding
   function Allocate return Scope_Bound_Locks_Test_Fixture_Access
     is (new Scope_Bound_Locks_Test_Fixture'(others => <>));

   not overriding
   procedure Reset (Obj : Scope_Bound_Locks_Test_Fixture);

   not overriding
   function N (Obj : Scope_Bound_Locks_Test_Fixture) return Integer;

   package Scope_Bound_Locks_T_F is new Apsepp.Generic_Fixture
     (Fixture_Type        => Scope_Bound_Locks_Test_Fixture,
      Fixture_Type_Access => Scope_Bound_Locks_Test_Fixture_Access,
      Default_Allocator   => Allocate);

   function Instance return Scope_Bound_Locks_Test_Fixture_Access
     renames Scope_Bound_Locks_T_F.Instance;

end Apsepp_Scope_Bound_Locks_Test_Fixture;
