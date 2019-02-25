-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Generic_Fixture;
with Apsepp.Debug_Trace_Class.Stub; use Apsepp.Debug_Trace_Class.Stub;

package Apsepp_Scope_Debug_Test_Fixture is

   procedure Increment_Scope_Entry_Count;

   procedure Increment_Scope_Exit_Count;

   type Scope_Debug_Test_Fixture is tagged limited null record;

   type Scope_Debug_Test_Fixture_Access
     is not null access all Scope_Debug_Test_Fixture;

   not overriding
   procedure Reset (Obj : Scope_Debug_Test_Fixture);

   not overriding
   function Scope_Entry_Count (Obj : Scope_Debug_Test_Fixture) return Natural;

   not overriding
   function Scope_Exit_Count (Obj : Scope_Debug_Test_Fixture) return Natural;

   function Allocate return Scope_Debug_Test_Fixture_Access
     is (new Scope_Debug_Test_Fixture'(null record));

   package Scope_Debug_T_F is new Apsepp.Generic_Fixture
     (Fixture_Type        => Scope_Debug_Test_Fixture,
      Fixture_Type_Access => Scope_Debug_Test_Fixture_Access,
      Default_Allocator   => Allocate);

   function Instance return Scope_Debug_Test_Fixture_Access
     renames Scope_Debug_T_F.Instance;

   type SDFDT is limited new Debug_Trace_Stub with null record;

   overriding
   procedure Trace (Obj         : SDFDT;
                    Message     : String;
                    Entity_Name : String := "");

end Apsepp_Scope_Debug_Test_Fixture;
