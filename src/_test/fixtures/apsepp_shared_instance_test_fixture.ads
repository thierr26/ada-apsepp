-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Generic_Fixture,
     Apsepp.Generic_Shared_Instance;

package Apsepp_Shared_Instance_Test_Fixture is

   procedure Increment_Lock_Count;

   procedure Increment_Unlock_Count;

   procedure CB;

   type Shared_Instance_Test_Fixture is tagged limited null record;

   type Shared_Instance_Test_Fixture_Access
     is not null access all Shared_Instance_Test_Fixture;

   not overriding
   procedure Reset (Obj : Shared_Instance_Test_Fixture);

   not overriding
   function Lock_Count (Obj : Shared_Instance_Test_Fixture) return Natural;

   not overriding
   function Unlock_Count (Obj : Shared_Instance_Test_Fixture) return Natural;

   not overriding
   function CB_Calls_Count (Obj : Shared_Instance_Test_Fixture) return Natural;

   function Allocate return Shared_Instance_Test_Fixture_Access
     is (new Shared_Instance_Test_Fixture'(null record));

   package Shared_Instance_T_F is new Apsepp.Generic_Fixture
     (Fixture_Type        => Shared_Instance_Test_Fixture,
      Fixture_Type_Access => Shared_Instance_Test_Fixture_Access,
      Default_Allocator   => Allocate);

   function Instance return Shared_Instance_Test_Fixture_Access
     renames Shared_Instance_T_F.Instance;

   type SIFLI_Interfa is limited interface;

   not overriding
   function Primitive (Obj : SIFLI_Interfa) return Character is abstract;

   type SIFLI_Concrete_1 is limited new SIFLI_Interfa with null record;

   overriding
   function Primitive (Obj : SIFLI_Concrete_1) return Character
     is ('1');

   SIFLI_Concrete_1_Instance : aliased SIFLI_Concrete_1;

   type SIFLI_Concrete_2 is limited new SIFLI_Interfa with null record;

   overriding
   function Primitive (Obj : SIFLI_Concrete_2) return Character
     is ('2');

   not overriding
   function Allocate_SIFLI_Concrete_2 return SIFLI_Concrete_2
     is (null record);

   SIFLI_Concrete_2_Instance : aliased SIFLI_Concrete_2;

   package SIFLI_Shared_Instance_L is new Apsepp.Generic_Shared_Instance
     (Instance_Ancestor_Type => SIFLI_Interfa,
      Lock_CB                => Increment_Lock_Count'Access);

   function Allocate_SIFLI_Concrete_1_L
     return SIFLI_Shared_Instance_L.Instance_Type_Access
     is (new SIFLI_Concrete_1'(null record));

   function Allocate_SIFLI_Concrete_2_L
     return SIFLI_Shared_Instance_L.Instance_Type_Access
     is (new SIFLI_Concrete_2'(null record));

   package SIFLI_Shared_Instance_U is new Apsepp.Generic_Shared_Instance
     (Instance_Ancestor_Type => SIFLI_Interfa,
      Unlock_CB              => Increment_Unlock_Count'Access);

   function Allocate_SIFLI_Concrete_1_U
     return SIFLI_Shared_Instance_U.Instance_Type_Access
     is (new SIFLI_Concrete_1'(null record));

   function Allocate_SIFLI_Concrete_2_U
     return SIFLI_Shared_Instance_U.Instance_Type_Access
     is (new SIFLI_Concrete_2'(null record));

end Apsepp_Shared_Instance_Test_Fixture;
