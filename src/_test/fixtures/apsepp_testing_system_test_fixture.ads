-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Tags;                   use Ada.Tags;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Apsepp.Tags;                use Apsepp.Tags;
with Apsepp.Characters;          use Apsepp.Characters;
with Apsepp.Test_Reporter_Class; use Apsepp.Test_Reporter_Class;
with Apsepp_Test_Node_Barrier,
     Apsepp.Generic_Fixture;

package Apsepp_Testing_System_Test_Fixture is

   function Char_Name_Image (Char : ISO_646) return String;

   function Tag_To_Char (T : Tag) return ISO_646;

   function Char_To_Tag (Char : ISO_646_Upper_Letter) return Tag;

   type Testing_System_Test_Fixture is tagged limited null record;

   not overriding
   function A (Obj : Testing_System_Test_Fixture) return Tag;

   not overriding
   function B (Obj : Testing_System_Test_Fixture) return Tag;

   not overriding
   function C (Obj : Testing_System_Test_Fixture) return Tag;

   not overriding
   function D (Obj : Testing_System_Test_Fixture) return Tag;

   not overriding
   function E (Obj : Testing_System_Test_Fixture) return Tag;

   not overriding
   function A_R (Obj : Testing_System_Test_Fixture) return Tag;

   not overriding
   function B_R (Obj : Testing_System_Test_Fixture) return Tag;

   not overriding
   function C_R (Obj : Testing_System_Test_Fixture) return Tag;

   not overriding
   function D_R (Obj : Testing_System_Test_Fixture) return Tag;

   not overriding
   function E_R (Obj : Testing_System_Test_Fixture) return Tag;

   not overriding
   procedure Run_Test
     (Obj : Testing_System_Test_Fixture;
      Exp : Tag_Array_Access;
      V   : Apsepp_Test_Node_Barrier.Validate_Proc);

   type Testing_System_Test_Fixture_Access
     is not null access all Testing_System_Test_Fixture;

   function Allocate return Testing_System_Test_Fixture_Access
     is (new Testing_System_Test_Fixture'(null record));

   package Testing_System_T_F is new Apsepp.Generic_Fixture
     (Fixture_Type        => Testing_System_Test_Fixture,
      Fixture_Type_Access => Testing_System_Test_Fixture_Access,
      Default_Allocator   => Allocate);

   function Instance return Testing_System_Test_Fixture_Access
     renames Testing_System_T_F.Instance;

end Apsepp_Testing_System_Test_Fixture;
