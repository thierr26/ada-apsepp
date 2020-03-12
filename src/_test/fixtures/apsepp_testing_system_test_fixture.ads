-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Tags;             use Ada.Tags;
with Apsepp.Fixture_Class; use Apsepp.Fixture_Class;

with Apsepp.Test_Node_Class.Protected_Test_Node_Barrier;
  use Apsepp.Test_Node_Class.Protected_Test_Node_Barrier;

with Apsepp.Generic_Singleton;

package Apsepp_Testing_System_Test_Fixture is

   type Apsepp_Testing_System_T_F is limited new Fixture_Interfa with private;

   not overriding
   function A (Obj : Apsepp_Testing_System_T_F) return Tag;

   not overriding
   function B (Obj : Apsepp_Testing_System_T_F) return Tag;

   not overriding
   function C (Obj : Apsepp_Testing_System_T_F) return Tag;

   not overriding
   function D (Obj : Apsepp_Testing_System_T_F) return Tag;

   not overriding
   function E (Obj : Apsepp_Testing_System_T_F) return Tag;

   not overriding
   function A_R (Obj : Apsepp_Testing_System_T_F) return Tag;

   not overriding
   function B_R (Obj : Apsepp_Testing_System_T_F) return Tag;

   not overriding
   function C_R (Obj : Apsepp_Testing_System_T_F) return Tag;

   not overriding
   function D_R (Obj : Apsepp_Testing_System_T_F) return Tag;

   not overriding
   function E_R (Obj : Apsepp_Testing_System_T_F) return Tag;

   not overriding
   procedure Run_Test
     (Obj                        : Apsepp_Testing_System_T_F;
      Expected_Tags_Array_Access : not null access Tag_Array;
      Validate_Procedure         : Validate_Proc);

private

   package Apsepp_Testing_System_T_F_Singleton
     is new Apsepp.Generic_Singleton;

   type Apsepp_Testing_System_T_F is limited new Fixture_Interfa with record

      Singleton_Instance : Apsepp_Testing_System_T_F_Singleton.Singleton;

   end record;

end Apsepp_Testing_System_Test_Fixture;
