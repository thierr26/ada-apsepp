-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Test_Node_Class.Generic_Case_And_Suite_Run_Body;

package Apsepp.Test_Node_Class.Case_Root is

   type Test_Case_Root is limited new Test_Node_Interfa with private

     with Type_Invariant'Class =>
            Test_Case_Root.Invariant_Class_Test_Case_Root;

   not overriding
   function Invariant_Class_Test_Case_Root
     (Obj : Test_Case_Root) return Boolean
     is (Test_Case_Root'Class (Obj).Child_Count = 0
           and then
         Test_Case_Root'Class (Obj).No_Subtasking);

   overriding
   function Routine_Count (Obj : Test_Case_Root) return Test_Routine_Count
     is (0);

   overriding
   function Routine
     (Obj : Test_Case_Root;
      K   : Test_Routine_Index) return not null access procedure;

   overriding
   procedure Run (Obj     : in out Test_Case_Root;
                  Outcome :    out Test_Outcome;
                  Kind    :        Run_Kind       := Assert_Cond_And_Run_Test);

   overriding
   function Child_Count (Obj : Test_Case_Root) return Test_Node_Count
     is (0);

   overriding
   function Child (Obj : Test_Case_Root;
                   K   : Test_Node_Index)
     return not null access Test_Node_Interfa'Class;

   overriding
   function No_Subtasking (Obj : Test_Case_Root) return Boolean
     is (True);

   procedure Run_Body
     is new Generic_Case_And_Suite_Run_Body (Work => Run_Test_Routines);

   overriding
   function Has_Early_Test (Obj : Test_Case_Root) return Boolean
     is (False);

   overriding
   function Early_Run_Done (Obj : Test_Case_Root) return Boolean
     is (True);

   overriding
   procedure Early_Run (Obj : in out Test_Case_Root) is null;

private

   type Test_Case_Root is limited new Test_Node_Interfa with null record;

end Apsepp.Test_Node_Class.Case_Root;
