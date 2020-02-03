-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Test_Node_Class.Generic_Case_And_Suite_Run_Body;

package Apsepp.Test_Node_Class.Case_Stub is

   type Test_Case_Stub is limited new Test_Node_Interfa with private

     with Type_Invariant'Class =>
            Test_Case_Stub.Invariant_Class_Test_Case_Stub;

   not overriding
   function Invariant_Class_Test_Case_Stub
     (Obj : Test_Case_Stub) return Boolean
     is (Test_Case_Stub'Class (Obj).Child_Count = 0
           and then
         Test_Case_Stub'Class (Obj).No_Subtasking);

   overriding
   function Routine_Count (Obj : Test_Case_Stub) return Test_Routine_Count
     is (0);

   overriding
   function Routine (Obj : Test_Case_Stub;
                     K   : Test_Routine_Index) return Test_Routine;

   overriding
   procedure Run (Obj     : in out Test_Case_Stub;
                  Outcome :    out Test_Outcome;
                  Kind    :        Run_Kind       := Assert_Cond_And_Run_Test);

   overriding
   function Child_Count (Obj : Test_Case_Stub) return Test_Node_Count
     is (0);

   overriding
   function Child (Obj : Test_Case_Stub;
                   K   : Test_Node_Index) return Test_Node_Access;

   overriding
   function No_Subtasking (Obj : Test_Case_Stub) return Boolean
     is (True);

   procedure Run_Body
     is new Generic_Case_And_Suite_Run_Body (Work => Run_Test_Routines);

   overriding
   function Has_Early_Test (Obj : Test_Case_Stub) return Boolean
     is (False);

   overriding
   function Early_Run_Done (Obj : Test_Case_Stub) return Boolean
     is (True);

   overriding
   procedure Early_Run (Obj : in out Test_Case_Stub) is null;

private

   type Test_Case_Stub is limited new Test_Node_Interfa with null record;

end Apsepp.Test_Node_Class.Case_Stub;
