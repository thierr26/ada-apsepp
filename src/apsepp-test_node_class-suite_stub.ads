-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Test_Node_Class.Generic_Case_And_Suite_Run_Body;

package Apsepp.Test_Node_Class.Suite_Stub is

   type Test_Suite_Stub is limited new Test_Node_Interfa with private

     with Type_Invariant'Class =>
            Test_Suite_Stub.Invariant_Class_Test_Suite_Stub;

   not overriding
   function Invariant_Class_Test_Suite_Stub
     (Obj : Test_Suite_Stub) return Boolean
     is (Test_Suite_Stub'Class (Obj).Routine_Count = 0
           and then
         Test_Suite_Stub'Class (Obj).Has_Early_Test);

   overriding
   procedure Run
     (Obj     : in out Test_Suite_Stub;
      Outcome :    out Test_Outcome;
      Kind    :        Run_Kind        := Assert_Cond_And_Run_Test);

   overriding
   function Child_Count (Obj : Test_Suite_Stub) return Test_Node_Count
     is (0);

   overriding
   function Child (Obj : Test_Suite_Stub;
                   K   : Test_Node_Index) return Test_Node_Access;

   overriding
   function Routine_Count (Obj : Test_Suite_Stub) return Test_Routine_Count
     is (0);

   overriding
   function Routine (Obj : Test_Suite_Stub;
                     K   : Test_Routine_Index) return Test_Routine
     is (Null_Test_Routine'Access)

     with Pre'Class => K <= Obj.Routine_Count;

   overriding
   function No_Subtasking (Obj : Test_Suite_Stub) return Boolean
     is (False);

   overriding
   function Has_Early_Test (Obj : Test_Suite_Stub) return Boolean
     is (True);

   overriding
   function Early_Run_Done (Obj : Test_Suite_Stub) return Boolean;

   overriding
   procedure Early_Run (Obj : in out Test_Suite_Stub);

   procedure Run_Children (Obj     :     Test_Node_Interfa'Class;
                           Outcome : out Test_Outcome;
                           Kind    :     Run_Kind);

   procedure Run_Body
     is new Generic_Case_And_Suite_Run_Body (Work => Run_Children);

private

   type Test_Suite_Stub is limited new Test_Node_Interfa with record
      Early_Run_Done_Flag : Boolean := False;
   end record;

end Apsepp.Test_Node_Class.Suite_Stub;
