-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Test_Node_Class.Generic_Case_And_Suite_Run_Body;

package Apsepp.Test_Node_Class.Suite_Stub is

   type Test_Suite_Stub is limited new Test_Node_Interfa with private;

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
     is (0)

     with Post'Class => Routine_Count'Result = 0;

   overriding
   function Routine (Obj : Test_Suite_Stub;
                     K   : Test_Routine_Index) return Test_Routine
     is (Null_Test_Routine'Access)

     with Pre'Class => K <= Obj.Routine_Count;

   overriding
   function No_Subtasking (Obj : Test_Suite_Stub) return Boolean
     is (False);

   procedure Run_Children (Obj     :     Test_Node_Interfa'Class;
                           Outcome : out Test_Outcome;
                           Kind    :     Run_Kind);

   procedure Run_Body
     is new Generic_Case_And_Suite_Run_Body (Work => Run_Children);

private

   type Test_Suite_Stub is limited new Test_Node_Interfa with null record;

end Apsepp.Test_Node_Class.Suite_Stub;
