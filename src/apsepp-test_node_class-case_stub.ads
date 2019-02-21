-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Test_Node_Class.Generic_Case_And_Suite_Run_Body;

package Apsepp.Test_Node_Class.Case_Stub is

   type Test_Case_Stub is limited new Test_Node_Interfa with private;

   overriding
   function Routine_Count (Obj : Test_Case_Stub) return Test_Routine_Count
     is (0);

   overriding
   function Routine (Obj : Test_Case_Stub; K : Test_Routine_Index)
     return Test_Routine;

   overriding
   procedure Run (Obj     :     Test_Case_Stub;
                  Outcome : out Test_Outcome;
                  Kind    :     Run_Kind       := Assert_Cond_And_Run_Test);

   overriding
   function Child_Count (Obj : Test_Case_Stub) return Test_Node_Count
     is (0)

     with Post'Class => Child_Count'Result = 0;

   overriding
   function Child (Obj : Test_Case_Stub;
                   K   : Test_Node_Index) return Test_Node_Access;

   overriding
   function No_Subtasking (Obj : Test_Case_Stub) return Boolean
     is (True)

     with Post'Class => No_Subtasking'Result;

   procedure Run_Body
     is new Apsepp.Test_Node_Class.Generic_Case_And_Suite_Run_Body
              (Work => Run_Test_Routines);

private

   type Test_Case_Stub is limited new Test_Node_Interfa with null record;

end Apsepp.Test_Node_Class.Case_Stub;
