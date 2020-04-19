-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Test_Case_Count_Types; use Apsepp.Test_Case_Count_Types;

with Apsepp.Logical_Array_Instance.Assertions_W_Debug_Trace;

-- TODOC: This package is withed by package
-- 'Apsepp.Test_Node_Class.Abstract_Test_Case' (which belongs to project
-- Apsepp). That's why it had to be kept in project Apsepp and could not be
-- moved to project Apsepp_Test.
package Apsepp.Test_Node_Class.Abstract_Simu_Test_Case is

   use Logical_Array_Instance.Assertions_W_Debug_Trace
         .L_A_Assertions_W_Debug_Trace;

   type Test_Routine_Destiny_Kind is (No_Failure,
                                      Access_Failure,
                                      Setup_Failure,
                                      Test_Assertion_Failure,
                                      Handled_Test_Failure,
                                      Contract_Failure,
                                      Other_Failure);

   type Test_Routine_Destiny is record
      Kind                         : Test_Routine_Destiny_Kind;
      Successful_Test_Assert_Count : Test_Assert_Count;
   end record;

   type Simu_Test_Case_Story
     is array (Test_Routine_Index range <>) of Test_Routine_Destiny;

   type Simu_Test_Case is abstract limited new Test_Node_Interfa with private
     with Type_Invariant'Class
            => All_True ((Simu_Test_Case.Story_Equiv_To_Routine,
                          Simu_Test_Case.Child_Count = 0,
                          not Simu_Test_Case.Has_Early_Test));

   overriding
   function Child_Count (Obj : Simu_Test_Case)
     return Test_Node_Count
     is (0);

   -- TODOC: Always fails because a test case has no child. <2020-03-08>
   overriding
   function Child (Obj : Simu_Test_Case;
                   K   : Test_Node_Index)
     return not null access Test_Node_Interfa'Class;

   overriding
   function No_Subtasking (Obj : Simu_Test_Case) return Boolean
     is (True);

   overriding
   function Has_Early_Test (Obj : Simu_Test_Case) return Boolean
     is (False);

   overriding
   function Early_Run_Done (Obj : Simu_Test_Case) return Boolean
     is (True);

   overriding
   procedure Early_Run (Obj : in out Simu_Test_Case) is null;

   overriding
   procedure Run
     (Obj     : in out Simu_Test_Case;
      Outcome :    out Test_Outcome;
      Kind    :        Run_Kind       := Assert_Cond_And_Run_Test);

   not overriding
   function Story (Obj : Simu_Test_Case)
     return Simu_Test_Case_Story is abstract;

   not overriding
   function Routine_Count (Obj : Simu_Test_Case) return Test_Routine_Index
     is (Simu_Test_Case'Class (Obj).Story'Length);

   not overriding
   function Routine
     (Obj : Simu_Test_Case;
      K   : Test_Routine_Index) return not null access procedure;

   not overriding
   procedure Set_Up_Routine (Obj : Simu_Test_Case);

   not overriding
   function Story_Equiv_To_Routine (Obj : Simu_Test_Case) return Boolean;

private

   type Simu_Test_Case
     is abstract limited new Test_Node_Interfa with null record;

end Apsepp.Test_Node_Class.Abstract_Simu_Test_Case;
