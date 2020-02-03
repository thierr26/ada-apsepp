-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

private with Ada.Exceptions;
private with Ada.Tags;
with Apsepp.Test_Node_Class.Case_Stub; use Apsepp.Test_Node_Class.Case_Stub;
  use Apsepp.Test_Node_Class;

package Apsepp.Abstract_Early_Test_Case is

   subtype Test_Routine is Test_Node_Class.Test_Routine;

   type Early_Test_Case is abstract limited new Test_Case_Stub with private

     with Type_Invariant'Class =>
            Early_Test_Case.Invariant_Class_Early_Test_Case;

   not overriding
   function Invariant_Class_Early_Test_Case
     (Obj : Early_Test_Case) return Boolean
     is (Early_Test_Case'Class (Obj).Routine_Count = 1
           and then
         Early_Test_Case'Class (Obj).Has_Early_Test);

   overriding
   function Has_Early_Test (Obj : Early_Test_Case) return Boolean
     is (True);

   overriding
   function Early_Run_Done (Obj : Early_Test_Case) return Boolean;

   not overriding
   function Early_Routine (Obj : Early_Test_Case) return Test_Routine
     is abstract;

   overriding
   procedure Early_Run (Obj : in out Early_Test_Case);

   overriding
   function Routine_Count (Obj : Early_Test_Case) return Test_Routine_Count
     is (1);

   overriding
   function Routine (Obj : Early_Test_Case;
                     K   : Test_Routine_Index) return Test_Routine;

   overriding
   procedure Run
     (Obj     : in out Early_Test_Case;
      Outcome :    out Test_Outcome;
      Kind    :        Run_Kind        := Assert_Cond_And_Run_Test);

   procedure Early_Run_Body (Obj : in out Early_Test_Case'Class);

private

   use Ada.Exceptions,
       Ada.Tags;

   type Early_Test_Case is abstract limited new Test_Case_Stub with record
      Early_Run_Done_Flag : Boolean                     := False;
      E                   : Exception_Occurrence_Access;
   end record;

   ----------------------------------------------------------------------------

   protected Data_Store is

      function Locked return Boolean;

      -- TODOC: Post => Locked <2019-03-27>
      entry Set (T : Tag;
                 E : Exception_Occurrence_Access);

      -- TODOC: Pre => Locked <2019-06-09>
      function T return Tag;

      -- TODOC: Pre => Locked <2019-06-09>
      function E return Exception_Occurrence_Access;

      -- TODOC: Post => not Locked <2019-03-27>
      procedure Reset;

   private

      Locked_Flag : Boolean                     := False;
      T_Val       : Tag;
      E_Access    : Exception_Occurrence_Access;

   end Data_Store;

   ----------------------------------------------------------------------------

end Apsepp.Abstract_Early_Test_Case;
