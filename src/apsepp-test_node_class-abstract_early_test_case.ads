-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

private with Ada.Exceptions;

with Apsepp.Test_Node_Class.Abstract_Test_Case;
  use Apsepp.Test_Node_Class.Abstract_Test_Case;

package Apsepp.Test_Node_Class.Abstract_Early_Test_Case is

   type Early_Test_Case is abstract limited new Test_Node_Interfa with private
     with Type_Invariant'Class => Early_Test_Case.Child_Count = 0
                                    and then
                                  Early_Test_Case.Has_Early_Test;

   overriding
   function Child_Count (Obj : Early_Test_Case) return Test_Node_Count
     is (0);

   -- TODOC: Always fails because a test case has no child.
   overriding
   function Child (Obj : Early_Test_Case;
                   K   : Test_Node_Index)
     return not null access Test_Node_Interfa'Class;

   overriding
   function No_Subtasking (Obj : Early_Test_Case) return Boolean
     is (True);

   overriding
   function Has_Early_Test (Obj : Early_Test_Case) return Boolean
     is (True);

   overriding
   function Early_Run_Done (Obj : Early_Test_Case) return Boolean;

   not overriding
   function Early_Routine
     (Obj : Early_Test_Case) return not null access procedure is abstract;

   overriding
   procedure Early_Run (Obj : in out Early_Test_Case);

   overriding
   procedure Run
     (Obj     : in out Early_Test_Case;
      Outcome :    out Test_Outcome;
      Kind    :        Run_Kind     := Assert_Cond_And_Run_Test);

   procedure Early_Run_Body (Obj : in out Early_Test_Case'Class);

private

   use Ada.Exceptions;

   type Early_Test_Case is abstract limited new Test_Node_Interfa with record

      Early_Run_Done_Flag : Boolean                     := False;

      Error               : Exception_Occurrence_Access;

   end record;

end Apsepp.Test_Node_Class.Abstract_Early_Test_Case;