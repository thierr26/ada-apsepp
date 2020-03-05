-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Unchecked_Deallocation,
     Ada.Tags,
     Apsepp.Test_Node_Class.Generic_Case_And_Suite_Run_Body,
     Apsepp.Test_Node_Class.Private_Test_Reporter;

with Apsepp.Test_Node_Class.Abstract_Test_Case;
  use Apsepp.Test_Node_Class.Abstract_Test_Case;

package body Apsepp.Test_Node_Class.Abstract_Early_Test_Case is

   ----------------------------------------------------------------------------

   overriding
   function Child (Obj : Early_Test_Case;
                   K   : Test_Node_Index)
     return not null access Test_Node_Interfa'Class is

      pragma Unreferenced (Obj, K);

   begin

      pragma Warnings (Off, "null value not allowed here");

      return null; -- A test case has no child. The function has to fail.

      pragma Warnings (On, "null value not allowed here");

   end Child;

   ----------------------------------------------------------------------------

   overriding
   procedure Early_Run (Obj : in out Early_Test_Case) is

   begin

      Children_Early_Test_Handler (Obj).Early_Run; -- Inherited procedure
                                                   -- call.

      Early_Test_Case'Class (Obj).Early_Routine.all;

   exception

      when E : others => Obj.Error := Save_Occurrence (E);

   end Early_Run;

   ----------------------------------------------------------------------------

   overriding
   procedure Run
     (Obj     : in out Early_Test_Case;
      Outcome :    out Test_Outcome;
      Kind    :        Run_Kind        := Assert_Cond_And_Run_Test) is

      -----------------------------------------------------

      procedure Work (Obj     :     Test_Node_Interfa'Class;
                      Outcome : out Test_Outcome) is

         use Ada.Tags,
             Private_Test_Reporter;

         T   : constant Tag := Obj'Tag;

         Err : constant Exception_Occurrence_Access
           := Early_Test_Case'Class (Obj).Error;

         E_M_Str : constant String
           := (if Err = null or else Exception_Message (Err.all)'Length
                                       =
                                     0 then
                  ""
               else
                  " with """ & Exception_Message (Err.all) & """");

         Msg : constant String := (if Err = null then
                                      ""
                                   else
                                      "Early test routine has raised "
                                        & Exception_Name (Err.all) & E_M_Str);

      begin

         Outcome := Passed;

         Test_Reporter.Report_Test_Routine_Start (Node_Tag      => T,
                                                  Routine_Index => 1);

         Assert (Node_Tag => T,
                 Cond     => Err = null,
                 Message  => Msg);

         Test_Reporter.Report_Passed_Test_Routine
           (Node_Tag      => T,
            Routine_Index => 1);

      exception

         when others =>
            Outcome := Failed;
            Test_Reporter.Report_Failed_Test_Routine
              (Node_Tag      => T,
               Routine_Index => 1);

      end Work;

      -----------------------------------------------------

      procedure Early_Test_Case_Run_Body
        is new Generic_Case_And_Suite_Run_Body (Work);

      -----------------------------------------------------

      procedure Free_Exception is new Ada.Unchecked_Deallocation
        (Object => Exception_Occurrence,
         Name   => Exception_Occurrence_Access);

      -----------------------------------------------------

      function Cond return Boolean
        is (Early_Test_Case'Class (Obj).Early_Run_Done);

      -----------------------------------------------------

   begin

      Early_Test_Case_Run_Body (Obj, Outcome, Kind, Cond'Access);

      case Kind is
         when Check_Cond               => null;
         when Assert_Cond_And_Run_Test => Free_Exception (Obj.Error);
      end case;

      declare
         Outc : Test_Outcome;
      begin
         Children_Early_Test_Handler (Obj).Run (Outc, Kind); -- Inherited
                                                             -- procedure call.
      end;

   end Run;

   ----------------------------------------------------------------------------

end Apsepp.Test_Node_Class.Abstract_Early_Test_Case;
