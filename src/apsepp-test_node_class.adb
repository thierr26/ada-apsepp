-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Assertions;
with Apsepp.Test_Reporter_Class;
with Apsepp.Test_Node_Class.Private_Test_Reporter;

package body Apsepp.Test_Node_Class is

   ----------------------------------------------------------------------------

   procedure Run_Test_Routines (Obj     :     Test_Node_Interfa'Class;
                                Outcome : out Test_Outcome;
                                Kind    :     Run_Kind) is

      use Apsepp.Test_Node_Class.Private_Test_Reporter;

      pragma Unreferenced (Kind);

      K              : Test_Routine_Count := 0;
      R              : Test_Routine       := Null_Test_Routine'Access;
      Unexpected_Err : Boolean            := False;
      R_Outcome      : Test_Outcome;

      -----------------------------------------------------

      function Done return Boolean is

         N   : constant Test_Routine_Count := Obj.Routine_Count;
         Ret :          Boolean            := K >= Obj.Routine_Count;

      begin

         if Unexpected_Err then
            Ret     := True;
            Outcome := Failed;
            Test_Reporter.Report_Test_Routines_Cancellation
              (Obj'Tag,
               Apsepp.Test_Reporter_Class.Test_Routine_Index (K),
               Apsepp.Test_Reporter_Class.Test_Routine_Index (N));
         end if;

         return Ret;

      end Done;

      -----------------------------------------------------

   begin

      Outcome := Passed;

      while not Done loop

         K := K + 1;
         R_Outcome := Failed;
         Unexpected_Err := False;

         Test_Reporter.Report_Test_Routine_Start
           (Obj'Tag, Apsepp.Test_Reporter_Class.Test_Routine_Index (K));

         begin

            R := Obj.Routine (K);

            begin

               Obj.Setup_Routine;

               begin
                  Test_Reporter.Set_Unreported_Routine_Exception_Details_Flag
                    (Obj'Tag);
                  R.all;
                  R_Outcome := Passed;
                  Test_Reporter.Report_Passed_Test_Routine (Obj'Tag);
               exception
                  when E : others =>
                     if Test_Reporter.Unreported_Routine_Exception_Details then
                        Test_Reporter.Report_Unexpected_Routine_Exception
                          (Obj'Tag, E);
                     end if;
                     Test_Reporter.Report_Failed_Test_Routine (Obj'Tag);
               end;
               Test_Reporter.Reset_Unreported_Routine_Exception_Details_Flag
                 (Obj'Tag);

            exception
               when others =>
                  Unexpected_Err := True;
                  Test_Reporter.Report_Failed_Test_Routine_Setup (Obj'Tag);
            end;

         exception

            when others =>
               Unexpected_Err := True;
               Test_Reporter.Report_Failed_Test_Routine_Access (Obj'Tag);

         end;

         Outcome := (case R_Outcome is
                        when Failed => Failed,
                        when Passed => Outcome);

      end loop;

   end Run_Test_Routines;

   ----------------------------------------------------------------------------

   procedure Assert (Node_Tag : Tag; Cond : Boolean; Message : String := "") is

      use Ada.Assertions;
      use Apsepp.Test_Node_Class.Private_Test_Reporter;

   begin

      if Cond then

         Test_Reporter.Report_Passed_Test_Assert (Node_Tag);

      else

         Test_Reporter.Report_Failed_Test_Assert (Node_Tag, Message);
         Test_Reporter.Reset_Unreported_Routine_Exception_Details_Flag
           (Node_Tag);

         if Message'Length = 0 then
            raise Assertion_Error;
         else
            raise Assertion_Error with Message;
         end if;

      end if;

   end Assert;

   ----------------------------------------------------------------------------

end Apsepp.Test_Node_Class;
