-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Assertions,
     Ada.Tags,
     Apsepp.Generic_Discrete_Operations,
     Apsepp.Protected_Barrier,
     Apsepp.Test_Node_Class.Abstract_Test_Case;

package body Apsepp.Test_Node_Class.Abstract_Simu_Test_Case is

   use Ada.Tags,
       Protected_Barrier;

   ----------------------------------------------------------------------------

   Global_Data_Protection_Barrier : Barrier; -- Initial state: opened.

   type Global_Data_Type is record
      T : Tag;
      D : Test_Routine_Destiny;
   end record;

   Global_Data : Global_Data_Type;

   function Get_Global_Data return Global_Data_Type is
   begin
      Ada.Assertions.Assert
        (Global_Data_Protection_Barrier.Closed,
         "Attempt to get global data while protection barrier is opened.");
      return Global_Data;
   end Get_Global_Data;
   -- TODO: Check whether this protection is appropriate. <2020-03-08>

   ----------------------------------------------------------------------------

   procedure Simu_Test_Routine is

      Simu_Test_Routine_Other_Error : exception;

      Data    : constant Global_Data_Type     := Get_Global_Data;
      D       : constant Test_Routine_Destiny := Data.D;
      T       : constant Tag                  := Data.T;
      Handled :          Boolean              := False;

   begin

      -- Re-open protection barrier.
      Global_Data_Protection_Barrier.Open;

      for K in 1 .. D.Successful_Test_Assert_Count loop

         Abstract_Test_Case.Assert (T, True);

      end loop;

      case D.Kind is

         when No_Failure =>
            null;

         when Test_Assertion_Failure =>
            Abstract_Test_Case.Assert (T,
                                       False,
                                       "Simulated test assertion failure.");

         when Handled_Test_Failure =>
            Handled := True;
            Abstract_Test_Case.Assert
              (T,
               False,
               "Simulated test assertion failure (handled).");

         when Contract_Failure =>
            raise Ada.Assertions.Assertion_Error
              with "Simulated contract failure.";

         when Other_Failure =>
            raise Simu_Test_Routine_Other_Error
              with "Simulated unexpected test routine failure.";

         when Access_Failure =>
            null; -- Impossible case, has already
                  -- caused error and test routine
                  -- abortion in Routine.

         when Setup_Failure =>
            null; -- Impossible case, has already
                  -- caused error and test routine
                  -- abortion in Setup_Routine.

      end case;

   exception
      when others =>
         if not Handled then
            raise;
         end if;
   end Simu_Test_Routine;

   ----------------------------------------------------------------------------

   overriding
   function Child (Obj : Simu_Test_Case;
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
   procedure Run
     (Obj     : in out Simu_Test_Case;
      Outcome :    out Test_Outcome;
      Kind    :        Run_Kind       := Assert_Cond_And_Run_Test) is

      -----------------------------------------------------

      function Cond return Boolean
        is (True);

      -----------------------------------------------------

   begin

      Apsepp.Test_Node_Class.Abstract_Test_Case.Run_Body (Obj,
                                                          Outcome,
                                                          Kind,
                                                          Cond'Access);

   end Run;

   ----------------------------------------------------------------------------

   not overriding
   function Routine
     (Obj : Simu_Test_Case;
      K   : Test_Routine_Index) return not null access procedure is

      Simu_Test_Routine_Access_Error : exception;

      package Test_Routine_Count_Operations is new Generic_Discrete_Operations
        (Discrete_Type => Test_Routine_Index,
         Diff_Type     => Test_Routine_Count);
      use Test_Routine_Count_Operations;

      F : constant Test_Routine_Index
        := Simu_Test_Case'Class (Obj).Story'First;
      D : constant Test_Routine_Destiny
        := Simu_Test_Case'Class (Obj).Story (Val (K, F));

   begin

      case D.Kind is

         when No_Failure
            | Setup_Failure
            | Test_Assertion_Failure
            | Handled_Test_Failure
            | Contract_Failure
            | Other_Failure =>
            null;

         when Access_Failure =>
            raise Simu_Test_Routine_Access_Error
              with "Simulated test routine access failure";

      end case;

      -- Close protection barrier and assign global data.
      Global_Data_Protection_Barrier.Close;
      Global_Data.T := Simu_Test_Case'Class (Obj)'Tag;
      Global_Data.D := D;

      return Simu_Test_Routine'Access;

   end Routine;

   ----------------------------------------------------------------------------

   not overriding
   procedure Setup_Routine (Obj : Simu_Test_Case) is

      Simu_Test_Routine_Setup_Error : exception;

      Data    : constant Global_Data_Type     := Get_Global_Data;
      D       : constant Test_Routine_Destiny := Data.D;

      pragma Unreferenced (Obj);

   begin

      case D.Kind is

         when No_Failure
            | Test_Assertion_Failure
            | Handled_Test_Failure
            | Contract_Failure
            | Other_Failure  =>
            null;

         when Setup_Failure  =>

            -- Re-open protection barrier.
            Global_Data_Protection_Barrier.Open;

            raise Simu_Test_Routine_Setup_Error
              with "Simulated test routine access failure";

         when Access_Failure =>
            null; -- Impossible case, has already caused
                  -- error and test routine abortion in
                  -- Routine.

      end case;

   end Setup_Routine;

   ----------------------------------------------------------------------------

   not overriding
   function Story_Equiv_To_Routine (Obj : Simu_Test_Case) return Boolean is

      Story_Length : constant Test_Routine_Count
        := Simu_Test_Case'Class (Obj).Story'Length;

      Routine_Count : constant Test_Routine_Index
        := Simu_Test_Case'Class (Obj).Routine_Count;

   begin

      return Routine_Count = Story_Length;

   end Story_Equiv_To_Routine;

   ----------------------------------------------------------------------------

end Apsepp.Test_Node_Class.Abstract_Simu_Test_Case;
