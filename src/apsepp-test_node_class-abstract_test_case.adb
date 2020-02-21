-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Assertions,
     Apsepp.Test_Node_Class.Private_Test_Reporter;

package body Apsepp.Test_Node_Class.Abstract_Test_Case is

   ----------------------------------------------------------------------------

   overriding
   function Child (Obj : Test_Case;
                   K   : Test_Node_Index)
     return not null access Test_Node_Interfa'Class is

      pragma Unreferenced (Obj, K);

   begin

      pragma Warnings (Off, "null value not allowed here");

      return null; -- A test case a no child. The function has to fail.

      pragma Warnings (On, "null value not allowed here");

   end Child;

   ----------------------------------------------------------------------------

   use all type Safe_Test_Assert_Count_Operations.Safe_Integer;

   ----------------------------------------------------------------------------

   function Initial_Case_Status
     (Routine_Index : Test_Routine_Index := 1) return Case_Status
     is (Routine_Index  => Routine_Index,
         Assert_Count   => Create (0),
         Assert_Outcome => Passed);

   ----------------------------------------------------------------------------

   protected body Case_Status_Map_Handler is

      -----------------------------------------------------

      procedure Switch_Key_If_Needed (Node_Tag : Tag) is

         use Case_Status_Hashed_Maps;

         procedure Update_Map_With_Work_Data is
            C : constant Cursor := M.Find (T);
         begin
            if C = No_Element then
               M.Insert (Key      => T,
                         New_Item => S);
            else
               M.Replace_Element (Position => C,
                                  New_Item => S);
            end if;
         end Update_Map_With_Work_Data;

         procedure Extract_Work_Data is
            C : constant Cursor := M.Find (Node_Tag);
         begin
            T := Node_Tag;
            S := (if C = No_Element then
                     Initial_Case_Status
                  else
                     Element (C));
         end Extract_Work_Data;

      begin

         if T /= Node_Tag then

            if T /= No_Tag then
               Update_Map_With_Work_Data;
            end if;

            Extract_Work_Data;

         end if;

      end Switch_Key_If_Needed;

      -----------------------------------------------------

      procedure Reset_Routine_State (Node_Tag      : Tag;
                                     Routine_Index : Test_Routine_Index) is

      begin

         Switch_Key_If_Needed (Node_Tag);
         S := Initial_Case_Status (Routine_Index);

      end Reset_Routine_State;

      -----------------------------------------------------

      procedure Increment_Assert_Count (Node_Tag : Tag) is

      begin

         Switch_Key_If_Needed (Node_Tag);
         Inc (S.Assert_Count);

      end Increment_Assert_Count;

      -----------------------------------------------------

      procedure Set_Failed_Outcome (Node_Tag : Tag) is

      begin

         Switch_Key_If_Needed (Node_Tag);
         S.Assert_Outcome := Failed;

      end Set_Failed_Outcome;

      -----------------------------------------------------

      procedure Get_Assert_Count (Node_Tag      :     Tag;
                                  Routine_Index : out Test_Routine_Index;
                                  Count         : out Safe_Test_Assert_Count)
        is

      begin

         Switch_Key_If_Needed (Node_Tag);
         Routine_Index := S.Routine_Index;
         Count := S.Assert_Count;

      end Get_Assert_Count;

      -----------------------------------------------------

      procedure Get_Assert_Outcome (Node_Tag :     Tag;
                                    Outcome  : out Test_Outcome) is

      begin

         Switch_Key_If_Needed (Node_Tag);
         Outcome := S.Assert_Outcome;

      end Get_Assert_Outcome;

      -----------------------------------------------------

      procedure Delete (Node_Tag : Tag) is

         use Case_Status_Hashed_Maps;

      begin

         if M.Length = 0 then
            T := No_Tag;
         else

            declare
               C : Cursor := M.Find (Node_Tag);
            begin

               if C /= No_Element then
                  M.Delete (C);
               end if;

               if T = Node_Tag and then M.Length > 0 then
                  declare
                     C_First : constant Cursor := M.First;
                  begin
                     T := Key (C_First);
                     S := Element (C_First);
                  end;
               elsif T = Node_Tag then
                  T := No_Tag;
               end if;

            end;

         end if;

      end Delete;

      -----------------------------------------------------

      function Invariant return Boolean
        is (
             (
               T /= No_Tag
                 or else
               M.Length = 0
             )
               and then
             not M.Contains (No_Tag)
           );

      -----------------------------------------------------

      function Count return Count_Type
        is (M.Length + (if T = No_Tag or else M.Contains (T) then
                           0
                        else
                           1));

      -----------------------------------------------------

      function To_Array return Case_Tag_Status_Array is

         use Case_Status_Hashed_Maps;

         N : constant Count_Type := Count;
         Ret : Case_Tag_Status_Array (1 .. N);

         procedure Populate_Current is
         begin
            if N /= 0 then
               Ret(1).T := T;
               Ret(1).S := S;
            end if;
         end Populate_Current;

         procedure Populate_Others is
            K : Index_Type := 1;
            procedure Process (C : Cursor) is
               Key_C : constant Tag := Key (C);
            begin
               if Key_C /= T then
                  K := K + 1;
                  Ret(K).T := Key_C;
                  Ret(K).S := Element (C);
               end if;
            end Process;
         begin
            M.Iterate (Process'Access);
            Ada.Assertions.Assert ((N = 0 and then K = 1)
                                     or else
                                   (N > 0 and then K = N));
         end Populate_Others;

      begin

         Populate_Current;
         Populate_Others;
         return Ret;

      end To_Array;

      -----------------------------------------------------

   end Case_Status_Map_Handler;

   ----------------------------------------------------------------------------

   procedure Run_Test_Routines (Obj     :     Test_Node_Interfa'Class;
                                Outcome : out Test_Outcome;
                                Kind    :     Run_Kind) is

      use Ada.Assertions,
          Private_Test_Reporter;

      pragma Unreferenced (Kind);

      T : constant Tag := Obj'Tag;

      K   : Test_Routine_Count := 0;
      R   : access procedure   := Null_Test_Routine'Access;
      Err : Boolean            := False; -- "Unexpected error" flag.

      -----------------------------------------------------

      function Done return Boolean is

         N   : constant Test_Routine_Count
           := Test_Case'Class (Obj).Routine_Count;

         Ret : Boolean := K >= N;

      begin

         if Err and then not Ret then
            Ret     := True;
            Outcome := Failed;
            Test_Reporter.Report_Test_Routines_Cancellation (Obj'Tag,
                                                             K + 1,
                                                             N);
         end if;

         return Ret;

      end Done;

      -----------------------------------------------------

   begin

      Outcome := Passed;

      while not Done loop

         K   := K + 1;
         Err := True;

         Case_Status_Map_Handler.Reset_Routine_State (T, K);
         Test_Reporter.Report_Test_Routine_Start (T, K);

         begin

            R := Test_Case'Class (Obj).Routine (K);

            begin

               Test_Case'Class (Obj).Setup_Routine;

               declare
                  Assert_Outcome : Test_Outcome;
               begin
                  R.all;
                  Err := False;
                  Case_Status_Map_Handler.Get_Assert_Outcome
                    (T, Assert_Outcome);
                  case Assert_Outcome is
                     when Failed =>
                        raise Assertion_Error; -- Causes a jump to
                                               -- Assertion_Error handler
                                               -- below. Happens when a test
                                               -- assertion has failed but has
                                               -- been handled in the test
                                               -- routine.
                     when Passed =>
                        null;
                  end case;
                  Test_Reporter.Report_Passed_Test_Routine (T, K);
               exception
                  when Run_E : Assertion_Error =>
                     Case_Status_Map_Handler.Get_Assert_Outcome
                       (T, Assert_Outcome);
                     Err     := False;
                     Outcome := Failed;
                     case Assert_Outcome is
                        when Failed => -- Exception very likely originates in
                                       -- a failed test assertion and not in
                                       -- an "unexpected error".
                           Test_Reporter.Report_Failed_Test_Routine (T, K);
                        when Passed => -- Exception may originates in a failed
                                       -- contract.
                           Test_Reporter.Report_Unexpected_Routine_Exception
                             (T, K, Run_E);
                     end case;
                  when Run_E : others => -- Exception originates in an
                                         -- unexpected error.
                     Test_Reporter.Report_Unexpected_Routine_Exception
                       (T, K, Run_E);
               end;

            exception
               when Setup_E : others =>
                  Test_Reporter.Report_Failed_Test_Routine_Setup
                    (T, K, Setup_E);
            end;

         exception

            when Access_E : others =>
               Test_Reporter.Report_Failed_Test_Routine_Access
                 (T, K, Access_E);

         end;

      end loop;

      Case_Status_Map_Handler.Delete (T);

   end Run_Test_Routines;

   ----------------------------------------------------------------------------

   procedure Assert (Node_Tag : Tag; Cond : Boolean; Message : String := "") is

      use Ada.Assertions,
          Private_Test_Reporter;

      K     : Test_Routine_Index;
      Count : Safe_Test_Assert_Count;

   begin

      Case_Status_Map_Handler.Increment_Assert_Count (Node_Tag);
      Case_Status_Map_Handler.Get_Assert_Count (Node_Tag, K, Count);

      if Cond then

         Test_Reporter.Report_Passed_Test_Assert
           (Node_Tag, K, not Sat (Count), Val (Count));

      else

         Case_Status_Map_Handler.Set_Failed_Outcome (Node_Tag);

         begin
            raise Assertion_Error with Message;
         exception
            when E : others =>
               Test_Reporter.Report_Failed_Test_Assert
                 (Node_Tag, K, not Sat (Count), Val (Count), E);
               raise;
         end;

      end if;

   end Assert;

   ----------------------------------------------------------------------------

   overriding
   procedure Run (Obj     : in out Test_Case;
                  Outcome :    out Test_Outcome;
                  Kind    :        Run_Kind      := Assert_Cond_And_Run_Test)
     is

      -----------------------------------------------------

      function Cond return Boolean
        is (True);

      -----------------------------------------------------

   begin

      Run_Body (Obj, Outcome, Kind, Cond'Access);

   end Run;

   ----------------------------------------------------------------------------

begin

   Ada.Assertions.Assert (Case_Status_Map_Handler.Invariant);

end Apsepp.Test_Node_Class.Abstract_Test_Case;
