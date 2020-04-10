-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Assertions,
     Apsepp.Generic_Discrete_Operations,
     Apsepp.Test_Node_Class.Private_Test_Reporter;

-- This package references type 'Simu_Test_Case' declared in package
-- 'Apsepp.Test_Node_Class.Abstract_Simu_Test_Case'. That's why
-- 'Apsepp.Test_Node_Class.Abstract_Simu_Test_Case' could not be moved to
-- project Apsepp_Test and had to be kept in Apsepp.
with Apsepp.Test_Node_Class.Abstract_Simu_Test_Case;

package body Apsepp.Test_Node_Class.Abstract_Test_Case is

   ----------------------------------------------------------------------------

   use all type Safe_Test_Assert_Count_Operations.Safe_Integer;

   ----------------------------------------------------------------------------

   function Pre_Test_Routine_Case_Status
     (Routine_Index : Test_Routine_Index) return Case_Status
     is (Routine_Index  => Routine_Index,
         Assert_Count   => Create (0),
         Assert_Outcome => Passed);

   ----------------------------------------------------------------------------

   protected body Case_Status_Map_Handler is

      -----------------------------------------------------

      procedure Switch_Key_If_Needed (Node_Tag : Tag) is

         use Case_Status_Hashed_Maps;

      begin

         if T /= Node_Tag then
            -- T is not the wanted tag.

            if T /= No_Tag then
               -- 'T' is a "real" test node tag and not the default tag value
               -- 'No_Tag'. This means that there is valuable information in
               -- 'S'.

               -- Create an entry in map 'M' with key 'T' and data 'S' if there
               -- is not already an entry with such key, or replace the entry.
               if M.Contains (T) then
                  M(T) := S;
               else
                  M.Insert (Key      => T,
                            New_Item => S);
               end if;

            end if;

            -- Extract from map 'M' the data associated with key 'Node_Tag' and
            -- assign to S (and set 'T' to 'Node_Tag'). If there is no
            -- 'Node_Tag' key in the map, than S is set to an appropriate
            -- initial value.
            T := Node_Tag;
            S := (if M.Contains (Node_Tag) then
                     M(T)
                  else
                     Pre_Test_Routine_Case_Status (Routine_Index => 1));

         else
            -- 'T' is already the wanted tag (and 'S' is the associated
            -- status). Nothing more to do.

            null;

         end if;

      end Switch_Key_If_Needed;

      -----------------------------------------------------

      procedure Reset_Case_Status (Node_Tag      : Tag;
                                   Routine_Index : Test_Routine_Index) is

      begin

         Switch_Key_If_Needed (Node_Tag);
         S := Pre_Test_Routine_Case_Status (Routine_Index);

      end Reset_Case_Status;

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
         Count         := S.Assert_Count;

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

         use Ada.Containers,
             Case_Status_Hashed_Maps;

      begin

         if M.Length = 0 then
            -- Tag 'Node_Tag' has no matching entry in the map (map is empty).

            -- Just reset 'T'.
            T := No_Tag;

         else
            -- Tag 'Node_Tag' may have a matching entry in the map (map is not
            -- empty).

            declare
               Position : Cursor := M.Find (Node_Tag);
            begin

               if Position /= No_Element then
                  -- Tag 'Node_Tag' has a matching entry in the map.

                  -- Delete the entry.
                  M.Delete (Position);

               end if;

               if T = Node_Tag and then M.Length > 0 then
                  -- 'Node_Tag' is the current value of 'T' and the map is not
                  -- empty.

                  -- Set 'T' and 'S' to the key and element value of the first
                  -- map entry. (It could have been any entry.)
                  T := Key (M.First);
                  S := M(T);

               elsif T = Node_Tag then
                  -- 'Node_Tag' is the current value of 'T' but the map is
                  -- empty.

                  -- Just reset 'T'.
                  T := No_Tag;

               end if;

            end;

         end if;

      end Delete;

      -----------------------------------------------------

      function Invariant return Boolean is

         use Ada.Containers;

      begin

         return (
                  (
                    T /= No_Tag
                      or else
                    M.Length = 0          -- 'T = No_Tag' implies empty map.
                  )
                    and then
                  not M.Contains (No_Tag) -- No 'No_Tag' entry in the map.
                );

      end Invariant;

      -----------------------------------------------------

      function Count return Test_Node_Count
        is (
             Test_Node_Count (M.Length) -- Number of entries in the map...
               +
             (if T = No_Tag or else M.Contains (T) then
                 0
              else                      -- ... plus 1 for 'T' value (unless 'T'
                 1)                     -- is 'No_Tag' or already in the map).
           );

      -----------------------------------------------------

      function To_Array return Case_Tag_Status_Array is

         use Case_Status_Hashed_Maps;

         N   : constant Test_Node_Count := Count;
         K   :          Test_Node_Count := 0;

         -- Intializing the 'T' components of the elements of the returned
         -- array to 'No_Tag' ensures that the post-condition is violated if
         -- the function fails to assign any of the array components.
         Ret : Case_Tag_Status_Array (1 .. N)
           := (others => (T => No_Tag,
                          S => Pre_Test_Routine_Case_Status (1)));

         procedure Push_To_Array (Status : Case_Tag_Status) is
         begin
            K      := K + 1;
            Ret(K) := Status;
         end Push_To_Array;

      begin

         if N /= 0 then

            -- Copy 'T' and 'S' values to the first element of the returned
            -- array.
            Push_To_Array ((T => T,
                            S => S));

            -- Copy map elements to other elements of the array (making sure to
            -- skip 'T' (already processed above)).
            for Position in M.Iterate loop
               declare
                  K : constant Tag := Key (Position);
               begin
                  if K /= T then
                     Push_To_Array ((T => K,
                                     S => M(K)));
                  end if;
               end;
            end loop;

         else
            -- No data available.

            -- Nothing more to do.
            null;

         end if;

         return Ret;

      end To_Array;

      -----------------------------------------------------

   end Case_Status_Map_Handler;

   ----------------------------------------------------------------------------

   procedure Run_Test_Routines (Obj     :     Test_Node_Interfa'Class;
                                Outcome : out Test_Outcome) is

      use Ada.Assertions,
          Private_Test_Reporter,
          Abstract_Simu_Test_Case;

      Is_Simu_Test_Case : constant Boolean
        := Is_Descendant_At_Same_Level (Descendant => Obj'Tag,
                                        Ancestor   => Simu_Test_Case'Tag);

      -----------------------------------------------------

      function Routine_Count return Test_Routine_Count
        is (if Is_Simu_Test_Case then
               Simu_Test_Case'Class (Obj).Routine_Count
            else
               Test_Case'Class (Obj).Routine_Count);

      -----------------------------------------------------

      function Routine (K : Test_Routine_Index)
        return not null access procedure
        is (if Is_Simu_Test_Case then
               Simu_Test_Case'Class (Obj).Routine (K)
            else
               Test_Case'Class (Obj).Routine (K));

      -----------------------------------------------------

      procedure Set_Up_Routine is
      begin
         if Is_Simu_Test_Case then
            Simu_Test_Case'Class (Obj).Set_Up_Routine;
         else
            Test_Case'Class (Obj).Set_Up_Routine;
         end if;
      end Set_Up_Routine;

      -----------------------------------------------------

      T : constant Tag                := Obj'Tag;
      N : constant Test_Routine_Count := Routine_Count;

      R   : access procedure   := Null_Test_Routine'Access;
      Err : Boolean            := False; -- "Unexpected error" flag.

   begin

      Outcome := Passed;

      -- Loop over test case test routines. Loop is exited prematurely if 'Err'
      -- is true (that is after a test routine run has failed with an
      -- "unexpected error"), see the exit statement at the bottom of the loop.
      for K in 1 .. N loop

         Err := True;

         Case_Status_Map_Handler.Reset_Case_Status (T, K);
         Test_Reporter.Report_Test_Routine_Start (Node_Tag      => T,
                                                  Routine_Index => K);

         begin

            R := Routine (K);

            begin

               Set_Up_Routine;

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
                                               -- 'Assertion_Error handler'
                                               -- below. Happens when a test
                                               -- assertion has failed but has
                                               -- been handled in the test
                                               -- routine.
                     when Passed =>
                        null;
                  end case;
                  Test_Reporter.Report_Passed_Test_Routine
                    (Node_Tag      => T,
                     Routine_Index => K);
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
                           Test_Reporter.Report_Failed_Test_Routine
                             (Node_Tag      => T,
                              Routine_Index => K);
                        when Passed => -- Exception may originates in a failed
                                       -- contract.
                           Test_Reporter.Report_Unexpected_Routine_Exception
                             (Node_Tag      => T,
                              Routine_Index => K,
                              Error         => Run_E);
                     end case;
                  when Run_E : others => -- Exception originates in an
                                         -- "unexpected error".
                     Test_Reporter.Report_Unexpected_Routine_Exception
                       (Node_Tag      => T,
                        Routine_Index => K,
                        Error         => Run_E);
                     -- 'Err' is true here.
               end;

            exception
               when Setup_E : others =>
                  Test_Reporter.Report_Failed_Test_Routine_Setup
                    (Node_Tag      => T,
                     Routine_Index => K,
                     Error         => Setup_E);
            end;

         exception

            when Access_E : others =>
               Test_Reporter.Report_Failed_Test_Routine_Access
                 (Node_Tag      => T,
                  Routine_Index => K,
                  Error         => Access_E);

         end;

         if Err then

            Outcome := Failed;

            if K < N then
               -- The current test routine is not the last one.

               -- Report cancellation of remaining test routines and exit the
               -- loop.
               Test_Reporter.Report_Test_Routines_Cancellation
                 (Node_Tag            => Obj'Tag,
                  First_Routine_Index => K + 1,
                  Last_Routine_Index  => N);

               exit; -- Early exit.

            end if;

         end if;

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
           (Node_Tag         => Node_Tag,
            Routine_Index    => K,
            Assert_Num_Avail => not Sat (Count),
            Assert_Num       => Val (Count));

      else

         Case_Status_Map_Handler.Set_Failed_Outcome (Node_Tag);

         begin
            raise Assertion_Error with Message;
         exception
            when E : others =>
               Test_Reporter.Report_Failed_Test_Assert
                 (Node_Tag         => Node_Tag,
                  Routine_Index    => K,
                  Assert_Num_Avail => not Sat (Count),
                  Assert_Num       => Val (Count),
                  Error            => E);
               raise;
         end;

      end if;

   end Assert;

   ----------------------------------------------------------------------------

   overriding
   function Child (Obj : Test_Case;
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
   procedure Run (Obj     : in out Test_Case;
                  Outcome :    out Test_Outcome;
                  Kind    :        Run_Kind     := Assert_Cond_And_Run_Test) is

      -----------------------------------------------------

      function Cond return Boolean
        is (True);

      -----------------------------------------------------

   begin

      Run_Body (Obj, Outcome, Kind, Cond'Access);

   end Run;

   ----------------------------------------------------------------------------

   package Test_Routine_Count_Operations
     is new Generic_Discrete_Operations
     (Discrete_Type => Test_Routine_Index,
      Diff_Type     => Test_Routine_Count'Base);

   use Test_Routine_Count_Operations;

   ----------------------------------------------------------------------------

   not overriding
   function Routine
     (Obj : Test_Case;
      K   : Test_Routine_Index) return not null access procedure is

      Routine_Array_First : constant Test_Routine_Index
        := Test_Case'Class (Obj).Routine_Array'First;

      function Routine_Array return Test_Routine_Array
        renames Test_Case'Class (Obj).Routine_Array;

   begin

      return Routine_Array (Val (K, Routine_Array_First));

   end Routine;

   ----------------------------------------------------------------------------

   not overriding
   function Routine_Array_Equiv_To_Routine (Obj : Test_Case) return Boolean is

      Routine_Array_Length : constant Test_Routine_Count
        := Test_Case'Class (Obj).Routine_Array'Length;

      Routine_Array_First : constant Test_Routine_Index
        := Test_Case'Class (Obj).Routine_Array'First;

      -- The following renaming cannot be used in the definitions above.
      -- ("Cannot call 'Routine_Array' before body seen.")
      function Routine_Array return Test_Routine_Array
        renames Test_Case'Class (Obj).Routine_Array;

      Routine_Count : constant Test_Routine_Count
        := Test_Case'Class (Obj).Routine_Count;

      function Routine
        (K : Test_Routine_Index) return not null access procedure
        renames Test_Case'Class (Obj).Routine;

   begin

      return (
               Routine_Count = Routine_Array_Length
                 and then
               (
                 for all K in Routine_Array'Range =>
                   Routine (Rank (K, Routine_Array_First)) = Routine_Array(K)
               )
             );

   end Routine_Array_Equiv_To_Routine;

   ----------------------------------------------------------------------------

begin

   -- Do a check of the invariant of 'Case_Status_Map_Handler' before any
   -- operation.
   Ada.Assertions.Assert (Case_Status_Map_Handler.Invariant);

end Apsepp.Test_Node_Class.Abstract_Test_Case;
