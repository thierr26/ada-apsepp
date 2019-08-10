-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Tags,
     Ada.Exceptions,
     Ada.Assertions,
     Ada.Unchecked_Deallocation,
     Ada.Characters.Handling,
     Apsepp.Characters,
     Apsepp.Test_Node_Class.Testing,
     Apsepp.Test_Node_Class.Abstract_Simu_Case,
     Apsepp.Test_Node_Class.Runner_Sequential.Create,
     Apsepp.Generic_Shared_Instance.Access_Setter,
     Apsepp.Debug_Trace,
     Apsepp.Debug_Trace_Class.Standard,
     Apsepp.Scope_Debug,
     Apsepp.Test_Reporter_Data_Struct_Class;

package body Apsepp_Test_Node_Class_Early_Test_Case is

   use Ada.Tags,
       Ada.Exceptions,
       Ada.Characters.Handling,
       Apsepp.Characters,
       Apsepp.Test_Node_Class.Testing,
       Apsepp.Test_Node_Class.Abstract_Simu_Case,
       Apsepp.Test_Node_Class.Runner_Sequential,
       Apsepp.Test_Node_Class,
       Apsepp.Scope_Debug,
       Apsepp.Test_Reporter_Data_Struct_Class;

   Debug_Trace : Apsepp.Debug_Trace_Class.Standard.Debug_Trace_Standard;

   subtype Simu_Case_Char_Name is ISO_646_Upper_Letter range 'A' .. 'E';

   ----------------------------------------------------------------------------

   type Simu_Test_Case_A is limited new Simu_Test_Case with null record;

   overriding
   function Story (Obj : Simu_Test_Case_A) return Simu_Test_Case_Story;

   overriding
   function Story (Obj : Simu_Test_Case_A) return Simu_Test_Case_Story
     is ((Kind => No_Failure,             Successful_Test_Assert_Count => 3),
         (Kind => Test_Assertion_Failure, Successful_Test_Assert_Count => 2),
         (Kind => No_Failure,             Successful_Test_Assert_Count => 1),
         (Kind => No_Failure,             Successful_Test_Assert_Count => 0));

   type Simu_Test_Case_B is limited new Simu_Test_Case with null record;

   overriding
   function Story (Obj : Simu_Test_Case_B) return Simu_Test_Case_Story;

   overriding
   function Story (Obj : Simu_Test_Case_B) return Simu_Test_Case_Story
     is ((Kind => Other_Failure,          Successful_Test_Assert_Count => 5),
         (Kind => No_Failure,             Successful_Test_Assert_Count => 0));
   -- 2nd test routine not run (unexpected failure on 1st test routine causes
   -- the following test routines to be cancelled).

   type Simu_Test_Case_C is limited new Simu_Test_Case with null record;

   overriding
   function Story (Obj : Simu_Test_Case_C) return Simu_Test_Case_Story;

   overriding
   function Story (Obj : Simu_Test_Case_C) return Simu_Test_Case_Story
     is ((Kind => No_Failure,             Successful_Test_Assert_Count => 7),
         (Kind => No_Failure,             Successful_Test_Assert_Count => 6));

   type Simu_Test_Case_D is limited new Simu_Test_Case with null record;

   overriding
   function Story (Obj : Simu_Test_Case_D) return Simu_Test_Case_Story;

   overriding
   function Story (Obj : Simu_Test_Case_D) return Simu_Test_Case_Story
     is ((Kind => No_Failure,             Successful_Test_Assert_Count => 1),
         (Kind => Access_Failure,         Successful_Test_Assert_Count => 0),
         (Kind => No_Failure,             Successful_Test_Assert_Count => 0));
   -- 3rd test routine not run (access failure on 2nd test routine causes the
   -- following test routines to be cancelled).

   type Simu_Test_Case_E is limited new Simu_Test_Case with null record;

   overriding
   function Story (Obj : Simu_Test_Case_E) return Simu_Test_Case_Story;

   overriding
   function Story (Obj : Simu_Test_Case_E) return Simu_Test_Case_Story
     is ((Kind => Handled_Test_Failure,   Successful_Test_Assert_Count => 3),
         (Kind => No_Failure,             Successful_Test_Assert_Count => 2),
         (Kind => Contract_Failure,       Successful_Test_Assert_Count => 4),
         (Kind => Setup_Failure,          Successful_Test_Assert_Count => 0),
         (Kind => No_Failure,             Successful_Test_Assert_Count => 0));
   -- 4th test routine not run (setup failure on 3rd test routine causes the
   -- following test routines to be cancelled).

      -----------------------------------------------------

   type Test_Runner_Sequential_A
     is limited new Test_Runner_Sequential with null record;

   type Test_Runner_Sequential_B
     is limited new Test_Runner_Sequential with null record;

   type Test_Runner_Sequential_C
     is limited new Test_Runner_Sequential with null record;

   type Test_Runner_Sequential_D
     is limited new Test_Runner_Sequential with null record;

   type Test_Runner_Sequential_E
     is limited new Test_Runner_Sequential with null record;

      -----------------------------------------------------

   Simu_Case_A : aliased Simu_Test_Case_A;
   Simu_Case_B : aliased Simu_Test_Case_B;
   Simu_Case_C : aliased Simu_Test_Case_C;
   Simu_Case_D : aliased Simu_Test_Case_D;
   Simu_Case_E : aliased Simu_Test_Case_E;
   Simu_Case : constant array (Simu_Case_Char_Name) of Test_Node_Access
     := (Simu_Case_A'Access,
         Simu_Case_B'Access,
         Simu_Case_C'Access,
         Simu_Case_D'Access,
         Simu_Case_E'Access);

   Reporter : aliased Test_Reporter_Test_Node_Barrier;

   Runner_A : aliased Test_Runner_Sequential_A
     := (Create (Simu_Case('A'), Reporter'Access) with null record);

   Runner_B : aliased Test_Runner_Sequential_B
     := (Create (Simu_Case('B'), Reporter'Access) with null record);

   Runner_C : aliased Test_Runner_Sequential_C
     := (Create (Simu_Case('C'), Reporter'Access) with null record);

   Runner_D : aliased Test_Runner_Sequential_D
     := (Create (Simu_Case('D'), Reporter'Access) with null record);

   Runner_E : aliased Test_Runner_Sequential_E
     := (Create (Simu_Case('E'), Reporter'Access) with null record);

   Runner : constant array (Simu_Case_Char_Name) of Test_Node_Access
     := (Runner_A'Access,
         Runner_B'Access,
         Runner_C'Access,
         Runner_D'Access,
         Runner_E'Access);

   A : constant Tag := Simu_Case('A')'Tag;
   B : constant Tag := Simu_Case('B')'Tag;
   C : constant Tag := Simu_Case('C')'Tag;
   D : constant Tag := Simu_Case('D')'Tag;
   E : constant Tag := Simu_Case('E')'Tag;

   A_R : constant Tag := Runner ('A')'Tag;
   B_R : constant Tag := Runner ('B')'Tag;
   C_R : constant Tag := Runner ('C')'Tag;
   D_R : constant Tag := Runner ('D')'Tag;
   E_R : constant Tag := Runner ('E')'Tag;

   ----------------------------------------------------------------------------

   function Tag_To_Char (T : Tag) return ISO_646 is

      First                        : Boolean := True;
      Found, Loop_Done, Runner_Tag : Boolean := False;
      Ret                          : ISO_646 := Simu_Case'First;

   begin

      -- TODO: Write function Find or Find_First in Generic_Array_Operations.
      -- <2019-06-09>
      while not (Found or else Loop_Done) loop
         Loop_Done := Ret >= Simu_Case'Last;
         if not (Loop_Done or else First) then
            Ret := ISO_646'Succ (Ret);
         end if;
         First := False;
         if Ret <= Simu_Case'Last then
            if Simu_Case(Ret)'Tag = T then
               Found := True;
            elsif Runner(Ret)'Tag = T then
               Found := True;
               Runner_Tag := True;
            end if;
         end if;
      end loop;

      Ada.Assertions.Assert (Found,
                             "Unexpected tag: " & (if T = No_Tag then
                                                      "No_Tag"
                                                   else
                                                      Expanded_Name (T)));

      if Runner_Tag then
         Ret := To_Lower (Ret);
      end if;

      return Ret;

   end Tag_To_Char;

   ----------------------------------------------------------------------------

   function Char_To_Tag (Char : ISO_646_Upper_Letter) return Tag is

      K         : Simu_Case_Char_Name := Simu_Case_Char_Name'First;
      Last_Done : Boolean             := False;

      function Done return Boolean
        is (Last_Done or else Char not in Simu_Case_Char_Name);

   begin

      Ada.Assertions.Assert
        (not Done, Char & " not in " & K & " .. " & Simu_Case_Char_Name'Last);

      while not Done loop

         if Tag_To_Char (Simu_Case(K)'Tag) = Char
              or else
            K = Simu_Case_Char_Name'Last then

            Last_Done := True;

         else
            K := Simu_Case_Char_Name'Succ (K);
         end if;

      end loop;

      Ada.Assertions.Assert
        (Tag_To_Char (Simu_Case(K)'Tag) = Char,
         "Cannot find tag corresponding to '" & Char & "'");

      return Simu_Case(K)'Tag;

   end Char_To_Tag;

   ----------------------------------------------------------------------------

   Expected : aliased constant Routine_State_Array
     := ((T => A_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => A,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => A,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => A_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => A_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => A_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => A,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => B_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => B,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => B,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => B_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => B_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => B_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => B,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => C_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => C,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => C,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => C_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => C_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => C_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => C,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => D_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => D,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => D,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => D_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => D_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => D_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => D,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => E_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => E,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => E,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => E_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => E_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => E_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => E,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => A,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => A,   Routine_I => 1, Assert_C => 0, Assert_O => Passed),
         (T => A,   Routine_I => 1, Assert_C => 1, Assert_O => Passed),
         (T => A,   Routine_I => 1, Assert_C => 2, Assert_O => Passed),
         (T => A,   Routine_I => 1, Assert_C => 3, Assert_O => Passed),
         (T => A,   Routine_I => 1, Assert_C => 3, Assert_O => Passed),
         (T => A,   Routine_I => 2, Assert_C => 0, Assert_O => Passed),
         (T => A,   Routine_I => 2, Assert_C => 1, Assert_O => Passed),
         (T => A,   Routine_I => 2, Assert_C => 2, Assert_O => Passed),
         (T => A,   Routine_I => 2, Assert_C => 3, Assert_O => Failed),
         (T => C,   Routine_I => 2, Assert_C => 3, Assert_O => Failed),
         (T => C,   Routine_I => 1, Assert_C => 0, Assert_O => Passed),
         (T => C,   Routine_I => 1, Assert_C => 1, Assert_O => Passed),
         (T => C,   Routine_I => 1, Assert_C => 2, Assert_O => Passed),
         (T => C,   Routine_I => 1, Assert_C => 3, Assert_O => Passed),
         (T => C,   Routine_I => 1, Assert_C => 4, Assert_O => Passed),
         (T => C,   Routine_I => 1, Assert_C => 5, Assert_O => Passed),
         (T => C,   Routine_I => 1, Assert_C => 6, Assert_O => Passed),
         (T => C,   Routine_I => 1, Assert_C => 7, Assert_O => Passed),
         (T => C,   Routine_I => 1, Assert_C => 7, Assert_O => Passed),
         (T => C,   Routine_I => 2, Assert_C => 0, Assert_O => Passed),
         (T => C,   Routine_I => 2, Assert_C => 1, Assert_O => Passed),
         (T => A,   Routine_I => 2, Assert_C => 3, Assert_O => Failed),
         (T => A,   Routine_I => 3, Assert_C => 0, Assert_O => Passed),
         (T => A,   Routine_I => 3, Assert_C => 1, Assert_O => Passed),
         (T => A,   Routine_I => 3, Assert_C => 1, Assert_O => Passed),
         (T => A,   Routine_I => 4, Assert_C => 0, Assert_O => Passed),
         (T => A,   Routine_I => 4, Assert_C => 0, Assert_O => Passed),
         (T => A,   Routine_I => 4, Assert_C => 0, Assert_O => Passed),
         (T => C,   Routine_I => 2, Assert_C => 2, Assert_O => Passed),
         (T => C,   Routine_I => 2, Assert_C => 3, Assert_O => Passed),
         (T => C,   Routine_I => 2, Assert_C => 4, Assert_O => Passed),
         (T => C,   Routine_I => 2, Assert_C => 5, Assert_O => Passed),
         (T => C,   Routine_I => 2, Assert_C => 6, Assert_O => Passed),
         (T => C,   Routine_I => 2, Assert_C => 6, Assert_O => Passed),
         (T => C,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => C_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => B,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => B,   Routine_I => 1, Assert_C => 0, Assert_O => Passed),
         (T => B,   Routine_I => 1, Assert_C => 1, Assert_O => Passed),
         (T => B,   Routine_I => 1, Assert_C => 2, Assert_O => Passed),
         (T => D,   Routine_I => 1, Assert_C => 5, Assert_O => Passed),
         (T => D,   Routine_I => 1, Assert_C => 0, Assert_O => Passed),
         (T => D,   Routine_I => 1, Assert_C => 1, Assert_O => Passed),
         (T => D,   Routine_I => 1, Assert_C => 1, Assert_O => Passed),
         (T => D,   Routine_I => 2, Assert_C => 0, Assert_O => Passed),
         (T => D,   Routine_I => 2, Assert_C => 0, Assert_O => Passed),
         (T => D,   Routine_I => 2, Assert_C => 0, Assert_O => Passed),
         (T => D,   Routine_I => 2, Assert_C => 0, Assert_O => Passed),
         (T => E,   Routine_I => 2, Assert_C => 0, Assert_O => Passed),
         (T => E,   Routine_I => 1, Assert_C => 0, Assert_O => Passed),
         (T => E,   Routine_I => 1, Assert_C => 1, Assert_O => Passed),
         (T => E,   Routine_I => 1, Assert_C => 2, Assert_O => Passed),
         (T => B,   Routine_I => 1, Assert_C => 3, Assert_O => Passed),
         (T => B,   Routine_I => 1, Assert_C => 4, Assert_O => Passed),
         (T => B,   Routine_I => 1, Assert_C => 5, Assert_O => Passed),
         (T => B,   Routine_I => 1, Assert_C => 5, Assert_O => Passed),
         (T => B,   Routine_I => 1, Assert_C => 5, Assert_O => Passed),
         (T => B,   Routine_I => 1, Assert_C => 5, Assert_O => Passed),
         (T => E,   Routine_I => 1, Assert_C => 3, Assert_O => Passed),
         (T => E,   Routine_I => 1, Assert_C => 4, Assert_O => Failed),
         (T => E,   Routine_I => 1, Assert_C => 4, Assert_O => Failed),
         (T => E,   Routine_I => 2, Assert_C => 0, Assert_O => Passed),
         (T => E,   Routine_I => 2, Assert_C => 1, Assert_O => Passed),
         (T => E,   Routine_I => 2, Assert_C => 2, Assert_O => Passed),
         (T => E,   Routine_I => 2, Assert_C => 2, Assert_O => Passed),
         (T => E,   Routine_I => 3, Assert_C => 0, Assert_O => Passed),
         (T => E,   Routine_I => 3, Assert_C => 1, Assert_O => Passed),
         (T => E,   Routine_I => 3, Assert_C => 2, Assert_O => Passed),
         (T => E,   Routine_I => 3, Assert_C => 3, Assert_O => Passed),
         (T => E,   Routine_I => 3, Assert_C => 4, Assert_O => Passed),
         (T => E,   Routine_I => 3, Assert_C => 4, Assert_O => Passed),
         (T => E,   Routine_I => 4, Assert_C => 0, Assert_O => Passed),
         (T => E,   Routine_I => 4, Assert_C => 0, Assert_O => Passed),
         (T => E,   Routine_I => 4, Assert_C => 0, Assert_O => Passed),
         (T => E,   Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => E_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => B_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => D_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed),
         (T => A_R, Routine_I => 0, Assert_C => 0, Assert_O => Passed));

   Barrier_Param : aliased Test_Node_Barrier_Param
     := (Tag_To_Char            => Tag_To_Char'Access,
         Char_To_Tag            => Char_To_Tag'Access,
         Expected_Routine_State => Expected'Access,
         others                 => <>);

   Barrier : aliased Test_Node_Barrier;

   ----------------------------------------------------------------------------

   procedure Test_Node_Class_Early_Test is

      package Debug_Trace_Standard_Access_Setter
        is new Apsepp.Debug_Trace.Shared_Instance.Access_Setter
        (Inst_Access => Debug_Trace'Access);

      Entity_Name : constant String
        := "Apsepp_Test_Node_Class_Early_Test_Case.Test_Node_Class_Early_Test";
      C_D_T : constant Controlled_Debug_Tracer := Create (Entity_Name);

      pragma Unreferenced (Debug_Trace_Standard_Access_Setter, C_D_T);

      procedure Free_E is new Ada.Unchecked_Deallocation
        (Object => Exception_Occurrence,
         Name   => Exception_Occurrence_Access);

      Barrier_Stimulus : Test_Node_Barrier_Stimulus_Task;

      Runner_Task : array (Simu_Case_Char_Name) of Test_Runner_Task;
      Runner_Task_E
        : array (Simu_Case_Char_Name) of Exception_Occurrence_Access;

   begin

      Ada.Assertions.Assert
        ((for all E of Simu_Case =>
            Is_Descendant_At_Same_Level (E'Tag, Simu_Test_Case'Tag)),
         "Simu_Case elements are not all descendant of Simu_Test_Case");

      Ada.Assertions.Assert
        ((for all E of Runner =>
            Is_Descendant_At_Same_Level (E'Tag,
                                            Test_Runner_Sequential'Tag)),
         "Runner elements are not all descendant of Test_Runner_Sequential");

      Ada.Assertions.Assert
        ((for all K_1 in Simu_Case'Range =>
            (for all K_2 in Simu_Case'Range =>
               K_1 = K_2
                 or else
               Simu_Case(K_1)'Tag /= Simu_Case(K_2)'Tag)),
         "Duplicate tag in Simu_Case array");

      Ada.Assertions.Assert
        ((for all K_1 in Runner'Range =>
            (for all K_2 in Runner'Range =>
               K_1 = K_2
                 or else
               Runner(K_1)'Tag /= Simu_Case(K_2)'Tag)),
         "Duplicate tag in Runner array");

      Reporter.Set_Barrier (Barrier'Access);

      Barrier.Set_Param (Barrier_Param);
      Barrier_Stimulus.Set_Barrier (Barrier'Access);

      for K in Runner_Task'Range loop
         Runner_Task(K).Set_Runner (Runner(K));
      end loop;

      for K in Runner_Task'Range loop
         Runner_Task(K).Get_E (Runner_Task_E(K));
      end loop;

      abort Barrier_Stimulus;

      for K in Runner_Task_E'Range loop
         if Runner_Task_E(K) /= null then
            begin
               Reraise_Occurrence (Runner_Task_E(K).all);
            exception
               when others =>
                  for E of Runner_Task_E loop
                     Free_E (E);
                  end loop;
                  raise;
            end;
         end if;
      end loop;

      declare
         F       : constant Boolean     := Barrier.Timed_Out;
         N       : constant Event_Count := Barrier.Cross_Count_On_Time_Out;
         M       : constant String      := (if F then
                                               (if N = 0 then
                                                   ", no crossing happened"
                                                else
                                                   " after crossing ")
                                            else
                                               "");
         Image_N :          String      := (if F and then N > 0 then
                                               Event_Index'Image (N)
                                            else
                                               "");
      begin
         if Image_N'Length > 0 then
            Image_N(Image_N'First) := '#';
         end if;
         Ada.Assertions.Assert (not Barrier.Timed_Out,
                                "Barrier timed out" & M & Image_N);
      end;

      Ada.Assertions.Assert (not Barrier.Saturated,
                             "Barrier saturated (quite an exploit)");

      Ada.Assertions.Assert (not Barrier.Overflowed, "Barrier overflowed, "
        & "looks like Barrier_Param.Expected_Routine_State'Length should be "
        & "greater (than"
        & Integer'Image (Barrier_Param.Expected_Routine_State'Length) & ")");

      Ada.Assertions.Assert (Barrier.Completed,
        "Barrier_Param.Expected_Routine_State'Length is"
        & Integer'Image (Barrier_Param.Expected_Routine_State'Length)
        & ", only" & Event_Count'Image (Barrier.Cross_Count) & " crossing(s) "
        & "done");

      Ada.Assertions.Assert (not Barrier.Failed_Validation,
         "Failed validation, please analyse trace for details");

      Ada.Assertions.Assert (To_Array'Length = 0,
         "Array returned by To_Array is expected to be empty at this point");

   exception

      when Others =>
         for Ta of Runner_Task loop
            abort Ta;
         end loop;
         abort Barrier_Stimulus;
         raise;

   end Test_Node_Class_Early_Test;

   ----------------------------------------------------------------------------

   overriding
   function Early_Routine (Obj : Apsepp_Test_Node_Class_E_T_C)
     return Apsepp.Abstract_Early_Test_Case.Test_Routine
     is (Test_Node_Class_Early_Test'Access);

   ----------------------------------------------------------------------------

end Apsepp_Test_Node_Class_Early_Test_Case;
