-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Exceptions;                  use Ada.Exceptions;
with Ada.Calendar;                    use Ada.Calendar;
with Ada.Characters.Handling;         use Ada.Characters.Handling;
with Apsepp.Test_Reporter_Class.Stub; use Apsepp.Test_Reporter_Class.Stub;
with Apsepp.Characters;               use Apsepp.Characters;
with Apsepp.Test_Reporter_Data_Struct_Class;
  use Apsepp.Test_Reporter_Data_Struct_Class;
with Apsepp.Generic_Array_Operations;
with Apsepp.Generic_Prot_Integer;
private with Apsepp.Scope_Debug;

package Apsepp.Test_Node_Class.Testing is

   ----------------------------------------------------------------------------

   type Flattened_Routine_State is record
      Routine_I : Test_Routine_Count;
      Assert_C  : Test_Assert_Count;
      Assert_O  : Test_Outcome;
      T         : Tag;
   end record;

   function "<" (Left, Right : Flattened_Routine_State) return Boolean
     is (Left.T < Right.T);

   function To_Flattened_Routine_State
     (T_R_S : Tag_Routine_State) return Flattened_Routine_State
     is (T         => T_R_S.T,
         Routine_I => T_R_S.S.Routine_Index,
         Assert_C  => Prot_Test_Assert_Count.Val (T_R_S.S.Assert_Count),
         Assert_O  => T_R_S.S.Assert_Outcome);

   function From_Flattened_Routine_State
     (F_R_S : Flattened_Routine_State) return Tag_Routine_State
     is (T => F_R_S.T,
         S => (Routine_Index  => F_R_S.Routine_I,
               Assert_Count   =>
                 Prot_Test_Assert_Count.Create (F_R_S.Assert_C),
               Assert_Outcome => F_R_S.Assert_O));

   type Routine_State_Array
     is array (Event_Index range <>) of Flattened_Routine_State;

   type Routine_State_Array_Access is access constant Routine_State_Array;

   package Flattened_Routine_State_Array_Operations
     is new Generic_Array_Operations (Index_Type   => Event_Index,
                                      Element_type => Flattened_Routine_State,
                                      Array_Type   => Routine_State_Array,
                                      "<"          => "<");

   use Flattened_Routine_State_Array_Operations;

   function To_Array return Routine_State_Array

     with Post => To_Array'Result'First = 1
                    and then
                  Monotonic_Incr (To_Array'Result(2 .. To_Array'Result'Last));

   ----------------------------------------------------------------------------

   type Tag_To_Char_Func is access function (T : Tag) return ISO_646;

   type Char_To_Tag_Func
     is access function (Char : ISO_646_Upper_Letter) return Tag;

   Default_Test_Node_Barrier_Refresh_Delay : constant Day_Duration := 0.7;
                                                                -- 0.7 seconds.

   -- TODOC: Refresh_Delay component is the delay between two consective calls
   -- to Refresh procedure of type Test_Node_Barrier done by a task of type
   -- Test_Node_Barrier_Stimulus_Task. Delay between last crossing and Barrier
   -- actual time out is in any case lower than Refresh_Delay * 2. <2019-06-29>
   type Test_Node_Barrier_Param is record
      Tag_To_Char            : not null Tag_To_Char_Func;
      Char_To_Tag            : not null Char_To_Tag_Func;
      Expected_Routine_State : not null Routine_State_Array_Access;
      Refresh_Delay          : Day_Duration
        := Default_Test_Node_Barrier_Refresh_Delay ;
   end record;

   -- TODOC: Saturation is highly unlikely (would imply an unreasonably long
   -- array in the test node barrier parameter record (component
   -- Expected_Routine_State of Test_Node_Barrier_Param)). <2019-06-13>
   type Test_Node_Barrier_Permanent_Opening_Cause is (None,
                                                      Saturation,
                                                      Overflow,
                                                      Time_Out);

   type Test_Node_Barrier_Param_Access is access all Test_Node_Barrier_Param;

   package Prot_Event_Count is new Generic_Prot_Integer (Event_Count);

   protected type Test_Node_Barrier is

      procedure Set_Param (Param : not null Test_Node_Barrier_Param_Access)

        with Pre => Param.Expected_Routine_State'First = 1;

      entry Cross(ISO_646);

      procedure Refresh;

      function Get_Param return not null Test_Node_Barrier_Param_Access;

      function Cross_Count return Event_Count;

      function Timed_Out return Boolean;

      -- TODOC: Meaningless if not Timed_Out. <2019-06-13>
      function Cross_Count_On_Time_Out return Event_Count;

      function Saturated return Boolean;

      function Overflowed return Boolean;

      function Completed return Boolean;

      function Failed_Validation return Boolean;

   private

      P : Test_Node_Barrier_Param_Access;

      Crossing_Count : Prot_Event_Count.O_P_I_Type
        := Prot_Event_Count.Create (0);

      -- TODOC: Meaningless if Permanent_Opening_Cause /= Time_Out.
      -- <2019-06-13>
      Crossing_Count_On_Time_Out : Event_Count;

      Latest_Crossing_Date : Time := Clock;

      Permanent_Opening_Cause : Test_Node_Barrier_Permanent_Opening_Cause
        := None;

      Failed_Validation_Flag : Boolean := False;

   end Test_Node_Barrier;

   type Test_Node_Barrier_Access is access all Test_Node_Barrier;

   task type Test_Node_Barrier_Stimulus_Task is

      entry Set_Barrier (Barrier : not null Test_Node_Barrier_Access);

   end Test_Node_Barrier_Stimulus_Task;

   ----------------------------------------------------------------------------

   task type Test_Runner_Task is

      entry Set_Runner (Runner : Test_Node_Access);

      entry Get_E (E : out Exception_Occurrence_Access);

   end Test_Runner_Task;

   ----------------------------------------------------------------------------

   type Test_Reporter_Test_Node_Barrier
     is limited new Test_Reporter_Stub with private;

   not overriding
   procedure Set_Barrier (Obj     : in out Test_Reporter_Test_Node_Barrier;
                          Barrier :        not null Test_Node_Barrier_Access);

   not overriding
   function Arriv_To_Cross_Message
     (Obj            : Test_Reporter_Test_Node_Barrier;
      Operation_Name : String;
      Node_Tag       : Tag) return String;

   overriding
   procedure Report_Failed_Child_Test_Node_Access
     (Obj                : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag           :        Tag;
      First_Child        :        Boolean;
      Previous_Child_Tag :        Tag;
      E                  :        Exception_Occurrence);

   overriding
   procedure Report_Unexpected_Node_Cond_Check_Error
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag;
      E        :        Exception_Occurrence);

   overriding
   procedure Report_Unexpected_Node_Run_Error
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag;
      E        :        Exception_Occurrence);

   overriding
   procedure Report_Node_Cond_Check_Start
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag);

   overriding
   procedure Report_Passed_Node_Cond_Check
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag);

   overriding
   procedure Report_Failed_Node_Cond_Check
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag);

   overriding
   procedure Report_Passed_Node_Cond_Assert
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag);

   overriding
   procedure Report_Failed_Node_Cond_Assert
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag);

   overriding
   procedure Report_Node_Run_Start
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag);

   overriding
   procedure Report_Test_Routine_Start
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count);

   overriding
   procedure Report_Test_Routines_Cancellation
     (Obj             : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag        :        Tag;
      First_K, Last_K :        Test_Node_Class.Test_Routine_Count);

   overriding
   procedure Report_Failed_Test_Routine_Access
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count;
      E        :        Exception_Occurrence);

   overriding
   procedure Report_Failed_Test_Routine_Setup
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count;
      E        :        Exception_Occurrence);

   overriding
   procedure Report_Passed_Test_Assert
     (Obj              : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag         :        Tag;
      K                :        Test_Node_Class.Test_Routine_Count;
      Assert_Num_Avail :        Boolean;
      Assert_Num       :        Test_Node_Class.Test_Assert_Count);

   overriding
   procedure Report_Failed_Test_Assert
     (Obj              : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag         :        Tag;
      K                :        Test_Node_Class.Test_Routine_Count;
      Assert_Num_Avail :        Boolean;
      Assert_Num       :        Test_Node_Class.Test_Assert_Count;
      E                :        Exception_Occurrence);

   overriding
   procedure Report_Unexpected_Routine_Exception
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count;
      E        :        Exception_Occurrence);

   overriding
   procedure Report_Passed_Test_Routine
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count);

   overriding
   procedure Report_Failed_Test_Routine
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count);

   overriding
   procedure Report_Passed_Node_Run
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag);

   overriding
   procedure Report_Failed_Node_Run
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag);

   ----------------------------------------------------------------------------

private

   use Apsepp.Scope_Debug;

   type Test_Reporter_Test_Node_Barrier
     is limited new Test_Reporter_Stub with record

      B     : Test_Node_Barrier_Access;
      C_D_T : Controlled_Debug_Tracer (0) := Create_N ("");

   end record;

end Apsepp.Test_Node_Class.Testing;
