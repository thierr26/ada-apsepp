-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Characters.Handling;         use Ada.Characters.Handling;
with Ada.Tags;                        use Ada.Tags;
with Ada.Calendar;                    use Ada.Calendar;
with Ada.Exceptions;                  use Ada.Exceptions;
with Apsepp.Tags;                     use Apsepp.Tags;
with Apsepp.Characters;               use Apsepp.Characters;
with Apsepp.Test_Reporter_Class.Stub; use Apsepp.Test_Reporter_Class.Stub;
with Apsepp.Scope_Debug;              use Apsepp.Scope_Debug;
with Apsepp.Generic_Prot_Integer,
     Apsepp.Test_Node_Class;
use Apsepp;

package Apsepp_Test_Node_Barrier is

   ----------------------------------------------------------------------------

   type Test_Event_Kind is (Failed_Child_Test_Node_Access,
                            Unexpected_Node_Cond_Check_Error,
                            Unexpected_Node_Run_Error,
                            Node_Cond_Check_Start,
                            Passed_Node_Cond_Check,
                            Failed_Node_Cond_Check,
                            Passed_Node_Cond_Assert,
                            Failed_Node_Cond_Assert,
                            Node_Run_Start,
                            Test_Routine_Start,
                            Test_Routines_Cancellation,
                            Failed_Test_Routine_Access,
                            Failed_Test_Routine_Setup,
                            Passed_Test_Assert,
                            Failed_Test_Assert,
                            Unexpected_Routine_Exception,
                            Passed_Test_Routine,
                            Failed_Test_Routine,
                            Passed_Node_Run,
                            Failed_Node_Run);

   type Char_Name_Image_Func is access function (Char : ISO_646) return String;

   type Tag_To_Char_Func is access function (T : Tag) return ISO_646;

   type Char_To_Tag_Func
     is access function (Char : ISO_646_Upper_Letter) return Tag;

   type Validate_Proc is access procedure (Crossing_Count : Positive;
                                           Event_Kind     : Test_Event_Kind;
                                           Char           : ISO_646;
                                           Char_To_Tag    : Char_To_Tag_Func;
                                           Msg_Pref       : String);

   type Test_Node_Barrier_Permanent_Opening_Cause is (None,
                                                      Saturation,
                                                      Overflow,
                                                      Time_Out);

   package Prot_Natural is new Apsepp.Generic_Prot_Integer (Natural);

   protected type Test_Node_Barrier is

      procedure Setup (Ch_I  : not null Char_Name_Image_Func;
                       T_T_C : not null Tag_To_Char_Func;
                       C_T_C : not null Char_To_Tag_Func;
                       V     : not null Validate_Proc;
                       Exp   : not null Tag_Array_Access);

      entry Cross(ISO_646) (Event_Kind : Test_Event_Kind);

      procedure Time_Out;

      function Cross_Count return Natural;

      function Timed_Out return Boolean;

      -- TODOC: Meaningless if not Timed_Out. <2019-06-13>
      function Cross_Count_On_Time_Out return Natural;

      function Saturated return Boolean;

      function Overflowed return Boolean;

      -- TODOC: Meaningless if not Overflowed. <2019-08-16>
      function Cross_Count_On_Overflow return Natural;

      function Completed return Boolean;

      function Failed_Validation return Boolean;

   private

      Expected_Tag : Tag_Array_Access;

      Crossing_Count : Prot_Natural.O_P_I_Type;

      -- TODOC: Meaningless if Permanent_Opening_Cause /= Time_Out.
      -- <2019-08-08>
      Crossing_Count_On_Time_Out : Natural;

      -- TODOC: Meaningless if Permanent_Opening_Cause /= Overflow.
      -- <2019-08-16>
      Crossing_Count_On_Overflow : Natural;

      Permanent_Opening_Cause : Test_Node_Barrier_Permanent_Opening_Cause;

      Failed_Validation_Flag : Boolean;

      Char_Name_Image : Char_Name_Image_Func;

      Tag_To_Char : Tag_To_Char_Func;

      Char_To_Tag : Char_To_Tag_Func;

      Validate : Validate_Proc;

   end Test_Node_Barrier;

   type Test_Node_Barrier_Access is access all Test_Node_Barrier;

   ----------------------------------------------------------------------------

   task type Test_Node_Barrier_Monitor is

      entry Setup (Barrier           : not null Test_Node_Barrier_Access;
                   Monitoring_Period : Day_Duration := 0.7); -- 0.7 seconds.

   end Test_Node_Barrier_Monitor;

   ----------------------------------------------------------------------------

   type Test_Reporter_W_Barrier is limited new Test_Reporter_Stub with private;

   not overriding
   procedure Setup (Obj   : in out   Test_Reporter_W_Barrier;
                    B     : not null Test_Node_Barrier_Access;
                    Ch_I  : not null Char_Name_Image_Func;
                    T_T_C : not null Tag_To_Char_Func);

   overriding
   procedure Report_Failed_Child_Test_Node_Access
     (Obj                : in out Test_Reporter_W_Barrier;
      Node_Tag           :        Tag;
      Previous_Child_Tag :        Tag;
      E                  :        Exception_Occurrence);

   overriding
   procedure Report_Unexpected_Node_Cond_Check_Error
     (Obj      : in out Test_Reporter_W_Barrier;
      Node_Tag :        Tag;
      E        :        Exception_Occurrence);

   overriding
   procedure Report_Unexpected_Node_Run_Error
     (Obj      : in out Test_Reporter_W_Barrier;
      Node_Tag :        Tag;
      E        :        Exception_Occurrence);

   overriding
   procedure Report_Node_Cond_Check_Start
     (Obj      : in out Test_Reporter_W_Barrier;
      Node_Tag :        Tag);

   overriding
   procedure Report_Passed_Node_Cond_Check
     (Obj      : in out Test_Reporter_W_Barrier;
      Node_Tag :        Tag);

   overriding
   procedure Report_Failed_Node_Cond_Check
     (Obj      : in out Test_Reporter_W_Barrier;
      Node_Tag :        Tag);

   overriding
   procedure Report_Passed_Node_Cond_Assert
     (Obj      : in out Test_Reporter_W_Barrier;
      Node_Tag :        Tag);

   overriding
   procedure Report_Failed_Node_Cond_Assert
     (Obj      : in out Test_Reporter_W_Barrier;
      Node_Tag :        Tag);

   overriding
   procedure Report_Node_Run_Start (Obj      : in out Test_Reporter_W_Barrier;
                                    Node_Tag :        Tag);

   overriding
   procedure Report_Test_Routine_Start
     (Obj      : in out Test_Reporter_W_Barrier;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count);

   overriding
   procedure Report_Test_Routines_Cancellation
     (Obj             : in out Test_Reporter_W_Barrier;
      Node_Tag        :        Tag;
      First_K, Last_K :        Test_Node_Class.Test_Routine_Count);

   overriding
   procedure Report_Failed_Test_Routine_Access
     (Obj      : in out Test_Reporter_W_Barrier;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count;
      E        :        Exception_Occurrence);

   overriding
   procedure Report_Failed_Test_Routine_Setup
     (Obj      : in out Test_Reporter_W_Barrier;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count;
      E        :        Exception_Occurrence);

   overriding
   procedure Report_Passed_Test_Assert
     (Obj              : in out Test_Reporter_W_Barrier;
      Node_Tag         :        Tag;
      K                :        Test_Node_Class.Test_Routine_Count;
      Assert_Num_Avail :        Boolean;
      Assert_Num       :        Test_Node_Class.Test_Assert_Count);

   overriding
   procedure Report_Failed_Test_Assert
     (Obj              : in out Test_Reporter_W_Barrier;
      Node_Tag         :        Tag;
      K                :        Test_Node_Class.Test_Routine_Count;
      Assert_Num_Avail :        Boolean;
      Assert_Num       :        Test_Node_Class.Test_Assert_Count;
      E                :        Exception_Occurrence);

   overriding
   procedure Report_Unexpected_Routine_Exception
     (Obj      : in out Test_Reporter_W_Barrier;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count;
      E        :        Exception_Occurrence);

   overriding
   procedure Report_Passed_Test_Routine
     (Obj      : in out Test_Reporter_W_Barrier;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count);

   overriding
   procedure Report_Failed_Test_Routine
     (Obj      : in out Test_Reporter_W_Barrier;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count);

   overriding
   procedure Report_Passed_Node_Run (Obj      : in out Test_Reporter_W_Barrier;
                                     Node_Tag :        Tag);

   overriding
   procedure Report_Failed_Node_Run (Obj      : in out Test_Reporter_W_Barrier;
                                     Node_Tag :        Tag);

   ----------------------------------------------------------------------------

private

   type Test_Reporter_W_Barrier is limited new Test_Reporter_Stub with record
      C_D_T           : Controlled_Debug_Tracer (0) := Create_N ("");
      Barrier         : Test_Node_Barrier_Access;
      Char_Name_Image : Char_Name_Image_Func;
      Tag_To_Char     : Tag_To_Char_Func;
   end record;

   not overriding
   function Arriv_To_Cross_Message (Obj            : Test_Reporter_W_Barrier;
                                    Operation_Name : String;
                                    Node_Tag       : Tag) return String;

end Apsepp_Test_Node_Barrier;
