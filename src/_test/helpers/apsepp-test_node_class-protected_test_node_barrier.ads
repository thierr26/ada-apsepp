-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Tags;                use Ada.Tags;
with Ada.Calendar;            use Ada.Calendar;
with Apsepp.Characters;       use Apsepp.Characters;
with Apsepp.Test_Event_Class; use Apsepp.Test_Event_Class;

with Apsepp.Generic_Safe_Integer_Operations;

package Apsepp.Test_Node_Class.Protected_Test_Node_Barrier is

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

   type Char_Name_Image_Func
     is access function (Char : ISO_646) return String;

   type Tag_To_Char_Func
     is access function (T : Tag) return ISO_646;

   type Char_To_Tag_Func
     is access function (Char : ISO_646_Upper_Letter) return Tag;

   type Validate_Proc
     is access procedure (Crossing_Count : Positive;
                          Event_Kind     : Test_Event_Kind;
                          Event_Data     : Test_Event_Data;
                          Char           : ISO_646;
                          Char_To_Tag    : Char_To_Tag_Func;
                          Msg_Pref       : String);

   type Test_Node_Barrier_Permanent_Opening_Cause is (None,
                                                      Saturation,
                                                      Overflow,
                                                      Time_Out);

   package Safe_Natural_Operations
     is new Apsepp.Generic_Safe_Integer_Operations (Integer_Type => Natural);

   protected type Test_Node_Barrier is

      procedure Set_Up
        (Char_Name_Image_Function   : not null Char_Name_Image_Func;
         Tag_To_Char_Function       : not null Tag_To_Char_Func;
         Char_To_Tag_Function       : not null Char_To_Tag_Func;
         Validate_Procedure         : not null Validate_Proc;
         Expected_Tags_Array_Access : not null access Tag_Array);

      entry Cross(ISO_646) (Event_Kind : Test_Event_Kind;
                            Event_Data : Test_Event_Data);

      procedure Time_Out;

      function Cross_Call_Count return Natural;

      function Timed_Out return Boolean;

      -- TODOC: Meaningless if 'not Timed_Out'. <2019-06-13>
      function Cross_Call_Count_On_Time_Out return Natural;

      function Saturated return Boolean;

      function Overflowed return Boolean;

      -- TODOC: Meaningless if 'not Overflowed'. <2019-08-16>
      function Cross_Call_Count_On_Overflow return Natural;

      function Completed return Boolean;

      function Failed_Validation return Boolean;

   private

      Expected_Tags_Array : access Tag_Array;

      Crossing_Count : Safe_Natural_Operations.Safe_Integer;

      -- TODOC: Meaningless if 'Permanent_Opening_Cause /= Time_Out'.
      -- <2019-08-08>
      Crossing_Count_On_Time_Out : Natural;

      -- TODOC: Meaningless if 'Permanent_Opening_Cause /= Overflow'.
      -- <2019-08-16>
      Crossing_Count_On_Overflow : Natural;

      Permanent_Opening_Cause : Test_Node_Barrier_Permanent_Opening_Cause;

      Failed_Validation_Flag : Boolean;

      Char_Name_Image : Char_Name_Image_Func;

      Tag_To_Char : Tag_To_Char_Func;

      Char_To_Tag : Char_To_Tag_Func;

      Validate : Validate_Proc;

   end Test_Node_Barrier;

   task type Test_Node_Barrier_Time_Out_Trigger
     (Barrier_Access : not null access Test_Node_Barrier) is

      entry Set_Up (Period : Day_Duration := 0.7); -- 0.7 seconds.

   end Test_Node_Barrier_Time_Out_Trigger;

end Apsepp.Test_Node_Class.Protected_Test_Node_Barrier;
