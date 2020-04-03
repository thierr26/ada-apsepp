-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Exceptions,
     Apsepp.Finalized_Debug_Tracer.Generic_Instantiator,
     Apsepp.Generic_Discrete_Operations,
     Apsepp.Generic_Logical_Array;

package body Apsepp.Test_Node_Class.Protected_Test_Node_Barrier is

   ----------------------------------------------------------------------------

   -- TODO: Create a generic package for hierarchical name manipulation.
   -- <2020-03-07>
   function Prefixed_Scope (Prefix, Scope_Name : String) return String
     is (Prefix & "." & Scope_Name);

   ----------------------------------------------------------------------------

   function Full_Local_Scope_Name (Local_Scope_Name : String) return String
     is (Prefixed_Scope ("Apsepp.Test_Node_Class.Protected_Test_Node_Barrier",
                          Local_Scope_Name));

   ----------------------------------------------------------------------------

   task body Test_Node_Barrier_Time_Out_Trigger is

      Loop_Period      : Day_Duration;
      Cross_Call_Count : Natural;

   begin

      accept Set_Up (Period : Day_Duration := 0.7) do
         Loop_Period      := Period;
         Cross_Call_Count := Barrier_Access.Cross_Call_Count;
      end Set_Up;

      while not Barrier_Access.Completed loop

         delay Loop_Period;

         if Barrier_Access.Cross_Call_Count = Cross_Call_Count then
            -- Crossing count has not changed since last iteration. The test
            -- node barrier is considered blocked.

            -- Time out the test node barrier. (This opens the barrier for ever
            -- and thus unblock it.)
            Barrier_Access.Time_Out;

            -- The task is now useless and can be terminated.
            exit;

         end if;

         Cross_Call_Count := Barrier_Access.Cross_Call_Count;

      end loop;

   end Test_Node_Barrier_Time_Out_Trigger;

   ----------------------------------------------------------------------------

   subtype Post_Cross_Cause
     is Test_Node_Barrier_Permanent_Opening_Cause range Saturation .. Overflow;

   protected body Test_Node_Barrier is

      -----------------------------------------------------

      function Protected_Subprogram_Name
        (Subprogram_Name : String) return String
        is (Prefixed_Scope ("Test_Node_Barrier", Subprogram_Name));

      -----------------------------------------------------

      function Permanently_Opened return Boolean
        is (case Permanent_Opening_Cause is
               when None       => False,
               when Saturation
                    | Overflow
                    | Time_Out => True);

      -----------------------------------------------------

      procedure Set_Up
        (Char_Name_Image_Function   : not null Char_Name_Image_Func;
         Tag_To_Char_Function       : not null Tag_To_Char_Func;
         Char_To_Tag_Function       : not null Char_To_Tag_Func;
         Validate_Procedure         : not null Validate_Proc;
         Expected_Tags_Array_Access : not null access constant Tag_Array) is

      begin

         Char_Name_Image     := Char_Name_Image_Function;
         Tag_To_Char         := Tag_To_Char_Function;
         Char_To_Tag         := Char_To_Tag_Function;
         Validate            := Validate_Procedure;
         Expected_Tags_Array := Expected_Tags_Array_Access;

         Crossing_Count          := Safe_Natural_Operations.Create (0);
         Permanent_Opening_Cause := None;
         Failed_Validation_Flag  := False;

      end Set_Up;

      -----------------------------------------------------

      function Cross_Condition
        (Char                      : ISO_646;
         Pre_Cross                 : Boolean          := True;
         Post_Cross_Analyzed_Cause : Post_Cross_Cause := Saturation)
         return Boolean is

         Sat_Cause, Ov_Cause, Ret : Boolean;

      begin

         case Post_Cross_Analyzed_Cause is
            when Saturation =>
               Sat_Cause := True;
               Ov_Cause  := Pre_Cross;
            when Overflow   =>
               Sat_Cause := Pre_Cross;
               Ov_Cause  := True;
         end case;

         declare

            package Positive_Operations
              is new Generic_Discrete_Operations (Discrete_Type => Natural,
                                                  Diff_Type     => Integer);

            use Safe_Natural_Operations;

            N : constant Positive := Expected_Tags_Array'Length;

            -- Function return value is a logical OR of four conditions:
            --
            -- 1. 'Permanently_Opened'.
            --
            -- 2. Saturated crossing count (actually crossing_count + 1 in the
            -- 'Pre_Cross' case because crossing count has not been incremented
            -- yet).
            --
            -- 3. Crossing count exceeds 'N' (actually crossing_count + 1 in
            -- the 'Pre_Cross' case because crossing count has not been
            -- incremented yet).
            --
            -- 4. 'Char' value is the one awaited by the barrier.
            --
            -- In the 'not Pre_Cross' case, only one of those condition is
            -- taken into account: #2 if 'Sat_Cause' and #3 if 'Ov_Cause'.

            -- Condition 1.
            Ret_1 : constant Boolean := Permanently_Opened
                                          and then
                                        Pre_Cross;

            -- Condition 2.
            C_C : constant Safe_Integer := Inc (Crossing_Count,
                                                (if Pre_Cross then
                                                    1
                                                 else
                                                    0));
            Ret_2 : constant Boolean := Sat_Cause and then Sat (C_C);

            -- Condition 3.
            Val_C_C : constant Natural := Val (C_C);
            Ret_3   : constant Boolean := Ov_Cause and then Val_C_C > N;

            Index_C_C : constant Natural
              := Positive_Operations.Val (Val_C_C, Expected_Tags_Array'First);

            package Logical_Array
              is new Generic_Logical_Array (Index_Type => Positive);

            use Logical_Array;

         begin

            Ret := Some_True ((Ret_1,
                               Ret_2,
                               Ret_3,
                               Pre_Cross
                                 and then
                               Tag_To_Char
                                 (Expected_Tags_Array (Index_C_C)) = Char));

         end;

         return Ret;

      end Cross_Condition;

      -----------------------------------------------------

      entry Cross(for Char in ISO_646)
        (Event_Kind : Test_Event_Kind;
         Event_Data : Test_Event_Data) when Cross_Condition (Char) is

         use Ada.Exceptions,
             Safe_Natural_Operations,
             Finalized_Debug_Tracer;

         -- PORT: Defining 'C_D_T' as an instance of generic package
         -- 'Finalized_Debug_Tracer.Generic_Instantiator' causes a compiler
         -- crash. <2020-03-08>
         Scope_Name : aliased constant String
           := Full_Local_Scope_Name (Protected_Subprogram_Name ("Cross"));
         C_D_T : Finalized_Debug_Tracer.Controlled_Debug_Tracer
           (Scope_Name_Access => Scope_Name'Access,
            Kind               => N);
         -- package C_D_T
         --   is new Finalized_Debug_Tracer.Generic_Instantiator
         --   (Scope_Name =>
         --      Full_Local_Scope_Name (Protected_Subprogram_Name ("Cross")),
         --    Kind        => N);

         function Msg_Pref return String is
            C_C_Str : String := Natural'Image (Val (Crossing_Count));
         begin
            C_C_Str(1) := '#';
            return "Crossing " & C_C_Str & Char_Name_Image (Char);
         end Msg_Pref;

      begin

         -- Beware of the early return (see the return statement).

         Inc (Crossing_Count);

         if not Permanently_Opened then

            if Cross_Condition (Char                      => Char,
                                Pre_Cross                 => False,
                                Post_Cross_Analyzed_Cause => Saturation) then
               Permanent_Opening_Cause
                 := Saturation; -- Causes 'Permanently_Opened' to be True from
                                -- from now on.
               C_D_T.Trace ("Test node barrier saturated");
            elsif Cross_Condition (Char                      => Char,
                                   Pre_Cross                 => False,
                                   Post_Cross_Analyzed_Cause => Overflow) then
               Permanent_Opening_Cause    := Overflow; -- Ditto.
               Crossing_Count_On_Overflow := Val (Crossing_Count) - 1;
               C_D_T.Trace ("Test node barrier overflowed");
            end if;

            if Permanently_Opened then
               C_D_T.Trace (Msg_Pref(Msg_Pref'First .. Msg_Pref'Last - 2)
                            & " (Permanently opened)");
               return; -- Early return!
            end if;

         end if;

         Validate (Val (Crossing_Count),
                   Event_Kind,
                   Event_Data,
                   Char,
                   Char_To_Tag,
                   Msg_Pref);

         C_D_T.Trace (Msg_Pref(Msg_Pref'First .. Msg_Pref'Last - 2));

      exception

         when E : others => -- Should be 'Ada.Assertions.Assertion_Error'
                            -- raised by 'Validate'.
            C_D_T.Trace (Msg_Pref & Exception_Message (E));
            Failed_Validation_Flag := True;

      end Cross;

      -----------------------------------------------------

      procedure Time_Out is

         use Safe_Natural_Operations,
             Finalized_Debug_Tracer;

         package C_D_T
           is new Finalized_Debug_Tracer.Generic_Instantiator
           (Scope_Name =>
              Full_Local_Scope_Name (Protected_Subprogram_Name ("Time_Out")),
            Kind        => N);

      begin

         if not Permanently_Opened then

            Permanent_Opening_Cause
              := Time_Out; -- Causes 'Permanently_Opened' to be True from now
                           -- on.
            Crossing_Count_On_Time_Out := Val (Crossing_Count);
            C_D_T.Trace ("Test node barrier timed out");

         end if;

      end Time_Out;

      -----------------------------------------------------

      function Cross_Call_Count return Natural
        is (Safe_Natural_Operations.Val (Crossing_Count));

      -----------------------------------------------------

      function Timed_Out return Boolean
        is (case Permanent_Opening_Cause is
               when Time_Out     => True,
               when None
                    | Saturation
                    | Overflow   => False);

      -----------------------------------------------------

      function Cross_Call_Count_On_Time_Out return Natural
        is (Crossing_Count_On_Time_Out);

      -----------------------------------------------------

      function Saturated return Boolean
        is (case Permanent_Opening_Cause is
               when Saturation => True,
               when None
                    | Time_Out
                    | Overflow => False);

      -----------------------------------------------------

      function Overflowed return Boolean
        is (case Permanent_Opening_Cause is
               when Overflow     => True,
               when None
                    | Time_Out
                    | Saturation => False);

      -----------------------------------------------------

      function Cross_Call_Count_On_Overflow return Natural
        is (Crossing_Count_On_Overflow);

      -----------------------------------------------------

      function Completed return Boolean
        is (Cross_Call_Count = Expected_Tags_Array'Length);

      -----------------------------------------------------

      function Failed_Validation return Boolean
        is (Failed_Validation_Flag);

      -----------------------------------------------------

   end Test_Node_Barrier;

   ----------------------------------------------------------------------------

end Apsepp.Test_Node_Class.Protected_Test_Node_Barrier;
