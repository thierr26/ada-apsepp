-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Assertions,
     Ada.Characters.Handling,
     Ada.Strings.Fixed;

package body Apsepp.Test_Node_Class.Testing is

   use Ada.Characters.Handling;

   ----------------------------------------------------------------------------

   function Char_Name_Trace (Char : ISO_646) return String
     is (" (" & To_Upper (Char) & (if Is_Lower (Char) then
                                      "_R"
                                   else
                                      "") & "): ");

   ----------------------------------------------------------------------------

   function To_Array return Routine_State_Array is

      A : constant Tag_Routine_State_Array
        := Routine_State_Map_Handler.To_Array;

      Ret : Routine_State_Array (Event_Index (A'First)
                                   ..
                                 Event_Count (A'Last));

   begin

      for K in Ret'Range loop

         declare

            Elem : constant Flattened_Routine_State
              := To_Flattened_Routine_State (A(Index_Type (K)));

         begin

            if K = Ret'First then
               Ret(K) := Elem;
            else
               Insert_Incr (Ret(Ret'First + 1 .. K), K, Elem);
            end if;

         end;

      end loop;

      return Ret;

   end To_Array;

   ----------------------------------------------------------------------------

   protected body Test_Node_Barrier is

      -----------------------------------------------------

      procedure Set_Param (Param : not null Test_Node_Barrier_Param_Access) is

      begin

         P := Param;

      end Set_Param;

      -----------------------------------------------------

      function Permanently_Opened return Boolean
        is (case Permanent_Opening_Cause is
               when None                             => False,
               when Saturation | Overflow | Time_Out => True);

      -----------------------------------------------------

      function Cross_Condition (Char : ISO_646) return Boolean is

         use Prot_Event_Count;

         -- TODO: Write function Inc in Generic_Prot_Integer. <2019-06-13>
         Crossing_Count_Su : constant O_P_I_Type := Crossing_Count
                                                      +
                                                    Create (1);
         K                 : constant Event_Index := Val (Crossing_Count_Su);

      begin

         return Permanently_Opened
                  or else
                Sat (Crossing_Count_Su)
                  or else
                K > P.Expected_Routine_State'Last
                  or else
                P.Tag_To_Char (P.Expected_Routine_State(K).T) = Char;

      end Cross_Condition;

      -----------------------------------------------------

      entry Cross(for Char in ISO_646) when Cross_Condition (Char) is

         use Prot_Event_Count;

         Entity_Name : constant String
           := "Apsepp.Test_Node_Class.Testing.Test_Node_Barrier.Cross";
         C_D_T : constant Controlled_Debug_Tracer := Create_N (Entity_Name);

         Failed_Val : Boolean := False;

         function Msg_Pref return String is
            C_C_Str : String := Event_Count'Image (Val (Crossing_Count));
         begin
            C_C_Str(1) := '#';
            return "Crossing " & C_C_Str & Char_Name_Trace (Char);
         end Msg_Pref;

         procedure Put_Array (A : Routine_State_Array) is
            First_Done : Boolean := False;
         begin
            for E of A loop
               if not First_Done then
                  First_Done := True;
                  C_D_T.Trace (Msg_Pref
                    & "-----------------------------------------------------");
               end if;
               C_D_T.Trace
                 (Test_Routine_Count'Image (E.Routine_I)
                  & " " & Test_Assert_Count'Image (E.Assert_C)
                  & " " & Test_Outcome'Image (E.Assert_O)
                  & " " & Total_Expanded_Name (E.T));
            end loop;
            if First_Done then
               C_D_T.Trace (Msg_Pref
                 & "-----------------------------------------------------");
            end if;
         end Put_Array;

         procedure Validate is
            Is_Runner  : constant Boolean            := Is_Lower (Char);
            K          : constant Event_Index        := Val (Crossing_Count);
            Exp        : constant Flattened_Routine_State
              := P.Expected_Routine_State(K);
            Exp_R_I    : constant Test_Routine_Count := Exp.Routine_I;
            Exp_A_C    : constant Test_Assert_Count  := Exp.Assert_C;
            Exp_A_O    : constant Test_Outcome       := Exp.Assert_O;
         begin
            if Is_Runner then
               -- Just check that P.Expected_Routine_State contains
               -- conventional values (0 / Passed).
               -- TODO: Refactor (use generics). <2019-06-30>
               Ada.Assertions.Assert (Exp_R_I = 0,
                 Msg_Pref & "Exp_R_I =" & Test_Routine_Count'Image (Exp_R_I)
                 & ", 0 expected");
               Ada.Assertions.Assert (Exp_A_C = 0,
                 Msg_Pref & "Exp_A_C =" & Test_Assert_Count'Image (Exp_A_C)
                 & ", 0 expected");
               Ada.Assertions.Assert (Exp_A_O = Passed,
                 Msg_Pref & "Exp_A_O = " & Test_Outcome'Image (Exp_A_O)
                 & ", PASSED expected");
            else
               -- Do a real validation.
               declare
                  A   : Routine_State_Array     := To_Array;
                  K_T : Event_Index             := A'First;
                  K_F : Boolean                 := False;
                  T : constant Tag := P.Char_To_Tag (Char);
                  function Event_Index_Image (K : Event_Index) return String is
                     Img : constant String := Event_Index'Image (K);
                  begin
                     return Img(Img'First + 1 .. Img'Last);
                  end Event_Index_Image;
                  -- TODO: Refactor (use generics). <2019-06-30>
                  function A_K_R_I (K : Event_Count) return Test_Routine_Count
                    is (if K in A'Range then
                           A(K).Routine_I
                        else
                           0);
                  function A_K_A_C (K : Event_Count) return Test_Assert_Count
                    is (if K in A'Range then
                           A(K).Assert_C
                        else
                           0);
                  function A_K_A_O (K : Event_Count) return Test_Outcome
                    is (if K in A'Range then
                           A(K).Assert_O
                        else
                           Passed);
               begin
                  Put_Array (A);

                  -- Validate the "Routine_State_Map_Handler in initial state"
                  -- case.
                  -- TODO: Refactor (use generics). <2019-06-30>
                  Ada.Assertions.Assert (A'Length > 0
                                           or else
                                         Exp_R_I = 0,
                    Msg_Pref & "A'Length =" & Integer'Image (A'Length)
                    & ", Exp_R_I should be 0");
                  Ada.Assertions.Assert (A'Length > 0
                                           or else
                                         Exp_A_C = 0,
                    Msg_Pref & "A'Length =" & Integer'Image (A'Length)
                    & ", Exp_A_C should be 0");
                  Ada.Assertions.Assert (A'Length > 0
                                           or else
                                         Exp_A_O = Passed,
                    Msg_Pref & "A'Length =" & Integer'Image (A'Length)
                    & ", Exp_A_O should be PASSED");

                  -- TODO: Write function Find or Find_First in
                  -- Generic_Array_Operations. <2019-06-29>
                  if A'Length > 0 then
                     while A(K_T).T /= T and then K_T < A'Last loop
                        K_T := K_T + 1;
                     end loop;
                  end if;

                  -- Validate the Routine_State_Map_Handler private component S
                  -- (retrieved in A at index K_T) in the
                  -- "Routine_State_Map_Handler no more in initial state" case.
                  Ada.Assertions.Assert ((A'Length = 0 or else A(K_T).T /= T)
                                           or else
                                         A_K_R_I (K_T) = Exp_R_I,
                    Msg_Pref & "A'Length =" & Integer'Image (A'Length)
                    & "," & Test_Routine_Count'Image (A_K_R_I (K_T))
                    & " (A_K_R_I (" & Event_Index_Image (K_T) & ")) /="
                    & Test_Routine_Count'Image (Exp_R_I) & " (Exp_R_I)");
                  Ada.Assertions.Assert ((A'Length = 0 or else A(K_T).T /= T)
                                           or else
                                         A_K_A_C (K_T) = Exp_A_C,
                    Msg_Pref & "A'Length =" & Integer'Image (A'Length)
                    & "," & Test_Assert_Count'Image (A_K_A_C (K_T))
                    & " (A_K_A_C (" & Event_Index_Image (K_T) & ")) /="
                    & Test_Assert_Count'Image (Exp_A_C) & " (Exp_A_C)");
                  Ada.Assertions.Assert ((A'Length = 0 or else A(K_T).T /= T)
                                           or else
                                         A_K_A_O (K_T) = Exp_A_O,
                    Msg_Pref & "A'Length =" & Integer'Image (A'Length)
                    & ", " & Test_Outcome'Image (A_K_A_O (K_T))
                    & " (A_K_A_O (" & Event_Index_Image (K_T) & ")) /= "
                    & Test_Outcome'Image (Exp_A_O) & " (Exp_A_O)");
               end;
            end if;
         end Validate;

      begin

         Latest_Crossing_Date := Clock;
         Inc (Crossing_Count);

         if not Permanently_Opened then

            if Sat (Crossing_Count) then
               Permanent_Opening_Cause
                 := Saturation; -- Causes Permanently_Opened to be True from
                                -- now on.
                 C_D_T.Trace ("Test node barrier saturated");
            elsif Val (Crossing_Count) > P.Expected_Routine_State'Last then
               Permanent_Opening_Cause := Overflow; -- Ditto.
                 C_D_T.Trace ("Test node barrier overflowed");
            end if;

         end if;

         if not Permanently_Opened then
            begin
               Validate;
            exception
               when E : others => -- Probably Ada.Assertions.Assertion_Error.
                  declare
                     Msg       : constant String   := Exception_Message (E);
                     Colon_Idx : constant Positive
                       := Ada.Strings.Fixed.Index (Msg, ":");
                  begin
                     C_D_T.Trace (Msg_Pref & Msg(Colon_Idx + 2
                                                   ..
                                                 Msg'Last));
                  end;
                  Failed_Validation_Flag := True;
                  Failed_Val             := True;
            end;
         end if;
         if not Failed_Val then
            C_D_T.Trace (Msg_Pref(Msg_Pref'First .. Msg_Pref'Last - 2)
                         & (if Permanently_Opened then
                               " (Permanently opened)"
                            else
                               ""));
         end if;

      end Cross;

      -----------------------------------------------------

      procedure Refresh is

         use Prot_Event_Count;

         Entity_Name : constant String
           := "Apsepp.Test_Node_Class.Testing.Test_Node_Barrier.Refresh";
         C_D_T : constant Controlled_Debug_Tracer := Create_N (Entity_Name);

      begin

         if not Permanently_Opened
              and then
            Clock - Latest_Crossing_Date >= P.Refresh_Delay then

            Permanent_Opening_Cause    := Time_Out;
            Crossing_Count_On_Time_Out := Val (Crossing_Count);
            C_D_T.Trace ("Test node barrier timed out");

         end if;

      end Refresh;

      -----------------------------------------------------

      function Get_Param return not null Test_Node_Barrier_Param_Access
        is (P);

      -----------------------------------------------------

      function Cross_Count return Event_Count
        is (Prot_Event_Count.Val (Crossing_Count));

      -----------------------------------------------------

      function Timed_Out return Boolean
        is (case Permanent_Opening_Cause is
               when Time_Out                     => True,
               when None | Saturation | Overflow => False);

      -----------------------------------------------------

      function Cross_Count_On_Time_Out return Event_Count
        is (Crossing_Count_On_Time_Out);

      -----------------------------------------------------

      function Saturated return Boolean
        is (case Permanent_Opening_Cause is
               when Saturation                 => True,
               when None | Time_Out | Overflow => False);

      -----------------------------------------------------

      function Overflowed return Boolean
        is (case Permanent_Opening_Cause is
               when Overflow                     => True,
               when None | Time_Out | Saturation => False);

      -----------------------------------------------------

      function Completed return Boolean
        is (Cross_Count = P.Expected_Routine_State'Length);

      -----------------------------------------------------

      function Failed_Validation return Boolean
        is (Failed_Validation_Flag);

      -----------------------------------------------------

   end Test_Node_Barrier;

   ----------------------------------------------------------------------------

   task body Test_Node_Barrier_Stimulus_Task is

      B             : Test_Node_Barrier_Access;
      Refresh_Delay : Day_Duration
        := Default_Test_Node_Barrier_Refresh_Delay;

   begin

      loop
         -- Infinite loop. The task user is responsible for aborting the task.

         select
            accept Set_Barrier (Barrier : not null Test_Node_Barrier_Access) do
               B             := Barrier;
               Refresh_Delay := B.Get_Param.Refresh_Delay;
            end Set_Barrier;
         or
            delay Refresh_Delay;
            if B /= null then
               B.Refresh;
            end if;
         end select;

      end loop;

   end Test_Node_Barrier_Stimulus_Task;

   ----------------------------------------------------------------------------

   task body Test_Runner_Task is

      type Uninitialized_Test_Node_Access
        is access all Test_Node_Interfa'Class;

      Unused_Outcome : Test_Outcome;
      Ex             : Exception_Occurrence_Access;
      R              : Uninitialized_Test_Node_Access;

   begin

      accept Set_Runner (Runner : Test_Node_Access) do
         R := Uninitialized_Test_Node_Access (Runner);
      end Set_Runner;

      begin
         R.Run (Unused_Outcome);
      exception
         when E : others => Ex := Save_Occurrence (E);
      end;

      accept Get_E (E : out Exception_Occurrence_Access) do
         E := Ex;
      end Get_E;

   end Test_Runner_Task;

   ----------------------------------------------------------------------------

   not overriding
   procedure Set_Barrier
     (Obj     : in out Test_Reporter_Test_Node_Barrier;
      Barrier :        not null Test_Node_Barrier_Access) is

   begin

      Obj.B := Barrier;

   end Set_Barrier;

      -----------------------------------------------------

   not overriding
   function Arriv_To_Cross_Message
     (Obj            : Test_Reporter_Test_Node_Barrier;
      Operation_Name : String;
      Node_Tag       : Tag) return String
     is ("Apsepp.Test_Node_Class.Testing."
         & Operation_Name
         & Char_Name_Trace (Obj.B.Get_Param.Tag_To_Char (Node_Tag))
         & "Arriving to test node barrier");

      -----------------------------------------------------

   overriding
   procedure Report_Failed_Child_Test_Node_Access
     (Obj                : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag           :        Tag;
      First_Child        :        Boolean;
      Previous_Child_Tag :        Tag;
      E                  :        Exception_Occurrence) is

      pragma Unreferenced (First_Child, Previous_Child_Tag, E);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_Test_Node_Barrier'Class (Obj).Arriv_To_Cross_Message
           ("Report_Failed_Child_Test_Node_Access", Node_Tag));
      Obj.B.Cross (Obj.B.Get_Param.Tag_To_Char (Node_Tag));

   end Report_Failed_Child_Test_Node_Access;

      -----------------------------------------------------

   overriding
   procedure Report_Unexpected_Node_Cond_Check_Error
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag;
      E        :        Exception_Occurrence) is

      pragma Unreferenced (E);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_Test_Node_Barrier'Class (Obj).Arriv_To_Cross_Message
           ("Report_Unexpected_Node_Cond_Check_Error", Node_Tag));
      Obj.B.Cross (Obj.B.Get_Param.Tag_To_Char (Node_Tag));

   end Report_Unexpected_Node_Cond_Check_Error;

      -----------------------------------------------------

   overriding
   procedure Report_Unexpected_Node_Run_Error
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag;
      E        :        Exception_Occurrence) is

      pragma Unreferenced (E);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_Test_Node_Barrier'Class (Obj).Arriv_To_Cross_Message
           ("Report_Unexpected_Node_Run_Error", Node_Tag));
      Obj.B.Cross (Obj.B.Get_Param.Tag_To_Char (Node_Tag));

   end Report_Unexpected_Node_Run_Error;

      -----------------------------------------------------

   overriding
   procedure Report_Node_Cond_Check_Start
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag) is

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_Test_Node_Barrier'Class (Obj).Arriv_To_Cross_Message
           ("Report_Node_Cond_Check_Start", Node_Tag));
      Obj.B.Cross (Obj.B.Get_Param.Tag_To_Char (Node_Tag));

   end Report_Node_Cond_Check_Start;

      -----------------------------------------------------

   overriding
   procedure Report_Passed_Node_Cond_Check
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag) is

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_Test_Node_Barrier'Class (Obj).Arriv_To_Cross_Message
           ("Report_Passed_Node_Cond_Check", Node_Tag));
      Obj.B.Cross (Obj.B.Get_Param.Tag_To_Char (Node_Tag));

   end Report_Passed_Node_Cond_Check;

      -----------------------------------------------------

   overriding
   procedure Report_Failed_Node_Cond_Check
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag) is

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_Test_Node_Barrier'Class (Obj).Arriv_To_Cross_Message
           ("Report_Failed_Node_Cond_Check", Node_Tag));
      Obj.B.Cross (Obj.B.Get_Param.Tag_To_Char (Node_Tag));

   end Report_Failed_Node_Cond_Check;

      -----------------------------------------------------

   overriding
   procedure Report_Passed_Node_Cond_Assert
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag) is

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_Test_Node_Barrier'Class (Obj).Arriv_To_Cross_Message
           ("Report_Passed_Node_Cond_Assert", Node_Tag));
      Obj.B.Cross (Obj.B.Get_Param.Tag_To_Char (Node_Tag));

   end Report_Passed_Node_Cond_Assert;

      -----------------------------------------------------

   overriding
   procedure Report_Failed_Node_Cond_Assert
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag) is

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_Test_Node_Barrier'Class (Obj).Arriv_To_Cross_Message
           ("Report_Failed_Node_Cond_Assert", Node_Tag));
      Obj.B.Cross (Obj.B.Get_Param.Tag_To_Char (Node_Tag));

   end Report_Failed_Node_Cond_Assert;

      -----------------------------------------------------

   overriding
   procedure Report_Node_Run_Start
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag) is

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_Test_Node_Barrier'Class (Obj).Arriv_To_Cross_Message
           ("Report_Node_Run_Start", Node_Tag));
      Obj.B.Cross (Obj.B.Get_Param.Tag_To_Char (Node_Tag));

   end Report_Node_Run_Start;

      -----------------------------------------------------

   overriding
   procedure Report_Test_Routine_Start
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count) is

      pragma Unreferenced (K);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_Test_Node_Barrier'Class (Obj).Arriv_To_Cross_Message
           ("Report_Test_Routine_Start", Node_Tag));
      Obj.B.Cross (Obj.B.Get_Param.Tag_To_Char (Node_Tag));

   end Report_Test_Routine_Start;

      -----------------------------------------------------

   overriding
   procedure Report_Test_Routines_Cancellation
     (Obj             : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag        :        Tag;
      First_K, Last_K :        Test_Node_Class.Test_Routine_Count) is

      pragma Unreferenced (First_K, Last_K);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_Test_Node_Barrier'Class (Obj).Arriv_To_Cross_Message
           ("Report_Test_Routines_Cancellation", Node_Tag));
      Obj.B.Cross (Obj.B.Get_Param.Tag_To_Char (Node_Tag));

   end Report_Test_Routines_Cancellation;

      -----------------------------------------------------

   overriding
   procedure Report_Failed_Test_Routine_Access
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count;
      E        :        Exception_Occurrence) is

      pragma Unreferenced (K, E);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_Test_Node_Barrier'Class (Obj).Arriv_To_Cross_Message
           ("Report_Failed_Test_Routine_Access", Node_Tag));
      Obj.B.Cross (Obj.B.Get_Param.Tag_To_Char (Node_Tag));

   end Report_Failed_Test_Routine_Access;

      -----------------------------------------------------

   overriding
   procedure Report_Failed_Test_Routine_Setup
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count;
      E        :        Exception_Occurrence) is

      pragma Unreferenced (K, E);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_Test_Node_Barrier'Class (Obj).Arriv_To_Cross_Message
           ("Report_Failed_Test_Routine_Setup", Node_Tag));
      Obj.B.Cross (Obj.B.Get_Param.Tag_To_Char (Node_Tag));

   end Report_Failed_Test_Routine_Setup;

      -----------------------------------------------------

   overriding
   procedure Report_Passed_Test_Assert
     (Obj              : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag         :        Tag;
      K                :        Test_Node_Class.Test_Routine_Count;
      Assert_Num_Avail :        Boolean;
      Assert_Num       :        Test_Node_Class.Test_Assert_Count) is

      pragma Unreferenced (K, Assert_Num_Avail, Assert_Num);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_Test_Node_Barrier'Class (Obj).Arriv_To_Cross_Message
           ("Report_Passed_Test_Assert", Node_Tag));
      Obj.B.Cross (Obj.B.Get_Param.Tag_To_Char (Node_Tag));

   end Report_Passed_Test_Assert;

      -----------------------------------------------------

   overriding
   procedure Report_Failed_Test_Assert
     (Obj              : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag         :        Tag;
      K                :        Test_Node_Class.Test_Routine_Count;
      Assert_Num_Avail :        Boolean;
      Assert_Num       :        Test_Node_Class.Test_Assert_Count;
      E                :        Exception_Occurrence) is

      pragma Unreferenced (K, Assert_Num_Avail, Assert_Num, E);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_Test_Node_Barrier'Class (Obj).Arriv_To_Cross_Message
           ("Report_Failed_Test_Assert", Node_Tag));
      Obj.B.Cross (Obj.B.Get_Param.Tag_To_Char (Node_Tag));

   end Report_Failed_Test_Assert;

      -----------------------------------------------------

   overriding
   procedure Report_Unexpected_Routine_Exception
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count;
      E        :        Exception_Occurrence) is

      pragma Unreferenced (K, E);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_Test_Node_Barrier'Class (Obj).Arriv_To_Cross_Message
           ("Report_Unexpected_Routine_Exception", Node_Tag));
      Obj.B.Cross (Obj.B.Get_Param.Tag_To_Char (Node_Tag));

   end Report_Unexpected_Routine_Exception;

      -----------------------------------------------------

   overriding
   procedure Report_Passed_Test_Routine
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count) is

      pragma Unreferenced (K);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_Test_Node_Barrier'Class (Obj).Arriv_To_Cross_Message
           ("Report_Passed_Test_Routine", Node_Tag));
      Obj.B.Cross (Obj.B.Get_Param.Tag_To_Char (Node_Tag));

   end Report_Passed_Test_Routine;

      -----------------------------------------------------

   overriding
   procedure Report_Failed_Test_Routine
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count) is

      pragma Unreferenced (K);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_Test_Node_Barrier'Class (Obj).Arriv_To_Cross_Message
           ("Report_Failed_Test_Routine", Node_Tag));
      Obj.B.Cross (Obj.B.Get_Param.Tag_To_Char (Node_Tag));

   end Report_Failed_Test_Routine;

      -----------------------------------------------------

   overriding
   procedure Report_Passed_Node_Run
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag) is

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_Test_Node_Barrier'Class (Obj).Arriv_To_Cross_Message
           ("Report_Passed_Node_Run", Node_Tag));
      Obj.B.Cross (Obj.B.Get_Param.Tag_To_Char (Node_Tag));

   end Report_Passed_Node_Run;

      -----------------------------------------------------

   overriding
   procedure Report_Failed_Node_Run
     (Obj      : in out Test_Reporter_Test_Node_Barrier;
      Node_Tag :        Tag) is

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_Test_Node_Barrier'Class (Obj).Arriv_To_Cross_Message
           ("Report_Failed_Node_Run", Node_Tag));
      Obj.B.Cross (Obj.B.Get_Param.Tag_To_Char (Node_Tag));

   end Report_Failed_Node_Run;

   ----------------------------------------------------------------------------

end Apsepp.Test_Node_Class.Testing;
