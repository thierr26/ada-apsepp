-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Unchecked_Deallocation;

package body Apsepp_Test_Node_Barrier is

   ----------------------------------------------------------------------------

   function Test_Reporter_Proc_Name
     (Event_Kind : Test_Event_Kind) return String is

      Event_Kind_Str : String := Test_Event_Kind'Image (Event_Kind);

   begin

      for K in Event_Kind_Str'First + 1 .. Event_Kind_Str'Last loop
         if Event_Kind_Str(K - 1) /= '_' then
            Event_Kind_Str(K) := To_Lower (Event_Kind_Str(K));
         end if;
      end loop;

      return "Report_" & Event_Kind_Str;

   end;

   ----------------------------------------------------------------------------

   task body Test_Node_Barrier_Monitor is

      B           : access Test_Node_Barrier;
      Period      : Day_Duration;
      Cross_Count : Natural;

   begin

      accept Setup (Barrier           : not null Test_Node_Barrier_Access;
                    Monitoring_Period : Day_Duration := 0.7) do
         B           := Barrier;
         Period      := Monitoring_Period;
         Cross_Count := B.Cross_Count;
      end Setup;

      while not B.Completed loop

         delay Period;

         if B.Cross_Count = Cross_Count then
            B.Time_Out;
         end if;

         Cross_Count := B.Cross_Count;

      end loop;

   end Test_Node_Barrier_Monitor;

   ----------------------------------------------------------------------------

   protected body Test_Node_Barrier is

      -----------------------------------------------------

      function Permanently_Opened return Boolean
        is (case Permanent_Opening_Cause is
               when None            => False,
               when Saturation
                    | Overflow
                    | Time_Out      => True);

      -----------------------------------------------------

      procedure Setup (Ch_I  : not null Char_Name_Image_Func;
                       T_T_C : not null Tag_To_Char_Func;
                       C_T_C : not null Char_To_Tag_Func;
                       V     : not null Validate_Proc;
                       Exp   : not null Tag_Array_Access) is

      begin

         Char_Name_Image := Ch_I;
         Tag_To_Char     := T_T_C;
         Char_To_Tag     := C_T_C;
         Validate        := V;
         Expected_Tag    := Exp;

         Crossing_Count          := Prot_Natural.Create (0);
         Permanent_Opening_Cause := None;
         Failed_Validation_Flag  := False;

      end Setup;

      -----------------------------------------------------

      function Cross_Condition (Char : ISO_646) return Boolean is

         use Prot_Natural;

         -- TODO: Write function Inc in Generic_Prot_Integer. <2019-06-13>
         Crossing_Count_Su : constant O_P_I_Type := Crossing_Count
                                                      +
                                                    Create (1);
         K                 : constant Positive := Val (Crossing_Count_Su);

      begin

         return Permanently_Opened
                  or else
                Sat (Crossing_Count_Su)
                  or else
                K > Expected_Tag'Length
                  or else
                (
                  Tag_To_Char (Expected_Tag (K - 1 + Expected_Tag'First))
                    =
                  Char
                );

      end Cross_Condition;

      -----------------------------------------------------

      entry Cross(for Char in ISO_646)
        (Event_Kind : Test_Event_Kind) when Cross_Condition (Char) is

         use Prot_Natural;

         Entity_Name : constant String
           := "Apsepp_Test_Node_Barrier.Test_Node_Barrier.Cross";
         C_D_T : constant Controlled_Debug_Tracer := Create_N (Entity_Name);

         Failed_Val : Boolean := False;

         function Msg_Pref return String is
            C_C_Str : String := Natural'Image (Val (Crossing_Count));
         begin
            C_C_Str(1) := '#';
            return "Crossing " & C_C_Str & Char_Name_Image (Char);
         end Msg_Pref;

      begin

         Inc (Crossing_Count);

         if not Permanently_Opened then

            if Sat (Crossing_Count) then
               Permanent_Opening_Cause
                 := Saturation; -- Causes Permanently_Opened to be True from
                                -- from now on.
               C_D_T.Trace ("Test node barrier saturated");
            elsif Val (Crossing_Count) > Expected_Tag'Length then
               Permanent_Opening_Cause := Overflow; -- Ditto.
               Crossing_Count_On_Overflow := Val (Crossing_Count) - 1;
               C_D_T.Trace ("Test node barrier overflowed");
            end if;

         end if;

         if not Permanently_Opened then
            begin
               Validate (Val (Crossing_Count),
                         Event_Kind,
                         Char,
                         Char_To_Tag,
                         Msg_Pref);
            exception
               when E : others => -- Probably
                                  -- Ada.Assertions.Assertion_Error.
                  C_D_T.Trace (Msg_Pref & Exception_Message (E));
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

      procedure Time_Out is

         use Prot_Natural;

         Entity_Name : constant String
           := "Apsepp_Test_Node_Barrier.Test_Node_Barrier.Time_Out";
         C_D_T : constant Controlled_Debug_Tracer := Create_N (Entity_Name);

      begin

         if not Permanently_Opened then

            Permanent_Opening_Cause
              := Time_Out; -- Causes Permanently_Opened to be True from now
                           -- on.
            Crossing_Count_On_Time_Out := Val (Crossing_Count);
            C_D_T.Trace ("Test node barrier timed out");

         end if;

      end Time_Out;

      -----------------------------------------------------

      function Cross_Count return Natural
        is (Prot_Natural.Val (Crossing_Count));

      -----------------------------------------------------

      function Timed_Out return Boolean
        is (case Permanent_Opening_Cause is
               when Time_Out          => True,
               when None
                    | Saturation
                    | Overflow        => False);

      -----------------------------------------------------

      function Cross_Count_On_Time_Out return Natural
        is (Crossing_Count_On_Time_Out);

      -----------------------------------------------------

      function Saturated return Boolean
        is (case Permanent_Opening_Cause is
               when Saturation                 => True,
               when None | Time_Out | Overflow => False);

      -----------------------------------------------------

      function Overflowed return Boolean
        is (case Permanent_Opening_Cause is
               when Overflow          => True,
               when None
                    | Time_Out
                    | Saturation      => False);

      -----------------------------------------------------

      function Cross_Count_On_Overflow return Natural
        is (Crossing_Count_On_Overflow);

      -----------------------------------------------------

      function Completed return Boolean
        is (Cross_Count = Expected_Tag'Length);

      -----------------------------------------------------

      function Failed_Validation return Boolean
        is (Failed_Validation_Flag);

      -----------------------------------------------------

   end Test_Node_Barrier;

   ----------------------------------------------------------------------------

   not overriding
   procedure Setup (Obj   : in out   Test_Reporter_W_Barrier;
                    B     : not null Test_Node_Barrier_Access;
                    Ch_I  : not null Char_Name_Image_Func;
                    T_T_C : not null Tag_To_Char_Func) is

   begin

      Obj.Barrier         := B;
      Obj.Char_Name_Image := Ch_I;
      Obj.Tag_To_Char     := T_T_C;

   end Setup;

      -----------------------------------------------------

   not overriding
   function Arriv_To_Cross_Message (Obj            : Test_Reporter_W_Barrier;
                                    Operation_Name : String;
                                    Node_Tag       : Tag) return String
     is ("Apsepp_Test_Node_Barrier."
         & Operation_Name
         & Obj.Char_Name_Image (Obj.Tag_To_Char (Node_Tag))
         & "Arriving to test node barrier");

      -----------------------------------------------------

   overriding
   procedure Report_Failed_Child_Test_Node_Access
     (Obj                : in out Test_Reporter_W_Barrier;
      Node_Tag           :        Tag;
      Previous_Child_Tag :        Tag;
      E                  :        Exception_Occurrence) is

      Event_Kind : constant Test_Event_Kind := Failed_Child_Test_Node_Access;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      pragma Unreferenced (Previous_Child_Tag, E);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_W_Barrier'Class (Obj).Arriv_To_Cross_Message
           (Proc_Name, Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind);

   end Report_Failed_Child_Test_Node_Access;

      -----------------------------------------------------

   overriding
   procedure Report_Unexpected_Node_Cond_Check_Error
     (Obj      : in out Test_Reporter_W_Barrier;
      Node_Tag :        Tag;
      E        :        Exception_Occurrence) is

      Event_Kind : constant Test_Event_Kind
        := Unexpected_Node_Cond_Check_Error;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      pragma Unreferenced (E);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_W_Barrier'Class (Obj).Arriv_To_Cross_Message
           (Proc_Name, Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind);

   end Report_Unexpected_Node_Cond_Check_Error;

      -----------------------------------------------------

   overriding
   procedure Report_Unexpected_Node_Run_Error
     (Obj      : in out Test_Reporter_W_Barrier;
      Node_Tag :        Tag;
      E        :        Exception_Occurrence) is

      Event_Kind : constant Test_Event_Kind := Unexpected_Node_Run_Error;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      pragma Unreferenced (E);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_W_Barrier'Class (Obj).Arriv_To_Cross_Message
           (Proc_Name, Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind);

   end Report_Unexpected_Node_Run_Error;

      -----------------------------------------------------

   overriding
   procedure Report_Node_Cond_Check_Start
     (Obj      : in out Test_Reporter_W_Barrier;
      Node_Tag :        Tag) is

      Event_Kind : constant Test_Event_Kind := Node_Cond_Check_Start;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_W_Barrier'Class (Obj).Arriv_To_Cross_Message
           (Proc_Name, Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind);

   end Report_Node_Cond_Check_Start;

      -----------------------------------------------------

   overriding
   procedure Report_Passed_Node_Cond_Check
     (Obj      : in out Test_Reporter_W_Barrier;
      Node_Tag :        Tag) is

      Event_Kind : constant Test_Event_Kind := Passed_Node_Cond_Check;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_W_Barrier'Class (Obj).Arriv_To_Cross_Message
           (Proc_Name, Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind);

   end Report_Passed_Node_Cond_Check;

      -----------------------------------------------------

   overriding
   procedure Report_Failed_Node_Cond_Check
     (Obj      : in out Test_Reporter_W_Barrier;
      Node_Tag :        Tag) is

      Event_Kind : constant Test_Event_Kind := Failed_Node_Cond_Check;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_W_Barrier'Class (Obj).Arriv_To_Cross_Message
           (Proc_Name, Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind);

   end Report_Failed_Node_Cond_Check;

      -----------------------------------------------------

   overriding
   procedure Report_Passed_Node_Cond_Assert
     (Obj      : in out Test_Reporter_W_Barrier;
      Node_Tag :        Tag) is

      Event_Kind : constant Test_Event_Kind := Passed_Node_Cond_Assert;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_W_Barrier'Class (Obj).Arriv_To_Cross_Message
           (Proc_Name, Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind);

   end Report_Passed_Node_Cond_Assert;

      -----------------------------------------------------

   overriding
   procedure Report_Failed_Node_Cond_Assert
     (Obj      : in out Test_Reporter_W_Barrier;
      Node_Tag :        Tag) is

      Event_Kind : constant Test_Event_Kind := Failed_Node_Cond_Assert;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_W_Barrier'Class (Obj).Arriv_To_Cross_Message
           (Proc_Name, Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind);

   end Report_Failed_Node_Cond_Assert;

      -----------------------------------------------------

   overriding
   procedure Report_Node_Run_Start (Obj      : in out Test_Reporter_W_Barrier;
                                    Node_Tag :        Tag) is

      Event_Kind : constant Test_Event_Kind := Node_Run_Start;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_W_Barrier'Class (Obj).Arriv_To_Cross_Message
           (Proc_Name, Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind);

   end Report_Node_Run_Start;

      -----------------------------------------------------

   overriding
   procedure Report_Test_Routine_Start
     (Obj      : in out Test_Reporter_W_Barrier;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count) is

      Event_Kind : constant Test_Event_Kind := Test_Routine_Start;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      pragma Unreferenced (K);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_W_Barrier'Class (Obj).Arriv_To_Cross_Message
           (Proc_Name, Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind);

   end Report_Test_Routine_Start;

      -----------------------------------------------------

   overriding
   procedure Report_Test_Routines_Cancellation
     (Obj             : in out Test_Reporter_W_Barrier;
      Node_Tag        :        Tag;
      First_K, Last_K :        Test_Node_Class.Test_Routine_Count) is

      Event_Kind : constant Test_Event_Kind := Test_Routines_Cancellation;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      pragma Unreferenced (First_K, Last_K);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_W_Barrier'Class (Obj).Arriv_To_Cross_Message
           (Proc_Name, Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind);

   end Report_Test_Routines_Cancellation;

      -----------------------------------------------------

   overriding
   procedure Report_Failed_Test_Routine_Access
     (Obj      : in out Test_Reporter_W_Barrier;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count;
      E        :        Exception_Occurrence) is

      Event_Kind : constant Test_Event_Kind := Failed_Test_Routine_Access;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      pragma Unreferenced (K, E);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_W_Barrier'Class (Obj).Arriv_To_Cross_Message
           (Proc_Name, Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind);

   end Report_Failed_Test_Routine_Access;

      -----------------------------------------------------

   overriding
   procedure Report_Failed_Test_Routine_Setup
     (Obj      : in out Test_Reporter_W_Barrier;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count;
      E        :        Exception_Occurrence) is

      Event_Kind : constant Test_Event_Kind := Failed_Test_Routine_Setup;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      pragma Unreferenced (K, E);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_W_Barrier'Class (Obj).Arriv_To_Cross_Message
           (Proc_Name, Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind);

   end Report_Failed_Test_Routine_Setup;

      -----------------------------------------------------

   overriding
   procedure Report_Passed_Test_Assert
     (Obj              : in out Test_Reporter_W_Barrier;
      Node_Tag         :        Tag;
      K                :        Test_Node_Class.Test_Routine_Count;
      Assert_Num_Avail :        Boolean;
      Assert_Num       :        Test_Node_Class.Test_Assert_Count) is

      Event_Kind : constant Test_Event_Kind := Passed_Test_Assert;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      pragma Unreferenced (K, Assert_Num_Avail, Assert_Num);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_W_Barrier'Class (Obj).Arriv_To_Cross_Message
           (Proc_Name, Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind);

   end Report_Passed_Test_Assert;

      -----------------------------------------------------

   overriding
   procedure Report_Failed_Test_Assert
     (Obj              : in out Test_Reporter_W_Barrier;
      Node_Tag         :        Tag;
      K                :        Test_Node_Class.Test_Routine_Count;
      Assert_Num_Avail :        Boolean;
      Assert_Num       :        Test_Node_Class.Test_Assert_Count;
      E                :        Exception_Occurrence) is

      Event_Kind : constant Test_Event_Kind := Failed_Test_Assert;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      pragma Unreferenced (K, Assert_Num_Avail, Assert_Num, E);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_W_Barrier'Class (Obj).Arriv_To_Cross_Message
           (Proc_Name, Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind);

   end Report_Failed_Test_Assert;

      -----------------------------------------------------

   overriding
   procedure Report_Unexpected_Routine_Exception
     (Obj      : in out Test_Reporter_W_Barrier;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count;
      E        :        Exception_Occurrence) is

      Event_Kind : constant Test_Event_Kind := Unexpected_Routine_Exception;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      pragma Unreferenced (K, E);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_W_Barrier'Class (Obj).Arriv_To_Cross_Message
           (Proc_Name, Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind);

   end Report_Unexpected_Routine_Exception;

      -----------------------------------------------------

   overriding
   procedure Report_Passed_Test_Routine
     (Obj      : in out Test_Reporter_W_Barrier;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count) is

      Event_Kind : constant Test_Event_Kind := Passed_Test_Routine;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      pragma Unreferenced (K);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_W_Barrier'Class (Obj).Arriv_To_Cross_Message
           (Proc_Name, Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind);

   end Report_Passed_Test_Routine;

      -----------------------------------------------------

   overriding
   procedure Report_Failed_Test_Routine
     (Obj      : in out Test_Reporter_W_Barrier;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count) is

      Event_Kind : constant Test_Event_Kind := Failed_Test_Routine;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

      pragma Unreferenced (K);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_W_Barrier'Class (Obj).Arriv_To_Cross_Message
           (Proc_Name, Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind);

   end Report_Failed_Test_Routine;

      -----------------------------------------------------

   overriding
   procedure Report_Passed_Node_Run (Obj      : in out Test_Reporter_W_Barrier;
                                     Node_Tag :        Tag) is

      Event_Kind : constant Test_Event_Kind := Passed_Node_Run;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_W_Barrier'Class (Obj).Arriv_To_Cross_Message
           (Proc_Name, Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind);

   end Report_Passed_Node_Run;

      -----------------------------------------------------

   overriding
   procedure Report_Failed_Node_Run (Obj      : in out Test_Reporter_W_Barrier;
                                     Node_Tag :        Tag) is

      Event_Kind : constant Test_Event_Kind := Failed_Node_Run;
      Proc_Name  : constant String := Test_Reporter_Proc_Name (Event_Kind);

   begin

      Obj.C_D_T.Trace
        (Test_Reporter_W_Barrier'Class (Obj).Arriv_To_Cross_Message
           (Proc_Name, Node_Tag));
      Obj.Barrier.Cross (Obj.Tag_To_Char (Node_Tag)) (Event_Kind);

   end Report_Failed_Node_Run;

   ----------------------------------------------------------------------------

end Apsepp_Test_Node_Barrier;
