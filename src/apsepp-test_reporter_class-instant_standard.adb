-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Text_IO;
with Apsepp.Test_Node_Class;

package body Apsepp.Test_Reporter_Class.Instant_Standard is

   use Apsepp.Test_Node_Class;

   Child_Acc     : constant String := "accessing child";
   Child_Acc_1   : constant String := "accessing first child";
   Start         : constant String := "Start ";
   Cond_Checking : constant String := "condition checking";
   Cond_Assert   : constant String := "condition assertion";
   Test_Assert   : constant String := "test assertion";
   Ru            : constant String := "run";
   Unexp_Error   : constant String := "UNEXPECTED ERROR while ";

   ----------------------------------------------------------------------------

   generic
      type Integer_Type is range <>;
      Designation : String;
   function Kth (K : Integer_Type) return String;

   function Kth (K : Integer_Type) return String is
      K_Str : String := Integer_Type'Image (K);
   begin
      K_Str(K_Str'First) := '#';
      return Designation & " " & K_Str;
   end Kth;

   ----------------------------------------------------------------------------

   function Kth_Routine_Access is new Kth
     (Integer_Type => Test_Routine_Index,
      Designation  => "access to test routine");

   ----------------------------------------------------------------------------

   function Kth_Routine_Setup is new Kth
     (Integer_Type => Test_Routine_Index,
      Designation  => "setup of test routine");

   ----------------------------------------------------------------------------

   function Kth_Routine is new Kth (Integer_Type => Test_Routine_Index,
                                    Designation  => "test routine");

   ----------------------------------------------------------------------------

   function From_Kth_Routine is new Kth (Integer_Type => Test_Routine_Index,
                                         Designation  => "test routines");

   ----------------------------------------------------------------------------

   function To_Kth_Routine is new Kth (Integer_Type => Test_Routine_Index,
                                       Designation  => " to");

   ----------------------------------------------------------------------------

   function Kth_Test_Assert is new Kth (Integer_Type => Test_Assert_Count,
                                        Designation  => Test_Assert);

   ----------------------------------------------------------------------------

   function Kth_Kth (K_A : Test_Assert_Count;
                     K_R : Test_Routine_Index) return String
     is (Kth_Test_Assert (K_A) & " for " & Kth_Routine (K_R));

   ----------------------------------------------------------------------------

   function Routine_Range (First_K, Last_K : Test_Routine_Index) return String
     is (if Last_K = First_K then
            Kth_Routine (First_K)
         else
            From_Kth_Routine (First_K) & To_Kth_Routine (Last_K));

   ----------------------------------------------------------------------------

   function Outcome_Prepended (Outcome : Test_Outcome;
                               Head    : String) return String
     is ((case Outcome is
             when Failed => "FAILED",
             when Passed => "Passed") & " " & Head);

   ----------------------------------------------------------------------------

   function Test_Node_W_Tag (Node_Tag : Tag) return String
     is (" test node with tag " & Expanded_Name (Node_Tag));

   ----------------------------------------------------------------------------

   procedure Put_Report_Line (Head         : String;
                              Node_Tag     : Tag;
                              Prev_Brother : Tag    := No_Tag) is

      use Ada.Text_IO;

      Next_Brother : constant String
        := (if Prev_Brother = No_Tag then
               ""
            else
               " (next sibling of" & Test_Node_W_Tag (Prev_Brother) & ")");

   begin

      Put_Line (Head & " for" & Test_Node_W_Tag (Node_Tag) & Next_Brother);

   end Put_Report_Line;

   ----------------------------------------------------------------------------

   procedure Report_Test_Assert_Outcome
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag;
      Outcome  :        Test_Outcome) is

   begin

      if not Obj.Tag_Mismatch and then Node_Tag /= Obj.Tag_On_Assert_Reset then
         Obj.Tag_Mismatch := True;
      end if;
      Obj.Assert_Count := Obj.Assert_Count + 1;

      if Obj.Tag_Mismatch then
         Put_Report_Line (Outcome_Prepended (Outcome, Test_Assert), Node_Tag);
      else
         Put_Report_Line
           (Outcome_Prepended (Outcome,
                               Kth_Kth (Obj.Assert_Count, Obj.Routine_Index)),
            Node_Tag);
      end if;

   end Report_Test_Assert_Outcome;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Failed_Child_Test_Node_Access
     (Obj                : in out Test_Reporter_Instant_Standard;
      Node_Tag           :        Tag;
      First_Child        :        Boolean;
      Previous_Child_Tag :        Tag) is

      pragma Unreferenced (Obj);

   begin

      if First_Child then
         Put_Report_Line (Outcome_Prepended (Failed, Child_Acc_1), Node_Tag);
      else
         Put_Report_Line (Outcome_Prepended (Failed, Child_Acc),
                          Node_Tag,
                          Previous_Child_Tag);
      end if;

   end Report_Failed_Child_Test_Node_Access;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Unexpected_Node_Cond_Check_Error
     (Obj                : in out Test_Reporter_Instant_Standard;
      Node_Tag           :        Tag) is

      pragma Unreferenced (Obj);

   begin

      Put_Report_Line (Unexp_Error & "checking condition", Node_Tag);

   end Report_Unexpected_Node_Cond_Check_Error;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Unexpected_Node_Run_Error
     (Obj                : in out Test_Reporter_Instant_Standard;
      Node_Tag           :        Tag) is

      use Ada.Text_IO;

      pragma Unreferenced (Obj);

   begin

      Put_Line (Unexp_Error & "running" & Test_Node_W_Tag (Node_Tag));

   end Report_Unexpected_Node_Run_Error;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Node_Cond_Check_Start
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag) is

      pragma Unreferenced (Obj);

   begin

      Put_Report_Line (Start & Cond_Checking, Node_Tag);

   end Report_Node_Cond_Check_Start;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Passed_Node_Cond_Check
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag) is

      pragma Unreferenced (Obj);

   begin

      Put_Report_Line (Outcome_Prepended (Passed, Cond_Checking), Node_Tag);

   end Report_Passed_Node_Cond_Check;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Failed_Node_Cond_Check
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag) is

      pragma Unreferenced (Obj);

   begin

      Put_Report_Line (Outcome_Prepended (Failed, Cond_Checking), Node_Tag);

   end Report_Failed_Node_Cond_Check;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Passed_Node_Cond_Assert
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag) is

      pragma Unreferenced (Obj);

   begin

      Put_Report_Line (Outcome_Prepended (Passed, Cond_Assert), Node_Tag);

   end Report_Passed_Node_Cond_Assert;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Failed_Node_Cond_Assert
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag) is

      pragma Unreferenced (Obj);

   begin

      Put_Report_Line (Outcome_Prepended (Failed, Cond_Assert), Node_Tag);

   end Report_Failed_Node_Cond_Assert;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Node_Run_Start
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag) is

      pragma Unreferenced (Obj);

   begin

      Put_Report_Line (Start & Ru, Node_Tag);

   end Report_Node_Run_Start;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Test_Routine_Start
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag;
      K        :        Test_Routine_Index) is

   begin

      Obj.Tag_On_Assert_Reset := Node_Tag;
      Obj.Tag_Mismatch := False;
      Obj.Assert_Count := 0;
      Obj.Routine_Index := K;
      Put_Report_Line (Start & Kth_Routine (K), Node_Tag);

   end Report_Test_Routine_Start;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Test_Routines_Cancellation
     (Obj              : in out Test_Reporter_Instant_Standard;
      Node_Tag         :        Tag;
      First_K, Last_K  :        Test_Routine_Index) is

      pragma Unreferenced (Obj);

   begin

      Put_Report_Line
        ("CANCELLED " & Routine_Range (First_K, Last_K),
         Node_Tag);

   end Report_Test_Routines_Cancellation;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Failed_Test_Routine_Access
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag) is

   begin

      Put_Report_Line
        (Outcome_Prepended (Failed, Kth_Routine_Access (Obj.Routine_Index)),
         Node_Tag);

   end Report_Failed_Test_Routine_Access;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Failed_Test_Routine_Setup
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag) is

   begin

      Put_Report_Line
        (Outcome_Prepended (Failed, Kth_Routine_Setup (Obj.Routine_Index)),
         Node_Tag);

   end Report_Failed_Test_Routine_Setup;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Passed_Test_Assert
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag) is

   begin

      Report_Test_Assert_Outcome (Obj, Node_Tag, Passed);

   end Report_Passed_Test_Assert;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Failed_Test_Assert
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag;
      Message  :        String                         := "") is

   begin

      Report_Test_Assert_Outcome (Obj, Node_Tag, Failed);
      if Message'Length > 0 then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line("Message: " & Message);
         Ada.Text_IO.New_Line;
      end if;

   end Report_Failed_Test_Assert;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Passed_Test_Routine
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag) is

   begin

      Put_Report_Line
        (Outcome_Prepended (Passed, Kth_Routine (Obj.Routine_Index)),
         Node_Tag);

   end Report_Passed_Test_Routine;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Failed_Test_Routine
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag) is

   begin

      Put_Report_Line
        (Outcome_Prepended (Failed, Kth_Routine (Obj.Routine_Index)),
         Node_Tag);

   end Report_Failed_Test_Routine;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Passed_Node_Run
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag) is

      pragma Unreferenced (Obj);

   begin

      Put_Report_Line (Outcome_Prepended (Passed, Ru), Node_Tag);

   end Report_Passed_Node_Run;

   ----------------------------------------------------------------------------

   overriding
   procedure Report_Failed_Node_Run
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag) is

      pragma Unreferenced (Obj);

   begin

      Put_Report_Line (Outcome_Prepended (Failed, Ru), Node_Tag);

   end Report_Failed_Node_Run;

   ----------------------------------------------------------------------------

end Apsepp.Test_Reporter_Class.Instant_Standard;
