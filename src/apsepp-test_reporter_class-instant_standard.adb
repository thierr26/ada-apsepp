-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Text_IO;
with Apsepp.Test_Node_Class;

package body Apsepp.Test_Reporter_Class.Instant_Standard is

   use Test_Node_Class;

   ----------------------------------------------------------------------------

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
   function Kth (K : Integer_Type; K_Avail : Boolean := True) return String;

   ----------------------------------------------------------------------------

   function Kth (K : Integer_Type; K_Avail : Boolean := True) return String is
      K_Str : String := (if K_Avail then
                            Integer_Type'Image (K)
                         else
                            " [unavailable]");
   begin
      K_Str(K_Str'First) := '#';
      return Designation & " " & K_Str;
   end Kth;

   ----------------------------------------------------------------------------

   function Kth_Routine_Access is new Kth
     (Integer_Type => Test_Routine_Count,
      Designation  => "access to test routine");

   ----------------------------------------------------------------------------

   function Kth_Routine_Setup is new Kth
     (Integer_Type => Test_Routine_Count,
      Designation  => "setup of test routine");

   ----------------------------------------------------------------------------

   function Kth_Routine is new Kth (Integer_Type => Test_Routine_Count,
                                    Designation  => "test routine");

   ----------------------------------------------------------------------------

   function From_Kth_Routine is new Kth (Integer_Type => Test_Routine_Count,
                                         Designation  => "test routines");

   ----------------------------------------------------------------------------

   function To_Kth_Routine is new Kth (Integer_Type => Test_Routine_Count,
                                       Designation  => " to");

   ----------------------------------------------------------------------------

   function Kth_Test_Assert is new Kth (Integer_Type => Test_Assert_Count,
                                        Designation  => Test_Assert);

   ----------------------------------------------------------------------------

   function Kth_Kth (K_A_Avail : Boolean;
                     K_A       : Test_Assert_Count;
                     K_R       : Test_Routine_Count) return String
     is (Kth_Test_Assert (K_A, K_A_Avail) & " for " & Kth_Routine (K_R));

   ----------------------------------------------------------------------------

   function Routine_Range (First_K, Last_K : Test_Routine_Count) return String
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

   procedure Put_Exception_Message
     (Name, Message                : String;
      Quiet_If_Zero_Length_Message : Boolean := False) is

      Zero_Length_Message : constant Boolean := Message'Length = 0;
      Quiet               : constant Boolean := Zero_Length_Message
                                                  and then
                                                Quiet_If_Zero_Length_Message;

   begin

      if not Quiet then
         Ada.Text_IO.New_Line;
         if Zero_Length_Message then
            Ada.Text_IO.Put_Line(Name);
         else
            Ada.Text_IO.Put_Line(Name & ": " & Message);
         end if;
         Ada.Text_IO.New_Line;
      end if;

   end Put_Exception_Message;

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
     (Node_Tag         : Tag;
      Outcome          : Test_Outcome;
      K                : Test_Routine_Count;
      Assert_Num_Avail : Boolean;
      Assert_Num       : Test_Assert_Count) is

   begin

      Put_Report_Line
        (Outcome_Prepended (Outcome,
                            Kth_Kth (Assert_Num_Avail, Assert_Num, K)),
         Node_Tag);

   end Report_Test_Assert_Outcome;

   ----------------------------------------------------------------------------

   protected body Test_Reporter_Instant_Standard is

      -----------------------------------------------------

      procedure Provide_Node_Lineage (Node_Lineage :     Tag_Array;
                                      Active       : out Boolean) is

         pragma Unreferenced (Node_Lineage);

      begin

         Active := True;

      end Provide_Node_Lineage;

      -----------------------------------------------------

      procedure Report_Failed_Child_Test_Node_Access
        (Node_Tag           : Tag;
         First_Child        : Boolean;
         Previous_Child_Tag : Tag;
         E                  : Exception_Occurrence) is

      begin

         if First_Child then
            Put_Report_Line
              (Outcome_Prepended (Failed, Child_Acc_1), Node_Tag);
         else
            Put_Report_Line (Outcome_Prepended (Failed, Child_Acc),
                             Node_Tag,
                             Previous_Child_Tag);
         end if;
         Put_Exception_Message (Exception_Name (E), Exception_Message (E));

      end Report_Failed_Child_Test_Node_Access;

      -----------------------------------------------------

      procedure Report_Unexpected_Node_Cond_Check_Error
        (Node_Tag : Tag;
         E        : Exception_Occurrence) is

      begin

         Put_Report_Line (Unexp_Error & "checking condition", Node_Tag);
         Put_Exception_Message (Exception_Name (E), Exception_Message (E));

      end Report_Unexpected_Node_Cond_Check_Error;

      -----------------------------------------------------

      procedure Report_Unexpected_Node_Run_Error
        (Node_Tag : Tag;
         E        : Exception_Occurrence) is

         use Ada.Text_IO;

      begin

         Put_Line (Unexp_Error & "running" & Test_Node_W_Tag (Node_Tag));
         Put_Exception_Message (Exception_Name (E), Exception_Message (E));

      end Report_Unexpected_Node_Run_Error;

      -----------------------------------------------------

      procedure Report_Node_Cond_Check_Start (Node_Tag : Tag) is

      begin

         Put_Report_Line (Start & Cond_Checking, Node_Tag);

      end Report_Node_Cond_Check_Start;

      -----------------------------------------------------

      procedure Report_Passed_Node_Cond_Check (Node_Tag : Tag) is

      begin

         Put_Report_Line (Outcome_Prepended (Passed, Cond_Checking), Node_Tag);

      end Report_Passed_Node_Cond_Check;

      -----------------------------------------------------

      procedure Report_Failed_Node_Cond_Check (Node_Tag : Tag) is

      begin

         Put_Report_Line (Outcome_Prepended (Failed, Cond_Checking), Node_Tag);

      end Report_Failed_Node_Cond_Check;

      -----------------------------------------------------

      procedure Report_Passed_Node_Cond_Assert (Node_Tag : Tag) is

      begin

         Put_Report_Line (Outcome_Prepended (Passed, Cond_Assert), Node_Tag);

      end Report_Passed_Node_Cond_Assert;

      -----------------------------------------------------

      procedure Report_Failed_Node_Cond_Assert (Node_Tag : Tag) is

      begin

         Put_Report_Line (Outcome_Prepended (Failed, Cond_Assert), Node_Tag);

      end Report_Failed_Node_Cond_Assert;

      -----------------------------------------------------

      procedure Report_Node_Run_Start (Node_Tag : Tag) is

      begin

         Put_Report_Line (Start & Ru, Node_Tag);

      end Report_Node_Run_Start;

      -----------------------------------------------------

      procedure Report_Test_Routine_Start
        (Node_Tag : Tag;
         K        : Test_Routine_Count) is

      begin

         Put_Report_Line (Start & Kth_Routine (K), Node_Tag);

      end Report_Test_Routine_Start;

      -----------------------------------------------------

      procedure Report_Test_Routines_Cancellation
        (Node_Tag        : Tag;
         First_K, Last_K : Test_Routine_Count) is

      begin

         Put_Report_Line ("CANCELLED " & Routine_Range (First_K, Last_K),
                          Node_Tag);

      end Report_Test_Routines_Cancellation;

      -----------------------------------------------------

      procedure Report_Failed_Test_Routine_Access
        (Node_Tag : Tag;
         K        : Test_Routine_Count;
         E        : Exception_Occurrence) is

      begin

         Put_Report_Line
           (Outcome_Prepended (Failed, Kth_Routine_Access (K)), Node_Tag);
         Put_Exception_Message (Exception_Name (E), Exception_Message (E));

      end Report_Failed_Test_Routine_Access;

      -----------------------------------------------------

      procedure Report_Failed_Test_Routine_Setup
        (Node_Tag : Tag;
         K        : Test_Routine_Count;
         E        : Exception_Occurrence) is

      begin

         Put_Report_Line
           (Outcome_Prepended (Failed, Kth_Routine_Setup (K)), Node_Tag);
         Put_Exception_Message (Exception_Name (E), Exception_Message (E));

      end Report_Failed_Test_Routine_Setup;

      -----------------------------------------------------

      procedure Report_Passed_Test_Assert
        (Node_Tag         : Tag;
         K                : Test_Routine_Count;
         Assert_Num_Avail : Boolean;
         Assert_Num       : Test_Assert_Count) is

      begin

         Report_Test_Assert_Outcome
           (Node_Tag, Passed, K, Assert_Num_Avail, Assert_Num);

      end Report_Passed_Test_Assert;

      -----------------------------------------------------

      procedure Report_Failed_Test_Assert
        (Node_Tag         : Tag;
         K                : Test_Routine_Count;
         Assert_Num_Avail : Boolean;
         Assert_Num       : Test_Assert_Count;
         E                : Exception_Occurrence) is

      begin

         Report_Test_Assert_Outcome
           (Node_Tag, Failed, K, Assert_Num_Avail, Assert_Num);
         Put_Exception_Message ("Message", Exception_Message (E), True);

      end Report_Failed_Test_Assert;

      -----------------------------------------------------

      procedure Report_Unexpected_Routine_Exception
        (Node_Tag : Tag;
         K        : Test_Routine_Count;
         E        : Exception_Occurrence) is

      begin

         Put_Report_Line
           (Unexp_Error & "running " & Kth_Routine (K), Node_Tag);
         Put_Exception_Message (Exception_Name (E), Exception_Message (E));

      end Report_Unexpected_Routine_Exception;

      -----------------------------------------------------

      procedure Report_Passed_Test_Routine
        (Node_Tag : Tag;
         K        : Test_Routine_Count) is

      begin

         Put_Report_Line
           (Outcome_Prepended (Passed, Kth_Routine (K)), Node_Tag);

      end Report_Passed_Test_Routine;

      -----------------------------------------------------

      procedure Report_Failed_Test_Routine
        (Node_Tag : Tag;
         K        : Test_Routine_Count) is

      begin

         Put_Report_Line
           (Outcome_Prepended (Failed, Kth_Routine (K)), Node_Tag);

      end Report_Failed_Test_Routine;

      -----------------------------------------------------

      procedure Report_Passed_Node_Run (Node_Tag : Tag) is

      begin

         Put_Report_Line (Outcome_Prepended (Passed, Ru), Node_Tag);

      end Report_Passed_Node_Run;

      -----------------------------------------------------

      procedure Report_Failed_Node_Run (Node_Tag : Tag) is

      begin

         Put_Report_Line (Outcome_Prepended (Failed, Ru), Node_Tag);

      end Report_Failed_Node_Run;

      -----------------------------------------------------

   end Test_Reporter_Instant_Standard;

   ----------------------------------------------------------------------------

end Apsepp.Test_Reporter_Class.Instant_Standard;
