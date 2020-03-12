-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Text_IO,
     Apsepp.Test_Node_Class.Abstract_Early_Test_Case;

package body Apsepp.Test_Reporter_Class.Instant_Standard is

   use Test_Node_Class;

   ----------------------------------------------------------------------------

   Child_Acc             : constant String := "accessing child";
   Child_Acc_1           : constant String := "accessing first child";
   Early_Test_Routine    : constant String := "early test routine";
   Start                 : constant String := "Start ";
   Cond_Checking         : constant String := "condition checking";
   Cond_Assert           : constant String := "condition assertion";
   Early_Routine_Not_Run : constant String := " ("
                                              & Early_Test_Routine
                                              & " not run)";
   Test_Assert           : constant String := "test assertion";
   Test_Node_Run         : constant String := "run";
   Unexp_Error           : constant String := "UNEXPECTED ERROR while ";

   ----------------------------------------------------------------------------

   function Is_Early_Test (Node_Tag : Tag) return Boolean
     is (Is_Descendant_At_Same_Level
           (Descendant => Node_Tag,
            Ancestor   => Abstract_Early_Test_Case.Early_Test_Case'Tag));

   ----------------------------------------------------------------------------

   function Early_Routine_Not_Run_Compliment (Node_Tag : Tag;
                                              Str      : String) return String
     is (Str & (if Is_Early_Test (Node_Tag) then
                   Early_Routine_Not_Run
                else
                   ""));

   ----------------------------------------------------------------------------

   generic
      type Integer_Type is range <>;
      Designation : String;
   function Kth (K       : Integer_Type;
                 K_Avail : Boolean      := True) return String;

   ----------------------------------------------------------------------------

   function Kth (K       : Integer_Type;
                 K_Avail : Boolean      := True) return String is
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
     (Integer_Type => Test_Routine_Index,
      Designation  => "access to test routine");

   ----------------------------------------------------------------------------

   function Kth_Routine_Setup is new Kth
     (Integer_Type => Test_Routine_Index,
      Designation  => "setup of test routine");

   ----------------------------------------------------------------------------

   function Kth_Routine is new Kth
     (Integer_Type => Test_Routine_Index,
      Designation  => "test routine");

   ----------------------------------------------------------------------------

   function From_Kth_Routine is new Kth
     (Integer_Type => Test_Routine_Index,
      Designation  => "test routines");

   ----------------------------------------------------------------------------

   function To_Kth_Routine is new Kth
     (Integer_Type => Test_Routine_Index,
      Designation  => " to");

   ----------------------------------------------------------------------------

   function Kth_Test_Assert is new Kth
     (Integer_Type => Test_Assert_Index,
      Designation  => Test_Assert);

   ----------------------------------------------------------------------------

   function Kth_Kth
     (K_A_Avail : Boolean;
      K_A       : Test_Assert_Index;
      K_R       : Test_Routine_Index) return String
     is (Kth_Test_Assert (K_A, K_A_Avail) & " for " & Kth_Routine (K_R));

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

      Zero_Length_Name    : constant Boolean := Name'Length = 0;
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
            Ada.Text_IO.Put_Line(Name & (if Zero_Length_Name then
                                            ""
                                         else
                                            ": ") & Message);
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
      Routine_Index    : Test_Routine_Index;
      Assert_Num_Avail : Boolean;
      Assert_Num       : Test_Assert_Index) is

   begin

      Put_Report_Line (Outcome_Prepended (Outcome,
                                          (if Is_Early_Test (Node_Tag) then
                                              Early_Test_Routine
                                           else
                                              Kth_Kth (Assert_Num_Avail,
                                                       Assert_Num,
                                                       Routine_Index))),
                       Node_Tag);

   end Report_Test_Assert_Outcome;

   ----------------------------------------------------------------------------

   protected body Test_Reporter_Instant_Standard is

      -----------------------------------------------------

      function Is_Conflicting_Node_Tag (Node_Tag : Tag) return Boolean
        is (False);

      -----------------------------------------------------

      procedure Provide_Node_Lineage (Node_Lineage : Tag_Array) is

         pragma Unreferenced (Node_Lineage);

      begin

         null;

      end Provide_Node_Lineage;

      -----------------------------------------------------

      procedure Report_Failed_Child_Test_Node_Access
        (Node_Tag           : Tag;
         Previous_Child_Tag : Tag;
         Error              : Exception_Occurrence) is

      begin

         if Previous_Child_Tag = No_Tag then
            Put_Report_Line
              (Head     => Outcome_Prepended (Failed, Child_Acc_1),
               Node_Tag => Node_Tag);
         else
            Put_Report_Line
              (Head         => Outcome_Prepended (Failed, Child_Acc),
               Node_Tag     => Node_Tag,
               Prev_Brother => Previous_Child_Tag);
         end if;
         Put_Exception_Message (Name    => Exception_Name (Error),
                                Message => Exception_Message (Error));

      end Report_Failed_Child_Test_Node_Access;

      -----------------------------------------------------

      procedure Report_Unexpected_Node_Cond_Check_Error
        (Node_Tag : Tag;
         Error    : Exception_Occurrence) is

      begin

         Put_Report_Line (Head     => Unexp_Error & "checking condition",
                          Node_Tag => Node_Tag);
         Put_Exception_Message (Name    => Exception_Name (Error),
                                Message => Exception_Message (Error));

      end Report_Unexpected_Node_Cond_Check_Error;

      -----------------------------------------------------

      procedure Report_Unexpected_Node_Run_Error
        (Node_Tag : Tag;
         Error    : Exception_Occurrence) is

         use Ada.Text_IO;

      begin

         Put_Line (Unexp_Error & "running" & Test_Node_W_Tag (Node_Tag));
         Put_Exception_Message (Name    => Exception_Name (Error),
                                Message => Exception_Message (Error));

      end Report_Unexpected_Node_Run_Error;

      -----------------------------------------------------

      procedure Report_Node_Cond_Check_Start (Node_Tag : Tag) is

      begin

         Put_Report_Line (Head     => Start & Cond_Checking,
                          Node_Tag => Node_Tag);

      end Report_Node_Cond_Check_Start;

      -----------------------------------------------------

      procedure Report_Passed_Node_Cond_Check (Node_Tag : Tag) is

      begin

         Put_Report_Line
           (Head     => Outcome_Prepended (Passed, Cond_Checking),
            Node_Tag => Node_Tag);

      end Report_Passed_Node_Cond_Check;

      -----------------------------------------------------

      procedure Report_Failed_Node_Cond_Check (Node_Tag : Tag) is

      begin

         Put_Report_Line
           (Head     => Outcome_Prepended
                          (Failed,
                           Early_Routine_Not_Run_Compliment (Node_Tag,
                                                             Cond_Checking)),
            Node_Tag => Node_Tag);

      end Report_Failed_Node_Cond_Check;

      -----------------------------------------------------

      procedure Report_Passed_Node_Cond_Assert (Node_Tag : Tag) is

      begin

         Put_Report_Line (Head     => Outcome_Prepended (Passed,
                                                         Cond_Assert),
                          Node_Tag => Node_Tag);

      end Report_Passed_Node_Cond_Assert;

      -----------------------------------------------------

      procedure Report_Failed_Node_Cond_Assert (Node_Tag : Tag) is

      begin

         Put_Report_Line
           (Head     => Outcome_Prepended
                          (Failed,
                           Early_Routine_Not_Run_Compliment (Node_Tag,
                                                             Cond_Assert)),
            Node_Tag => Node_Tag);

      end Report_Failed_Node_Cond_Assert;

      -----------------------------------------------------

      procedure Report_Node_Run_Start (Node_Tag : Tag) is

      begin

         Put_Report_Line (Head     => Start & Test_Node_Run,
                          Node_Tag => Node_Tag);

      end Report_Node_Run_Start;

      -----------------------------------------------------

      procedure Report_Test_Routine_Start
        (Node_Tag      : Tag;
         Routine_Index : Test_Routine_Index) is

      begin

         if not Is_Early_Test (Node_Tag) then
            Put_Report_Line (Head     => Start & Kth_Routine (Routine_Index),
                             Node_Tag => Node_Tag);
         end if;

      end Report_Test_Routine_Start;

      -----------------------------------------------------

      procedure Report_Test_Routines_Cancellation
        (Node_Tag            : Tag;
         First_Routine_Index,
         Last_Routine_Index  : Test_Routine_Index) is

      begin

         Put_Report_Line
           (Head     => "CANCELLED "
                        & (if Last_Routine_Index = First_Routine_Index then
                              Kth_Routine (First_Routine_Index)
                           else
                              From_Kth_Routine (First_Routine_Index)
                              & To_Kth_Routine (Last_Routine_Index)),
            Node_Tag => Node_Tag);

      end Report_Test_Routines_Cancellation;

      -----------------------------------------------------

      procedure Report_Failed_Test_Routine_Access
        (Node_Tag      : Tag;
         Routine_Index : Test_Routine_Index;
         Error         : Exception_Occurrence) is

      begin

         Put_Report_Line
           (Head     => Outcome_Prepended (Failed,
                                           Kth_Routine_Access (Routine_Index)),
            Node_Tag => Node_Tag);
         Put_Exception_Message (Name    => Exception_Name (Error),
                                Message => Exception_Message (Error));

      end Report_Failed_Test_Routine_Access;

      -----------------------------------------------------

      procedure Report_Failed_Test_Routine_Setup
        (Node_Tag      : Tag;
         Routine_Index : Test_Routine_Index;
         Error         : Exception_Occurrence) is

      begin

         Put_Report_Line
           (Head     => Outcome_Prepended (Failed,
                                           Kth_Routine_Setup (Routine_Index)),
            Node_Tag => Node_Tag);
         Put_Exception_Message (Name    => Exception_Name (Error),
                                Message => Exception_Message (Error));

      end Report_Failed_Test_Routine_Setup;

      -----------------------------------------------------

      procedure Report_Passed_Test_Assert
        (Node_Tag         : Tag;
         Routine_Index    : Test_Routine_Index;
         Assert_Num_Avail : Boolean;
         Assert_Num       : Test_Assert_Index) is

      begin

         Report_Test_Assert_Outcome (Node_Tag,
                                     Passed,
                                     Routine_Index,
                                     Assert_Num_Avail,
                                     Assert_Num);

      end Report_Passed_Test_Assert;

      -----------------------------------------------------

      procedure Report_Failed_Test_Assert
        (Node_Tag         : Tag;
         Routine_Index    : Test_Routine_Index;
         Assert_Num_Avail : Boolean;
         Assert_Num       : Test_Assert_Index;
         Error            : Exception_Occurrence) is

      begin

         Report_Test_Assert_Outcome (Node_Tag,
                                     Failed,
                                     Routine_Index,
                                     Assert_Num_Avail,
                                     Assert_Num);
         Put_Exception_Message
           (Name                         => (if Is_Early_Test (Node_Tag) then
                                                ""
                                             else
                                                "Message"),
            Message                      => Exception_Message (Error),
            Quiet_If_Zero_Length_Message => True);

      end Report_Failed_Test_Assert;

      -----------------------------------------------------

      procedure Report_Unexpected_Routine_Exception
        (Node_Tag      : Tag;
         Routine_Index : Test_Routine_Index;
         Error         : Exception_Occurrence) is

      begin

         Put_Report_Line
           (Head     => Unexp_Error
                        & "running "
                        & Kth_Routine (Routine_Index),
            Node_Tag => Node_Tag);
         Put_Exception_Message (Name    => Exception_Name (Error),
                                Message => Exception_Message (Error));

      end Report_Unexpected_Routine_Exception;

      -----------------------------------------------------

      procedure Report_Passed_Test_Routine
        (Node_Tag      : Tag;
         Routine_Index : Test_Routine_Index) is

      begin

         if not Is_Early_Test (Node_Tag) then
            Put_Report_Line
              (Head     => Outcome_Prepended (Passed,
                                              Kth_Routine (Routine_Index)),
               Node_Tag => Node_Tag);
         end if;

      end Report_Passed_Test_Routine;

      -----------------------------------------------------

      procedure Report_Failed_Test_Routine
        (Node_Tag      : Tag;
         Routine_Index : Test_Routine_Index) is

      begin

         if not Is_Early_Test (Node_Tag) then
            Put_Report_Line
              (Head     => Outcome_Prepended (Failed,
                                              Kth_Routine (Routine_Index)),
               Node_Tag => Node_Tag);
         end if;

      end Report_Failed_Test_Routine;

      -----------------------------------------------------

      procedure Report_Passed_Node_Run (Node_Tag : Tag) is

      begin

         Put_Report_Line (Head     => Outcome_Prepended (Passed,
                                                         Test_Node_Run),
                          Node_Tag => Node_Tag);

      end Report_Passed_Node_Run;

      -----------------------------------------------------

      procedure Report_Failed_Node_Run (Node_Tag : Tag) is

      begin

         Put_Report_Line (Head     => Outcome_Prepended (Failed,
                                                         Test_Node_Run),
                          Node_Tag => Node_Tag);

      end Report_Failed_Node_Run;

      -----------------------------------------------------

   end Test_Reporter_Instant_Standard;

   ----------------------------------------------------------------------------

end Apsepp.Test_Reporter_Class.Instant_Standard;
