-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

package Apsepp.Test_Reporter_Class.Instant_Standard is

   type Test_Reporter_Instant_Standard
     is limited new Test_Reporter_Interfa with private;

   overriding
   procedure Set_Unreported_Routine_Exception_Details_Flag
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag);

   overriding
   procedure Reset_Unreported_Routine_Exception_Details_Flag
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag);

   overriding
   function Unreported_Routine_Exception_Details
     (Obj : Test_Reporter_Instant_Standard) return Boolean;

   overriding
   procedure Report_Failed_Child_Test_Node_Access
     (Obj                : in out Test_Reporter_Instant_Standard;
      Node_Tag           :        Tag;
      First_Child        :        Boolean;
      Previous_Child_Tag :        Tag);

   overriding
   procedure Report_Unexpected_Node_Cond_Check_Error
     (Obj                : in out Test_Reporter_Instant_Standard;
      Node_Tag           :        Tag);

   overriding
   procedure Report_Unexpected_Node_Run_Error
     (Obj                : in out Test_Reporter_Instant_Standard;
      Node_Tag           :        Tag);

   overriding
   procedure Report_Node_Cond_Check_Start
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag);

   overriding
   procedure Report_Passed_Node_Cond_Check
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag);

   overriding
   procedure Report_Failed_Node_Cond_Check
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag);

   overriding
   procedure Report_Passed_Node_Cond_Assert
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag);

   overriding
   procedure Report_Failed_Node_Cond_Assert
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag);

   overriding
   procedure Report_Node_Run_Start
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag);

   overriding
   procedure Report_Test_Routine_Start
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count);

   overriding
   procedure Report_Test_Routines_Cancellation
     (Obj              : in out Test_Reporter_Instant_Standard;
      Node_Tag         :        Tag;
      First_K, Last_K  :        Test_Node_Class.Test_Routine_Count);

   overriding
   procedure Report_Failed_Test_Routine_Access
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag);

   overriding
   procedure Report_Failed_Test_Routine_Setup
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag);

   overriding
   procedure Report_Passed_Test_Assert
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag);

   overriding
   procedure Report_Failed_Test_Assert
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag;
      Message  :        String                         := "");

   overriding
   procedure Report_Unexpected_Routine_Exception
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag;
      E        :        Exception_Occurrence);

   overriding
   procedure Report_Passed_Test_Routine
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag);

   overriding
   procedure Report_Failed_Test_Routine
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag);

   overriding
   procedure Report_Passed_Node_Run
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag);

   overriding
   procedure Report_Failed_Node_Run
     (Obj      : in out Test_Reporter_Instant_Standard;
      Node_Tag :        Tag);

private

   type Test_Reporter_Instant_Standard
     is limited new Test_Reporter_Interfa with record
      Unreported_Routine_Exception_Details_Flag : Boolean            := False;
      Tag_On_Assert_Reset                       : Tag                := No_Tag;
      Tag_Mismatch                              : Boolean            := False;
      Assert_Count                              : Test_Assert_Count  := 0;
      Routine_Index                             : Test_Routine_Count_Access;
   end record;

end Apsepp.Test_Reporter_Class.Instant_Standard;
