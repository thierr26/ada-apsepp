-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Tags; use Ada.Tags;
limited with Apsepp.Test_Node_Class;

package Apsepp.Test_Reporter_Class is

   type Test_Routine_Count_Access is access Test_Node_Class.Test_Routine_Count;

   type Test_Assert_Count is new Natural;

   type Test_Reporter_Interfa is limited interface;

   type Test_Reporter_Access is access all Test_Reporter_Interfa'Class;

   not overriding
   procedure Set_Unreported_Routine_Exception_Details_Flag
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag) is null;

   not overriding
   procedure Reset_Unreported_Routine_Exception_Details_Flag
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag) is null;

   not overriding
   function Unreported_Routine_Exception_Details
     (Obj : Test_Reporter_Interfa) return Boolean is abstract;

   not overriding
   procedure Report_Node_Lineage (Obj          : in out Test_Reporter_Interfa;
                                  Node_Lineage :        Tag_Array) is null;

   not overriding
   procedure Report_Failed_Child_Test_Node_Access
     (Obj                : in out Test_Reporter_Interfa;
      Node_Tag           :        Tag;
      First_Child        :        Boolean;
      Previous_Child_Tag :        Tag) is null;

   not overriding
   procedure Report_Unexpected_Node_Cond_Check_Error
     (Obj                : in out Test_Reporter_Interfa;
      Node_Tag           :        Tag) is null;

   not overriding
   procedure Report_Unexpected_Node_Run_Error
     (Obj                : in out Test_Reporter_Interfa;
      Node_Tag           :        Tag) is null;

   not overriding
   procedure Report_Node_Cond_Check_Start
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag) is null;

   not overriding
   procedure Report_Passed_Node_Cond_Check
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag) is null;

   not overriding
   procedure Report_Failed_Node_Cond_Check
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag) is null;

   not overriding
   procedure Report_Passed_Node_Cond_Assert
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag) is null;

   not overriding
   procedure Report_Failed_Node_Cond_Assert
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag) is null;

   not overriding
   procedure Report_Node_Run_Start
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag) is null;

   not overriding
   procedure Report_Test_Routine_Start
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count) is null;

   not overriding
   procedure Report_Test_Routines_Cancellation
     (Obj              : in out Test_Reporter_Interfa;
      Node_Tag         :        Tag;
      First_K, Last_K  :        Test_Node_Class.Test_Routine_Count) is null;

   not overriding
   procedure Report_Failed_Test_Routine_Access
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag) is null;

   not overriding
   procedure Report_Failed_Test_Routine_Setup
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag) is null;

   not overriding
   procedure Report_Passed_Test_Assert
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag) is null;

   not overriding
   procedure Report_Failed_Test_Assert
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag;
      Message  :        String                := "") is null;

   not overriding
   procedure Report_Unexpected_Routine_Exception
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag;
      E        :        Exception_Occurrence) is null;

   not overriding
   procedure Report_Passed_Test_Routine
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag) is null;

   not overriding
   procedure Report_Failed_Test_Routine
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag) is null;

   not overriding
   procedure Report_Passed_Node_Run
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag) is null;

   not overriding
   procedure Report_Failed_Node_Run
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag) is null;

   not overriding
   procedure Processing (Obj : in out Test_Reporter_Interfa) is null;

end Apsepp.Test_Reporter_Class;
