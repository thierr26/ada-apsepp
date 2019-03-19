-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Tags;       use Ada.Tags;
limited with Apsepp.Test_Node_Class;

package Apsepp.Test_Reporter_Class is

   type Test_Routine_Count_Access is access Test_Node_Class.Test_Routine_Count;

   type Test_Reporter_Interfa is limited interface;

   type Test_Reporter_Access is access all Test_Reporter_Interfa'Class;

   not overriding
   procedure Provide_Node_Lineage (Obj          : in out Test_Reporter_Interfa;
                                   Node_Lineage :        Tag_Array;
                                   Active       :    out Boolean) is null;

   -- TODOC: Called by Test_Node_Class.Suite_Stub.Run_Children.
   -- <2019-03-03>
   not overriding
   procedure Report_Failed_Child_Test_Node_Access
     (Obj                : in out Test_Reporter_Interfa;
      Node_Tag           :        Tag;
      First_Child        :        Boolean;
      Previous_Child_Tag :        Tag;
      E                  :        Exception_Occurrence) is null;

   -- TODOC: Called by Test_Node_Class.Generic_Case_And_Suite_Run_Body.
   -- <2019-03-02>
   not overriding
   procedure Report_Unexpected_Node_Cond_Check_Error
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag;
      E        :        Exception_Occurrence) is null;

   -- TODOC: Called by Test_Node_Class.Generic_Case_And_Suite_Run_Body.
   -- <2019-03-02>
   not overriding
   procedure Report_Unexpected_Node_Run_Error
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag;
      E        :        Exception_Occurrence) is null;

   -- TODOC: Called by Test_Node_Class.Generic_Case_And_Suite_Run_Body.
   -- <2019-03-02>
   not overriding
   procedure Report_Node_Cond_Check_Start
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag) is null;

   -- TODOC: Called by Test_Node_Class.Generic_Case_And_Suite_Run_Body.
   -- <2019-03-02>
   not overriding
   procedure Report_Passed_Node_Cond_Check
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag) is null;

   -- TODOC: Called by Test_Node_Class.Generic_Case_And_Suite_Run_Body.
   -- <2019-03-02>
   not overriding
   procedure Report_Failed_Node_Cond_Check
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag) is null;

   -- TODOC: Called by Test_Node_Class.Generic_Case_And_Suite_Run_Body.
   -- <2019-03-02>
   not overriding
   procedure Report_Passed_Node_Cond_Assert
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag) is null;

   -- TODOC: Called by Test_Node_Class.Generic_Case_And_Suite_Run_Body.
   -- <2019-03-02>
   not overriding
   procedure Report_Failed_Node_Cond_Assert
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag) is null;

   -- TODOC: Called by Test_Node_Class.Generic_Case_And_Suite_Run_Body.
   -- <2019-03-02>
   not overriding
   procedure Report_Node_Run_Start
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag) is null;

   -- TODOC: Called by Test_Node_Class.Run_Test_Routines. <2019-03-02>
   not overriding
   procedure Report_Test_Routine_Start
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count) is null;

   -- TODOC: Called by Test_Node_Class.Run_Test_Routines. <2019-03-02>
   not overriding
   procedure Report_Test_Routines_Cancellation
     (Obj             : in out Test_Reporter_Interfa;
      Node_Tag        :        Tag;
      First_K, Last_K :        Test_Node_Class.Test_Routine_Count) is null;

   -- TODOC: Called by Test_Node_Class.Run_Test_Routines. <2019-03-02>
   not overriding
   procedure Report_Failed_Test_Routine_Access
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count;
      E        :        Exception_Occurrence) is null;

   -- TODOC: Called by Test_Node_Class.Run_Test_Routines. <2019-03-02>
   not overriding
   procedure Report_Failed_Test_Routine_Setup
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count;
      E        :        Exception_Occurrence) is null;

   -- TODOC: Called by Test_Node_Class.Assert. <2019-03-02>
   not overriding
   procedure Report_Passed_Test_Assert
     (Obj              : in out Test_Reporter_Interfa;
      Node_Tag         :        Tag;
      K                :        Test_Node_Class.Test_Routine_Count;
      Assert_Num_Avail :        Boolean;
      Assert_Num       :        Test_Node_Class.Test_Assert_Count) is null;

   -- TODOC: Called by Test_Node_Class.Assert. <2019-03-02>
   not overriding
   procedure Report_Failed_Test_Assert
     (Obj              : in out Test_Reporter_Interfa;
      Node_Tag         :        Tag;
      K                :        Test_Node_Class.Test_Routine_Count;
      Assert_Num_Avail :        Boolean;
      Assert_Num       :        Test_Node_Class.Test_Assert_Count;
      E                :        Exception_Occurrence) is null;

   -- TODOC: Called by Test_Node_Class.Run_Test_Routines. <2019-03-02>
   not overriding
   procedure Report_Unexpected_Routine_Exception
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count;
      E        :        Exception_Occurrence) is null;

   -- TODOC: Called by Test_Node_Class.Run_Test_Routines. <2019-03-02>
   not overriding
   procedure Report_Passed_Test_Routine
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count) is null;

   -- TODOC: Called by Test_Node_Class.Run_Test_Routines. <2019-03-02>
   not overriding
   procedure Report_Failed_Test_Routine
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag;
      K        :        Test_Node_Class.Test_Routine_Count) is null;

   -- TODOC: Called by Test_Node_Class.Generic_Case_And_Suite_Run_Body.
   -- <2019-03-02>
   not overriding
   procedure Report_Passed_Node_Run
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag) is null;

   -- TODOC: Called by Test_Node_Class.Generic_Case_And_Suite_Run_Body.
   -- <2019-03-02>
   not overriding
   procedure Report_Failed_Node_Run
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag) is null;

   -- TODOC: Called by Test_Node_Class.Runner_Sequential.Run. <2019-03-03>
   not overriding
   procedure Process (Obj : in out Test_Reporter_Interfa) is null;

end Apsepp.Test_Reporter_Class;
