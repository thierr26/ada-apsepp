-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Tags;       use Ada.Tags;

limited with Apsepp.Test_Node_Class.Abstract_Test_Case;

package Apsepp.Test_Reporter_Class is

   type Test_Reporter_Interfa is limited interface;

   not overriding
   function Is_Conflicting_Node_Tag
     (Obj      : Test_Reporter_Interfa;
      Node_Tag : Tag) return Boolean is abstract;

   not overriding
   procedure Provide_Node_Lineage (Obj          : in out Test_Reporter_Interfa;
                                   Node_Lineage :        Tag_Array) is null
     with Pre'Class =>
            (for all T of Node_Lineage => T /= No_Tag)
              and then
            not Test_Reporter_Interfa'Class (Obj)
                  .Is_Conflicting_Node_Tag (Node_Lineage (Node_Lineage'Last));

   -- TODOC: Called by Test_Node_Class.Suite_Stub.Run_Children.
   -- <2019-03-03>
   not overriding
   procedure Report_Failed_Child_Test_Node_Access
     (Obj                : in out Test_Reporter_Interfa;
      Node_Tag           :        Tag;
      Previous_Child_Tag :        Tag;
      Error              :        Exception_Occurrence) is null;

   -- TODOC: Called by Test_Node_Class.Generic_Case_And_Suite_Run_Body.
   -- <2019-03-02>
   not overriding
   procedure Report_Unexpected_Node_Cond_Check_Error
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag;
      Error    :        Exception_Occurrence) is null;

   -- TODOC: Called by Test_Node_Class.Generic_Case_And_Suite_Run_Body.
   -- <2019-03-02>
   not overriding
   procedure Report_Unexpected_Node_Run_Error
     (Obj      : in out Test_Reporter_Interfa;
      Node_Tag :        Tag;
      Error    :        Exception_Occurrence) is null;

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
     (Obj           : in out Test_Reporter_Interfa;
      Node_Tag      :        Tag;
      Routine_Index :        Test_Node_Class.Abstract_Test_Case
                               .Test_Routine_Count)
     is null;

   -- TODOC: Called by Test_Node_Class.Run_Test_Routines. <2019-03-02>
   not overriding
   procedure Report_Test_Routines_Cancellation
     (Obj                                     : in out Test_Reporter_Interfa;
      Node_Tag                                :        Tag;
      First_Routine_Index, Last_Routine_Index :        Test_Node_Class
                                                         .Abstract_Test_Case
                                                         .Test_Routine_Count)
     is null;

   -- TODOC: Called by Test_Node_Class.Run_Test_Routines. <2019-03-02>
   not overriding
   procedure Report_Failed_Test_Routine_Access
     (Obj           : in out Test_Reporter_Interfa;
      Node_Tag      :        Tag;
      Routine_Index :        Test_Node_Class.Abstract_Test_Case
                               .Test_Routine_Count;
      Error         :        Exception_Occurrence) is null;

   -- TODOC: Called by Test_Node_Class.Run_Test_Routines. <2019-03-02>
   not overriding
   procedure Report_Failed_Test_Routine_Setup
     (Obj           : in out Test_Reporter_Interfa;
      Node_Tag      :        Tag;
      Routine_Index :        Test_Node_Class.Abstract_Test_Case
                               .Test_Routine_Count;
      Error         :        Exception_Occurrence) is null;

   -- TODOC: Called by Test_Node_Class.Assert. <2019-03-02>
   not overriding
   procedure Report_Passed_Test_Assert
     (Obj              : in out Test_Reporter_Interfa;
      Node_Tag         :        Tag;
      Routine_Index    :        Test_Node_Class.Abstract_Test_Case
                                  .Test_Routine_Count;
      Assert_Num_Avail :        Boolean;
      Assert_Num       :        Test_Node_Class.Abstract_Test_Case
                                  .Test_Assert_Count)
     is null;

   -- TODOC: Called by Test_Node_Class.Assert. <2019-03-02>
   not overriding
   procedure Report_Failed_Test_Assert
     (Obj              : in out Test_Reporter_Interfa;
      Node_Tag         :        Tag;
      Routine_Index    :        Test_Node_Class.Abstract_Test_Case
                                  .Test_Routine_Count;
      Assert_Num_Avail :        Boolean;
      Assert_Num       :        Test_Node_Class.Abstract_Test_Case
                                  .Test_Assert_Count;
      Error            :        Exception_Occurrence) is null;

   -- TODOC: Called by Test_Node_Class.Run_Test_Routines. <2019-03-02>
   not overriding
   procedure Report_Unexpected_Routine_Exception
     (Obj           : in out Test_Reporter_Interfa;
      Node_Tag      :        Tag;
      Routine_Index :        Test_Node_Class.Abstract_Test_Case
                               .Test_Routine_Count;
      Error         :        Exception_Occurrence) is null;

   -- TODOC: Called by Test_Node_Class.Run_Test_Routines. <2019-03-02>
   not overriding
   procedure Report_Passed_Test_Routine
     (Obj           : in out Test_Reporter_Interfa;
      Node_Tag      :        Tag;
      Routine_Index :        Test_Node_Class.Abstract_Test_Case
                               .Test_Routine_Count) is null;

   -- TODOC: Called by Test_Node_Class.Run_Test_Routines. <2019-03-02>
   not overriding
   procedure Report_Failed_Test_Routine
     (Obj           : in out Test_Reporter_Interfa;
      Node_Tag      :        Tag;
      Routine_Index :        Test_Node_Class.Abstract_Test_Case
                               .Test_Routine_Count) is null;

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
