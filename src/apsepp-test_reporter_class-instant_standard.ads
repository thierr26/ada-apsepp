-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package Apsepp.Test_Reporter_Class.Instant_Standard is

   protected type Test_Reporter_Instant_Standard
     is new Test_Reporter_Interfa with

      overriding
      function Is_Conflicting_Node_Tag (Node_Tag : Tag) return Boolean;

      overriding
      procedure Provide_Node_Lineage (Node_Lineage : Tag_Array);

      overriding
      procedure Report_Failed_Child_Test_Node_Access
        (Node_Tag           : Tag;
         Previous_Child_Tag : Tag;
         Error              : Exception_Occurrence);

      overriding
      procedure Report_Unexpected_Node_Cond_Check_Error
        (Node_Tag : Tag;
         Error    : Exception_Occurrence);

      overriding
      procedure Report_Unexpected_Node_Run_Error
        (Node_Tag : Tag;
         Error    : Exception_Occurrence);

      overriding
      procedure Report_Node_Cond_Check_Start (Node_Tag : Tag);

      overriding
      procedure Report_Passed_Node_Cond_Check (Node_Tag : Tag);

      overriding
      procedure Report_Failed_Node_Cond_Check (Node_Tag : Tag);

      overriding
      procedure Report_Passed_Node_Cond_Assert (Node_Tag : Tag);

      overriding
      procedure Report_Failed_Node_Cond_Assert (Node_Tag : Tag);

      overriding
      procedure Report_Node_Run_Start (Node_Tag : Tag);

      overriding
      procedure Report_Test_Routine_Start
        (Node_Tag      : Tag;
         Routine_Index : Test_Routine_Index);

      overriding
      procedure Report_Test_Routines_Cancellation
        (Node_Tag            : Tag;
         First_Routine_Index,
         Last_Routine_Index  : Test_Routine_Index);

      overriding
      procedure Report_Failed_Test_Routine_Access
        (Node_Tag      : Tag;
         Routine_Index : Test_Routine_Index;
         Error         : Exception_Occurrence);

      overriding
      procedure Report_Failed_Test_Routine_Setup
        (Node_Tag      : Tag;
         Routine_Index : Test_Routine_Index;
         Error         : Exception_Occurrence);

      overriding
      procedure Report_Passed_Test_Assert
        (Node_Tag         : Tag;
         Routine_Index    : Test_Routine_Index;
         Assert_Num_Avail : Boolean;
         Assert_Num       : Test_Assert_Index);

      overriding
      procedure Report_Failed_Test_Assert
        (Node_Tag         : Tag;
         Routine_Index    : Test_Routine_Index;
         Assert_Num_Avail : Boolean;
         Assert_Num       : Test_Assert_Index;
         Error            : Exception_Occurrence);

      overriding
      procedure Report_Unexpected_Routine_Exception
        (Node_Tag      : Tag;
         Routine_Index : Test_Routine_Index;
         Error         : Exception_Occurrence);

      overriding
      procedure Report_Passed_Test_Routine
        (Node_Tag      : Tag;
         Routine_Index : Test_Routine_Index);

      overriding
      procedure Report_Failed_Test_Routine
        (Node_Tag      : Tag;
         Routine_Index : Test_Routine_Index);

      overriding
      procedure Report_Passed_Node_Run (Node_Tag : Tag);

      overriding
      procedure Report_Failed_Node_Run (Node_Tag : Tag);

   end Test_Reporter_Instant_Standard;

end Apsepp.Test_Reporter_Class.Instant_Standard;
