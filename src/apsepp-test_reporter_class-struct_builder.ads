-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Test_Reporter_Data_Struct_Class.Impl;
  use Apsepp.Test_Reporter_Data_Struct_Class.Impl;
  use Apsepp.Test_Reporter_Data_Struct_Class;

package Apsepp.Test_Reporter_Class.Struct_Builder is

   ----------------------------------------------------------------------------

   protected type Test_Reporter_Struct_Builder
     (Processing : Test_Reporter_Data_Proc := null)
     is new Test_Reporter_Interfa with

      overriding
      function Is_Conflicting_Node_Tag (Node_Tag : Tag) return Boolean;

      overriding
      procedure Provide_Node_Lineage (Node_Lineage : Tag_Array);

      overriding
      procedure Report_Failed_Child_Test_Node_Access
        (Node_Tag           : Tag;
         Previous_Child_Tag : Tag;
         E                  : Exception_Occurrence);

      overriding
      procedure Report_Unexpected_Node_Cond_Check_Error
        (Node_Tag : Tag;
         E        : Exception_Occurrence);

      overriding
      procedure Report_Unexpected_Node_Run_Error
        (Node_Tag : Tag;
         E        : Exception_Occurrence);

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
        (Node_Tag : Tag;
         K        : Test_Node_Class.Test_Routine_Count);

      overriding
      procedure Report_Test_Routines_Cancellation
        (Node_Tag        : Tag;
         First_K, Last_K : Test_Node_Class.Test_Routine_Count);

      overriding
      procedure Report_Failed_Test_Routine_Access
        (Node_Tag : Tag;
         K        : Test_Node_Class.Test_Routine_Count;
         E        : Exception_Occurrence);

      overriding
      procedure Report_Failed_Test_Routine_Setup
        (Node_Tag : Tag;
         K        : Test_Node_Class.Test_Routine_Count;
         E        : Exception_Occurrence);

      overriding
      procedure Report_Passed_Test_Assert
        (Node_Tag         : Tag;
         K                : Test_Node_Class.Test_Routine_Count;
         Assert_Num_Avail : Boolean;
         Assert_Num       : Test_Node_Class.Test_Assert_Count);

      overriding
      procedure Report_Failed_Test_Assert
        (Node_Tag         : Tag;
         K                : Test_Node_Class.Test_Routine_Count;
         Assert_Num_Avail : Boolean;
         Assert_Num       : Test_Node_Class.Test_Assert_Count;
         E                : Exception_Occurrence);

      overriding
      procedure Report_Unexpected_Routine_Exception
        (Node_Tag : Tag;
         K        : Test_Node_Class.Test_Routine_Count;
         E        : Exception_Occurrence);

      overriding
      procedure Report_Passed_Test_Routine
        (Node_Tag : Tag;
         K        : Test_Node_Class.Test_Routine_Count);

      overriding
      procedure Report_Failed_Test_Routine
        (Node_Tag : Tag;
         K        : Test_Node_Class.Test_Routine_Count);

      overriding
      procedure Report_Passed_Node_Run (Node_Tag : Tag);

      overriding
      procedure Report_Failed_Node_Run (Node_Tag : Tag);

      overriding
      procedure Process;

      not overriding
      function Struct_Pointer return access constant Test_Reporter_Data;

   private

      Data : aliased Test_Reporter_Data;

   end Test_Reporter_Struct_Builder;

   ----------------------------------------------------------------------------

end Apsepp.Test_Reporter_Class.Struct_Builder;
