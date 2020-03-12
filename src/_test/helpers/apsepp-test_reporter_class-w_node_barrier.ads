-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Test_Reporter_Class.Sink; use Apsepp.Test_Reporter_Class.Sink;

with Apsepp.Test_Node_Class.Protected_Test_Node_Barrier;
  use Apsepp.Test_Node_Class.Protected_Test_Node_Barrier;

package Apsepp.Test_Reporter_Class.W_Node_Barrier is

   type Test_Reporter_W_Node_Barrier
     is limited new Test_Reporter_Sink with private;

   not overriding
   procedure Setup
     (Obj                      : in out Test_Reporter_W_Node_Barrier;
      Barrier_Access           :        not null access Test_Node_Barrier;
      Char_Name_Image_Function :        Char_Name_Image_Func;
      Tag_To_Char_Function     :        Tag_To_Char_Func);

   overriding
   procedure Report_Failed_Child_Test_Node_Access
     (Obj                : in out Test_Reporter_W_Node_Barrier;
      Node_Tag           :        Tag;
      Previous_Child_Tag :        Tag;
      Error              :        Exception_Occurrence);

   overriding
   procedure Report_Unexpected_Node_Cond_Check_Error
     (Obj      : in out Test_Reporter_W_Node_Barrier;
      Node_Tag :        Tag;
      Error    :        Exception_Occurrence);

   overriding
   procedure Report_Unexpected_Node_Run_Error
     (Obj      : in out Test_Reporter_W_Node_Barrier;
      Node_Tag :        Tag;
      Error    :        Exception_Occurrence);

   overriding
   procedure Report_Node_Cond_Check_Start
     (Obj      : in out Test_Reporter_W_Node_Barrier;
      Node_Tag :        Tag);

   overriding
   procedure Report_Passed_Node_Cond_Check
     (Obj      : in out Test_Reporter_W_Node_Barrier;
      Node_Tag :        Tag);

   overriding
   procedure Report_Failed_Node_Cond_Check
     (Obj      : in out Test_Reporter_W_Node_Barrier;
      Node_Tag :        Tag);

   overriding
   procedure Report_Passed_Node_Cond_Assert
     (Obj      : in out Test_Reporter_W_Node_Barrier;
      Node_Tag :        Tag);

   overriding
   procedure Report_Failed_Node_Cond_Assert
     (Obj      : in out Test_Reporter_W_Node_Barrier;
      Node_Tag :        Tag);

   overriding
   procedure Report_Node_Run_Start
     (Obj      : in out Test_Reporter_W_Node_Barrier;
      Node_Tag :        Tag);

   overriding
   procedure Report_Test_Routine_Start
     (Obj           : in out Test_Reporter_W_Node_Barrier;
      Node_Tag      :        Tag;
      Routine_Index :        Test_Routine_Index);

   overriding
   procedure Report_Test_Routines_Cancellation
     (Obj                 : in out Test_Reporter_W_Node_Barrier;
      Node_Tag            :        Tag;
      First_Routine_Index,
      Last_Routine_Index  :        Test_Routine_Index);

   overriding
   procedure Report_Failed_Test_Routine_Access
     (Obj           : in out Test_Reporter_W_Node_Barrier;
      Node_Tag      :        Tag;
      Routine_Index :        Test_Routine_Index;
      Error         :        Exception_Occurrence);

   overriding
   procedure Report_Failed_Test_Routine_Setup
     (Obj           : in out Test_Reporter_W_Node_Barrier;
      Node_Tag      :        Tag;
      Routine_Index :        Test_Routine_Index;
      Error         :        Exception_Occurrence);

   overriding
   procedure Report_Passed_Test_Assert
     (Obj              : in out Test_Reporter_W_Node_Barrier;
      Node_Tag         :        Tag;
      Routine_Index    :        Test_Routine_Index;
      Assert_Num_Avail :        Boolean;
      Assert_Num       :        Test_Assert_Index);

   overriding
   procedure Report_Failed_Test_Assert
     (Obj              : in out Test_Reporter_W_Node_Barrier;
      Node_Tag         :        Tag;
      Routine_Index    :        Test_Routine_Index;
      Assert_Num_Avail :        Boolean;
      Assert_Num       :        Test_Assert_Index;
      Error            :        Exception_Occurrence);

   overriding
   procedure Report_Unexpected_Routine_Exception
     (Obj           : in out Test_Reporter_W_Node_Barrier;
      Node_Tag      :        Tag;
      Routine_Index :        Test_Routine_Index;
      Error         :        Exception_Occurrence);

   overriding
   procedure Report_Passed_Test_Routine
     (Obj           : in out Test_Reporter_W_Node_Barrier;
      Node_Tag      :        Tag;
      Routine_Index :        Test_Routine_Index);

   overriding
   procedure Report_Failed_Test_Routine
     (Obj           : in out Test_Reporter_W_Node_Barrier;
      Node_Tag      :        Tag;
      Routine_Index :        Test_Routine_Index);

   overriding
   procedure Report_Passed_Node_Run
     (Obj      : in out Test_Reporter_W_Node_Barrier;
      Node_Tag :        Tag);

   overriding
   procedure Report_Failed_Node_Run
     (Obj      : in out Test_Reporter_W_Node_Barrier;
      Node_Tag :        Tag);

private

   type Test_Reporter_W_Node_Barrier
     is limited new Test_Reporter_Sink with record
      Barrier         : access Test_Node_Barrier;
      Char_Name_Image : Char_Name_Image_Func;
      Tag_To_Char     : Tag_To_Char_Func;
   end record;

   not overriding
   function Arriv_To_Cross_Message
     (Obj            : Test_Reporter_W_Node_Barrier;
      Operation_Name : String;
      Node_Tag       : Tag) return String;

end Apsepp.Test_Reporter_Class.W_Node_Barrier;
