-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Calendar;                 use Ada.Calendar;
with Apsepp.Test_Case_Count_Types; use Apsepp.Test_Case_Count_Types;

with Apsepp.Generic_Logical_Array.Assertions_W_Debug_Trace;

package Apsepp.Test_Reporter_Data_Struct_Class.Impl.Arrays is

   type Node_Tag_W_Parent_Index is record
      Node_Tag     : Tag;
      Parent_Index : Natural;
      Active       : Boolean;
   end record;

   type Node_Tag_Tree_As_Array
     is array (Positive range <>) of Node_Tag_W_Parent_Index;

   function Tree_Node_Tag (A : Node_Tag_Tree_As_Array;
                           K : Positive) return Tag
     is (A(K).Node_Tag)
     with Pre => K in A'Range;

   function Tree_Node_Parent_Tag (A : Node_Tag_Tree_As_Array;
                                  K : Positive) return Tag
     is (if A(K).Parent_Index = 0 then
            No_Tag
         else
            A(A(K).Parent_Index).Node_Tag)
     with Pre => K in A'Range;

   function Is_Tree_Node_Active (A : Node_Tag_Tree_As_Array;
                                 K : Positive) return Boolean
     is (A(K).Active)
     with Pre => K in A'Range;

   type Node_Tag_Test_Event_Data is record
      Node_Tag           : Tag;
      Has_E              : Boolean;
      Date               : Time;
      Previous_Child_Tag : Tag;
      Routine_Index,
      Last_Routine_Index : Test_Routine_Count;
      Assert_Num         : Test_Assert_Count;
   end record;

   type Node_Tag_Test_Event_Data_Array
     is array (Positive range <>) of Node_Tag_Test_Event_Data;

   function Current_Routine_Index (A : Node_Tag_Test_Event_Data_Array;
                                   K : Positive) return Test_Routine_Count
     with Pre => K in A'Range;

   function Current_Assert_Num (A : Node_Tag_Test_Event_Data_Array;
                                K : Positive) return Test_Assert_Count
     with Pre => K in A'Range;

   function Has_Duplicate_Node_Tag (Obj : Test_Reporter_Data) return Boolean;

   function Tree_Node_Count
     (Obj : Test_Reporter_Data) return Natural;

   function Event_Vector_Length
     (Obj : Test_Reporter_Data) return Natural;

   package Logical_Array is new Generic_Logical_Array (Index_Type => Positive);

   package Logical_Array_Assertions_W_Debug_Trace
     is new Logical_Array.Assertions_W_Debug_Trace;

   use Logical_Array_Assertions_W_Debug_Trace;

   -- TODOC: Duplicated node tags not supported. <2019-11-06>
   -- TODOC: "Lossy" because exception details are lost in the conversion.
   -- <2020-03-28>
   procedure Lossy_Conversion_To_Arrays
     (Obj           :     Test_Reporter_Data;
      Node_Tag_Tree : out Node_Tag_Tree_As_Array;
      Event_Data    : out Node_Tag_Test_Event_Data_Array)
     with Pre => All_True ((not Has_Duplicate_Node_Tag (Obj),
                            Node_Tag_Tree'Length = Tree_Node_Count (Obj),
                            Event_Data'Length    = Event_Vector_Length (Obj)));

end Apsepp.Test_Reporter_Data_Struct_Class.Impl.Arrays;
