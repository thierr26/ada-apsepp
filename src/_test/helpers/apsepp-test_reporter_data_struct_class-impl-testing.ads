-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Calendar;           use Ada.Calendar;
with Apsepp.Test_Node_Class; use Apsepp.Test_Node_Class;

package Apsepp.Test_Reporter_Data_Struct_Class.Impl.Testing is

   type Node_Tag_W_Parent_Index is record
      Node_Tag     : Tag;
      Parent_Index : Natural;
      Active       : Boolean;
   end record;

   type Node_Tag_Tree_As_Array
     is array (Positive range <>) of Node_Tag_W_Parent_Index;

   function Tree_Tag (A : Node_Tag_Tree_As_Array; K : Positive) return Tag
     is (A(K).Node_Tag)

     with Pre => K in A'Range;

   function Tree_Parent_Tag (A : Node_Tag_Tree_As_Array;
                             K : Positive) return Tag
     is (if A(K).Parent_Index = 0 then
            No_Tag
         else
            A(A(K).Parent_Index).Node_Tag)

     with Pre => K in A'Range;

   function Tree_Active (A : Node_Tag_Tree_As_Array;
                         K : Positive) return Boolean
     is (A(K).Active)

     with Pre => K in A'Range;

   type Flattened_Event_Data is record
      Node_Tag           : Tag;
      Has_E              : Boolean;
      Date               : Time;
      Previous_Child_Tag : Tag;
      R_Index            : Test_Routine_Count;
      Assert_Num         : Test_Assert_Count;
   end record;

   type Event_Data_Array
     is array (Positive range <>) of Flattened_Event_Data;

   function Current_R_Index (A : Event_Data_Array;
                             K : Positive) return Test_Routine_Count

     with Pre => K in A'Range;

   function Current_Assert_Num (A : Event_Data_Array;
                                K : Positive) return Test_Assert_Count

     with Pre => K in A'Range;

   function Has_Duplicate_Node_Tag (Obj : Test_Reporter_Data) return Boolean;

   function Node_Tag_Tree_Node_Count
     (Obj : Test_Reporter_Data) return Natural;

   function Event_Vector_Length
     (Obj : Test_Reporter_Data) return Natural;

   function Node_Event_Count (Obj      : Test_Reporter_Data;
                              Node_Tag : Tag) return Natural

     with Pre => not Has_Duplicate_Node_Tag (Obj);

   -- TODOC: Duplicated node tags not supported. <2019-11-06>
   procedure To_Arrays (Obj           :     Test_Reporter_Data;
                        Node_Tag_Tree : out Node_Tag_Tree_As_Array;
                        Event_Data    : out Event_Data_Array)

     with Pre => not Has_Duplicate_Node_Tag (Obj)
                   and then
                 Node_Tag_Tree'Length = Node_Tag_Tree_Node_Count (Obj)
                   and then
                 Event_Data'Length    = Event_Vector_Length (Obj);

   procedure To_Node_Event_Array (Obj             :     Test_Reporter_Data;
                                  Node_Tag        :     Tag;
                                  Event_Data      :     Event_Data_Array;
                                  Node_Event_Data : out Event_Data_Array)

     with Pre => not Has_Duplicate_Node_Tag (Obj)
                   and then
                 Event_Data'Length = Node_Event_Count (Obj, Node_Tag);

   function Latest_R_Index (Obj      : Test_Reporter_Data;
                            Node_Tag : Tag) return Test_Routine_Index

     with Pre => not Has_Duplicate_Node_Tag (Obj);

end Apsepp.Test_Reporter_Data_Struct_Class.Impl.Testing;
