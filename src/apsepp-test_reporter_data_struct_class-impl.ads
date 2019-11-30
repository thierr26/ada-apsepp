-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

private with Ada.Containers.Vectors,
             Ada.Containers.Hashed_Maps,
             Ada.Containers.Multiway_Trees;
with Apsepp.Tags; use Apsepp.Tags;

package Apsepp.Test_Reporter_Data_Struct_Class.Impl is

   type Test_Reporter_Data
     is limited new Test_Reporter_Data_Interfa with private;

   overriding
   function Is_Empty (Obj : Test_Reporter_Data) return Boolean;

   overriding
   procedure Reset (Obj : in out Test_Reporter_Data);

   overriding
   function Is_Active (Obj      : Test_Reporter_Data;
                       Node_Tag : Tag) return Boolean;

   overriding
   procedure Include_Node (Obj          : in out Test_Reporter_Data;
                           Node_Lineage :        Tag_Array);

   overriding
   procedure Add_Event (Obj      : in out Test_Reporter_Data;
                        Node_Tag :        Tag;
                        Event    :        Test_Event_Base'Class);

private

   package Event_Index_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Event_Index,
      Element_Type => Event_Index);

   type Node_Data is record
      T                  : Tag;
      Event_Index_Vector : Event_Index_Vectors.Vector;
   end record;

   function Same_Tag (N_D_1, N_D_2 : Node_Data) return Boolean
     is (N_D_1.T = N_D_2.T);

   package Node_Data_Trees is new Ada.Containers.Multiway_Trees
     (Element_Type => Node_Data,
      "="          => Same_Tag);

   use Node_Data_Trees; -- Makes "=" for type Node_Data_Trees.Cursor directly
                        -- visible.

   package Node_Data_Hashed_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Tag,
      Element_Type    => Node_Data_Trees.Cursor,
      Hash            => Tag_Hash,
      Equivalent_Keys => "=");

   type Node_Event is record
      Node_Data_Cursor : Node_Data_Trees.Cursor;
      Event            : Test_Event_Access;
   end record;

   package Node_Event_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Event_Index,
      Element_Type => Node_Event);

   type Test_Reporter_Data
     is limited new Test_Reporter_Data_Interfa with record
      Node_Data_Tree  : Node_Data_Trees.Tree;
      Event_Vector    : Node_Event_Vectors.Vector;
      Active_Node_Map : Node_Data_Hashed_Maps.Map;
   end record;

   not overriding
   function Is_Active_Node (Obj : Test_Reporter_Data;
                            T   : Tag) return Boolean
     is (Obj.Active_Node_Map.Contains (T))

     with Post'Class
       => not Is_Active_Node'Result
            or else
          Obj.Node_Data_Tree.Contains ((T                  => T,
                                        Event_Index_Vector => <>));

   not overriding
   procedure Add_And_Or_Set_Active_Node (Obj : in out Test_Reporter_Data;
                                         T   :        Tag;
                                         C   :    out Node_Data_Trees.Cursor)

     with Post'Class
       => Obj.Node_Data_Tree.Contains ((T                  => T,
                                        Event_Index_Vector => <>))
            and then
          Obj.Is_Active_Node (T);

end Apsepp.Test_Reporter_Data_Struct_Class.Impl;
