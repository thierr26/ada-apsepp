-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Unchecked_Deallocation;

package body Apsepp.Test_Reporter_Data_Struct_Class.Impl is

   ----------------------------------------------------------------------------

   not overriding
   procedure Add_And_Or_Set_Active_Node
     (Obj : in out Test_Reporter_Data;
      T   :        Tag;
      C   :    out Node_Data_Trees.Cursor) is

      use Node_Data_Hashed_Maps; -- Makes "=" for type
                                 -- Node_Data_Hashed_Maps.Cursor directly
                                 -- visible.

      Active_Node_Map_C : constant Node_Data_Hashed_Maps.Cursor
        := Obj.Active_Node_Map.Find (T);

   begin

      if Active_Node_Map_C = Node_Data_Hashed_Maps.No_Element then

         C := Node_Data_Trees.No_Element;

         for Cu in Obj.Node_Data_Tree.Iterate loop
            if Node_Data_Trees.Element (Cu).T = T then
               C := Cu;
               exit;
            end if;
         end loop;

         if C = Node_Data_Trees.No_Element then

            -- TODO: Refactor the Insert_Child calls.
            Obj.Node_Data_Tree.Insert_Child
              (Parent   => Root (Obj.Node_Data_Tree),
               Before   => Node_Data_Trees.No_Element,
               New_Item => (T                  => T,
                            Event_Index_Vector => <>),
               Position => C);

         end if;

         Obj.Active_Node_Map.Insert (Key      => T,
                                     New_Item => C);

      else

         C := Element (Active_Node_Map_C);

      end if;

   end Add_And_Or_Set_Active_Node;

   ----------------------------------------------------------------------------

   overriding
   function Is_Empty (Obj : Test_Reporter_Data) return Boolean
     is (Obj.Active_Node_Map.Is_Empty
           and then
         Obj.Event_Vector.Is_Empty
           and then
         Obj.Node_Data_Tree.Is_Empty);

   ----------------------------------------------------------------------------

   overriding
   procedure Reset (Obj : in out Test_Reporter_Data) is

      -----------------------------------------------------

      procedure Clean_Up_Events (Position : Node_Event_Vectors.Cursor) is

         N_E : Node_Event := Node_Event_Vectors.Element (Position);

         procedure Free is new Ada.Unchecked_Deallocation
           (Object => Test_Event_Base'Class,
            Name   => Test_Event_Access);

      begin

         N_E.Event.Clean_Up;
         Free (N_E.Event);

      end Clean_Up_Events;

      -----------------------------------------------------

   begin

      Obj.Active_Node_Map.Clear;

      Obj.Event_Vector.Iterate (Clean_Up_Events'Access);
      Obj.Event_Vector.Clear;

      Obj.Node_Data_Tree.Clear;

   end Reset;

   ----------------------------------------------------------------------------

   overriding
   function Is_Active (Obj      : Test_Reporter_Data;
                       Node_Tag : Tag) return Boolean
     is (Obj.Active_Node_Map.Contains (Node_Tag));

   ----------------------------------------------------------------------------

   overriding
   procedure Include_Node (Obj          : in out Test_Reporter_Data;
                           Node_Lineage :        Tag_Array) is

      C : Cursor := Root (Obj.Node_Data_Tree);

   begin

      for T of Node_Lineage loop

         declare

            Insertion_Required : Boolean := Is_Leaf (C);

         begin

            for Child_C in Obj.Node_Data_Tree.Iterate_Children (C) loop

               if Element (Child_C).T = T then
                  C := Child_C;
                  exit;
               end if;

               Insertion_Required := Child_C = Last_Child (C);

            end loop;

            if Insertion_Required then

               declare
                  Position : Cursor;
               begin
                  Obj.Node_Data_Tree.Insert_Child
                    (Parent   => C,
                     Before   => No_Element,
                     New_Item => (T                  => T,
                                  Event_Index_Vector => <>),
                     Position => Position);
                  C := Position;
               end;

            end if;

         end;

      end loop;

   end Include_Node;

   ----------------------------------------------------------------------------

   overriding
   procedure Add_Event (Obj      : in out Test_Reporter_Data;
                        Node_Tag :        Tag;
                        Event    :        Test_Event_Base'Class) is

      -----------------------------------------------------

      procedure Update_Node_Event_Vector (Element : in out Node_Data) is
      begin
         Element.Event_Index_Vector.Append (Obj.Event_Vector.Last_Index);
      end Update_Node_Event_Vector;

      -----------------------------------------------------

      C : Cursor;

   begin

      Obj.Add_And_Or_Set_Active_Node (Node_Tag, C);

      Obj.Event_Vector.Append (
        (Node_Data_Cursor => C,
         Event            => new Test_Event_Base'Class'(Event)));

      Obj.Node_Data_Tree.Update_Element (C, Update_Node_Event_Vector'Access);

      if Event.Is_Node_Run_Final_Event then
         Obj.Active_Node_Map.Delete (Key => Node_Tag);
      end if;

   end Add_Event;

   ----------------------------------------------------------------------------

end Apsepp.Test_Reporter_Data_Struct_Class.Impl;
