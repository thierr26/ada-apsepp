-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Unchecked_Deallocation;

package body Apsepp.Test_Reporter_Data_Struct_Class.Impl is

   ----------------------------------------------------------------------------

   not overriding
   procedure Create_Node_Child (Obj      : in out Test_Reporter_Data;
                                Position : in out Node_Data_Trees.Cursor;
                                T        :        Tag) is

      Parent : constant Node_Data_Trees.Cursor := Position;

   begin

      Obj.Node_Data_Tree.Insert_Child
        (Parent   => Parent,
         Before   => Node_Data_Trees.No_Element,
         New_Item => (T                  => T,
                      Event_Index_Vector => <>),
         Position => Position);
      -- Value 'Node_Data_Trees.No_Element' for parameter 'Before' implies that
      -- the new child is inserted after the last existing child.
      -- REF: ARM A18.10(167/3). <2020-03-25>

   end Create_Node_Child;

   ----------------------------------------------------------------------------

   not overriding
   procedure Add_And_Or_Set_Active_Node
     (Obj      : in out Test_Reporter_Data;
      T        :        Tag;
      Position :    out Node_Data_Trees.Cursor) is

      use type Node_Data_Hashed_Maps.Cursor;

      -- Search for node tag 'T' in the active nodes map.
      Active_Node_Map_C : constant Node_Data_Hashed_Maps.Cursor
        := Obj.Active_Node_Map.Find (T);

   begin

      if Active_Node_Map_C = Node_Data_Hashed_Maps.No_Element then
         -- Node tag 'T' not found in the active nodes map.

         -- Search node tag 'T' in the node data tree.
         Position := Node_Data_Trees.No_Element;
         for K in Obj.Node_Data_Tree.Iterate loop
            if Obj.Node_Data_Tree(K).T = T then
               Position := K;
               exit; -- Early exit.
            end if;
         end loop;

         if Position = Node_Data_Trees.No_Element then
            -- Node tag 'T' not found in the node data tree.

            -- Insert a new node for 'T' in the node data tree and get a cursor
            -- to that node in 'Position'.
            Position := Node_Data_Trees.Root (Obj.Node_Data_Tree);
            Obj.Create_Node_Child (Position, T);

         end if;

         -- Add the 'T' node to the active nodes map.
         Obj.Active_Node_Map.Insert (Key      => T,
                                     New_Item => Position);

      else

         -- Get a cursor to the 'T' node of the active nodes map.
         Position := Node_Data_Hashed_Maps.Element (Active_Node_Map_C);

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
         Free (Test_Event_Access (N_E.Event));

      end Clean_Up_Events;

      -----------------------------------------------------

   begin

      -- Clear the active nodes map.
      Obj.Active_Node_Map.Clear;

      -- For every event in the event vector, run the 'Clean_Up' primitive and
      -- free the event.
      Obj.Event_Vector.Iterate (Clean_Up_Events'Access);
      -- Clear the event vector.
      Obj.Event_Vector.Clear;

      -- Clear the node data treee.
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

      -- Initialize 'Position' to the node data tree root.
      Position : Node_Data_Trees.Cursor
        := Node_Data_Trees.Root (Obj.Node_Data_Tree);

   begin

      for T of Node_Lineage loop

         declare

            Child_Creation_Required : Boolean := True;

         begin

            -- Iterate over children of the node designated by 'Position'.
            for Child_C in Obj.Node_Data_Tree.Iterate_Children (Position) loop

               if Obj.Node_Data_Tree(Child_C).T = T then
                  -- The node designated by 'Position' has a child with node
                  -- tag 'T'.

                  -- Have 'Position' designate this child node and stop
                  -- requiring child creation.
                  Position := Child_C;
                  Child_Creation_Required := False;

                  exit; -- Early exit.

               end if;

            end loop;

            if Child_Creation_Required then

               -- For the node designated by 'Position', create a child with
               -- node tag 'T' and get a cursor to the new node in 'Position'.
               Obj.Create_Node_Child (Position, T);

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

      Position : Node_Data_Trees.Cursor;

   begin

      -- Make sure a node with node tag 'Node_Tag' is in the active nodes map.
      Obj.Add_And_Or_Set_Active_Node (Node_Tag, Position);

      -- Append event to the vector event.
      Obj.Event_Vector.Append (
        (Node_Data_Cursor => Position,
         Event            => new Test_Event_Base'Class'(Event)));

      -- Update the data of the associated node data in the node data tree
      -- (i.e. add the event index to the event index vector).
      Obj.Node_Data_Tree.Update_Element (Position,
                                         Update_Node_Event_Vector'Access);

      if Event.Is_Node_Run_Final_Event then
         -- No more event can occur related to the test node.

         -- Delete the node tag from the active nodes map.
         Obj.Active_Node_Map.Delete (Key => Node_Tag);

      end if;

   end Add_Event;

   ----------------------------------------------------------------------------

end Apsepp.Test_Reporter_Data_Struct_Class.Impl;
