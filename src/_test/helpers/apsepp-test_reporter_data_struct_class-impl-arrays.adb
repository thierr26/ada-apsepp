-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Assertions,
     Ada.Exceptions,
     Apsepp.Calendar;

package body Apsepp.Test_Reporter_Data_Struct_Class.Impl.Arrays is

   ----------------------------------------------------------------------------

   function Current_Routine_Index (A : Node_Tag_Test_Event_Data_Array;
                                   K : Positive) return Test_Routine_Count is

      T : constant Tag := A(K).Node_Tag;

   begin

      for E of reverse A(1 .. K - 1) loop

         if E.Node_Tag = T and then E.Routine_Index /= 0 then
            return E.Routine_Index; -- Early return.
         end if;

      end loop;

      return 0; -- This line can be reached.

   end Current_Routine_Index;

   ----------------------------------------------------------------------------

   function Current_Assert_Num (A : Node_Tag_Test_Event_Data_Array;
                                K : Positive) return Test_Assert_Count is

      T : constant Tag := A(K).Node_Tag;

   begin

      for E of reverse A(1 .. K - 1) loop

         if E.Node_Tag = T
              and then
            E.Routine_Index = A(K).Routine_Index
              and then
            E.Assert_Num /= 0 then

            return E.Assert_Num; -- Early return.

         end if;

      end loop;

      return 0; -- This line can be reached.

   end Current_Assert_Num;

   ----------------------------------------------------------------------------

   function Node_Data_Has_Duplicated_Tag
     (Obj     : Test_Reporter_Data;
      Element : Node_Data) return Boolean is

      T         : constant Tag := Element.T;
      Tag_Found : Boolean      := False;

   begin

      for E of Obj.Node_Data_Tree loop

         if E.T = T then

            if Tag_Found then
               -- Tag 'T' already found once.

               -- Here we know that 'T' is a duplicated tag in the test node
               -- tree.
               return True; -- Early return.
            end if;

            Tag_Found := True;

         end if;

      end loop;

      -- Here we know that 'T' has not been found more than once.
      return False;

   end Node_Data_Has_Duplicated_Tag;

   ----------------------------------------------------------------------------

   function Has_Duplicate_Node_Tag (Obj : Test_Reporter_Data) return Boolean
     is (for some E of Obj.Node_Data_Tree
           => Node_Data_Has_Duplicated_Tag (Obj, E));

   ----------------------------------------------------------------------------

   function Tree_Node_Count
     (Obj : Test_Reporter_Data) return Natural
     is (Natural (Obj.Node_Data_Tree.Node_Count) - 1);
     -- The "- 1" is to account for the fact that
     -- Ada.Containers.Multiway_Trees.Node_Count returns 1 for an empty tree
     -- (because there is always at least a root node).
     -- REF: ARM A18.10(74/3). <2020-03-15>

   ----------------------------------------------------------------------------

   function Event_Vector_Length
     (Obj : Test_Reporter_Data) return Natural
     is (Natural (Obj.Event_Vector.Length));

   ----------------------------------------------------------------------------

   procedure Lossy_Conversion_To_Arrays
     (Obj           :     Test_Reporter_Data;
      Node_Tag_Tree : out Node_Tag_Tree_As_Array;
      Event_Data    : out Node_Tag_Test_Event_Data_Array) is

      Index      : Positive;
      First_Iter : Boolean;

      -----------------------------------------------------

      procedure Process_Node_Tag (Position : Node_Data_Trees.Cursor) is

      begin

         if First_Iter then
            Index      := Node_Tag_Tree'First;
            First_Iter := False;
         else
            Index := Index + 1;
         end if;

         declare
            A : Node_Tag_W_Parent_Index renames Node_Tag_Tree(Index);
         begin
            A.Node_Tag := Node_Data_Trees.Element (Position).T;
            A.Active   := Obj.Active_Node_Map.Contains (A.Node_Tag);
         end;

      end Process_Node_Tag;

      -----------------------------------------------------

      procedure Process_Parent_Index (Position : Node_Data_Trees.Cursor) is

         Parent_Position : constant Node_Data_Trees.Cursor
           := Node_Data_Trees.Parent (Position);

      begin

         if First_Iter then
            Index      := Node_Tag_Tree'First;
            First_Iter := False;
         else
            Index := Index + 1;
         end if;

         if not Node_Data_Trees.Is_Root (Parent_Position) then

            declare

               A : Node_Tag_W_Parent_Index
                 renames Node_Tag_Tree(Index);

               Parent_Tag : constant Tag
                 := Node_Data_Trees.Element (Parent_Position).T;

               K : Natural := Index;
               -- After the loops below, K should be the index in Node_Tag_Tree
               -- of the parent of the element at Position in the tree (or at
               -- index Index in Node_Tag_Tree). The search is
               -- started at Index because the tree traversal is
               -- supposed to be depth-first, post-order (the children are
               -- visited before the parents).

               procedure Process is
               begin
                  K := K + 1;
                  if Node_Tag_Tree(K).Node_Tag = Parent_Tag then
                     A.Parent_Index := K;
                  end if;
               end Process;

            begin

               A.Parent_Index := 0;

               while A.Parent_Index = 0 and then K < Node_Tag_Tree'Last loop
                  Process;
               end loop;
               K := 0;
               while A.Parent_Index = 0 and then K < Index loop
                  Process;
               end loop;
               Ada.Assertions.Assert
                 (A.Parent_Index /= 0,
                  "Unable to build Node_Tag_Tree array");

            end;

         else
            Node_Tag_Tree(Index).Parent_Index := 0;
         end if;

      end Process_Parent_Index;

      -----------------------------------------------------

      procedure Process_Event_Data (Position : Node_Event_Vectors.Cursor) is

         use Ada.Exceptions,
             Apsepp.Calendar,
             Node_Event_Vectors;

         E : constant Test_Event_Access := Element (Position).Event;
         T : constant Tag
           := Node_Data_Trees.Element (Element (Position).Node_Data_Cursor).T;

      begin

         if First_Iter then
            Index      := Event_Data'First;
            First_Iter := False;
         else
            Index := Index + 1;
         end if;

         declare
            A : Node_Tag_Test_Event_Data renames Event_Data(Index);
         begin
            A.Node_Tag           := T;
            A.Has_E              := E.Exception_Access /= null;
            A.Date               := (if E.Has_Timestamp then
                                        E.Timestamp
                                     else
                                        Time_First);
            A.Previous_Child_Tag := (if E.Has_Previous_Child_Tag then
                                        E.Previous_Child_Tag
                                     else
                                        No_Tag);
            A.Routine_Index      := (if E.Has_Routine_Index then
                                        E.Routine_Index
                                     else
                                        0);
            A.Last_Routine_Index := (if E.Has_Last_Cancelled_Routine_Index then
                                        E.Last_Cancelled_Routine_Index
                                     else
                                        0);
            A.Assert_Num         := (if E.Has_Assert_Num then
                                        E.Assert_Num
                                     else
                                        0);
         end;

      end Process_Event_Data;

      -----------------------------------------------------

   begin

      First_Iter := True;
      Obj.Node_Data_Tree.Iterate (Process_Node_Tag'Access);

      First_Iter := True;
      Obj.Node_Data_Tree.Iterate (Process_Parent_Index'Access);

      First_Iter := True;
      Obj.Event_Vector.Iterate (Process_Event_Data'Access);

   end Lossy_Conversion_To_Arrays;

   ----------------------------------------------------------------------------

end Apsepp.Test_Reporter_Data_Struct_Class.Impl.Arrays;
