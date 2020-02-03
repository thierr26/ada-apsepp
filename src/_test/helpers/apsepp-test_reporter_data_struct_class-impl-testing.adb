-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Exceptions,
     Ada.Assertions,
     Apsepp.Calendar;

package body Apsepp.Test_Reporter_Data_Struct_Class.Impl.Testing is

   ----------------------------------------------------------------------------

   function Current_R_Index (A : Event_Data_Array;
                             K : Positive) return Test_Routine_Count is

      T   : constant Tag                := A(K).Node_Tag;
      Ret :          Test_Routine_Count := 0;

   begin

      for E of reverse A(1 .. K - 1) loop

         if E.Node_Tag = T and then E.R_Index /= 0 then
            Ret := E.R_Index;
            exit;
         end if;

      end loop;

      return Ret;

   end Current_R_Index;

   ----------------------------------------------------------------------------

   function Current_Assert_Num (A : Event_Data_Array;
                                K : Positive) return Test_Assert_Count is

      T   : constant Tag               := A(K).Node_Tag;
      Ret :          Test_Assert_Count := 0;

   begin

      for E of reverse A(1 .. K - 1) loop

         if E.Node_Tag = T
              and then
            E.R_Index = A(K).R_Index
              and then
            E.Assert_Num /= 0 then

            Ret := E.Assert_Num;
            exit;

         end if;

      end loop;

      return Ret;

   end Current_Assert_Num;

   ----------------------------------------------------------------------------

   function Multiple_Tag_Occurrences (Obj      : Test_Reporter_Data;
                                      Position : Cursor) return Boolean is

      T     : constant Tag     := Element (Position).T;
      Count :          Natural := 0;

   begin

      -- TODO: Refactor the "for E of Obj.Node_Data_Tree" loops. <2019-11-07>
      for E of Obj.Node_Data_Tree loop

         if E.T = T then
            Count := Count + 1;
         end if;

         exit when Count > 1;
      end loop;

      return Count > 1;

   end Multiple_Tag_Occurrences;

   ----------------------------------------------------------------------------

   function Has_Duplicate_Node_Tag (Obj : Test_Reporter_Data) return Boolean
     is (for some C in Obj.Node_Data_Tree.Iterate
           => Multiple_Tag_Occurrences (Obj, C));

   ----------------------------------------------------------------------------

   function Node_Tag_Tree_Node_Count
     (Obj : Test_Reporter_Data) return Natural
     is (Natural (Obj.Node_Data_Tree.Node_Count) - 1);
     -- The "- 1" is to account for the fact that
     -- Ada.Containers.Multiway_Trees.Node_Count returns 1 for an empty tree
     -- (because there is always at least a root node).

   ----------------------------------------------------------------------------

   function Event_Vector_Length
     (Obj : Test_Reporter_Data) return Natural
     is (Natural (Obj.Event_Vector.Length));

   ----------------------------------------------------------------------------

   function Node_Event_Count (Obj      : Test_Reporter_Data;
                              Node_Tag : Tag) return Natural is

      Ret : Natural := 0;

   begin

      for E of Obj.Node_Data_Tree loop

         if E.T = Node_Tag then

            Ret := Natural (E.Event_Index_Vector.Length);

            exit;
         end if;
      end loop;

      return Ret;

   end Node_Event_Count;

   ----------------------------------------------------------------------------

   procedure To_Arrays (Obj           :     Test_Reporter_Data;
                        Node_Tag_Tree : out Node_Tag_Tree_As_Array;
                        Event_Data    : out Event_Data_Array) is

      K_Node_Tag_Tree : Natural := 0;
      K_Event_Data    : Natural := 0;

      -----------------------------------------------------

      procedure Process_Node_Tag (Position : Cursor) is

      begin

         K_Node_Tag_Tree := K_Node_Tag_Tree + 1;

         declare
            A : Node_Tag_W_Parent_Index renames Node_Tag_Tree(K_Node_Tag_Tree);
         begin
            A.Node_Tag := Element (Position).T;
            A.Active   := Obj.Active_Node_Map.Contains (A.Node_Tag);
         end;

      end Process_Node_Tag;

      -----------------------------------------------------

      procedure Process_Parent_Index (Position : Cursor) is

         Parent_Position : constant Cursor := Parent (Position);

      begin

         K_Node_Tag_Tree := K_Node_Tag_Tree + 1;

         if not Is_Root (Parent_Position) then

            declare

               A : Node_Tag_W_Parent_Index
                 renames Node_Tag_Tree(K_Node_Tag_Tree);

               Parent_Tag : constant Tag     := Element (Parent_Position).T;
               K          :          Natural := K_Node_Tag_Tree;
               -- After the loops below, K should be the index in Node_Tag_Tree
               -- of the parent of the element at Position in the tree (or at
               -- index K_Node_Tag_Tree in Node_Tag_Tree). The search is
               -- started at K_Node_Tag_Tree because the tree traversal is
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
               while A.Parent_Index = 0 and then K < K_Node_Tag_Tree loop
                  Process;
               end loop;
               Ada.Assertions.Assert
                 (A.Parent_Index /= 0,
                  "Unable to build Node_Tag_Tree array");

            end;

         else
            Node_Tag_Tree(K_Node_Tag_Tree).Parent_Index := 0;
         end if;

      end Process_Parent_Index;

      -----------------------------------------------------

      procedure Process_Event_Data (Position : Node_Event_Vectors.Cursor) is

         use Ada.Exceptions,
             Apsepp.Calendar,
             Node_Event_Vectors;

         E : constant Test_Event_Access := Element (Position).Event;
         T : constant Tag := Element (Element (Position).Node_Data_Cursor).T;

      begin

         K_Event_Data := K_Event_Data + 1;

         declare
            A : Flattened_Event_Data renames Event_Data(K_Event_Data);
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
            A.R_Index            := (if E.Has_R_Index then
                                        E.R_Index
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

      Obj.Node_Data_Tree.Iterate (Process_Node_Tag'Access);

      K_Node_Tag_Tree := 0;
      Obj.Node_Data_Tree.Iterate (Process_Parent_Index'Access);

      Obj.Event_Vector.Iterate (Process_Event_Data'Access);

   end To_Arrays;

   ----------------------------------------------------------------------------

   procedure To_Node_Event_Array (Obj             :     Test_Reporter_Data;
                                  Node_Tag        :     Tag;
                                  Event_Data      :     Event_Data_Array;
                                  Node_Event_Data : out Event_Data_Array) is

      K : Natural := 0;

   begin

      for E of Obj.Node_Data_Tree loop

         if E.T = Node_Tag then

            for Index of E.Event_Index_Vector loop
               K                  := K + 1;
               Node_Event_Data(K) := Event_Data(Positive (Index));
            end loop;

            exit;
         end if;
      end loop;

   end To_Node_Event_Array;

   ----------------------------------------------------------------------------

   function Lastest_R
     (Obj   : Test_Reporter_Data;
      E_I_V : Event_Index_Vectors.Vector) return Test_Routine_Index is

      Ret : Test_Routine_Count := 0;

   begin

      for K of reverse E_I_V loop

         declare
            Ev : constant Test_Event_Access
              := Obj.Event_Vector.Element (K).Event;
         begin
            if Ev.Has_R_Index then
               Ret := Ev.R_Index;
               exit;
            end if;
         end;
      end loop;

      return Ret;

   end Lastest_R;

   ----------------------------------------------------------------------------

   function Latest_R_Index (Obj      : Test_Reporter_Data;
                            Node_Tag : Tag) return Test_Routine_Index is

      Ret : Test_Routine_Count := 0;

   begin

      for E of Obj.Node_Data_Tree loop

         if E.T = Node_Tag then

            Ret := Lastest_R (Obj, E.Event_Index_Vector);

            exit;
         end if;
      end loop;

      return Ret;

   end Latest_R_Index;

   ----------------------------------------------------------------------------

end Apsepp.Test_Reporter_Data_Struct_Class.Impl.Testing;
