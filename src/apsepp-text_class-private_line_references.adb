-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Unchecked_Deallocation,
     Apsepp.Barrier_Class.Finalized_Handler,
     Apsepp.Generic_Lexicographic_Comparisons.Discrete,
     Apsepp.Generic_Lexicographic_Comparisons.Lower_Than;

package body Apsepp.Text_Class.Private_Line_References is

   ----------------------------------------------------------------------------

   function "<" (Left, Right : Line_Copy_Key) return Boolean is

      type Compar_Level is (Compar_Index_Text_Address,
                            Compar_Index_Index,
                            Compar_Index_Hash);

      package Lexi_Compar
        is new Generic_Lexicographic_Comparisons (Index_Type => Compar_Level);

      function Compar_Index is new Lexi_Compar.Discrete
        (Discrete_Type => Text_Line_Index);

      function Compar_Hash is new Lexi_Compar.Discrete
        (Discrete_Type => Text_Hash);

      function Lower_Than is new Lexi_Compar.Lower_Than;

      function Comparison
        (Index : Compar_Level) return Lexi_Compar.Comparison_Outcome
        is (case Index is
               when Compar_Index_Text_Address =>
                  (if Left.Text_Address = Right.Text_Address then
                      Lexi_Compar.EQ
                   elsif Left.Text_Address < Right.Text_Address then
                      Lexi_Compar.LT
                   else
                      Lexi_Compar.GT),
               when Compar_Index_Index        =>
                  Compar_Index (Left.Index, Right.Index),
               when Compar_Index_Hash         =>
                  Compar_Hash (Left.Hash, Right.Hash));

   begin

      return Lower_Than (Comparison'Access,
                         Compar_Index_Text_Address,
                         Compar_Index_Hash);

   end "<";

   ----------------------------------------------------------------------------

   function Text_Address_From_Key
     (Key : Line_Copy_Key) return System.Address
     is (Key.Text_Address);

   ----------------------------------------------------------------------------

   function Line_Index_From_Key
     (Key : Line_Copy_Key) return Text_Line_Index
     is (Key.Index);

   ----------------------------------------------------------------------------

   function Is_In_Set (Key : Line_Copy_Key) return Boolean
     is (L_C_R_Ordered_Sets_Generic_Keys.Contains (Container => L_C_R_Set,
                                                   Key       => Key));

   ----------------------------------------------------------------------------

   function Key (L_C : Line_Copy) return Line_Copy_Key
     is (L_C.Key);

   ----------------------------------------------------------------------------

   not overriding
   function "<" (Left, Right : Line_Copy) return Boolean
     is (Left.Key < Right.Key);

   ----------------------------------------------------------------------------

   function Line_Copy_Line_Access
     (L_C : Line_Copy) return Character_Array_Access
     is (L_C.A);

   ----------------------------------------------------------------------------

   procedure Get_Line_Copy (Text :     Text_Interfa'Class;
                            K    :     Text_Line_Index;
                            Data : out Line_Copy) is

      -- Instantiate a controlled barrier handler.
      use Barrier_Class.Finalized_Handler;
      H : Controlled_Barrier_Handler (L_C_R_Set_Protection_Barrier'Access);

      pragma Unreferenced (H);

      use Safe_Reference_Counter,
          L_C_R_Ordered_Sets,
          L_C_R_Ordered_Sets_Generic_Keys;

      Constant_A_Access : Constant_Character_Array_Access;
      First             : Character_Index;
      Last              : Character_Count;

      Key : Line_Copy_Key;

      Position : Cursor;

   begin

      Text.Get_Line_As_Access_And_Slice_Bounds (K     => K,
                                                A     => Constant_A_Access,
                                                First => First,
                                                Last  => Last);

      Key := (Text_Address => Text'Address,
              Index        => K,
              Hash         => Hash (Constant_A_Access(First .. Last)));

      Position := Find (Container => L_C_R_Set,
                        Key       => Key);

      if Position = No_Element then

         declare

            Ref_C : Safe_Counter; -- Initial value: 0.

         begin

            Inc (Ref_C); -- Incrementation to 1.

            Data := (Key   => Key,
                     Ref_C => Ref_C,
                     A     =>
                       new Character_Array'(Constant_A_Access(First .. Last)));

            L_C_R_Set.Insert (New_Item => Data);

         end;

      else

         declare

            procedure Process (L_C : in out Line_Copy) is
            begin
               Inc (L_C.Ref_C); -- Increment reference count.
               if Sat (L_C.Ref_C) then
                  null;
                  -- TODO: Issue one (and only one, warning level) log entry.
               end if;
            end Process;

         begin

            Update_Element_Preserving_Key (Container => L_C_R_Set,
                                           Position  => Position,
                                           Process   => Process'Access);

            Data := Element (Position);

         end;

      end if;

   end Get_Line_Copy;

   ----------------------------------------------------------------------------

   procedure Add_Line_Copy_Reference (Key : Line_Copy_Key) is

      -- Instantiate a controlled barrier handler.
      use Barrier_Class.Finalized_Handler;
      H : Controlled_Barrier_Handler (L_C_R_Set_Protection_Barrier'Access);

      pragma Unreferenced (H);

      use Safe_Reference_Counter,
          L_C_R_Ordered_Sets,
          L_C_R_Ordered_Sets_Generic_Keys;

      Position : constant Cursor := Find (Container => L_C_R_Set,
                                          Key       => Key);

      -----------------------------------------------------

      procedure Process (L_C : in out Line_Copy) is
      begin
         Inc (L_C.Ref_C); -- Increment reference count.
      end Process;

      -----------------------------------------------------

   begin

      Update_Element_Preserving_Key (Container => L_C_R_Set,
                                     Position  => Position,
                                     Process   => Process'Access);

   end Add_Line_Copy_Reference;

   ----------------------------------------------------------------------------

   procedure Remove_Line_Copy_Reference (Key : Line_Copy_Key) is

      -- Instantiate a controlled barrier handler.
      use Barrier_Class.Finalized_Handler;
      H : Controlled_Barrier_Handler (L_C_R_Set_Protection_Barrier'Access);

      pragma Unreferenced (H);

      use Safe_Reference_Counter,
          L_C_R_Ordered_Sets,
          L_C_R_Ordered_Sets_Generic_Keys;

      Position : Cursor := Find (Container => L_C_R_Set,
                                 Key       => Key);

      Deletion_Required : Boolean;

      -----------------------------------------------------

      procedure Process (L_C : in out Line_Copy) is
         procedure Free is new Ada.Unchecked_Deallocation
           (Object => Character_Array,
            Name   => Character_Array_Access);
      begin
         Dec (L_C.Ref_C); -- Decrement reference count.
         Deletion_Required := Val (L_C.Ref_C) = 0;
         if Deletion_Required then
            Free (L_C.A);
         end if;
      end Process;

      -----------------------------------------------------

   begin

      Update_Element_Preserving_Key (Container => L_C_R_Set,
                                     Position  => Position,
                                     Process   => Process'Access);

      if Deletion_Required then
         L_C_R_Set.Delete (Position);
      end if;

   end Remove_Line_Copy_Reference;

   ----------------------------------------------------------------------------

end Apsepp.Text_Class.Private_Line_References;
