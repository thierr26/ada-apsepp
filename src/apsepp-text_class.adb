-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Generic_Multiplicative_Hash;

package body Apsepp.Text_Class is

   use all type Safe_Character_Count_Operations.Safe_Integer;

   -- The following declaration ensures that the range of 'Character_Count' is
   -- at least as wide as the range of 'Text_Line_Count'. (In the opposite
   -- case, the compiler would reject it.)
   subtype Character_Count_Line_Count_Constrained
     is Character_Count range Character_Count (Text_Line_Count'First)
                                ..
                              Character_Count (Text_Line_Count'Last);

   ----------------------------------------------------------------------------

   function Hash (A             : Character_Array;
                  Initial_Value : Text_Hash       := 0) return Text_Hash is

      Multiplier : constant Text_Hash := 65599;

      type Chunk is mod 2 ** 8; -- Range 0 to 255.

      type Chunk_Array is array (Character_Index range <>) of Chunk;

      Function H_1 is new Generic_Multiplicative_Hash
        (Key_Slice_Type   => Character,
         Chunk_Type       => Chunk,
         Chunk_Index_Type => Character_Index,
         Chunk_Array_Type => Chunk_Array,
         Hash_Type        => Text_Hash,
         Multiplier       => Multiplier);

      subtype Character_Array_16 is Character_Array (1 .. 16);

      Function H_16 is new Generic_Multiplicative_Hash
        (Key_Slice_Type   => Character_Array_16,
         Chunk_Type       => Chunk,
         Chunk_Index_Type => Character_Index,
         Chunk_Array_Type => Chunk_Array,
         Hash_Type        => Text_Hash,
         Multiplier       => Multiplier);

      H_16_Call_Count : constant Character_Count
        := A'Length / Character_Array_16'Length;

      Slice_First : Character_Index := A'First;
      Slice_Last  : Character_Index;

      Ret : Text_Hash := Initial_Value;

   begin

      for K in 1 .. H_16_Call_Count loop
         Slice_Last  := Slice_First - 1 + Character_Array_16'Length;
         Ret         := H_16 (A(Slice_First .. Slice_Last), Ret);
         Slice_First := Slice_Last + 1;
      end loop;

      for C of A(Slice_First .. A'Last) loop
         Ret := H_1 (C, Ret);
      end loop;

      return Ret;

   end Hash;

   ----------------------------------------------------------------------------

   function EOL_Weight (Line_Count       : Text_Line_Count;
                        EOL              : EOL_Kind;
                        Include_Last_EOL : Boolean)
     return Safe_Character_Count_Operations.Natural_Safe_Integer
     is (
          Create (
                   Character_Count_Line_Count_Constrained
                     (Line_Count
                        -
                      (if Include_Last_EOL or else Line_Count = 0 then
                          0
                       else
                          1))
                 )
            *
          Create (EOL_Length (EOL))
        );

   ----------------------------------------------------------------------------

   function Text_Weight
     (Character_Length : Character_Count;
      EOL_Weight       : Safe_Character_Count_Operations.Natural_Safe_Integer)
     return Safe_Character_Count_Operations.Natural_Safe_Integer
     is (Inc (EOL_Weight, Character_Length));

   ----------------------------------------------------------------------------

   function To_String_Truncates
     (Obj              : Text_Interfa'Class;
      EOL              : EOL_Kind           := LF;
      Include_Last_EOL : Boolean            := False) return Boolean is

      W : constant Safe_Character_Count_Operations.Safe_Integer
        := Text_Weight (Obj.Character_Length,
                        EOL_Weight (Obj.Line_Count,
                                    EOL,
                                    Include_Last_EOL));

      use Safe_Character_Count_Natural_Conversions;

   begin

      return Sat (W) or else Is_Over (W);

   end To_String_Truncates;

   ----------------------------------------------------------------------------

   function To_String_Length
     (Obj              : Text_Interfa'Class;
      EOL              : EOL_Kind           := LF;
      Include_Last_EOL : Boolean            := False) return Natural is

   begin

      if Obj.To_String_Truncates then
         return Natural'Last; -- Early return.
      end if;

      -- We get there only if 'not Obj.To_String_Truncates', which ensures that
      -- the type conversion below won't raise 'Constraint_Error'.

      return Natural (Val (Text_Weight (Obj.Character_Length,
                                        EOL_Weight (Obj.Line_Count,
                                                    EOL,
                                                    Include_Last_EOL))));

   end To_String_Length;

   ----------------------------------------------------------------------------

end Apsepp.Text_Class;
