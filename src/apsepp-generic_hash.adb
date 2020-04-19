-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Generic_Discrete_Operations;

function Apsepp.Generic_Hash
  (Key_Slice     : Key_Slice_Type;
   Initial_Value : Hash_Type      := Default_Initial_Value) return Hash_Type is

   package Chunk_Index_Discrete_Operations
     is new Generic_Discrete_Operations (Discrete_Type => Chunk_Index_Type,
                                         Diff_Type     => Integer);

   use Chunk_Index_Discrete_Operations;

   Chunk_Count : constant Natural
     := Key_Slice'Size / Chunk_Array_Type'Component_Size;

   First : constant Chunk_Index_Type := Chunk_Index_Type'First;
   Last  : constant Chunk_Index_Type := Val (Rk    => Chunk_Count,
                                             First => First);

   A : Chunk_Array_Type (First .. Last)
     with Address => Key_Slice'Address;

   Ret : Hash_Type := Initial_Value;

begin

   for Chunk of A loop

      Ret := Hash_Function (Chunk              => Chunk,
                            Current_Hash_Value => Ret);

   end loop;

   return Ret;

end Apsepp.Generic_Hash;
