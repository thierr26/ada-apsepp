-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Generic_Hash;

function Apsepp.Generic_Multiplicative_Hash
  (Key_Slice     : Key_Slice_Type;
   Initial_Value : Hash_Type      := Default_Initial_Value) return Hash_Type is

      -----------------------------------------------------

   function Multiplier_Star (X : Hash_Type) return Hash_Type
     is (Multiplier * X);

      -----------------------------------------------------

   Multiply
     : constant not null access function (X : Hash_Type) return Hash_Type
     := (if Multiply_Function = null then
            Multiplier_Star'Access
         else
            Multiply_Function);

   subtype Hash_Type_Constrained_Chunk_Type
     is Hash_Type range Hash_Type (Chunk_Type'First)
                          ..
                        Hash_Type (Chunk_Type'Last);

   function Hash_Function
     (Chunk              : Chunk_Type;
      Current_Hash_Value : Hash_Type) return Hash_Type
     is (Hash_Type_Constrained_Chunk_Type (Chunk)
           +
         Multiply (Current_Hash_Value));

   function Hash is new Generic_Hash
     (Key_Slice_Type        => Key_Slice_Type,
      Chunk_Type            => Chunk_Type,
      Chunk_Index_Type      => Chunk_Index_Type,
      Chunk_Array_Type      => Chunk_Array_Type,
      Hash_Type             => Hash_Type,
      Hash_Function         => Hash_Function,
      Default_Initial_Value => Default_Initial_Value);

begin

   return Hash (Key_Slice, Initial_Value);

end Apsepp.Generic_Multiplicative_Hash;
