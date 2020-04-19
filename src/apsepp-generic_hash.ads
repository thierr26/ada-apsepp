-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

-- TODOC: Meant to be used in a loop, with statements like
-- 'H := Apsepp.Generic_Hash (Key_Slice, H);', 'Key_Slice' being each time a
-- new slice of the actual key. <2020-04-15>
-- TODOC: The function internally declares an object of type 'Chunk_Array_Type'
-- at the address of 'Key_Slice' to get a view of 'Key_Slice' as an array of
-- objects of type 'Chunk_Type'. That's why the size of 'Key_Slice' must be a
-- multiple of the size of the components of 'Chunk_Array_Type' objects (see
-- the pre-condition). <2020-04-15>
generic

   type Key_Slice_Type is private;

   type Chunk_Type is private;

   type Chunk_Index_Type is (<>);

   type Chunk_Array_Type is array (Chunk_Index_Type range <>) of Chunk_Type;

   type Hash_Type is private;

   with function Hash_Function
     (Chunk              : Chunk_Type;
      Current_Hash_Value : Hash_Type) return Hash_Type;

   Default_Initial_Value : Hash_Type;

function Apsepp.Generic_Hash
  (Key_Slice     : Key_Slice_Type;
   Initial_Value : Hash_Type      := Default_Initial_Value) return Hash_Type
  with Pre => Key_Slice'Size rem Chunk_Array_Type'Component_Size = 0;
