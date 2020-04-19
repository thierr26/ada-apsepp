-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

-- TODOC: Meant to be used in a loop, with statements like
-- 'H := Apsepp.Generic_Multiplicative_Hash (Key_Slice, H);', 'Key_Slice' being
-- each time a new slice of the actual key. <2020-04-15>
-- TODOC: The function internally declares an object of type 'Chunk_Array_Type'
-- at the address of 'Key_Slice' to get a view of 'Key_Slice' as an array of
-- objects of type 'Chunk_Type'. That's why the size of 'Key_Slice' must be a
-- multiple of the size of the components of 'Chunk_Array_Type' objects (see
-- the pre-condition). <2020-04-15>
-- TODOC: The range of 'Chunk_Type' must not be wider than the range of
-- 'Hash_Type'. <2020-04-15>
-- TODOC: If 'Multiply_Function = null', then 'Multiplier' is used as
-- multiplier and the multiplication is performed using operator "*". In the
-- opposite case, 'Multiplier' is ignored and 'Multiply_Function' is used
-- instead. It is supposed to multiply 'X' by a constant multiplier. The point
-- of this is to make it possible to use a multiplication implementation faster
-- than operator "*". No performance test of this Ada function has been done at
-- all. There are probably many causes for poor performance in the
-- implementation (even when not using "*"). <2020-04-15>
-- TODOC: Actual parameters suggestions:
-- * 'Chunk_Type': modulus '2 ** 8'.
-- * 'Hash_Type' : modulus '2 ** 32'.
-- * 'Multiplier': prime numbers 65599 or 65587. <2020-04-15>
-- REF: Ozan Yigit, hash function for SDBM:
-- http://mail-index.netbsd.org/tech-perform/2001/11/28/0023.html;
-- http://www.cs.yorku.ca/~oz/hash.html;
-- https://github.com/davidar/sdbm/blob/master/hash.c;
-- http://cvs.ossp.org/fileview?f=ossp-pkg/act/act_hash_fct.c;
-- https://github.com/gvlx/gawk/blob/master/str_array.c;
-- GNAT source file "s-strhas.adb". <2020-04-15>
generic

   type Key_Slice_Type is private;

   type Chunk_Type is mod <>;

   type Chunk_Index_Type is (<>);

   type Chunk_Array_Type is array (Chunk_Index_Type range <>) of Chunk_Type;

   type Hash_Type is mod <>;

   Multiplier : Hash_Type := 0;

   Multiply_Function : access function
                                (X : Hash_Type) return Hash_Type := null;

   Default_Initial_Value : Hash_Type := 0;

function Apsepp.Generic_Multiplicative_Hash
  (Key_Slice     : Key_Slice_Type;
   Initial_Value : Hash_Type      := Default_Initial_Value) return Hash_Type
  with Pre => Key_Slice_Type'Size rem Chunk_Array_Type'Component_Size = 0;
