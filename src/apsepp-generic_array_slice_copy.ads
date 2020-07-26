-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

generic

   type Index_Type is (<>);

   type Element_Type is private;

   type Array_Type is array (Index_Type range <>) of Element_Type;

   type Diff_Type is range <>;

   Copy_First_Index : Index_Type := Index_Type'First;

function Apsepp.Generic_Array_Slice_Copy
  (A           : Array_Type;
   First, Last : Index_Type) return Array_Type;
