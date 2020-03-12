-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Tags; use Ada.Tags;

with Apsepp.Generic_Array_Operations;

package body Apsepp.Test_Node_Class is

   ----------------------------------------------------------------------------

   type Child_Tag_Array is array (Test_Node_Index range <>) of Tag;

   package Child_Tag_Array_Operations
     is new Generic_Array_Operations (Index_Type   => Test_Node_Index,
                                      Element_Type => Tag,
                                      Array_Type   => Child_Tag_Array);

   function Has_No_Children_W_Same_Tags
     (Obj : Test_Node_Interfa'Class) return Boolean is

      A : Child_Tag_Array (1 .. Obj.Child_Count);

   begin

      for K in A'Range loop
         A(K) := Obj.Child (K)'Tag;
      end loop;

      return Child_Tag_Array_Operations.No_Duplicates (A);

   end Has_No_Children_W_Same_Tags;

   ----------------------------------------------------------------------------

end Apsepp.Test_Node_Class;
