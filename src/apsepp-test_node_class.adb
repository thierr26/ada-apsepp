-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Tags; use Ada.Tags;

with Apsepp.Generic_Discrete_Operations.Is_Lim_Array_Wo_Dup;

package body Apsepp.Test_Node_Class is

   ----------------------------------------------------------------------------

   function Has_No_Children_W_Same_Tags
     (Obj : Test_Node_Interfa'Class) return Boolean is

      type Child_Tag_Array is array (Test_Node_Index range <>) of Tag;

      A : Child_Tag_Array (1 .. Obj.Child_Count);

      function As_Is (X : Tag) return Tag
        is (X);

      function No_Duplicates
        is new Test_Node_Index_Operations.Is_Lim_Array_Wo_Dup
        (Element_Type          => Tag,
         Array_Type            => Child_Tag_Array,
         Element_Func_Ret_Type => Tag,
         Element_Func          => As_Is);

   begin

      -- Build an array of tags containing the values of the test node children
      -- tags.
      for K in A'Range loop
         A(K) := Obj.Child (K)'Tag;
      end loop;

      return No_Duplicates (A);

   end Has_No_Children_W_Same_Tags;

   ----------------------------------------------------------------------------

end Apsepp.Test_Node_Class;
