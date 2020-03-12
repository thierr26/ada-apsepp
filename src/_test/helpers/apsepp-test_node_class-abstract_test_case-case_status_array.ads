-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Tags,
     Apsepp.Generic_Array_Operations.W_Elem_Sort;

package Apsepp.Test_Node_Class.Abstract_Test_Case.Case_Status_Array is

   type Flat_Tag_Case_Status is record

      T : Tag;

      Routine_I : Test_Routine_Count;

      Assert_C : Test_Assert_Count;

      Assert_O : Test_Outcome;

   end record;

   function "<" (Left, Right : Flat_Tag_Case_Status) return Boolean
     is (Tags."<" (Left.T, Right.T));

   type Flat_Tag_Case_Status_Array
     is array (Positive range <>) of Flat_Tag_Case_Status;

   package Flat_Tag_Case_Status_Array_Operations
     is new Apsepp.Generic_Array_Operations
       (Index_Type   => Positive,
        Element_Type => Flat_Tag_Case_Status,
        Array_Type   => Flat_Tag_Case_Status_Array);

   package Flat_Tag_Case_Status_Array_Operations_W_Elem_Sort
     is new Flat_Tag_Case_Status_Array_Operations.W_Elem_Sort;

   use Flat_Tag_Case_Status_Array_Operations_W_Elem_Sort;

   function Case_Status_Array return Flat_Tag_Case_Status_Array
     with Post => Case_Status_Array'Result'First = 1
                    and then
                  Monotonic_Incr
                    (Case_Status_Array'Result(2
                                                ..
                                              Case_Status_Array'Result'Last));

end Apsepp.Test_Node_Class.Abstract_Test_Case.Case_Status_Array;
