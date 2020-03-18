-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Tags;

private with Ada.Containers.Generic_Array_Sort,
             Apsepp.Generic_Discrete_Operations.Is_Monotonic_Incr_Lim_Array;

package Apsepp.Test_Node_Class.Abstract_Test_Case.Case_Status_Array is

   type Flat_Tag_Case_Status is record

      T : Tag;

      Routine_I : Test_Routine_Count;

      Assert_C : Test_Assert_Count;

      Assert_O : Test_Outcome;

   end record;

   type Flat_Tag_Case_Status_Array
     is array (Positive range <>) of Flat_Tag_Case_Status;

   function Case_Status_Array return Flat_Tag_Case_Status_Array
     with Post => Flat_Tag_Case_Status_Array_Invariant
                    (Case_Status_Array'Result);

   function Flat_Tag_Case_Status_Array_Invariant
     (X : Flat_Tag_Case_Status_Array) return Boolean;

private

   function "<" (Left, Right : Flat_Tag_Case_Status) return Boolean
     is (Tags."<" (Left.T, Right.T));

   package Positive_Operations is new Generic_Discrete_Operations
     (Discrete_Type => Positive,
      Diff_Type     => Integer);

   function Flat_Tag_Case_Status_As_Is
     (X : Flat_Tag_Case_Status) return Flat_Tag_Case_Status
     is (X);

   function Is_Monotonic_Incr
     is new Positive_Operations.Is_Monotonic_Incr_Lim_Array
     (Element_Type          => Flat_Tag_Case_Status,
      Array_Type            => Flat_Tag_Case_Status_Array,
      Element_Func_Ret_Type => Flat_Tag_Case_Status,
      Element_Func          => Flat_Tag_Case_Status_As_Is);

   procedure Sort is new Ada.Containers.Generic_Array_Sort
     (Index_Type   => Positive,
      Element_Type => Flat_Tag_Case_Status,
      Array_Type   => Flat_Tag_Case_Status_Array);

end Apsepp.Test_Node_Class.Abstract_Test_Case.Case_Status_Array;
