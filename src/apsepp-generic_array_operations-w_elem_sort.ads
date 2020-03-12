-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

generic

   with function "<" (Left, Right : Element_Type) return Boolean is <>;

package Apsepp.Generic_Array_Operations.W_Elem_Sort is

   function Monotonic_Incr (A      : Array_Type;
                            Strict : Boolean    := True) return Boolean
     is (for all K in First_Succ (A) .. A'Last =>
          Pred (A, K) < A(K) or else (not Strict
                                        and then
                                      Pred (A, K) = A(K)));

   procedure Insert_Incr (A                : in out Array_Type;
                          Max_Insert_Index :        Index_Type;
                          Elem             :        Element_Type;
                          Search_From_Last :        Boolean      := False)
     with Pre  => Monotonic_Incr (A      => A(A'First .. Last_Pred (A)),
                                  Strict => False)
                    and then
                  Max_Insert_Index in A'Range,
          Post => Monotonic_Incr (A      => A(A'First .. Max_Insert_Index),
                                  Strict => False);

end Apsepp.Generic_Array_Operations.W_Elem_Sort;
