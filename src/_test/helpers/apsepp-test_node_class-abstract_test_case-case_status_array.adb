-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Logical_Array_Instance.Assertions_W_Debug_Trace;

package body Apsepp.Test_Node_Class.Abstract_Test_Case.Case_Status_Array is

   ----------------------------------------------------------------------------

   function Flat_Tag_Case_Status_Array_Invariant
     (X : Flat_Tag_Case_Status_Array) return Boolean is

      use Logical_Array_Instance.Assertions_W_Debug_Trace
            .L_A_Assertions_W_Debug_Trace;

   begin

      return All_True((X'First = 1,
                       Is_Monotonic_Incr (X(2 .. X'Last))));

   end Flat_Tag_Case_Status_Array_Invariant;

   ----------------------------------------------------------------------------

   function Case_Status_Array return Flat_Tag_Case_Status_Array is

      A : constant Case_Tag_Status_Array := Case_Status_Map_Handler.To_Array;

      Ret : Flat_Tag_Case_Status_Array (Positive (A'First) -- 1.
                                          ..
                                        Natural (A'Last));

      use Safe_Test_Assert_Count_Operations;

   begin

      for K in Ret'Range loop

         Ret(K) := (T         => A(Test_Node_Index (K)).T,
                    Routine_I => A(Test_Node_Index (K)).S.Routine_Index,
                    Assert_C  => Val (A(Test_Node_Index (K)).S.Assert_Count),
                    Assert_O  => A(Test_Node_Index (K)).S.Assert_Outcome);

         Sort (Ret (Ret'First + 1 .. K)); -- The first element of the array is
                                          -- intentionally excluded from the
                                          -- sort.

      end loop;

      return Ret;

   end Case_Status_Array;

   ----------------------------------------------------------------------------

end Apsepp.Test_Node_Class.Abstract_Test_Case.Case_Status_Array;
