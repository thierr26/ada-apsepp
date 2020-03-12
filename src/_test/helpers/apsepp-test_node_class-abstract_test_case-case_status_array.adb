-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package body Apsepp.Test_Node_Class.Abstract_Test_Case.Case_Status_Array is

   ----------------------------------------------------------------------------

   function Case_Status_Array return Flat_Tag_Case_Status_Array is

      A : constant Case_Tag_Status_Array := Case_Status_Map_Handler.To_Array;

      Ret : Flat_Tag_Case_Status_Array (Positive (A'First) -- 1.
                                          ..
                                        Natural (A'Last));

      use Flat_Tag_Case_Status_Array_Operations;

   begin

      for K in Ret'Range loop

         declare

            use Safe_Test_Assert_Count_Operations;

            Elem : constant Flat_Tag_Case_Status
              := (T         => A(Test_Node_Index (K)).T,
                  Routine_I => A(Test_Node_Index (K)).S.Routine_Index,
                  Assert_C  => Val (A(Test_Node_Index (K)).S.Assert_Count),
                  Assert_O  => A(Test_Node_Index (K)).S.Assert_Outcome);

         begin

            if K = Ret'First then

               -- Make sure the first element of 'A' is placed at first
               -- position in 'Ret'.
               Ret(K) := Elem;

            else

               -- Insert other elements (with sorting) in 'Ret' slice starting
               -- at element 2.
               Insert_Incr (Ret(First_Succ (Ret) .. K), K, Elem);

            end if;

         end;

      end loop;

      return Ret;

   end Case_Status_Array;

   ----------------------------------------------------------------------------

end Apsepp.Test_Node_Class.Abstract_Test_Case.Case_Status_Array;
