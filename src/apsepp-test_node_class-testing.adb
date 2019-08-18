-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

package body Apsepp.Test_Node_Class.Testing is

   ----------------------------------------------------------------------------

   function To_Array return Routine_State_Array is

      A : constant Tag_Routine_State_Array
        := Routine_State_Map_Handler.To_Array;

      Ret : Routine_State_Array (Positive (A'First)
                                   ..
                                 Natural (A'Last));

   begin

      for K in Ret'Range loop

         declare

            Elem : constant Flattened_Routine_State
              := To_Flattened_Routine_State (A(Index_Type (K)));

         begin

            if K = Ret'First then
               Ret(K) := Elem;
            else
               Insert_Incr (Ret(Ret'First + 1 .. K), K, Elem);
            end if;

         end;

      end loop;

      return Ret;

   end To_Array;

   ----------------------------------------------------------------------------

end Apsepp.Test_Node_Class.Testing;
