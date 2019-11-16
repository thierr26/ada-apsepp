-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Tags;                        use Ada.Tags;
with Apsepp.Test_Node_Class.Testing;  use Apsepp.Test_Node_Class.Testing;
with Apsepp.Abstract_Early_Test_Case; use Apsepp.Abstract_Early_Test_Case;

package Apsepp_Test_Node_Class_Early_Test_Case is

   function Expected_Routine_State_Array return Routine_State_Array;

   function Routine_State_Array_To_Tag_Array
     (A : Routine_State_Array) return Tag_Array

     with Post => Routine_State_Array_To_Tag_Array'Result'First = A'First
                    and then
                  Routine_State_Array_To_Tag_Array'Result'Length = A'Length
                    and then
                  (for all K in A'Range
                    => Routine_State_Array_To_Tag_Array'Result(K) = A(K).T);

   type Apsepp_Test_Node_Class_E_T_C
     is limited new Early_Test_Case with null record;

   overriding
   function Early_Routine
     (Obj : Apsepp_Test_Node_Class_E_T_C) return Test_Routine;

end Apsepp_Test_Node_Class_Early_Test_Case;
