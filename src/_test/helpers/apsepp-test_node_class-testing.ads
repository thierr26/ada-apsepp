-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Generic_Array_Operations,
     Apsepp.Tags;

package Apsepp.Test_Node_Class.Testing is

   type Flattened_Routine_State is record
      Routine_I : Test_Routine_Count;
      Assert_C  : Test_Assert_Count;
      Assert_O  : Test_Outcome;
      T         : Tag;
   end record;

   function "<" (Left, Right : Flattened_Routine_State) return Boolean
     is (Tags."<" (Left.T, Right.T));

   type Routine_State_Array
     is array (Positive range <>) of Flattened_Routine_State;

   package Flattened_Routine_State_Array_Operations
     is new Generic_Array_Operations (Index_Type   => Positive,
                                      Element_type => Flattened_Routine_State,
                                      Array_Type   => Routine_State_Array,
                                      "<"          => "<");

   use Flattened_Routine_State_Array_Operations;

   function To_Array return Routine_State_Array

     with Post => To_Array'Result'First = 1
                    and then
                  Monotonic_Incr (To_Array'Result(2 .. To_Array'Result'Last));

private

   function To_Flattened_Routine_State
     (T_R_S : Tag_Routine_State) return Flattened_Routine_State
     is (T         => T_R_S.T,
         Routine_I => T_R_S.S.Routine_Index,
         Assert_C  => Prot_Test_Assert_Count.Val (T_R_S.S.Assert_Count),
         Assert_O  => T_R_S.S.Assert_Outcome);

end Apsepp.Test_Node_Class.Testing;
