-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Abstract_Early_Test_Case; use Apsepp.Abstract_Early_Test_Case;

package Apsepp_Test_Reporter_Class_Struct_Builder_Early_Test_Case is

   -- TODOC: This test case uses the same fixture as
   -- Apsepp_Test_Node_Class_Early_Test_Case.Apsepp_Test_Node_Class_E_T_C
   -- (simulation test cases run concurrently thanks to a special blocking test
   -- reporter using a "test node barrier") to benefit from the already coded
   -- traces. The test case could have been designed differently with a direct
   -- exercising of an instance of
   -- Apsepp.Test_Reporter_Class.Struct_Builder.Test_Reporter_Struct_Builder
   -- and in this case would have been a "normal" test case (not an "early"
   -- test case). <2019-09-08>
   type Apsepp_Test_Reporter_Class_Struct_Builder_E_T_C
     is limited new Early_Test_Case with null record;

   overriding
   function Early_Routine
     (Obj : Apsepp_Test_Reporter_Class_Struct_Builder_E_T_C)
     return Test_Routine;

end Apsepp_Test_Reporter_Class_Struct_Builder_Early_Test_Case;
