-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package body Apsepp.Test_Node_Class.Case_Root is

   ----------------------------------------------------------------------------

   overriding
   function Routine
     (Obj : Test_Case_Root;
      K   : Test_Routine_Index) return not null access procedure
     is (Null_Test_Routine'Access);

   ----------------------------------------------------------------------------

   overriding
   procedure Run (Obj     : in out Test_Case_Root;
                  Outcome :    out Test_Outcome;
                  Kind    :        Run_Kind       := Assert_Cond_And_Run_Test)
     is

      -----------------------------------------------------

      function Cond return Boolean
        is (True);

      -----------------------------------------------------

   begin

      Run_Body (Obj, Outcome, Kind, Cond'Access);

   end Run;

   ----------------------------------------------------------------------------

   T_C_S : aliased Test_Case_Root;

   overriding
   function Child (Obj : Test_Case_Root;
                   K   : Test_Node_Index)
     return not null access Test_Node_Interfa'Class
     is (T_C_S'Access); -- Raises because of class-wide pre-condition violation
                        -- (K <= Obj.Child_Count).

   ----------------------------------------------------------------------------

end Apsepp.Test_Node_Class.Case_Root;
