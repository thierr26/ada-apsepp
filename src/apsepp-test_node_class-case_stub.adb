-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

package body Apsepp.Test_Node_Class.Case_Stub is

   ----------------------------------------------------------------------------

   overriding
   function Routine (Obj : Test_Case_Stub;
                     K   : Test_Routine_Index) return Test_Routine
     is (Null_Test_Routine'Access);

   ----------------------------------------------------------------------------

   overriding
   procedure Run (Obj     : in out Test_Case_Stub;
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

   T_C_S : aliased Test_Case_Stub;

   overriding
   function Child (Obj : Test_Case_Stub;
                   K   : Test_Node_Index) return Test_Node_Access
     is (T_C_S'Access); -- Raises because of class-wide pre-condition violation
                        -- (K <= Obj.Child_Count).

   ----------------------------------------------------------------------------

end Apsepp.Test_Node_Class.Case_Stub;
