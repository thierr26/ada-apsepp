-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Command_Line;
with Apsepp.Test_Node_Class.Runner_Sequential.Create;

package body Apsepp_Test_Harness is

   ----------------------------------------------------------------------------

   procedure Apsepp_Test_Procedure is

      use Ada.Command_Line;
      use Apsepp.Test_Node_Class;
      use Apsepp.Test_Node_Class.Runner_Sequential;

      Test_Runner : constant Test_Runner_Sequential
        := Create (Scope_Bound_Locks_T_C'Access, Reporter'Access);

      Outcome : Test_Outcome;

   begin

      Test_Runner.Run (Outcome);
      Set_Exit_Status (if Outcome = Passed then
                          Success
                       else
                          Failure);

   end Apsepp_Test_Procedure;

   ----------------------------------------------------------------------------

end Apsepp_Test_Harness;
