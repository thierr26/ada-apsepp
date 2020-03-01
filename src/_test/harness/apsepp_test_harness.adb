-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Command_Line,
     Apsepp.Test_Node_Class.Runner_Sequential.Create;

package body Apsepp_Test_Harness is

   ----------------------------------------------------------------------------

   procedure Apsepp_Test_Procedure is

      use Ada.Command_Line,
          Apsepp.Test_Node_Class.Runner_Sequential;

      use all type Apsepp.Test_Node_Class.Test_Outcome;

      Test_Runner : Test_Runner_Sequential := Create (Test_Suite'Access);

      Outcome : Apsepp.Test_Node_Class.Test_Outcome;

   begin

      Test_Runner.Early_Run;
      Test_Runner.Run (Outcome);
      Set_Exit_Status (if Outcome = Passed then
                          Success
                       else
                          Failure);

   end Apsepp_Test_Procedure;

   ----------------------------------------------------------------------------

end Apsepp_Test_Harness;
