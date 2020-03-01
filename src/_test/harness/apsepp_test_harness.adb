-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Output,
     Apsepp.Generic_Shared_Instance.Finalized_S_R_Dealloc,
     Apsepp.Test_Node_Class.Runner_Sequential.Create;

package body Apsepp_Test_Harness is

   ----------------------------------------------------------------------------

   procedure Apsepp_Test_Procedure is

      use Apsepp.Output,
          Apsepp.Test_Node_Class,
          Apsepp.Test_Node_Class.Runner_Sequential;

      Output_Lock_Holder : Output_Shared_Instance.Holder;

      Output_Instance_Access : constant Output_Standard_Access
        := (if Output_Lock_Holder.Holds then
               new Output_Standard
            else
               null);

      package Output_S_R is new Output_Shared_Instance.Finalized_S_R_Dealloc
        (Instance_Access      => Output_Instance_Access,
         Lock_Holder_Type     => Output_Shared_Instance.Holder,
         Lock_Holder_Instance => Output_Lock_Holder);

      Test_Runner : Test_Runner_Sequential := Create (Test_Suite'Access);

      Outcome : Test_Outcome;

      pragma Unreferenced (Output_S_R);

   begin

      Output.Put_Line ("Hello world!");

      Test_Runner.Early_Run;
      Test_Runner.Run (Outcome);
      Output.Put_Line (Test_Outcome'Image (Outcome));

   end Apsepp_Test_Procedure;

   ----------------------------------------------------------------------------

end Apsepp_Test_Harness;
