-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

package body Apsepp.Test_Node_Class.Private_Test_Reporter is

   ----------------------------------------------------------------------------

   procedure Run_Instance_Process_Primitive is

   begin

      Shared_Instance.Instance.Process;

   end Run_Instance_Process_Primitive;

   ----------------------------------------------------------------------------

   function Test_Reporter return Test_Reporter_Access
     is (Test_Reporter_Access (Shared_Instance_Fallback_Switch.Instance_FS));

   ----------------------------------------------------------------------------

end Apsepp.Test_Node_Class.Private_Test_Reporter;
