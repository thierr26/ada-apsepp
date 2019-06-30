-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Generic_Shared_Instance.Fallback_Switch;
with Apsepp.Test_Reporter_Class.Stub.Create; use Apsepp.Test_Reporter_Class;

private package Apsepp.Test_Node_Class.Private_Test_Reporter is

   procedure Run_Instance_Process_Primitive;

   package Shared_Instance is new Generic_Shared_Instance
     (Instance_Ancestor_Type => Test_Reporter_Interfa,
      Unlock_CB              => Run_Instance_Process_Primitive'Access);

   function Test_Reporter return Test_Reporter_Access;

private

   use Test_Reporter_Class.Stub;

   -- TODO: Consider removing Test_Reporter_Stub creational function.
   -- <2019-06-08>
   Fallback_Instance : aliased Test_Reporter_Stub := Create;

   package Shared_Instance_Fallback_Switch
     is new Shared_Instance.Fallback_Switch (Fallback_Instance'Access);

end Apsepp.Test_Node_Class.Private_Test_Reporter;
