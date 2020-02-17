-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Generic_Shared_Instance;
with Apsepp.Scope_Bound_Locking;      use Apsepp.Scope_Bound_Locking;
with Apsepp.Test_Reporter_Class.Sink; use Apsepp.Test_Reporter_Class.Sink;
  use Apsepp.Test_Reporter_Class;

private package Apsepp.Test_Node_Class.Private_Test_Reporter is

   Test_Reporter_Fallback_Instance : aliased Test_Reporter_Sink;

   package Test_Reporter_Shared_Instance is new Generic_Shared_Instance
     (Instance_Ancestor_Type   => Test_Reporter_Interfa,
      Lock_Type                => Lock,
      Fallback_Instance_Access => Test_Reporter_Fallback_Instance'Access);

   function Test_Reporter return not null access Test_Reporter_Interfa'Class;

end Apsepp.Test_Node_Class.Private_Test_Reporter;
