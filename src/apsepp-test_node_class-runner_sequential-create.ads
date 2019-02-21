-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Test_Reporter_Class; use Apsepp.Test_Reporter_Class;

function Apsepp.Test_Node_Class.Runner_Sequential.Create
  (Root_Node_Access              : Test_Node_Access;
   Test_Reporter_Instance_Access : Test_Reporter_Access                := null;
   Reporter_Access_Set_CB        : Apsepp.Scope_Bound_Locks.SB_Lock_CB := null)
  return Test_Runner_Sequential;
