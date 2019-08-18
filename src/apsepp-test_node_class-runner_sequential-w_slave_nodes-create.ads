-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Test_Reporter_Class; use Apsepp.Test_Reporter_Class;
with Apsepp.Scope_Bound_Locks;   use Apsepp.Scope_Bound_Locks;

function Apsepp.Test_Node_Class.Runner_Sequential.W_Slave_Nodes.Create
  (Root_Node_Access              : Test_Node_Access;
   Test_Reporter_Instance_Access : Test_Reporter_Access         := null;
   Reporter_Access_Set_CB        : Scope_Bound_Locks.SB_Lock_CB := null;
   Slaves                        : Test_Node_Array)
  return Test_Runner_Sequential_W_Slave_Tasks;
