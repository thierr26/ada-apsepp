-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Test_Node_Class.Runner_Sequential.Create;

function Apsepp.Test_Node_Class.Runner_Sequential.W_Slave_Nodes.Create
  (Root_Node_Access              : Test_Node_Access;
   Test_Reporter_Instance_Access : Test_Reporter_Access         := null;
   Reporter_Access_Set_CB        : Scope_Bound_Locks.SB_Lock_CB := null;
   Slaves                        : Test_Node_Array)
  return Test_Runner_Sequential_W_Slave_Tasks is

   use Ada.Finalization;

   Sla : constant Test_Node_Array_Access := new Test_Node_Array'(Slaves);

begin

   return (Apsepp.Test_Node_Class.Runner_Sequential.Create (Root_Node_Access,
                                          Test_Reporter_Instance_Access,
                                          Reporter_Access_Set_CB)
             with Slaves => (Limited_Controlled with A => Sla));

end Apsepp.Test_Node_Class.Runner_Sequential.W_Slave_Nodes.Create;
