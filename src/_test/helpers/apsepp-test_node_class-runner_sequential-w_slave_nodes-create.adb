-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Test_Node_Class.Runner_Sequential.Create;

function Apsepp.Test_Node_Class.Runner_Sequential.W_Slave_Nodes.Create
  (Root_Test_Node_Access         : not null access Test_Node_Interfa'Class;
   Test_Reporter_Instance_Access : access Test_Reporter_Interfa'Class := null;
   Slaves                        : Test_Node_Array)
  return Test_Runner_Sequential_W_Slave_Nodes is

   Sla : constant Test_Node_Array_Access := new Test_Node_Array'(Slaves);

   use Ada.Finalization;

begin

   return (Create (Root_Test_Node_Access,
                   Test_Reporter_Instance_Access)
             with Slaves => (Limited_Controlled with A => Sla));

end Apsepp.Test_Node_Class.Runner_Sequential.W_Slave_Nodes.Create;
