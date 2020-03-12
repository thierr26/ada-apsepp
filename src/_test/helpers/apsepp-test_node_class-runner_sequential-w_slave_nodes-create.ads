-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Test_Node_Class.Abstract_Test_Suite;
  use Apsepp.Test_Node_Class.Abstract_Test_Suite;

with Apsepp.Test_Reporter_Class; use Apsepp.Test_Reporter_Class;

function Apsepp.Test_Node_Class.Runner_Sequential.W_Slave_Nodes.Create
  (Root_Test_Node_Access         : not null access Test_Node_Interfa'Class;
   Test_Reporter_Instance_Access : access Test_Reporter_Interfa'Class := null;
   Slaves                        : Test_Node_Array)
  return Test_Runner_Sequential_W_Slave_Nodes;
