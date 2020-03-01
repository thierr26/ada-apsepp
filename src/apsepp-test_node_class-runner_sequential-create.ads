-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Test_Reporter_Class; use Apsepp.Test_Reporter_Class;

function Apsepp.Test_Node_Class.Runner_Sequential.Create
  (Root_Test_Node_Access         : not null access Test_Node_Interfa'Class;
   Test_Reporter_Instance_Access : access Test_Reporter_Interfa'Class := null)
  return Test_Runner_Sequential;
