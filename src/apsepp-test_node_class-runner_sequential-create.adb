-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Test_Node_Class.Suite_Stub.Create;

function Apsepp.Test_Node_Class.Runner_Sequential.Create
  (Root_Node_Access              : Test_Node_Access;
   Test_Reporter_Instance_Access : Test_Reporter_Access                := null;
   Reporter_Access_Set_CB        : Apsepp.Scope_Bound_Locks.SB_Lock_CB := null)
  return Test_Runner_Sequential is

begin

   return (Apsepp.Test_Node_Class.Suite_Stub.Create
             with Child_Access    => Root_Node_Access,
                  Reporter_Access => Test_Reporter_Instance_Access,
                  R_A_S_CB        => Reporter_Access_Set_CB);

end Apsepp.Test_Node_Class.Runner_Sequential.Create;
