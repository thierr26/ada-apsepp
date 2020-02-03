-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Scope_Bound_Locks;          use Apsepp.Scope_Bound_Locks;
with Apsepp.Test_Reporter_Class;
with Apsepp.Test_Node_Class.Suite_Stub; use Apsepp.Test_Node_Class.Suite_Stub;

package Apsepp.Test_Node_Class.Runner_Sequential is

   type Test_Runner_Sequential is limited new Test_Suite_Stub with private;

   overriding
   function Child_Count (Obj : Test_Runner_Sequential) return Test_Node_Count
     is (1);

   overriding
   function Child (Obj : Test_Runner_Sequential;
                   K   : Test_Node_Index) return Test_Node_Access;

   overriding
   function No_Subtasking (Obj : Test_Runner_Sequential) return Boolean
     is (True);

   overriding
   function Routine (Obj : Test_Runner_Sequential;
                     K   : Test_Routine_Index) return Test_Routine;

   overriding
   procedure Run
     (Obj     : in out Test_Runner_Sequential;
      Outcome :    out Test_Outcome;
      Kind    :        Run_Kind               := Assert_Cond_And_Run_Test);

private

   use Test_Reporter_Class;

   type Test_Runner_Sequential is limited new Test_Suite_Stub with record
      Child_Access         : Test_Node_Access;
      Reporter_Access      : Test_Reporter_Access;
      R_A_S_CB             : SB_Lock_CB;
   end record;

end Apsepp.Test_Node_Class.Runner_Sequential;
