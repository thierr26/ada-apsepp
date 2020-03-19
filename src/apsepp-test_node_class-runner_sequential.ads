-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Test_Node_Class.Abstract_Children_Early_Test_Handler;
  use Apsepp.Test_Node_Class.Abstract_Children_Early_Test_Handler;

private with Apsepp.Test_Reporter_Class;

package Apsepp.Test_Node_Class.Runner_Sequential is

   type Test_Runner_Sequential
     is limited new Children_Early_Test_Handler with private
     with Type_Invariant'Class =>
            Test_Runner_Sequential.Runner_Sequential_Invariant;

   overriding
   function Child_Count (Obj : Test_Runner_Sequential) return Test_Node_Count
     is (1);

   overriding
   function Child (Obj : Test_Runner_Sequential;
                   K   : Test_Node_Index)
     return not null access Test_Node_Interfa'Class;

   overriding
   function No_Subtasking (Obj : Test_Runner_Sequential) return Boolean
     is (True);

   overriding
   procedure Run
     (Obj     : in out Test_Runner_Sequential;
      Outcome :    out Test_Outcome;
      Kind    :        Run_Kind               := Assert_Cond_And_Run_Test);

   function Runner_Sequential_Invariant
     (Obj : Test_Runner_Sequential'Class) return Boolean;

private

   use Test_Reporter_Class;

   type Test_Runner_Sequential
     is limited new Children_Early_Test_Handler with record

      Check_Cond_Run_Done : Boolean := False;

      Child_Access : not null access Test_Node_Interfa'Class;

      Reporter_Access : access Test_Reporter_Interfa'Class;

   end record;

end Apsepp.Test_Node_Class.Runner_Sequential;
