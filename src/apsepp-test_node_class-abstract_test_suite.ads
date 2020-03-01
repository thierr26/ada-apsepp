-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Test_Node_Class.Abstract_Children_Early_Test_Handler;
  use Apsepp.Test_Node_Class.Abstract_Children_Early_Test_Handler;

package Apsepp.Test_Node_Class.Abstract_Test_Suite is

   type Test_Node_Array is array (Test_Node_Index range <>)
     of not null access Test_Node_Interfa'Class;

   -- TODO: Check if zero child would be valid. <2020-02-26>
   type Test_Suite
     is abstract limited new Children_Early_Test_Handler with private
     with Type_Invariant'Class =>
            Test_Suite.Child_Count >= 1
              and then
            Test_Suite.Child_Array_Equiv_To_Child
              and then
            Test_Suite.Has_Early_Test;

   not overriding
   function Child_Array (Obj : Test_Suite) return Test_Node_Array
     is abstract;

   overriding
   function Child_Count (Obj : Test_Suite) return Test_Node_Count
     is (Test_Suite'Class (Obj).Child_Array'Length);

   overriding
   function Child (Obj : Test_Suite;
                   K   : Test_Node_Index)
     return not null access Test_Node_Interfa'Class;

   overriding
   function No_Subtasking (Obj : Test_Suite) return Boolean
     is (False);

   overriding
   procedure Run (Obj     : in out Test_Suite;
                  Outcome :    out Test_Outcome;
                  Kind    :        Run_Kind     := Assert_Cond_And_Run_Test);

   not overriding
   function Child_Array_Equiv_To_Child (Obj : Test_Suite) return Boolean;

private

   type Test_Suite
     is abstract limited new Children_Early_Test_Handler with null record;

end Apsepp.Test_Node_Class.Abstract_Test_Suite;
