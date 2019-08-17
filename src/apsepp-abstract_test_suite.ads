-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Test_Node_Class.Suite_Stub; use Apsepp.Test_Node_Class.Suite_Stub;
  use Apsepp.Test_Node_Class;

package Apsepp.Abstract_Test_Suite is

   type Test_Node_Array
     is array (Test_Node_Index range <>) of Test_Node_Access;

   type Test_Suite is abstract limited new Test_Suite_Stub with private;

   not overriding
   function Child_Array (Obj : Test_Suite) return Test_Node_Array
     is abstract;

   overriding
   function Child_Count (Obj : Test_Suite) return Test_Node_Count
     is (Test_Suite'Class (Obj).Child_Array'Length);

   overriding
   function Child (Obj : Test_Suite;
                   K   : Test_Node_Index) return Test_Node_Access
     is (Test_Suite'Class (Obj).Child_Array (K));

   overriding
   function Routine (Obj : Test_Suite;
                     K   : Test_Routine_Index) return Test_Routine;

   procedure Run_Body (Obj     :     Test_Node_Interfa'Class;
                       Outcome : out Test_Outcome;
                       Kind    :     Run_Kind;
                       Cond    :     not null access function return Boolean)
     renames Test_Node_Class.Suite_Stub.Run_Body;

private

   type Test_Suite is abstract limited new Test_Suite_Stub with null record;

end Apsepp.Abstract_Test_Suite;
