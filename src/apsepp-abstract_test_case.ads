-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Test_Node_Class.Case_Stub; use Apsepp.Test_Node_Class.Case_Stub;
  use Apsepp.Test_Node_Class;

package Apsepp.Abstract_Test_Case is

   type Test_Routine_Array
     is array (Test_Routine_Index range <>) of Test_Routine;

   type Test_Case is abstract limited new Test_Case_Stub with private;

   not overriding
   function Routine_Array (Obj : Test_Case) return Test_Routine_Array
     is abstract;

   overriding
   function Routine_Count (Obj : Test_Case) return Test_Routine_Count
     is (Test_Case'Class (Obj).Routine_Array'Length);

   overriding
   function Routine (Obj : Test_Case;
                     K   : Test_Routine_Index) return Test_Routine
     is (Test_Case'Class (Obj).Routine_Array (K));

   procedure Run_Body (Obj     :     Test_Node_Interfa'Class;
                       Outcome : out Test_Outcome;
                       Kind    :     Run_Kind;
                       Cond    :     not null access function return Boolean)
     renames Test_Node_Class.Case_Stub.Run_Body;

private

   type Test_Case is abstract limited new Test_Case_Stub with null record;

end Apsepp.Abstract_Test_Case;
