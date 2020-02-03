-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package body Apsepp.Abstract_Test_Suite is

   overriding
   function Routine (Obj : Test_Suite;
                     K   : Test_Routine_Index) return Test_Routine
     is (Null_Test_Routine'Access);

end Apsepp.Abstract_Test_Suite;
