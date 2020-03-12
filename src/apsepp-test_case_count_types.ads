-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package Apsepp.Test_Case_Count_Types is

   type Test_Routine_Count is new Natural;

   subtype Test_Routine_Index
     is Test_Routine_Count range 1 .. Test_Routine_Count'Last;

   type Test_Assert_Count is new Natural;

   subtype Test_Assert_Index
     is Test_Assert_Count range 1 .. Test_Assert_Count'Last;

end Apsepp.Test_Case_Count_Types;
