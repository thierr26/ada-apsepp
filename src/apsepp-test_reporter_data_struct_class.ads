-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

package Apsepp.Test_Reporter_Data_Struct_Class is

   type Event_Count is new Natural;

   subtype Event_Index is Event_Count range 1 .. Event_Count'Last;

end Apsepp.Test_Reporter_Data_Struct_Class;
