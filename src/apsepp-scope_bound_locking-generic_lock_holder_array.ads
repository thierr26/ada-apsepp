-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

generic

   type Index_Type is (<>);

package Apsepp.Scope_Bound_Locking.Generic_Lock_Holder_Array is

   type Lock_Holder_Array
     is array (Index_Type) of not null access Lock_Holder'Class;

   function None_Locked (A : Lock_Holder_Array) return Boolean
     is (for all L_H of A => not L_H.L.Locked);

   function All_Holding (A : Lock_Holder_Array) return Boolean
     is (for all L_H of A => L_H.Holds);

end Apsepp.Scope_Bound_Locking.Generic_Lock_Holder_Array;
