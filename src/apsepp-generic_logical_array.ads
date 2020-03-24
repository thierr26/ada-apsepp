-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

generic

   type Index_Type is (<>);

package Apsepp.Generic_Logical_Array is

   type Logical_Array is array (Index_Type range <>) of Boolean;

   -- TODOC: Stops on first false element.
   function All_True (A : Logical_Array) return Boolean
     is (for all E of A => E);

   -- TODOC: Stops on first true element.
   function Some_True (A : Logical_Array) return Boolean
     is (for some E of A => E);

end Apsepp.Generic_Logical_Array;
