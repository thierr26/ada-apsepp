-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

generic

   type Return_Type is private;

package Apsepp.Generic_Array_Operations.W_F_Elem is

   -- TODOC: Implementations favors memory over speed (no storage of 'F (E)'
   -- values). <2020-03-09>
   function No_Duplicates
     (F : not null access function (E : Element_Type) return Return_Type;
      A : Array_Type) return Boolean
     is (for all K_1 in A'Range =>
          (for all K_2 in A'Range =>
            K_1 = K_2
              or else
            F (A(K_1)) /= F (A(K_2))));

end Apsepp.Generic_Array_Operations.W_F_Elem;
