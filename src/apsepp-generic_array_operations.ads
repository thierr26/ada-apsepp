-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

generic

   type Index_Type is (<>);

   type Element_Type is private;

   type Array_Type is array (Index_Type range <>) of Element_Type;

package Apsepp.Generic_Array_Operations is

   -- Don't do run-time pre-condition check in this package. (Let
   -- 'Constraint_Error' be raised in case of pre-condition violation.)
   pragma Assertion_Policy (Pre => Ignore);

   function First_Succ (A : Array_Type) return Index_Type
     is (Index_Type'Succ (A'First));

   function Last_Pred (A : Array_Type) return Index_Type
     is (Index_Type'Pred (A'Last));

   function Pred (A : Array_Type;
                  K : Index_Type) return Element_Type
     is (A (Index_Type'Pred (K)));

   function No_Duplicates
     (A : Array_Type) return Boolean
     is (for all K_1 in A'Range =>
          (for all K_2 in A'Range =>
            K_1 = K_2
              or else
            A(K_1) /= A(K_2)));

end Apsepp.Generic_Array_Operations;
