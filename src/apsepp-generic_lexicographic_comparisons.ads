-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

generic

   type Index_Type is (<>);

package Apsepp.Generic_Lexicographic_Comparisons is

   type Comparison_Outcome is (LT, EQ, GT);

   type Comparison_Func
     is access function (Index : Index_Type) return Comparison_Outcome;

end Apsepp.Generic_Lexicographic_Comparisons;
