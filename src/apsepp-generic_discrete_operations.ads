-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

generic

   type Discrete_Type is (<>);

   type Diff_Type is range <>;

package Apsepp.Generic_Discrete_Operations is

   pragma Pure (Apsepp.Generic_Discrete_Operations);

   function Diff (X_1, X_2 : Discrete_Type) return Diff_Type
     is (Diff_Type (Discrete_Type'Pos (X_2) - Discrete_Type'Pos (X_1)));

   function Dist (X_1, X_2 : Discrete_Type) return Diff_Type
     is (abs (Diff (X_1, X_2)))
     with Post => Dist'Result >= 0;

   function Rank (Idx, First : Discrete_Type) return Diff_Type
     is (Diff (First, Idx) + 1)
     with Post => Idx < First or else Rank'Result >= 1;

   function Val (Rk    : Diff_Type;
                 First : Discrete_Type) return Discrete_Type
     is (Discrete_Type'Val (Discrete_Type'Pos (First) - 1 + Rk))
     with Post => Rk < 1 or else Val'Result >= First;

end Apsepp.Generic_Discrete_Operations;
