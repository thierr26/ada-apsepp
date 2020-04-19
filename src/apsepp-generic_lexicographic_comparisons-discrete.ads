-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

generic

   type Discrete_Type is (<>);

function Apsepp.Generic_Lexicographic_Comparisons.Discrete
  (Left, Right : Discrete_Type) return Comparison_Outcome;
