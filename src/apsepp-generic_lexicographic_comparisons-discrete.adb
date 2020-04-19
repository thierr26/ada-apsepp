-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

function Apsepp.Generic_Lexicographic_Comparisons.Discrete
  (Left, Right : Discrete_Type) return Comparison_Outcome is

begin

   return (if Left < Right then
              LT
           elsif Left = Right then
              EQ
           else
              GT);

end Apsepp.Generic_Lexicographic_Comparisons.Discrete;
