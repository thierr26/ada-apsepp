-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

function Apsepp.Generic_Discrete_Operations.Is_Lim_Array_Wo_Dup
  (A : Array_Type) return Boolean is

  function Are_Not_Dups (Left, Right : Discrete_Type) return Boolean
    is (Element_Func (A(Left)) /= Element_Func (A(Right)));

begin

   return (for all K_1 in A'Range =>
            (for all K_2 in Discrete_Type'Succ (K_1) .. A'Last =>
              Are_Not_Dups (K_1, K_2)));

end Apsepp.Generic_Discrete_Operations.Is_Lim_Array_Wo_Dup;
