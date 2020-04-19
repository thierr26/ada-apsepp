-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

function Apsepp.Generic_Lexicographic_Comparisons.Lower_Than
  (F                       : not null Comparison_Func;
   First_Index, Last_Index : Index_Type) return Boolean is

   F_Outcome : Comparison_Outcome;
   Ret       : Boolean;

begin

   for Index in First_Index .. Last_Index loop

      F_Outcome := F (Index);

      Ret := F_Outcome = LT;
      exit when Ret;     -- Early exit.

      Ret := F_Outcome = EQ;
      exit when not Ret; -- Early exit.

      -- We get there only if 'F_Outcome = EQ'.

      -- Setting 'Ret' to false here is useful in the 'Index = Last_Index'
      -- case. Without that, the function would return true although the
      -- comparison outcome is 'EQ'.
      Ret := False;

   end loop;

   return Ret;

end Apsepp.Generic_Lexicographic_Comparisons.Lower_Than;
