-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

-- Force run-time pre-condition check in this unit.
pragma Assertion_Policy (Pre => Check);

-- TODOC: Only tests value 'LT' and 'EQ' of the comparison function result.
-- <2020-04-13>
generic
function Apsepp.Generic_Lexicographic_Comparisons.Lower_Than
  (F                       : not null Comparison_Func;
   First_Index, Last_Index : Index_Type) return Boolean
  with Pre => Last_Index >= First_Index;
