-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

-- TODOC: This package relies on the ''Pos' attribute, which converts values of
-- signed integer types to 'Universal_Integer' and makes possible comparisons
-- of values of different types with no risk of getting a 'Constraint_Error'
-- exception. <2020-03-31>
-- REF: https://en.wikibooks.org/wiki/Ada_Programming/Type_System#Elaborated_Discussion_of_Types_for_Signed_Integer_Types. <2020-03-31>
-- REF: "Programming in Ada 2012" by John Barnes, section 6.8. <2020-03-31>
generic

   type Alternative_Type is (<>);

package Apsepp.Generic_Safe_Integer_Operations.Conversions is

   function Is_Over (X : Safe_Integer) return Boolean
     is (
          Integer_Type'Pos (Val (X))
            >
          Alternative_Type'Pos (Alternative_Type'Last)
        );

end Apsepp.Generic_Safe_Integer_Operations.Conversions;
