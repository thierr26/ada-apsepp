-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

generic
   type Discrete_Type is (<>);
package Apsepp.Generic_Discrete_Operations is

   -- Don't evaluate the pre-conditions in this package. Just let
   -- Constraint_Error be raised in case of violation.
   pragma Assertion_Policy (Pre => Ignore);

   function Fi return Discrete_Type
     is (Discrete_Type'First);

   function La return Discrete_Type
     is (Discrete_Type'Last);

   function Pr (X : Discrete_Type) return Discrete_Type
     is (Discrete_Type'Pred (X))

     with Pre => X > Fi;

   function Su (X : Discrete_Type) return Discrete_Type
     is (Discrete_Type'Succ (X))

     with Pre => X < La;

end Apsepp.Generic_Discrete_Operations;
