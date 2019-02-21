-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Test_Reporter_Class.Instant_Standard.Create;
  use Apsepp.Test_Reporter_Class.Instant_Standard;
with Apsepp_Scope_Bound_Locks_Test_Case;
  use Apsepp_Scope_Bound_Locks_Test_Case;

package Apsepp_Test_Harness is

   procedure Apsepp_Test_Procedure;

private

   Reporter : aliased Test_Reporter_Instant_Standard := Create;

   Scope_Bound_Locks_T_C : aliased Apsepp_Scope_Bound_Locks_T_C;

end Apsepp_Test_Harness;
