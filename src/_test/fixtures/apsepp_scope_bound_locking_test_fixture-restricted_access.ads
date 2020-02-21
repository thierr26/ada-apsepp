-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Generic_Restricted_Access_Fixture,
     Apsepp.Scope_Bound_Locking;

package Apsepp_Scope_Bound_Locking_Test_Fixture.Restricted_Access is

   package Apsepp_Scope_Bound_Locking_T_F_Restr
     is new Apsepp.Generic_Restricted_Access_Fixture
     (Fixture_Type => Apsepp_Scope_Bound_Locking_T_F);

end Apsepp_Scope_Bound_Locking_Test_Fixture.Restricted_Access;
