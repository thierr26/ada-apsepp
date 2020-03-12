-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Generic_Restricted_Access_Fixture;

package Apsepp_Testing_System_Test_Fixture.Restricted_Access is

   package Apsepp_Testing_System_T_F_Restr
     is new Apsepp.Generic_Restricted_Access_Fixture
     (Fixture_Type => Apsepp_Testing_System_T_F);

end Apsepp_Testing_System_Test_Fixture.Restricted_Access;
