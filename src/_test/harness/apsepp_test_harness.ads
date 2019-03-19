-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

private with Apsepp.Test_Reporter_Class.Instant_Standard,
             Apsepp_Test_Suite;

package Apsepp_Test_Harness is

   procedure Apsepp_Test_Procedure;

private

   use Apsepp.Test_Reporter_Class.Instant_Standard;
   use Apsepp_Test_Suite;

   Reporter   : aliased Test_Reporter_Instant_Standard;
   Test_Suite : aliased Apsepp_T_S;

end Apsepp_Test_Harness;
